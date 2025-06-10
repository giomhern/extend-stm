# nolint start

# --- Libraries ---

library(stm)
library(MCMCpack)
library(ggplot2)
library(topicmodels)
library(slam)
library(Matrix)

# ------ Figure 2 ------

# --- Parameters ---

set.seed(123)
K <- 3 # Number of topics
V <- 500 # Vocabulary size
Ns <- seq(100, 1000, 100) # Sample sizes
zeta <- 40 # Expected words per document (Poisson)
ATE_true <- 0.2 # True ATE size
G0 <- 1 / 3 # Concentration parameter for Dirichlet

# --- Helper Functions ---

# Generate topic-word distributions (beta) from Dirichlet(.05)
generate_beta <- function(K, V) {
    beta <- matrix(0, nrow = K, ncol = V)
    for (k in 1:K) {
        beta[k, ] <- as.numeric(rdirichlet(1, rep(0.05, V)))
    }
    beta
}

# Simulate documents, with treatment assigned to half
simulate_docs <- function(N_docs, zeta, alpha_control, alpha_treated, beta) {
    docs <- list()
    treatment <- c(rep(0, N_docs / 2), rep(1, N_docs / 2))
    treatment <- sample(treatment) # Randomize order

    for (i in 1:N_docs) {
        alpha <- if (treatment[i] == 1) alpha_treated else alpha_control
        theta <- as.numeric(rdirichlet(1, alpha))
        Nd <- rpois(1, zeta)
        words <- integer(Nd)
        for (j in 1:Nd) {
            z <- sample(1:K, 1, prob = theta)
            w <- sample(1:V, 1, prob = beta[z, ])
            words[j] <- w
        }
        docs[[i]] <- words
    }
    list(docs = docs, treatment = treatment)
}

# Convert each document to STM format (table of word indices and counts)
convert_to_stm_format <- function(doc, vocab) {
    tab <- table(doc)
    word_indices <- match(names(tab), vocab) - 1 # STM expects 0-based indices
    rbind(word_indices, as.integer(tab))
}

# Robust extraction of effect row from STM summary
extract_effect_row <- function(topic_effect, treat_row) {
    if (!treat_row %in% rownames(topic_effect)) {
        return(NULL)
    }

    est <- topic_effect[treat_row, "Estimate"]
    se <- topic_effect[treat_row, "Std. Error"]
    lower <- est - 1.96 * se
    upper <- est + 1.96 * se

    return(c(
        Estimate = est,
        Lower = lower,
        Upper = upper
    ))
}

# --- Simulation & Estimation Loop ---

results <- data.frame(N = integer(), ATE = numeric(), lower = numeric(), upper = numeric(), condition = character())

for (effect_type in c("No Treatment Effect", "Treatment Effect")) {
    for (N in Ns) {
        cat(sprintf("\n--- N = %d, Condition = %s ---\n", N, effect_type))

        beta <- generate_beta(K, V)
        if (effect_type == "No Treatment Effect") {
            alpha_control <- c(0.3, 0.4, 0.3)
            alpha_treated <- c(0.3, 0.4, 0.3)
        } else {
            alpha_control <- c(0.3, 0.4, 0.3)
            alpha_treated <- c(0.3 - ATE_true, 0.4, 0.3 + ATE_true)
        }

        sim <- simulate_docs(N, zeta, alpha_control, alpha_treated, beta)
        meta_df <- data.frame(treatment = factor(sim$treatment))
        vocab <- as.character(1:V)
        docs_stm <- lapply(sim$docs, convert_to_stm_format, vocab = vocab)

        out <- tryCatch(
            prepDocuments(docs_stm, vocab = vocab, meta = meta_df, verbose = FALSE),
            error = function(e) {
                cat("prepDocuments failed\n")
                return(NULL)
            }
        )
        if (is.null(out)) next

        fit <- tryCatch(
            stm(out$documents, out$vocab, K = K, prevalence = ~treatment, data = out$meta, verbose = FALSE, init.type = "Spectral"),
            error = function(e) {
                cat("STM model failed\n")
                return(NULL)
            }
        )
        if (is.null(fit)) next

        effect <- tryCatch(
            estimateEffect(1:K ~ treatment, fit, meta = out$meta, uncertainty = "Global"),
            error = function(e) {
                cat("estimateEffect failed\n")
                return(NULL)
            }
        )
        if (is.null(effect)) next

        # --- Select best topic dynamically ---
        eff_summary <- summary(effect)
        topic_ates <- sapply(eff_summary$tables, function(tab) {
            if ("treatment1" %in% rownames(tab)) {
                return(abs(tab["treatment1", "Estimate"]))
            } else {
                return(NA)
            }
        })

        best_topic <- which.max(topic_ates)
        if (!is.na(best_topic)) {
            topic_effect <- eff_summary$tables[[best_topic]]
            if ("treatment1" %in% rownames(topic_effect)) {
                vals <- extract_effect_row(topic_effect, "treatment1")
                if (!is.null(vals)) {
                    results <- rbind(results, data.frame(
                        N = N,
                        ATE = as.numeric(vals["Estimate"]),
                        lower = as.numeric(vals["Lower"]),
                        upper = as.numeric(vals["Upper"]),
                        condition = effect_type
                    ))
                    cat(sprintf("✓ Success (Topic %d)\n", best_topic))
                } else {
                    cat("⚠️ Skipped: could not compute CI from selected topic\n")
                }
            } else {
                cat("⚠️ Skipped: treatment1 row missing from best topic\n")
            }
        } else {
            cat("⚠️ Skipped: could not select best topic\n")
        }
    }
}

# --- Plotting ---
results_no <- subset(results, condition == "No Treatment Effect")
results_yes <- subset(results, condition == "Treatment Effect")
# Save to PNG file
pdf("output/validation/figure2_stm_ate.pdf", width = 12, height = 6)

# Setup plotting layout
par(mfrow = c(1, 2), mar = c(5, 4, 4, 2) + 0.1)

# Plot function
plot_ate_panel <- function(data, main_title, true_line) {
    x_jitter <- jitter(data$N, amount = 10)
    plot(data$N, data$ATE,
        ylim = c(-0.3, 0.5),
        pch = 1,
        xlab = "Number of Documents", ylab = "ATE",
        main = main_title,
        type = "n"
    )
    segments(x0 = x_jitter, y0 = data$lower, y1 = data$upper, col = "gray40")
    points(x_jitter, data$ATE, pch = 1, col = "black")
    abline(h = true_line, col = "red", lty = 2)
}

# Generate plots
plot_ate_panel(results_no, "No Treatment Effect", 0)
plot_ate_panel(results_yes, "Treatment Effect", ATE_true)

# Finish writing the PNG file
dev.off()


# nolint end
