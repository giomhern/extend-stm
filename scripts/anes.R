# nolint start
library(stm)
library(xtable)

documents <- readLdac("data/final_anes.csv")
meta <- read.csv("data/final_anes_metadata.csv")
vocab <- read.csv("data/final_anes_vocab.csv")$term
documents <- documents[meta$pid_summary > 0 & meta$highest.grade.completed > 0 & meta$age > 0]
meta <- meta[meta$pid_summary > 0 & meta$highest.grade.completed > 0 & meta$age > 0, ]


data <- prepDocuments(documents = documents, vocab = vocab, meta = meta)
documents <- data$documents
vocab <- data$vocab
meta <- data$meta

View(meta)
stm_stm_mod <- stm(documents, vocab,
    K = 60,
    prevalence = ~ s(pid_summary) + s(age) + s(`highest.grade.completed`) + s(`highest.grade.completed` * pid_summary),
    data = meta,
    init.type = "Spectral",
    seed = 1234
)

labelTopics(stm_stm_mod, n = 15)

pdf("output/anes/anes_topics_15.pdf", width = 7, height = 12)
plot(stm_stm_mod, type = "summary", n = 2)
dev.off()

meta$pid_group <- ifelse(meta$pid_summary < 3, 1, 2)

effect_stm_model <- estimateEffect(
    formula = ~ highest.grade.completed * pid_group,
    stmobj = stm_stm_mod,
    metadata = meta,
    documents = documents,
    uncertainty = "Global"
)

pdf("output/anes/interaction_plot_topic40.pdf", width = 8, height = 6)

plot.estimateEffect(
    effect_stm_model,
    covariate = "highest.grade.completed",
    stm_moderator = "pid_group",
    stm_moderator.value = 1, # For Democrats
    topics = 40,
    method = "continuous",
    stm_model = stm_stm_mod,
    linecol = "blue",
    xlab = "Years of Education",
    ylab = "Probability of Topic",
    ylim = c(0.02, 0.1),
    labeltype = "prob",
    xlim = c(13, 17),
    main = "STM War Topic and Education",
    printlegend = FALSE,
    ci.level = 0.9
)

plot.estimateEffect(
    effect_stm_model,
    covariate = "highest.grade.completed",
    stm_moderator = "pid_summary",
    stm_moderator.value = 2, # For Republicans
    topics = 40,
    method = "continuous",
    stm_model = stm_stm_mod,
    linecol = "red",
    labeltype = "prob",
    add = TRUE,
    ci.level = 0.9
)

# Add labels
text(16, 0.09, "Democrat", col = "blue", cex = 1.2)
text(14.5, 0.03, "Republican", col = "red", cex = 1.2)

dev.off()

anes_code <- 3

# Create binary indicator: 1 if any of the mippol1_codeX == anes_code, else 0
meta$warall <- as.integer(rowSums(meta[, paste0("mippol1_code", 1:8)] == anes_code, na.rm = TRUE) > 0)

# Subset by party ID
sub_dem <- meta$pid_summary < 3
sub_rep <- meta$pid_summary >= 3

# Open PDF to save plot
pdf("output/anes/anes_war_topic_loess.pdf", width = 8, height = 6)

# Democrats
dem_df <- meta[sub_dem, ]
fit_dem <- loess(warall ~ highest.grade.completed, data = dem_df, span = 2)
x_dem <- seq(min(dem_df$highest.grade.completed), max(dem_df$highest.grade.completed), length.out = 100)
pred_dem <- predict(fit_dem, newdata = data.frame(highest.grade.completed = x_dem), se = TRUE)

# Republicans
rep_df <- meta[sub_rep, ]
fit_rep <- loess(warall ~ highest.grade.completed, data = rep_df, span = 2)
x_rep <- seq(min(rep_df$highest.grade.completed), max(rep_df$highest.grade.completed), length.out = 100)
pred_rep <- predict(fit_rep, newdata = data.frame(highest.grade.completed = x_rep), se = TRUE)

# Plot
plot(x_dem, pred_dem$fit,
    type = "l", col = "blue",
    xlim = c(13, 17),
    ylim = c(0, .1),
    xlab = "Years of Education",
    ylab = "Probability of Topic",
    main = "ANES War Topic and Education"
)

lines(x_dem, pred_dem$fit + 1.64 * pred_dem$se, col = "blue", lty = 2)
lines(x_dem, pred_dem$fit - 1.64 * pred_dem$se, col = "blue", lty = 2)

lines(x_rep, pred_rep$fit, col = "red")
lines(x_rep, pred_rep$fit + 1.64 * pred_rep$se, col = "red", lty = 2)
lines(x_rep, pred_rep$fit - 1.64 * pred_rep$se, col = "red", lty = 2)

text(14, 0.01, "Republican", col = "red", cex = 1.2)
text(14.5, 0.08, "Democrat", col = "blue", cex = 1.2)

# Close PDF device
dev.off()


## tables human vs stm
# Helper to count docs with at least one topic > threshold
count_topic_match <- function(topic_ids, threshold = 0.15) {
    rowSums(stm_stm_mod$theta[, topic_ids] > threshold) > 0
}

# STM groupings
econ_topics <- c(56, 19, 20, 36, 39)
war_topics <- c(16, 40, 48)
dk_topics <- c(50, 30)
job_topics <- c(36, 39, 19, 48)

# STM counts
econ_stm <- sum(count_topic_match(econ_topics))
war_stm <- sum(count_topic_match(war_topics))
dk_stm <- sum(count_topic_match(dk_topics))
job_stm <- sum(count_topic_match(job_topics))

# Hand-coded counts
econ_hand <- sum(meta$mippol1_code1 %in% 50:55)
war_hand <- sum(meta$mippol1_code1 %in% c(3, 4))
dk_hand <- sum(meta$mippol1_code1 == 95)
job_hand <- sum(meta$mippol1_code1 == 27)

# Create comparison table
comparison_table <- data.frame(
    `STM Topic` = c("Economy", "War or Iraq War", "Don’t Know", "Unemployment and Job"),
    `STM Count` = c(econ_stm, war_stm, dk_stm, job_stm),
    `ANES Topic` = c("The Economy", "War, or Iraq War", "Don’t Know", "Employment"),
    `Hand-Coding Count` = c(econ_hand, war_hand, dk_hand, job_hand)
)

# Display (HTML for now)
library(xtable)
print(xtable(comparison_table, caption = "Comparison of STM to Hand Coding", align = "llrrr"), type = "html")
# nolint end
