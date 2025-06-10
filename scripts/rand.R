# nolint start
library(stm)

set.seed(123)

dataldac <- readLdac("data/final_tdm.csv")
vocabcoarse <- read.csv("data/tdm1011_vocab.csv")$term
meta <- read.csv("data/tdm1011_metadata.csv")
meta$treatment <- as.numeric(meta$condition_number == 10)

data <- prepDocuments(dataldac, vocabcoarse, meta = meta)
documents <- data$documents
vocab <- data$vocab
meta <- data$meta

model <- stm(documents, vocab, 5, prevalence = ~ meta$treatment)
labelTopics(model)
effect <- estimateEffect(1:5 ~ treatment, stmobj = model, metadata = meta, uncertainty = "Global")

top_words <- labelTopics(model, n = 20)
topic3_words <- strwrap(paste(top_words$frex[3, ], collapse = ", "), width = 60)
topic4_words <- strwrap(paste(top_words$frex[4, ], collapse = ", "), width = 60)
topic5_words <- strwrap(paste(top_words$frex[5, ], collapse = ", "), width = 60)

# --- Figure 11 ---
pdf("output/rand/figure11_topics_effect.pdf", width = 12, height = 6)

layout(matrix(1:2, nrow = 1), widths = c(0.9, 1.1)) # Adjust panel widths
par(mar = c(0, 0, 0, 0))

# LEFT: Topic 1 + Topic 4 words box
plot.new()
plot.window(xlim = c(0, 1), ylim = c(0, 1))
rect(0.1, 0.2, 0.9, 0.8)
segments(0.1, 0.5, 0.9, 0.5, lty = "dashed")

text(0.5, 0.73, "Topic 3:", font = 1, adj = 0.5)
y1 <- 0.70
for (line in topic3_words) {
    text(0.5, y1, line, cex = 0.9, adj = 0.5)
    y1 <- y1 - 0.03
}

text(0.5, 0.43, "Topic 4:", font = 1, adj = 0.5)
y2 <- 0.40
for (line in topic4_words) {
    text(0.5, y2, line, cex = 0.9, adj = 0.5)
    y2 <- y2 - 0.03
}

# RIGHT: plot.estimateEffect for topics 1 and 4
par(mar = c(5, 4, 2, 1)) # Reset margins for plot
plot.estimateEffect(
    effect,
    topics = c(3, 4),
    covariate = "treatment",
    method = "difference",
    cov.value1 = 0,
    cov.value2 = 1,
    xlab = "Difference in Topic Proportions (Treated-Control)",
    labeltype = "numbers",
    xlim = c(-.15, .15)
)

dev.off()

# --- Figure 12 ---
pdf("output/rand/figure12_fixed.pdf", width = 12, height = 6)

layout(matrix(1:2, nrow = 1), widths = c(0.9, 1.1))
par(mar = c(0, 0, 0, 0))

# LEFT: Topic 1 + Topic 5 word box
plot.new()
plot.window(xlim = c(0, 1), ylim = c(0, 1))
rect(0.1, 0.2, 0.9, 0.8)
segments(0.1, 0.5, 0.9, 0.5, lty = "dashed")

text(0.5, 0.73, "Topic 3:", font = 1, adj = 0.5)
y1 <- 0.70
for (line in topic3_words) {
    text(0.5, y1, line, cex = 0.9, adj = 0.5)
    y1 <- y1 - 0.03
}

text(0.5, 0.43, "Topic 5:", font = 1, adj = 0.5)
y2 <- 0.40
for (line in topic5_words) {
    text(0.5, y2, line, cex = 0.9, adj = 0.5)
    y2 <- y2 - 0.03
}

# RIGHT: plot.estimateEffect for topics 1 and 5
par(mar = c(5, 4, 2, 1))
plot.estimateEffect(
    effect,
    topics = c(3, 5),
    covariate = "treatment",
    method = "difference",
    cov.value1 = 0,
    cov.value2 = 1,
    xlab = "Difference in Topic Proportions (Treated-Control)",
    labeltype = "numbers",
    xlim = c(-.1, .1)
)

dev.off()

meta$normalized.contribution <- as.numeric(as.character(meta$normalized.contribution))
effect_norm <- estimateEffect(
    formula = c(3, 4) ~ normalized.contribution,
    stmobj = model,
    metadata = meta,
    documents = documents,
    uncertainty = "Global"
)

# Plot normalized contributions
pdf("output/rand/figure13_normalized_contributions.pdf", width = 8, height = 5)
plot.estimateEffect(
    effect_norm,
    covariate = "normalized.contribution",
    method = "continuous",
    topics = c(3, 4), # Replace with desired topic numbers
    xlab = "Mean Topic Proportions",
    ylab = "Mean Normalized Contributions",
    labeltype = "numbers",
    linecol = c("red", "blue"),
    xlim = c(0.05, 0.6),
    ylim = c(0.1, 0.3)
)
dev.off()

meta$gender <- factor(ifelse(meta$female == 1, "female", "male"))

# Assuming your STM model was fit like this:
model_with_content <- stm(
    documents = documents,
    vocab = vocab,
    K = 5,
    prevalence = ~ meta$treatment,
    content = ~ meta$gender,
    init.type = "Spectral"
)

pdf("output/rand/figure15_gender_vocab.pdf", width = 7, height = 6)
plot(model_with_content,
    type = "perspectives",
    topics = 1,
    covariate = "gender",
    cov.value1 = "female",
    cov.value2 = "male",
    text.cex = 1.1,
    plabels = c("Female", "Male")
)
dev.off()

# nolint end
