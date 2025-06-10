# nolint start
library(stm)

# 1. Extract Top Words (Figure 4)
top_words <- labelTopics(gadarianFit, n = 20)
topic1_words <- strwrap(paste(top_words$frex[1, ], collapse = ", "), width = 60)
topic2_words <- strwrap(paste(top_words$frex[2, ], collapse = ", "), width = 60)

top_words

pdf("output/gadarian/figure4_vocab_topics_centered.pdf", width = 6, height = 6)
par(mar = c(0, 0, 0, 0))
plot.new()
plot.window(xlim = c(0, 1), ylim = c(0, 1))

rect(0.1, 0.2, 0.9, 0.8)
segments(0.1, 0.5, 0.9, 0.5, lty = "dashed")

text(0.5, 0.73, "Topic 1:", font = 1, adj = 0.5)
y1 <- 0.70
for (line in topic1_words) {
    text(0.5, y1, line, cex = 0.9, adj = 0.5)
    y1 <- y1 - 0.035
}

text(0.5, 0.43, "Topic 2:", font = 1, adj = 0.5)
y2 <- 0.40
for (line in topic2_words) {
    text(0.5, y2, line, cex = 0.9, adj = 0.5)
    y2 <- y2 - 0.035
}

dev.off()

thought <- findThoughts(gadarianFit, texts = gadarian$open.ended.response, n = 10)

# 2. Extract Representative Responses (Figure 5 & 6)
pdf("output/gadarian/figure5_topic1_twoquotes.pdf", width = 8, height = 6)
par(mar = c(1, 1, 1, 1))
plot.new()
plot.window(xlim = c(0, 1), ylim = c(0, 1))

rect(0.05, 0.05, 0.95, 0.95)
segments(0.05, 0.5, 0.95, 0.5, lty = "dashed")
# Quote 1
text1 <- strwrap(thought$docs[[1]][6], width = 60)
y_start1 <- 0.82
for (line in text1) {
    text(0.5, y_start1, line, cex = 0.9)
    y_start1 <- y_start1 - 0.04
}

# Quote 2
text2 <- strwrap(thought$docs[[1]][4], width = 60)
y_start2 <- 0.35
for (line in text2) {
    text(0.5, y_start2, line, cex = 0.9)
    y_start2 <- y_start2 - 0.04
}
dev.off()

str(thought)

# Topic 2 responses
pdf("output/gadarian/figure6_topic2_twoquotes.pdf", width = 5, height = 4)
par(mar = c(1, 1, 1, 1))
plot.new()
plot.window(xlim = c(0, 1), ylim = c(0, 1))

# Box
rect(0.05, 0.05, 0.95, 0.95)
segments(0.05, 0.5, 0.95, 0.5, lty = "dashed")

# Quote 3
text3 <- strwrap(thought$docs[[2]][5], width = 40)
y_start3 <- 0.75
for (line in text3) {
    text(0.5, y_start3, line, cex = 0.9)
    y_start3 <- y_start3 - 0.04
}

head(gadarian)

# Quote 4
text4 <- strwrap(thought$docs[[2]][3], width = 40)
y_start4 <- 0.40
for (line in text4) {
    text(0.5, y_start4, line, cex = 0.9)
    y_start4 <- y_start4 - 0.04
}
dev.off()

# 3. Figure 7
treatment_effect <- estimateEffect(
    1:2 ~ treatment,
    gadarianFit,
    metadata = gadarian,
    uncertainty = "Global"
)
topic_labels <- c("Topic 1", "Topic 2")

pdf("output/gadarian/figure7_treatment_effect_custom.pdf", width = 10, height = 6)
plot(
    treatment_effect,
    covariate = "treatment",
    method = "difference",
    cov.value1 = 0,
    cov.value2 = 1,
    xlab = "Difference in Topic Proportions (Treated-Control)",
    labeltype = "custom",
    custom.labels = topic_labels,
    xlim = c(-0.1, 0.1)
)
dev.off()


temp <- textProcessor(documents = gadarian$open.ended.response, metadata = gadarian)
meta <- temp$meta

# 4. Figure 9
interaction_effect <- estimateEffect(
    formula = c(1) ~ treatment * pid_rep,
    gadarianFit, # or gadarianFit
    metadata = gadarian,
    uncertainty = "Global"
)

pdf("output/gadarian/figure8_partyid_treatment.pdf", width = 6, height = 5)

# Plot for Treated group (treatment = 1)
plot.estimateEffect(
    interaction_effect,
    covariate = "pid_rep",
    moderator = "treatment",
    moderator.value = 1,
    method = "continuous",
    topics = 1,
    model = gadarianFit,
    linecol = "red",
    ylab = "Topic Proportion",
    xaxt = "n",
    printlegend = FALSE,
    labeltype = "prob",
    ylim = c(0.2, 0.3),
)

# Plot for Control group (treatment = 0)
plot.estimateEffect(
    interaction_effect,
    covariate = "pid_rep",
    moderator = "treatment",
    moderator.value = 0,
    method = "continuous",
    topics = 1,
    model = gadarianFit,
    linecol = "blue",
    add = TRUE,
    printlegend = FALSE,
    labeltype = "prob"
)

# Custom x-axis
axis(
    1,
    at = c(0, 0.5, 1),
    labels = c("Strong\nDemocrat", "Moderate", "Strong\nRepublican"),
    padj = 0.5
)

# Custom legend
legend(
    "topright",
    legend = c("Treated", "Control"),
    col = c("red", "blue"),
    lty = 1,
    lwd = 2,
    bty = "n"
)

dev.off()
# nolint end
