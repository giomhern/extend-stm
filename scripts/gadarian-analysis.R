# nolint start
library(lda)
library(slam)
library(dplyr)
library(stm)
library(readr)
setwd("/Users/giomhern/04 Projects/topic-models")

# stm::gadarianFit

# docs <- readLdac("data/gadarian_final_tdm.csv")
# vocabs <- read.csv("data/gadarian_vocab.csv")$term
# meta <- read.csv("data/gadarian_metadata.csv")
# meta$treatment <- as.numeric(meta$treat == "1. worried")
# data <- prepDocuments(docs, vocabs, meta = meta)
# documents <- data$documents
# vocab <- data$vocab
# meta <- data$meta

# mod <- stm(documents, vocab, 3, prevalence = ~ meta$treatment, seed = 1234)
# labelTopics(mod)

# png("output/gadarian/topics.png", width = 800, height = 600)
# plot(mod, type = "labels", topics = 1:2, labeltype = "frex", n = 30)
# dev.off()

# thoughts <- findThoughts(model, texts = meta$open.ended.response, topics = 1:3, n = 10)
# topic1_indices <- thoughts$index[["Topic 1"]]
# png("output/gadarian/topic1_quote1.png", width = 600, height = 400)
# plotQuote(meta$open.ended.response[topic1_indices[1]])
# dev.off()

# topic2_indices <- thoughts$index[["Topic 2"]]
# png("output/gadarian/topic2_quote1.png", width = 600, height = 400)
# plotQuote(meta$open.ended.response[topic2_indices[7]])
# dev.off()

data(gadarianFit, package = "stm")
data(gadarian)

# Treatment Effect Plot (Updated method)
png("output/gadarian/treatment_effect.png", width = 800, height = 600)
treatment_effect <- estimateEffect(
    c(1:2) ~ treatment,
    gadarianFit,
    metadata = gadarian
)
plot(treatment_effect,
    covariate = "treatment",
    method = "difference",
    cov.value1 = 0,
    cov.value2 = 1,
    xlab = "Difference in Topic Proportions (Treated-Control)",
    labeltype = "numbers"
)
dev.off()

# Interaction Plot (Modern approach)
png("output/gadarian/interaction_plot.png", width = 800, height = 600)
interaction_effect <- estimateEffect(
    c(1:2) ~ treatment * pid_rep,
    gadarianFit, # or gadarianFit
    metadata = meta,
    documents = documents,
    uncertainty = "Global"
)

# First line: Treated
plot.estimateEffect(
    interaction_effect,
    covariate = "pid_rep",
    moderator = "treatment",
    moderator.value = 1,
    method = "continuous",
    topics = 1,
    model = model,
    linecol = "red",
    xlab = "Party ID",
    ylab = "Topic Proportion",
    xaxt = "n",
    main = "Predicted Proportion in Topic 1",
    printlegend = FALSE,
    ylim = c(0, 0.6)
)

# Second line: Control (added)
plot.estimateEffect(
    interaction_effect,
    covariate = "pid_rep",
    moderator = "treatment",
    moderator.value = 0,
    method = "continuous",
    topics = 1,
    model = model,
    linecol = "blue",
    add = TRUE,
    printlegend = FALSE
)

# Custom X-axis
axis(
    1,
    at = c(0, 0.5, 1),
    labels = c("Strong\nDemocrat", "Moderate", "Strong\nRepublican"),
    las = 1,
    cex.axis = 0.9,
    padj = 0.5
)

# Custom Legend
legend(
    "topleft",
    legend = c("Treated", "Control"),
    col = c("red", "blue"),
    lty = 1,
    lwd = 2,
    bty = "n",
    cex = 1.1
)
dev.off()

thoughts <- findThoughts(
    gadarianFit,
    texts = gadarian$open.ended.response,
    topics = 1:2,
    n = 10,
    meta = gadarian
)

png("topic1_quotes.png", width = 600, height = 400)
plotQuote(thoughts$docs[[1]][5], width = 40)
dev.off()

png("topic2_quotes.png", width = 600, height = 400)
plotQuote(thoughts$docs[[2]][1], width = 40)
dev.off()

topic_effects <- estimateEffect(
    1 ~ treatment,
    gadarianFit,
    metadata = gadarian
)
summary(topic_effects)

png("output/gadarian/topic_labels.png", width = 600, height = 600)
plot(gadarianFit, type = "labels")
dev.off()


# nolint end
