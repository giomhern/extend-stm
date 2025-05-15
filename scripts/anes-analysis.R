# nolint start
library(lda)
library(slam)
library(dplyr)
library(stm)
library(readr)
setwd("/Users/giomhern/04 Projects/topic-models")


documents <- readLdac("data/final_anes.csv")
meta <- read.csv("data/final_anes_metadata.csv")
vocab <- read.csv("data/final_anes_vocab.csv")$term
documents <- documents[meta$pid_summary > 0 & meta$highest.grade.completed > 0 & meta$age > 0]
meta <- meta[meta$pid_summary > 0 & meta$highest.grade.completed > 0 & meta$age > 0, ]

data <- prepDocuments(documents = documents, vocab = vocab, meta = meta)
documents <- data$documents
vocab <- data$vocab
meta <- data$meta
stm_mod <- stm(documents, vocab,
    K = 60,
    prevalence = ~ s(pid_summary) + s(age) + s(`highest.grade.completed`) + s(`highest.grade.completed` * pid_summary),
    data = meta,
    init.type = "Spectral",
    seed = 1234
)

labelTopics(stm_mod, n = 10)

png("output/stm_summary.png", width = 800, height = 1000)
plot(stm_mod, type = "summary", n = 2)
dev.off()

table(meta$pid_summary)

effect_model <- estimateEffect(
    formula = ~ highest.grade.completed * pid_summary,
    stmobj = stm_mod,
    metadata = meta,
    documents = documents,
    uncertainty = "Global"
)

if (!dir.exists("output")) dir.create("output")

png("output/war-topic-education-interaction.png", width = 800, height = 600)
plot.estimateEffect(
    effect_model,
    covariate = "highest.grade.completed",
    model = stm_mod,
    method = "continuous",
    topics = 40,
    moderator = "pid_summary",
    moderator.value = 1,
    linecol = "blue",
    main = "STM War Topic and Education",
    ylim = c(0, 0.1),
    xlim = c(13, 17),
    xlab = "Years of Education", ylab = "Probability of Topic",
    printlegend = FALSE
)

plot.estimateEffect(
    effect_model,
    covariate = "highest.grade.completed",
    model = stm_mod,
    method = "continuous",
    topics = 40,
    moderator = "pid_summary",
    moderator.value = 4,
    linecol = "red",
    add = TRUE
)

text(16.2, 0.085, "Democrat", col = "blue", cex = 1.2)
text(14.1, 0.035, "Republican", col = "red", cex = 1.2)
dev.off()

# nolint end
