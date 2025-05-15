# nolint start
library(lda)
library(slam)
library(dplyr)
library(stm)
library(readr)
setwd("/Users/giomhern/04 Projects/topic-models")


meta <- read.csv("data/final_anes_metadata.csv")
vocab <- read.csv("data/final_anes_vocab.csv")$term

fix_ldac_file <- function(original_path, output_path) {
    lines <- readLines(original_path, warn = FALSE)
    writeLines(c(lines, ""), con = output_path)
}

read_ldac_custom <- function(path) {
    lines <- readLines(path)
    lapply(lines, function(line) {
        tokens <- strsplit(line, " ")[[1]]
        tokens <- tokens[-1] # remove token count
        pairs <- strsplit(tokens, ":")
        word_ids <- as.integer(sapply(pairs, `[[`, 1)) + 1
        counts <- as.integer(sapply(pairs, `[[`, 2))
        rbind(word_ids, counts)
    })
}

fix_ldac_file("data/final_anes.csv", "data/final_anes_cleaned.ldac")
documents <- read_ldac_custom("data/final_anes_cleaned.ldac")

labelTopics(stm_mod, n = 10)
keep <- with(meta, pid_summary > 0 & `highest.grade.completed` > 0 & age > 0)
documents <- documents[keep]
meta <- meta[keep, ]

non_empty_docs <- sapply(documents, function(x) {
    !is.null(x) && ncol(x) > 0 && sum(x[2, ]) > 0
})

documents <- documents[non_empty_docs]
meta <- meta[non_empty_docs, ]
out <- prepDocuments(documents, vocab, meta)
documents <- out$documents
vocab <- out$vocab
meta <- out$meta


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
    topics = 59,
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
    topics = 59,
    moderator = "pid_summary",
    moderator.value = 4,
    linecol = "red",
    add = TRUE
)

text(16.2, 0.085, "Democrat", col = "blue", cex = 1.2)
text(14.1, 0.035, "Republican", col = "red", cex = 1.2)
dev.off()


plot(stm_mod, type = "labels", labeltype = "frex", n = 10)
plot(stm_mod, type = "perspectives", topics = c(2, 4))

# nolint end
