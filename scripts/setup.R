# nolint start

load("data/gadarian.RData")
ls()

str(gadarian)
export_df <- gadarian[, c("MetaID", "treatment", "pid_rep", "open.ended.response")]
write.csv(export_df, "gadarian_bertopic_input.csv", row.names = FALSE)
# nolint end
