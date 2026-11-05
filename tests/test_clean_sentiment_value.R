###############################################################################
# Tests for clean_sentiment_value() (src/92_llm_helper_funcs.R)
#
# Run from the project root:  Rscript tests/test_clean_sentiment_value.R
# Each row is a model answer and the score the parser must read from it
# (NA = answer rejected, the model is asked again).
###############################################################################

suppressMessages(library(stringr))
source("src/92_llm_helper_funcs.R")

cases <- tibble::tribble(
  ~answer,                                        ~expected,
  # Bare numbers
  "-0.5",                                         -0.5,
  "0.5",                                           0.5,
  "0",                                             0,
  "1",                                             1,
  "-1.0",                                         -1,
  "  0.3  \n",                                     0.3,
  ".5",                                            0.5,
  "-.5",                                          -0.5,
  # Formatted answers that the old parser read with the wrong sign
  "Sentiment: -0.5",                              -0.5,
  "**-0.5**",                                     -0.5,
  "-0.5.",                                        -0.5,
  "-0.5 (moderately negative)",                   -0.5,
  "`-0.3`",                                       -0.3,
  "Score : -0.7",                                 -0.7,
  # French decimal comma, read as 0 by the old parser
  "-0,5",                                         -0.5,
  "0,7",                                           0.7,
  "Note : -0,3",                                  -0.3,
  # Unicode minus and dashes
  "−0.5",                                    -0.5,
  "–0.4",                                    -0.4,
  # Several numbers: accepted only if exactly one is labelled
  "On a scale from -1.0 to 1.0, this is -0.4",    NA,
  "Sentiment: -0.4 (scale -1 to 1)",              -0.4,
  "-0.5 or 0.5",                                  NA,
  # Out of range or no number
  "5",                                            NA,
  "-2",                                           NA,
  "Neutral",                                      NA,
  "",                                             NA
)

got <- vapply(cases$answer, clean_sentiment_value, numeric(1), USE.NAMES = FALSE)
ok <- (is.na(got) & is.na(cases$expected)) |
      (!is.na(got) & !is.na(cases$expected) & abs(got - cases$expected) < 1e-9)

for (i in seq_len(nrow(cases))) {
  cat(sprintf("%s  %-45s -> %5s (expected %s)\n", if (ok[i]) "ok  " else "FAIL",
              encodeString(cases$answer[i], quote = '"'),
              format(got[i]), format(cases$expected[i])))
}
cat(sprintf("\n%d of %d cases pass\n", sum(ok), length(ok)))
if (!all(ok)) quit(status = 1)
