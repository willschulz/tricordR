# assignment sampler

library(tricordR)
tokens <- prepTokens("ws_tw", 1:9)

Sys.sleep(round(runif(1, 60, 1800)))
sampleEnglishTweeters5k(study_name = "spirals_assignment_sampling", token = tokens[[1]], minutes = 5)
