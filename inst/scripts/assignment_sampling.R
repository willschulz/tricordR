# assignment sampler

library(tricordR)
tokens <- prepTokens("ws_tw", 1:9)
sampleEnglishTweeters5k(study_name = "spirals_assignment_sampling", token = tokens[[1]], minutes = 5)
