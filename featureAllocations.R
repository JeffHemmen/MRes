FACE$TYPE = "DIPHTHONG"
FACE$FEATURE.NAME = "FACE"
FACE$POP[[1]] = c("lvp", "brm", "sse", "ilo", "ean")
FACE$POP[[2]] = c("gla", "shl", "uls", "roi", "ncl", "lan", "eyk", "crn", "nwa")

GOAT$TYPE = "DIPHTHONG"
GOAT$FEATURE.NAME = "GOAT"
GOAT$POP[[1]] = c("lvp", "brm", "sse", "ilo", "ean")
GOAT$POP[[2]] = c("gla", "shl", "uls", "roi", "ncl", "lan", "eyk", "crn", "nwa")

STRUT.FOOT$TYPE = "DISTANCE"
STRUT.FOOT$FEATURE.NAME = "STRUT.FOOT"
STRUT.FOOT$POP[[1]] = c("ncl", "lan", "lvp", "eyk", "brm")
STRUT.FOOT$POP[[2]] = c("gla", "shl", "uls", "roi", "crn", "sse", "ilo", "ean", "nwa")

GOOSE.FOOT$TYPE = "DISTANCE"
GOOSE.FOOT$FEATURE.NAME = "FOOT.GOOSE"
GOOSE.FOOT$POP[[1]]= c("gla", "shl", "uls")
GOOSE.FOOT$POP[[2]] = c("ncl", "eyk", "lvp", "lan", "nwa", "brm", "roi", "ean", "crn", "sse", "ilo")

BATH$TYPE = "RATIO"
BATH$FEATURE.NAME = "BATH"
BATH$POP[[1]] = c("roi", "sse", "ilo", "ean")
BATH$POP[[2]] = c("ncl", "lan", "lvp", "eyk", "brm", "nwa")

PALM.TRAP$TYPE = "DISTANCE"
PALM.TRAP$FEATURE.NAME = "PALM.TRAP"
PALM.TRAP$POP[[1]] = c("la", "shl", "uls", "crn")
PALM.TRAP$POP[[2]] = c("roi", "sse", "ilo", "ean", "ncl", "lan", "lvp", "eyk", "brm", "nwa")

numFeatures = 5
featureList = list(FACE, GOAT, STRUT.FOOT, GOOSE.FOOT, BATH, PALM.TRAP)