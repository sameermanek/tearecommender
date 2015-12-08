library(shiny)
# This is the global.R file -- it will be run from both the UI and server.

#source('../functions.R')
source('functions.R')
lst.data <- getData(log.cached = TRUE)
dt.data <- combineData(lst.data)
mat.similarity <- computeDistanceMatrix(dt.data)
dt.similarity <- getSimilarityTable(mat.similarity)


chr.choices <- dt.data$link
names(chr.choices) <- paste0(dt.data$vendor, ': ', dt.data$name)
chr.choices <- chr.choices[order(names(chr.choices))]

# Eventually try to find the photo of the tea; for now I'm just filling blank images w/ a public domain stock photo
dt.data[, image_link:=coalesce(image_link, 'https://www.dropbox.com/s/nym6mfzpv7siv64/tee-1022443_640.jpg?dl=1')]