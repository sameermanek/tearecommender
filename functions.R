# Functions for tea

# First, load necessary packages
library(data.table)
library(stringr)
library(lsa)
# library(devtools)
# install_github('sameermanek/mmisc')
library(mmisc) # This is a personal package (not in the CRAN)
# it can be installed w/ the two lines above

# Get the data from various sources
# Little clunky because there are different data formats
# Will return a list containing the data from the appropriate sources
# @param chr.source one or more of the sources named in the default
# @param log.cache Should I access the cached version of these results? 
#   This is included for web deployment -- leave it FALSE to ensure data is appropriate.
# @return Returns list of data.tables with the data from named sources
getData <- function(chr.source = c('david','tealuxe','teavana'), log.cached = FALSE) {
  
  # Do absolute basic checking on inputs.
  chr.source <- tolower(as.character(chr.source))
  chr.source <- chr.source[!is.na(chr.source)]
  chr.sources.available <- c('david','tealuxe','teavana')
  chr.source <- unique(sort(chr.source[chr.source %in% chr.sources.available]))
  if(length(chr.source) == 0) stop('Incorrectly specified sources')
  
  log.cached <- as.logical(log.cached)
  log.cached <- coalesce(log.cached[1],FALSE)
  
  if(log.cached == TRUE && 
     all(chr.source %in% chr.sources.available) && 
     all(chr.sources.available %in% chr.source) && 
     file.exists('data_cache.rds')) return(readRDS('data_cache.rds'))
  
  
  lst.data <- lapply(chr.source, function(x) {
    if(x == 'david') {
      # Read data
      dt.data <- fread('../data/David\'s Tea.csv', check.names = TRUE)
      
      # Get rid of extraneous (meaningless?) columns
      dt.data <- dt.data[, list(image_link = `product_image`,
                                name = `product_link_1/_title`,
                                link = `product_link_2`,
                                description = `product_link_2/_text`)]
      dt.data[, vendor:='David\'s Tea']
      return(dt.data)
    } else if(x == 'tealuxe') {
      # Read the data
      dt.data <- fread('../data/Tealuxe.All.csv')
      dt.data <- dt.data[, list(image_link = `browseproductimagecontainer_link`,
                                name = `browseproductimagecontainer_link/_title`,
                                link = `browseproducttitle_link`,
                                description = `browseproductdescription_description`)]
      dt.data[, vendor:='Tealuxe']
      return(dt.data)
    } else if(x == 'teavana') {
      # Read the basic data
      dt.data <- fread('../data/Teavana Data - Basic Data.csv')
      dt.data <- dt.data[, list(image_link = NA_character_,
                                name = name,
                                link = str_match(url, "(http.*)\\?")[, 2], # Removing extraneous parameters.
                                description = combinedDescription,
                                review_score = review_score,
                                recommendation_score = recommendation_score)]
      
      # Now try to get review data
      dt.reviews <- fread('../data/Teavana_reviews.csv')
      setnames(dt.reviews, str_replace_all(names(dt.reviews), '-', '_'))
      dt.data[, vendor:='Teavana']
      
      # link these into one slightly unmanagable data table
      setnames(dt.reviews, 'url','link')
      # Clean up the link (i.e., remove extraneous parameters)
      dt.reviews[, link:=str_match(link, "(http.*)\\?")[, 2]]
      dt.reviews <- dt.reviews[, list(review = 
                                        list(
                                          data.table(title = review_title, 
                                                     body = review_body, 
                                                     score = review_score, 
                                                     reviewer = reviewer_name))), 
                               by=list(link)]

      # little silly (data tables w/in data.tables), but works.
      dt.data <- merge(dt.data, dt.reviews, 'link', all.x = TRUE)
      return(dt.data)
    } else {
      warning('Unrecognized data source') # should never actually get here.
      return(NULL)
    }
  })
  names(lst.data) <- chr.source
  if(all(chr.source %in% chr.sources.available) && 
     all(chr.sources.available %in% chr.source)) saveRDS(lst.data, 'data_cache.rds')
  return(lst.data)
}

# combine the basic data (i.e., only the simplest things)
# Turn it into a data.table
# @param lst.data is the output of getData()
# @return dt.data, a data.table of the manageable data from lst.data
combineData <- function(lst.data = getData()) {
  dt.data <- rbindlist(lst.data, use.names = TRUE, fill = TRUE)
  # Remove anything that is a weird data format (e.g. the list of data.frames)
  chr.remove <- names(dt.data)[!vapply(dt.data, function(x) (is.numeric(x) | is.character(x)), logical(1))]
  dt.data[, chr.remove:=NULL, with = FALSE]
  return(dt.data)
}

# dt.data <- combineData(lst.data)
# Compute a similarity matrix
# Clean the data a little bit and compare on name and description.
# Of note, the name is only compared to other names; name is not compared to description (and vice versa)
# Computes a similarity matrix based on tf.idf normalization 
# and then computing the cosine of each set of vectors.
# Sadly, I think I'll need to split by word (no obvious phrasing that I can see)
# @param dt.data The output of combineData()
#   really any data.table with columns `name`, `description` and `link` as an identifier
# @return a named numeric matrix with the similarity scores (names correspond to links)
computeDistanceMatrix <- function(dt.data = combineData()) {

  dt.descriptions <- dt.data[, list(name, description)]
  # clean out non-alphanumeric characters
  dt.descriptions[] <- lapply(dt.descriptions, str_replace_all, pattern = '-', replace = '') # concat things like full-bodied into fullbodied
  dt.descriptions[] <- lapply(dt.descriptions, str_replace_all, pattern = '[^[:alnum:]]+', replace = ' ')
  dt.descriptions[] <- lapply(dt.descriptions, str_replace_all, pattern = '[[:space:]]+', replace = ' ')
  dt.descriptions[] <- lapply(dt.descriptions, tolower)
  
  # Now seperate by word
  dt.descriptions[] <- lapply(dt.descriptions, str_split, pattern = ' ')
  
  # append the name of the column (I don't want cross-columnular comparisons)
  dt.descriptions[] <- lapply(names(dt.descriptions), 
                              function(x) {
                                lapply(dt.descriptions[, x, with = FALSE][[1]], paste0, toupper(x))
                              })
  
  # Add a column w/ all the words (the column is filled w/ character vectors)
  dt.descriptions[, descr:=lapply(1:nrow(dt.descriptions), 
                                  function(x) {
                                    unlist(dt.descriptions[x, ])
                                    })]
  
  lst.descriptions <- dt.descriptions$descr
  # Make a table of the docusment frequency and the the frequencies.
  # This is a smoothed inverse frequency. Higher numbers are less common.
  num.idf <- log(1+length(lst.descriptions)/table(unlist(lapply(lst.descriptions, unique))))
  num.idf <- num.idf[order(names(num.idf))]
  lst.df <- lapply(lst.descriptions, table)
  lst.tf.idf <- lapply(lst.df, function(x) x * num.idf[names(x)])
  
  # Apparently it is standard practice to calculate the cosine similarity; 
  # i.e., the cosine of the angle between the tf-idf based feature vectors.
  # Each row represents a term, each column a document
  mat.feature.vectors <- vapply(
    lst.tf.idf, 
    function(x) {
      coalesce(x[names(num.idf)],0)
    },
    numeric(length(num.idf)))

  # Attach meaningful names to rows (terms) and columns (data sources)
  rownames(mat.feature.vectors) <- names(num.idf)
  colnames(mat.feature.vectors) <- dt.data$link
  
  # Compute the cosine similarity from this set of vectors.
  mat.similarity <- cosine(mat.feature.vectors)
  return(mat.similarity)
}

# Convert the matrix from computeDistanceMatrix into a data.table
# @param mat.similarity is the output of computeDistanceMatrix
# @return a data.table with columns `tea1`,`tea2`,`similarity`
getSimilarityTable <- function(mat.similarity = computeDistanceMatrix()) {
  dt.distance <- as.data.table(mat.similarity, keep.rownames = TRUE)
  dt.distance <- melt(dt.distance, id.vars = c('rn'))
  setnames(dt.distance, c('rn','variable','value'), c('tea1','tea2','similarity'))
  dt.distance[, tea2:=as.character(tea2)]
  dt.distance <- dt.distance[order(tea1, -similarity)]
  return(dt.distance)
}

# A function to use a vector of 'liked' teas and 'disliked' teas to determine which 
# other teas may be liked (or disliked)
# @param chr.liked.teas the URLs of teas that are liked
# @param chr.disliked.teas the URLs of teas that are disliked
# @param dt.similarity The output of the getSimilarityTable function
# @return a data.table of teas that might be liked, rank ordered (liked -> disliked)
recommendTea <- function(chr.liked.teas=character(0), chr.disliked.teas=character(0), dt.similarity) {
  if(!is.character(chr.liked.teas)) chr.liked.teas <- character(0)
  if(!is.character(chr.disliked.teas)) chr.disliked.teas <- character(0)

  dt.teas <- rbind(
    data.table(tea1 = chr.liked.teas, like = rep(TRUE, length(chr.liked.teas))),
    data.table(tea1 = chr.disliked.teas, like = rep(FALSE, length(chr.disliked.teas)))
  )
  # If I have no input, return blank output in appropriate format.
  if(nrow(dt.teas) == 0) return(data.table(likelihood = numeric(0),
                                           most_similar_tea = character(0),
                                           least_similar_tea = numeric(0),
                                           recommendation = character(0),
                                           rank = integer(0)))

  dt.teas <- merge(dt.teas, dt.similarity, by=c('tea1'))
  # Deal with disliked teas by setting their likelihood (of being liked) 
  # to 1 - similarity score
  dt.teas[, likelihood:=ifelse(like, similarity, 1-similarity)]
  
  # Aggregate by recommendation. 
  # Here we're using mean to aggregate.
  # I tested using other aggregation functions (max, mean^2) 
  # and found very little difference.
  dt.teas.agg <- dt.teas[, list(likelihood = mean(likelihood),
                                most_similar_tea = tea1[which.max(similarity)],
                                least_similar_tea = tea1[which.min(similarity)]), 
                         by=list(recommendation = tea2)]
  

  # Remove inputs (would be circular to say you like a tea that you already said you like)
  dt.teas.agg <- dt.teas.agg[!recommendation %in% c(chr.liked.teas, chr.disliked.teas)]
  
  # Convert to Z-score
  # While I only use ranking for individuals, this could be useful 
  # for cross-person comparisons
  dt.teas.agg[, likelihood:=(likelihood - mean(likelihood))/sd(likelihood)]

  dt.teas.agg[, rank:=rank(-likelihood)]

  return(dt.teas.agg[order(rank)])
}
