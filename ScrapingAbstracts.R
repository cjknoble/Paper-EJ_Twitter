# Load required packages
library(pdftools)
library(tm)
library(rvest)
library(dplyr)

# Web Scrape
link = "https://scholar.google.com/scholar?q=environmental+justice&hl=en&num=20&as_sdt=0,33"
page = read_html(link)

title = page %>% html_nodes(".gs_rt a") %>% html_text()
author = page %>% html_nodes(".gs_a") %>% html_text()
abstract = page %>% html_nodes(".gs_rs") %>% html_text()

journals = data.frame(title, author, abstract, stringsAsFactors = FALSE)
journals$abstract[1]

# PDF Scrape
files <- list.files(pattern = "pdf$")
studies <- lapply(files, pdf_text)
length(studies) # number of files

# Create PDF database
pdfdatabase <- Corpus(URISource(files), readerControl = list(reader = readPDF))

studies.tdm <- TermDocumentMatrix(pdfdatabase, control = list(removePunctuation = TRUE,
                                                              stopwords = TRUE,
                                                              tolower = TRUE,
                                                              stemming = FALSE,
                                                              removeNumbers = TRUE,
                                                              bounds = list(global = c(1, Inf))))

# Find frequent terms
ft <- findFreqTerms(studies.tdm, lowfreq = 500, highfreq = Inf) 
ft.dm <- as.matrix(studies.tdm[ft,]) # find frequency of words across all docs
ft.dm.final <- sort(apply(ft.dm, 1, sum), decreasing = TRUE)

# Save the final count of words across abstracts to a CSV file
output_file <- "Final_Count_of_Words_Across_Abstracts.csv"
write.csv(ft.dm.final, file = output_file)
