# Packages 
require(pdftools)
require(tm)

files <- list.files(pattern = "pdf$")
studies <- lapply(files, pdf_text)
length(studies) #number of files

# Create PDF database
pdfdatabase <- Corpus(URISource(files), readerControl = list(reader = readPDF))

studies.tdm <- TermDocumentMatrix(pdfdatabase, control = list(removePunctuation = T,
                                                              stopwords = T,
                                                              tolower = T,
                                                              stemming = F,
                                                              removeNumbers = T,
                                                              bounds = list(global = c(1, Inf))))

ft <- findFreqTerms(studies.tdm, lowfreq = 500, highfreq = Inf) 
ft.dm <- as.matrix(studies.tdm[ft,]) #find frequency of words across all docs
ft.dm.final <- sort(apply(ft.dm, 1, sum), decreasing = T)

write.csv(ft.dm.final, file = ".../Final Count of Words Across Abstracts.csv")
