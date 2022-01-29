library(utf8)
library(spacyr)
library(kableExtra)
library(coreNLP)
library(tokenizers.bpe)
library(quanteda)
library(quanteda.textstats)
library(quanteda.textplots)
library(ggplot2)

#spacy_install()
#spacy_download_langmodel('en')
spacy_initialize(model = "en_core_web_sm")


urlSanitaryLondon <- "https://www.gutenberg.org/cache/epub/47308/pg47308.txt"
lines <- readLines(urlSanitaryLondon,
                   encoding = "UTF-8")

grep(pattern="***", lines, fixed = TRUE)

grep(pattern = "THE health of the people", lines, fixed = TRUE)

grep(pattern = "INDEX", lines, fixed = TRUE)

linesQ <- lines[(155:17644)]

length(linesQ)
length(lines)

linesQ[1:5]
linesQ[17485:17490]

linesQ[!utf8_valid(linesQ)]

linesQ_NFC <- utf8_normalize(linesQ)
sum(linesQ_NFC != linesQ)

stringQ <- paste(linesQ, collapse = "\n")

paragraphs <- unlist(strsplit(stringQ, "\\n\\n\\n"))
which(paragraphs == '')

paragraphswoNL <- gsub("[\n]{1,}", " ", paragraphs)
paragraphs <- gsub("[ ]{2,}", " ", paragraphswoNL)
paragraphsClean <- gsub("[\\\"]{1,}", "", paragraphs)

footnotes_indexes = list()
for (string in paragraphsClean){
  footnote_index = unlist(gregexpr(pattern = "FOOTNOTES", string, fixed = TRUE))
  footnotes_indexes <- append(footnotes_indexes, footnote_index-1)
}

cleanChapters = list(substring(paragraphsClean[1], 1),
                    substring(paragraphsClean[2], 21),
                    substring(paragraphsClean[3], 22),
                    substring(paragraphsClean[4], 21),
                    substring(paragraphsClean[5], 20, footnotes_indexes[5]),
                    substring(paragraphsClean[7], 1, footnotes_indexes[7]),
                    substring(paragraphsClean[9], 1, footnotes_indexes[9]),
                    substring(paragraphsClean[11], 1, footnotes_indexes[11]),
                    substring(paragraphsClean[13], 1, footnotes_indexes[13]),
                    substring(paragraphsClean[15], 1, footnotes_indexes[15]),
                    substring(paragraphsClean[17], 1, footnotes_indexes[17])
                    )

cleanChaptersObj <- as.character(cleanChapters)

phrases <- spacy_tokenize(cleanChaptersObj, what = "sentence")
v_phrases <- unlist(phrases)
numphrases <- length(v_phrases)
sum(v_phrases=='')
#v_phrases <- v_phrases[-which(v_phrases=="")]
hist(nchar(v_phrases),
     main = 'Histogram of sentence size',
     xlab = 'Sentence size (number of characters',
     ylab = 'Ocurrences')

tokens <- spacy_tokenize(cleanChaptersObj,
                         remove_punct = TRUE)
v_tokens <- unlist(tokens)
v_tokens[1:10]
length(v_tokens)
length(unique(v_tokens))
head(sort(table(v_tokens), decreasing = TRUE), n = 25)
plot(head(sort(table(v_tokens), decreasing = TRUE), n = 10),
     xlab = "Token",
     ylab = "Ocurrences")


#downloadCoreNLP()

model <- bpe(unlist(cleanChaptersObj[1:7]), coverage = 0.9999, vocab_size = 7000)

subtoks2 <- bpe_encode(model, x = cleanChaptersObj[8:11], type='subwords')
head(unlist(subtoks2), n=20)

texts_caps <- unlist(cleanChaptersObj)
names(texts_caps) <- paste('Chap. ', 1:length(texts_caps))
corpus_capsQ <- corpus(texts_caps)
docvars(corpus_capsQ, field="Chapter") <- 1:length(texts_caps)
corpus_capsQ

dfm_capsQ <- dfm(tokens(corpus_capsQ),
                 tolower = TRUE)

distMatrix <- dist(as.matrix(dfm_capsQ),
                   method="euclidean")
groups <- hclust(distMatrix, method = "ward.D")

plot(groups,
     cex =1, #Size of labels
     hang= -1, #Same hight labels
     xlab = "", #Text of axis x
     ylab = "", #Text of axis y
     main = "" #Text of drawing
)
rect.hclust(groups, k=4)

topfeatures(dfm_capsQ)
dfm_capsQ_1 <- dfm(tokens(corpus_capsQ,
                          remove_punct = TRUE),
                   )

dfm_capsQ_2 <- dfm_remove(dfm_capsQ_1, stopwords("en"))
topfeatures(dfm_capsQ_2)

topfeatures(dfm_capsQ_2, decreasing = FALSE)

corpus_parts<- list()
dfm_parts_noPunct <- list()
dfm_parts_noPunct_noSW <- list()

for (i in 1:length(corpus_capsQ)){

  corpus_chap <- corpus_subset(corpus_capsQ,
                                Chapter == i)
  corpus_parts[[i]] <- corpus_chap
  dfm_parts_noPunct[[i]] <- dfm(tokens(corpus_chap, remove_punct=TRUE))
  dfm_parts_noPunct_noSW[[i]] <- dfm_remove(dfm_parts_noPunct[[i]],
                                            stopwords("en"))
}

for (i in 1:length(dfm_parts_noPunct_noSW)){
  print(paste("Top frecuent features for Chap.", i))
  print(topfeatures(dfm_parts_noPunct_noSW[[i]]))
  features_dfm <- textstat_frequency(dfm_parts_noPunct_noSW[[i]], n=50)

  features_dfm$feature <- with(features_dfm, reorder(feature, -frequency))

  print(ggplot(features_dfm, aes(x = feature, y = frequency)) +
          ggtitle(paste("Chapter:", i)) +
          geom_point() +
          theme(axis.text.x = element_text(angle = 90, hjust = 1)))
}

for (i in 1:length(dfm_parts_noPunct_noSW)){
  print(paste("Top less frecuent features for Chap.", i))
  print(topfeatures(dfm_parts_noPunct_noSW[[i]], decreasing = FALSE))
}


compare_corpus <- corpus_subset(corpus_capsQ, Chapter %in% c("11", "10"))

compare_dfm <- tokens(compare_corpus, remove_punct = TRUE) %>%
  tokens_remove(stopwords("en")) %>%
  tokens_group(groups = Chapter) %>%
  dfm()

result_keyness <- textstat_keyness(compare_dfm, target = "11")

textplot_keyness(result_keyness)



spacy_finalize()
sessionInfo()
