library(utf8)
library(spacyr)
library(kableExtra)
library(coreNLP)
library(tokenizers.bpe)
library(quanteda)
library(quanteda.textstats)
library(quanteda.textplots)
library(ggplot2)

set.seed(0)
#spacy_install()
#spacy_download_langmodel('en')
if (file.exists('Images')){
  print('Directory already exist')
} else {
  dir.create('Images')
}
spacy_initialize(model = "en_core_web_sm")

# Loading the book from the online repository
urlSanitaryLondon <- "https://www.gutenberg.org/cache/epub/47308/pg47308.txt"
lines <- readLines(urlSanitaryLondon,
                   encoding = "UTF-8")

grep(pattern="***", lines, fixed = TRUE)

# Identify the start and end of the text we are interesed in and select those
# lines
grep(pattern = "THE health of the people", lines, fixed = TRUE)

grep(pattern = "INDEX", lines, fixed = TRUE)

linesQ <- lines[(155:17644)]

# Display the firsts and last lines to check we have selected the part we are
# interested in and check that all characters are UTF8
linesQ[1:5]
linesQ[17485:17490]

linesQ[!utf8_valid(linesQ)]

linesQ_NFC <- utf8_normalize(linesQ)
sum(linesQ_NFC != linesQ)

# Generate a single string with the complete book and divide each in chapters
stringQ <- paste(linesQ, collapse = "\n")

chapters <- unlist(strsplit(stringQ, "\\n\\n\\n"))
which(chapters == '')

# Different preprocessing for cleaning the text. Deleting footnotes and
# merguing the different parts of the first chapter
chapterswoNL <- gsub("[\n]{1,}", " ", chapters)
chapters <- gsub("[ ]{2,}", " ", chapterswoNL)
chaptersSeparated <- gsub("[\\\"]{1,}", "", chapters)

footnotes_indexes = list()
for (string in chaptersSeparated){
  footnote_index = unlist(gregexpr(pattern = "FOOTNOTES", string, fixed = TRUE))
  footnotes_indexes <- append(footnotes_indexes, footnote_index-1)
}
chapter1 <- paste(substring(chaptersSeparated[1], 1),
                  substring(chaptersSeparated[2], 21),
                  substring(chaptersSeparated[3], 22),
                  substring(chaptersSeparated[4], 21),
                  substring(chaptersSeparated[5], 20, footnotes_indexes[5]))

cleanChapters = list(chapter1,
                    substring(chaptersSeparated[7], 1, footnotes_indexes[7]),
                    substring(chaptersSeparated[9], 1, footnotes_indexes[9]),
                    substring(chaptersSeparated[11], 1, footnotes_indexes[11]),
                    substring(chaptersSeparated[13], 1, footnotes_indexes[13]),
                    substring(chaptersSeparated[15], 1, footnotes_indexes[15]),
                    substring(chaptersSeparated[17], 1, footnotes_indexes[17])
                    )

cleanChaptersObj <- as.character(cleanChapters)

# Count the number of characters for each of the senteces. Globally and for each
# chapter. Save a histogram for each of them.
phrases <- spacy_tokenize(cleanChaptersObj, what = "sentence")
v_phrases <- unlist(phrases)
numphrases <- length(v_phrases)
sum(v_phrases=='')

png(file="Images/Sentences_size.png", width=1200, height=700)
hist(nchar(v_phrases),
     main = 'Histogram of sentence size',
     xlab = 'Sentence size (number of characters)',
     ylab = 'Ocurrences')
dev.off()

for (i in 1:length(cleanChaptersObj)){
  chapterPhrases <- spacy_tokenize(cleanChaptersObj[[i]], what = "sentence")
  chapterV_phrases <- unlist(chapterPhrases)
  png(file=paste("Images/Sentences_size_chapter", i, ".png"),
      width=1200, height=700)
  hist(nchar(chapterV_phrases),
       main = paste('Histogram of sentence size Chapters)', i),
       xlab = 'Sentence size (number of characters',
       ylab = 'Ocurrences')
  dev.off()
}

# Generate the corpus for each of the chapters.
texts_caps <- unlist(cleanChaptersObj)
names(texts_caps) <- paste('Chap. ', 1:length(texts_caps))
corpus_capsQ <- corpus(texts_caps)
docvars(corpus_capsQ, field="Chapter") <- 1:length(texts_caps)

# Compute the distance between the chapters. Generate a dendogram and save it.
dfm_capsQ <- dfm(tokens(corpus_capsQ),
                 tolower = TRUE)

distMatrix <- dist(as.matrix(dfm_capsQ),
                   method="euclidean")
groups <- hclust(distMatrix, method = "ward.D")

png(file=paste("Images/Distances_dendogram.png"),
    width=1200, height=700)
plot(groups,
     cex =1, #Size of labels
     hang= -1, #Same hight labels
     xlab = "", #Text of axis x
     ylab = "", #Text of axis y
     main = "" #Text of drawing
)
rect.hclust(groups, k=4)
dev.off()

# Compute the most used words globally and for each of the chapters. For each
# of the chapters also save a plot representing this frequency analysis.
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

# Display the word analysis for each chapter. Generate a plot and save it.
for (i in 1:length(dfm_parts_noPunct_noSW)){
  print(paste("Top frecuent features for Chap.", i))
  print(topfeatures(dfm_parts_noPunct_noSW[[i]]))
  features_dfm <- textstat_frequency(dfm_parts_noPunct_noSW[[i]], n=50)

  features_dfm$feature <- with(features_dfm, reorder(feature, -frequency))

  png(file=paste("Images/Frequent_word_chapter", i, ".png"),
      width=1200, height=700)
  print(ggplot(features_dfm, aes(x = feature, y = frequency)) +
        ggtitle(paste("Chapter:", i)) +
        geom_point() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)))
  dev.off()
}

# Display the less frequent words for each of the chapters.
for (i in 1:length(dfm_parts_noPunct_noSW)){
  print(paste("Top less frecuent features for Chap.", i))
  print(topfeatures(dfm_parts_noPunct_noSW[[i]], decreasing = FALSE))
}

# Generate a keyness analysis for each pair and chapter. Generate a plot and
# save it.
displayed = list()
`%!in%` <- Negate(`%in%`)
for (i in 1:length(corpus_capsQ)){
  for (j in 1:length(corpus_capsQ)){
    if (i!=j && paste(i,j) %!in% displayed){
      displayed <- append(displayed, paste(i,j))
      displayed <- append(displayed, paste(j,i))
      compare_corpus <- corpus_subset(corpus_capsQ, Chapter %in% c(i, j))

      compare_dfm <- tokens(compare_corpus, remove_punct = TRUE) %>%
        tokens_remove(stopwords("en")) %>%
        tokens_group(groups = Chapter) %>%
        dfm()

      result_keyness <- textstat_keyness(compare_dfm, target = as.character(i))
      keyness_plot <- textplot_keyness(result_keyness)
      png(file=paste("Images/Keyness_Chapter", i, "_Chapter", j, ".png"),
          width=1200, height=700)
      print(keyness_plot)
      dev.off()

    }

  }
}

spacy_finalize()
sessionInfo()

