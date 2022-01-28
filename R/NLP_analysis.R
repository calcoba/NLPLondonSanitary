library(utf8)
library(spacyr)
library(kableExtra)

#spacy_install()
#spacy_download_langmodel('en')
spacy_initialize(model = "en_core_web_sm")


urlSanitaryLondon <- "https://www.gutenberg.org/cache/epub/47308/pg47308.txt"
lines <- readLines(urlSanitaryLondon,
                   encoding = "UTF-8")
grep(pattern="***", lines, fixed = TRUE)

grep(pattern = "THE health of the people", lines, fixed = TRUE)

linesQ <- lines[-c(1:155)]

length(linesQ)
length(lines)

linesQ[1:5]

linesQ[!utf8_valid(linesQ)]

linesQ_NFC <- utf8_normalize(linesQ)
sum(linesQ_NFC != linesQ)

stringQ <- paste(linesQ, collapse = "\n")
paragraphs <- unlist(strsplit(stringQ, "\\n\\n\\n"))
parEmpty <- which(paragraphs == '')
#paragraphs <- paragraphs[-parEmpty]
length(paragraphs)
substring(paragraphs[1], 1, 200)

gsub("[\n]{1,}", " ", c(par1="with one \nbut also \n",
                        par2="with a seq of \n\nlike this"))

paragraphswoNL <- gsub("[\n]{1,}", " ", paragraphs)
substring(paragraphswoNL[1], 1, 200)

paragraphs <- gsub("[ ]{2,}", " ", paragraphswoNL)
substring(paragraphs[1], 1, 200)

phrases <- spacy_tokenize(paragraphs, what = "sentence")

v_phrases <- unlist(phrases)
numphrases <- length(v_phrases)
sum(v_phrases=='')
#v_phrases <- v_phrases[-which(v_phrases=="")]

hist(nchar(v_phrases),
     main = 'Histogram of sentence size',
     xlab = 'Sentence size (number of characters',
     ylab = 'Ocurrences')

tokens <- spacy_tokenize(paragraphs)
v_tokens <- unlist(tokens)
v_tokens[1:10]
length(v_tokens)
length(unique(v_tokens))
head(sort(table(v_tokens), decreasing = TRUE), n = 25)
plot(head(sort(table(v_tokens), decreasing = TRUE), n = 10),
     xlab = "Token",
     ylab = "Ocurrences")


tic <- Sys.time()
res <- lapply(v_phrases,
              spacy_parse,
              dependency = TRUE, nounphrase = TRUE
)

df <- res[[1]]
for (i in 2:length(res)){
  df <- rbind(df, res[[i]])
}

Sys.time()-tic

kable_styling(kable(df[1000:5000, c(3:ncol(df))]),
              font_size = 15
)

length(df)
spacy_finalize()
sessionInfo()
