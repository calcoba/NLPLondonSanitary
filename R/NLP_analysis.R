# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'
library(utf8)


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
