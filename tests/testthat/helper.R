toy_corpus <- "Once upon a time there was a tiny toy corpus.
It consisted of three sentences. And it lived happily ever after."

# for test-freqlist ====
flist <- freqlist(toy_corpus, as_text = TRUE)
subset_flist <- flist[c('once', 'lived', 'corpus')]

# for test-types ====
tps <- types(toy_corpus, as_text = TRUE)
chr <- c('one', 'one', 'two', 'three', 'two')

# for test-re ====
x <- tokenize("This is a sentence with a couple of words in it.")
pattern <- "[oe](.)(.)"

# for test-tokenize ====
ice_corpus <- "That ice-cream was extra-delicious, wasn't it?"
lines_corpus <- "
--start--
This
corpus
has
one
word
per
line
--end--
"
basic_tks <- tokenize(ice_corpus)
words_tks <- tokenize(ice_corpus, re_token_splitter = "\\W+")

# for test-conc ====
conc_data <- conc("A very small corpus", "\\w+", as_text = TRUE)
lines_text <- "
<text>
This is a text with more lines.
This is the second line.
And this is the third instance of This.
</text>
"

# for fnames ====
flist_fnames <- as_fnames(flist)
cwd_fnames <- as_fnames(c("folder/file1.txt", "folder/file2.txt", "folder/file3.txt"))
