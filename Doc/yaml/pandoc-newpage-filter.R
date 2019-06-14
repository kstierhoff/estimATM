#!/usr/bin/env Rscript

json_in <- file('stdin', 'r')

lat_newp <- '{"t":"RawBlock","c":["latex","\\\\newpage"]}'

doc_newp <- '{"t":"RawBlock","c":["openxml","<w:p><w:r><w:br w:type=\\"page\\"/></w:r></w:p>"]}'

ast <- paste(readLines(json_in, warn = FALSE), collapse = "\n")

ast <- gsub(lat_newp, doc_newp, ast, fixed = TRUE)

write(ast, "")

# Use \newpage in your document to specify page breaks (just like LaTeX!)
# Put this script in the directory you are compiling your R Markdown in.
# Use it in you YAML like this:
#---
#title: "Title
#author: "Author"
#output:  
#  word_document:
#    pandoc_args: [
#      "--filter", "pandoc-newpage-filter.R"
#    ]
#---
# You will have page breaks in your word doc!