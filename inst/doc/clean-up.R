#!/usr/bin/env Rscript

## deal with LyX filename mangling
x = readLines('cranvas-intro.tex')
idx = grep('\\\\documentclass', x)
if (idx > 1) x = x[-(1:(idx-1))]
idx = grep('\\\\bibliography\\{', x)
x[idx] = sub('\\{.*cranvas-intro._inst_doc_', '{', x[idx])
writeLines(x, 'cranvas-intro.tex')
file.rename('cranvas-intro.tex', 'cranvas-intro.Rnw')
## now we can cheat Sweave :-)
