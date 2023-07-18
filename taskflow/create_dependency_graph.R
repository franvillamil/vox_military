# setwd("~/Documents/Projects/cuarteles_militares")
if(!"stringr" %in% rownames(installed.packages())){install.packages("stringr")}
library(stringr)
# -------------------------

setwd("~/Documents/Projects/cuarteles_militares")
library(stringr)

# Load makefile
l = readLines(file("Makefile"))

# Remove comments
l = l[!grepl("^#", l)]

# Select lines with target & dependencies
l = l[grepl(".*/.*: .*/", l)]

# Get targets
t = gsub("/.*", "", l)

# Get dependencies
d = lapply(str_split(gsub(".*: ", "", l), " "),
  function(x) gsub("/.*", "", x))

# Repeat targets
t = rep(t, unlist(lapply(d, function(x) length(x))))

# Create matrix with repeated targets
m = matrix(c(unlist(d), t), ncol = 2)

# Drop duplicates and same target > output
m = unique(m[m[,1] != m[,2],])

# Dependencies
d_lines = apply(m, 1, paste, collapse = "->")

# Inverse order
# d_lines = d_lines[length(d_lines):1]

# Write file
writeLines(c("digraph G {", d_lines, "}"), file("taskflow/dependency_list.txt"))

# # Call shell command
# system(paste0("cd ", paste(unlist(str_split(getwd(), " ")), collapse = "\\ ")))
# system("dot -Grankdir=LR -Tpdf taskflow/dependency_list.txt -o taskflow/workflow.pdf")
# system("sips -s format jpeg taskflow/workflow.pdf --out taskflow/workflow.jpeg")
