#simulation 
setwd("~/Documents/CUNY/Simulation_604/Final_proj")

source("setup.R")
source("methods.R")

# standard setup params: 1000, 50, 36, 0.1, 0.05         boost free space
set.seed(1234)
data = setup(1000,50,36,0.1,0.05) # returns list, need to set to two dfs

#init books and shelves
books = data$books
shelves = data$shelves


# Sim Method order as outlined in v3.Rmd
# Weeding, Purchase (t and n) and shelve, check in, check out, dedupe

## RUN THIS BLOCK MANUALLY TO TEST##########
books = weeding(books,0.02)
new_texts = purchase_books('t',3,6)
new_ntexts = purchase_books('n',5,26)
books = add_non_textbooks(books,new_ntexts,shelves)
books = add_textbooks(books,new_texts,shelves)
books = check_ins(books,0.2)
books = check_outs(books,0.02)
books = de_dup(books)
shelves= update_shelves(books,shelves,36)
#########################################

data2 = setup(30,50,36,0.3,0.5)
books = data2$books

books$chkdout[c(11,13,26,27)] = 1
#books$dedupe[c(11,13,26,27)] = 1


# n number of textbooks to dedupe
n_dedup = round(runif(1,1,5))
print(n_dedup)
# get book ids for the master copies to be deduped 
master_ids = books[sample(which(books$copym==1),n_dedup),]$book_id
print(master_ids)
# set dedupe flags for any duplicates with chkdout=1; pull the ones with chkdout=0
books[which(books$dupeof %in% master_ids & books$chkdout==1), ]
books[which(books$dupeof %in% master_ids & books$chkdout==0), ]

if(length(books[which(books$dupeof %in% master_ids & books$chkdout==1), ]$dedupe > 0))
    books[which(books$dupeof %in% master_ids & books$chkdout==1), ]$dedupe = 1

if(nrow(books[which(books$dupeof %in% master_ids & books$chkdout==0), ])!=0)
    books = books[-which(books$dupeof %in% master_ids & !books$chkdout), ]









