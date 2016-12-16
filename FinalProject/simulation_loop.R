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

counter = 0
full_count = 0
reshelf_thresh = 5

while(T){
    ### SIM CODE
    books = weeding(books,0.02)
    new_texts = purchase_books('t',3,6)
    new_ntexts = purchase_books('n',5,26)
    books = add_non_textbooks(books,new_ntexts,shelves)
    books = add_textbooks(books,new_texts,shelves)
    books = check_ins(books,0.2)
    books = check_outs(books,0.02)
    books = de_dup(books)
    shelves= update_shelves(books,shelves,36)
    ####
    
    full_count = sum(shelves$full)       # get number full 
    if(full_count >= reshelf_thresh)     # break if >= threshold
        break
    counter = counter + 1
}

# The counter value is number of rounds  
counter



