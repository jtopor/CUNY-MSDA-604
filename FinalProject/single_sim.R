# Single sim wrapped in function
source("setup.R")   # these need to be in the same folder...
source("methods.R")

single_sim = function(nvols=1000, nshelves=50, shelf_width=36, sfree_space=3, 
                      textb_masters=0.05, reshelf_thresh=5, seed_val= NULL){
    
    set.seed(seed_val)   # specify seed
    data = setup(nvols, nshelves, shelf_width, sfree_space, textb_masters) # returns list, need to set to two dfs
    
    #init books and shelves
    books = data$books
    shelves = data$shelves
    delbks <- data$delbks
    
    counter = 0
    full_count = 0
    reshelf_thresh = 5
    
    while(T){
        ### SIM CODE
        books = weeding(books,0.02)
        # need to update available shelf space after weeding
        shelves= update_shelves(books,shelves,36)
        
        # purchase books - functions create data frames containing new books
        # to be added to collection
        new_texts = purchase_books('t',3,6)
        new_ntexts = purchase_books('n',5,26)
        
        # append data frame containing new_ntexts to books data frame
        books = add_non_textbooks(books,new_ntexts,shelves) # needs logic for checking shelf availability
        shelves= update_shelves(books,shelves,36)
        
        # append data frame containing new_texts to books data frame
        books = add_textbooks(books,new_texts,shelves) # needs logic for checking shelf availability
        shelves= update_shelves(books,shelves,36)
        
        books = check_ins(books,0.2) # needs logic for checking shelf availability
        shelves= update_shelves(books,shelves,36)
        
        books = check_outs(books,0.02)
        shelves= update_shelves(books,shelves,36)
        
        books = de_dup(books)
        shelves= update_shelves(books,shelves,36)
        ####
        
        full_count = sum(shelves$full)       # get number full 
        if(full_count >= reshelf_thresh)     # break if >= threshold
            break
        counter = counter + 1
    }
    
    # clear memory of data, books, shelves
    rm(data, books, shelves)
    
    # The counter value is number of rounds  
    # return(counter)
    return(list(iters = counter, delbks = delbks))
}
