
# Utility functions

# helper function that duplicates textbooks and assigns ids
duplicate_textbooks = function(last_indx, texts){     
    
    texts$book_id = seq(last_indx+1, last_indx+nrow(texts))  # id and set to master
    texts$copym = 1
    
    for(i in 1:nrow(texts)){
        n_dups = runif(1,1,3)
        dups = data.frame(lapply(texts, function(j){ 
            rep(j[i],n_dups) 
        }))
        dups$copym = 0                  # unset master, set btype, dupeof id
        dups$btype = 't'
        dups$dupeof = dups$book_id
        texts = rbind(texts,dups)             # bind to purchase order
    }
    texts$book_id = seq(last_indx+1, last_indx+nrow(texts))      # reset book ids
    return(texts)
}


book_widths = function(n,a,b){        # utility returns book widths between a and b for n books
    rtriangle(n, a, b, (a + b)/b )
}

get_shelf_ids = function(shelves){    # return vector of shelf ids that are available for shelving
    shelves$shelf_id[which(!shelves$full)]
}

# Simulation processes API

# updates shelves dataframe at end of each round 
update_shelves = function(books, shelves, shelf_width){
    
    for (i in 1:nrow(shelves)){ 
        new_width = sum(books[which(books$shelf_id==i & !books$chkdout),]$width) # calculate new width
        shelves[shelves$shelf_id==i,]$in_use = new_width        # set new widths for each shelf
    }
    shelves$perc_used = shelves$in_use/shelf_width  # reset perc_used
    shelves$full[which(shelves$perc_used > 0.95)] = 1  # set flag at full greater than 95%
    # if shelf was full and books are removed(deduping or checkout) set back to zero
    shelves$full[which(shelves$perc_used <= 0.95 & shelves$full == 1)] = 0
    
    return(shelves)
}

# set of purchases - 5 to 25 books (simulated book widths)
# set of textbook purchases (simulated book widths) - 2 copies of 3 to 5 books
# set flag to 'n', 't' for text-book or non-textbook
# return a new dataframe to be appended to books in outer scope
# this is a blank order... all field's build/add logic is done in the add()/update_shelves() functions
purchase_books = function(flag, min=5, max=26){
    
    if (flag=='n')      # branch for text/nontext -- R throws an exception if not 'n /'t'
        n_books = round(runif(1,min,max))
    else if (flag=='t')
        n_books = round(runif(1,min,max))
    
    width = book_widths(n_books,1,2) # calls utility function book_widths()
    btype = rep(flag,n_books)       # sets flags
    
    book_id = copym = dupeof = shelf_id = chkdout = dedupe = rep(0,n_books)
    purchase_order = data.frame(
        book_id, btype,copym,dupeof,shelf_id,width,chkdout,dedupe
    )
    
    return(purchase_order)
} 

# special logic for appending a textbook df to main library df
# textbooks need to be shelved together
add_textbooks = function(books,new_books, shelves){
    
    last_indx = tail(books$book_id,1)
    new_books$shelf_id = sample(get_shelf_ids(shelves),nrow(new_books)) # shelf ids assigned before duplication
    new_books = duplicate_textbooks(last_indx,new_books)
    books = rbind(books,new_books)
    return(books)
}



#speical logic for appending a non-textbook df to main library df
add_non_textbooks = function(books,new_books, shelves){
    
    last_indx = tail(books$book_id,1)
    new_books$book_id = seq(last_indx+1, last_indx+nrow(new_books))
    new_books$shelf_id = sample(get_shelf_ids(shelves),nrow(new_books)) # assign shelf id from available shelves
    books = rbind(books,new_books)
    return(books)
}


# round of weeding - 2% of the collection -> DO NOT weed books that have been checked out
weeding = function(books, perc=0.02){
    n_to_pull = round(nrow(books) * perc) # number of rows to randomly pull
    books = books[-sample(which(!books$chkdout), n_to_pull), ] # pull em, update and return
    # renumber indices of rows after removal of books
    rownames(books) <- 1:nrow(books)
    return(books)
}


# round of de-duplicating (textbooks) - follows logic in email. Last step in process
de_dup = function(books){
    
    # n number of textbooks to dedupe
    n_dedup = round(runif(1,1,5))
    # get book ids for the master copies to be deduped 
    master_ids = books[sample(which(books$copym==1),n_dedup),]$book_id
   
    # set dedupe flags for any duplicates with chkdout=1; pull the ones with chkdout=0
    if(length(books[which(books$dupeof %in% master_ids & books$chkdout==1), ]$dedupe > 0))
        books[which(books$dupeof %in% master_ids & books$chkdout==1), ]$dedupe = 1
    
    # if there is a subset of duplicates that are not checked out remove them 
    if(nrow(books[which(books$dupeof %in% master_ids & books$chkdout==0), ])!=0)
        books = books[-which(books$dupeof %in% master_ids & !books$chkdout), ]
    
    # renumber indices of rows after removal of books
    rownames(books) <- 1:nrow(books)
    
    return(books)
}

# 2% of the collection gets newly checked out -> check flag
# need to set deduping flag for textbooks that get checked out <<<<--
check_outs = function(books, perc=0.02){
    n_to_chkout = round(nrow(books) * perc) # number of rows to randomly checkout
    book_chkout = books[sample(which(!books$chkdout), n_to_chkout), ]$book_id
    
    books$chkdout[books$book_id %in% book_chkout] = 1
    return(books)
}


# 20% of the collection gets checked in (not the same as the check outs)
check_ins = function(books, perc=0.2){
    n_to_pull = round(length(books$chkdout[books$chkdout == 1])* perc) # 20% of previously checked out books (this might need to change) 
    
    book_returns = books[sample(which(books$chkdout==1), n_to_pull), ]
    
    # check_in deduping logic... if flag set to 1 in batch of returned books, remove from dataframe
    if (any(book_returns$dedupe==1)){
        dedupe_rows = book_returns[book_returns$dedupe == 1,]
        books = books[-which(books$book_id %in% dedupe_rows$book_id), ] 
        # renumber indices of rows after removal of books
        rownames(books) <- 1:nrow(books)
    }
    
    books$chkdout[books$book_id %in% book_returns$book_id] = 0
    return(books)
}


