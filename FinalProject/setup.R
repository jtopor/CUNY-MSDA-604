# can import this function using source("setup.R")
# returns list of 2 dataframes: books and shelves

setup = function(Nvols, Nshelves, shelf_width, sfree_space, textb_masters){
    
    library(triangle)
    
    # set max shelf space allowed to be consumed by books at start of simulation
    # sfree_space represents the total amount of free space in INCHES
    max_shelved <- shelf_width - sfree_space

    # Initialize 'books' data frame
    books <- data.frame(book_id = 1:Nvols, btype = character(Nvols), copym = numeric(Nvols), 
                        dupeof = numeric(Nvols), shelf_id = numeric(Nvols), width = numeric(Nvols),
                        chkdout =numeric(Nvols), dedupe = numeric(Nvols), stringsAsFactors = FALSE)
    
    # set book widths: distrib is between 1-2 inches; assume UNIFORM distribution
    # now sample from triangular distribution: Number of samples = number of books ('Nvols')
    a <- 1 ; b <- 2 # upper/lower bound 
    books$width <- rtriangle(Nvols, a, b, (a + b)/b )
    
    books$btype <- 'n' # initialize book type to n for non-textbook
    
    # then set percentage of books to textbook to textbook master copies
    # start by picking random sample of books
    tbm <- sort(sample(books$book_id, (Nvols * textb_masters), replace = FALSE))
    
    # For each book_id in tbm, set btype = 't', copym = 1, and create copies of
    for (i in tbm) {
        # if the book has not already been set to type 't', proceed
        # otherwise ignore and continue
        if (books$btype[i] != 't') {
            # set btype to 't'
            books$btype[i] <- 't'
            # set copym to '1'
            books$copym[i] <- 1
            # get number of copies of textbook via random uniform sample between 1 and 3
            tb_copies <- round(runif(1, min = 1, max = 3))
            
            # create copies of textbook directly adjacent to master copy
            k <- i + 1
            while (k <= Nvols & (k <= i + tb_copies) ) {
                books$btype[k] <- 't'
                books$dupeof[k] <- books$book_id[i]
                # set width of copy to width of master
                books$width[k] <- books$width[i]
                k <- k + 1
            }
        } 
    } 

    # Initialize 'shelves' data frame  --> full 0 or 1 flag for whether shelf is full
    shelves <- data.frame(shelf_id = 1:Nshelves, in_use = numeric(Nshelves), 
                          perc_used = numeric(Nshelves), full=numeric(Nshelves), stringsAsFactors = FALSE)
    
    k <- 1
    # sum book widths to ensure they don't exceed (max shelf width - free_space)
    for (i in 1: Nshelves) {
        # while space used on shelf < max space consumed by books and book index < Nvols
        while ((shelves$in_use[i] + books$width[k]) < max_shelved & (k <= Nvols) ) {
            # assign shelf to next book
            books$shelf_id[k] <- shelves$shelf_id[i]
            # add width of book to total used on current shelf
            shelves$in_use[i] <- shelves$in_use[i] + books$width[k]
            k <- k + 1
        } 
    } 
    
    ###########################
    # remove all books that were not able to be shelved
    books <- books[which(books$shelf_id != 0),]
    
    # calculate number of deleted books
    delbks <- Nvols - nrow(books)

    # select 10% of book_id values to set to 'checked out' status
    c_out <- sample(books$book_id, (nrow(books) * .10), replace = FALSE)
    
    # For each book_id in c_out, set chkdout = 1 and subtract
    # book width from shelf space used for appropriate shelf_id
    for (i in c_out) {
        # set chkdout flag to 1
        books$chkdout[i] <- 1
        # subtract width of chkdout book from space in use on its assigned shelf
        shelves$in_use[books$shelf_id[i]] <- shelves$in_use[books$shelf_id[i]] - books$width[i]
    }
    shelves$perc_used = shelves$in_use/shelf_width  # sets perc_used
    
    
    return(list(books=books, shelves=shelves, delbks = delbks)) # return books shelves
    
}

