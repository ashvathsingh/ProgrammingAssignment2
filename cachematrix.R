## My first function takes a matrix. As per the assignment guidelines we are to
## assume all of the matrices supplied are invertible. So the an invertible mat
## rix is taken and then a similar function to the one given as an example is 
## set which has all the data about the matrix in an environment which makes it 
## easier to take data from the environment. The second function lets R check 
## whether the inverse of the matrix has already been calculated or not by acces 
## sing the cache,  which is conincidentally where the data about the matrix is 
## stored.

## This function takes in a matrix as its argument. Then within the function th
## ere is another function which sets the matrix and its inverse to a specific 
## environment using the '<<-' operator. After that is set a blank functions is 
## passed to get the matrix from the afforementioned environment. The setinv 
## function does the real work and gets the inverse of the matrix which is aga
## in stored in the same environment using the '<<-'. getinv is another blank 
## function which gets the value of the inverse of the matrix from that enviro
## nment.The function prints out a list of all of these functions as its output.
## This list is the cached matrix and needs to be stored and put as the next 
## function's argument.

makeCacheMatrix <- function(m= matrix()){
        inv <- NULL
        set <- function (y){
                m <<- y
                inv <<- NULL
        }
        get <- function () m
        setinv <- function (inversion) inv <<- solve(m)
        getinv <- function () inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The cacheSolve is really just a variation of the example given in the assign
## ment description. Again the function takes as an argument the previous func
## tion's output which is a chaced matrix. This function solves calls the prev
## ious function's function (I know); to get the inverse. If the inverse has 
## been prviously calculated it displays a message that it is going to get 
## the cached data. If not it calls the previous function's function (last time)
## to give it the data about the matrix and then inverses it and sets the value
## of that inverse in that environment. And of course prints out the inverse.

cacheSolve <- function(m, ...) {
        inv <- m$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- m$get()
        inv <- solve(data, ...)
        m$setinv(inv)
        inv
}
