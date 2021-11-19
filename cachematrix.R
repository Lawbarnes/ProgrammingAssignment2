## The makeCacheMatrix function is used to cache the inverse matrix for later
## retrieval.  

makeCacheMatrix <- function(x = matrix()) { #defining the function 
	m<-NULL					# setting the value to NULL to be used later in code
	set<- function(y) {			# creating the function that will take the new values for the matrix.  Allow user to enter new matrix.
		x<<-y									
		m<<-NULL				# clearing prior values from that have been cached
	}
	get<-function(){x}			# retrieves value of x from the parent environment (the makeCacheMatrix function
	setinverse <- function(inverse) {m<<- inverse} # assigns the value of the invervse matrix to the parent environment
	getinverse <- function() {m}		# retrieves the value of the inverse matrix from the parent enviroment
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) # outputs a list of objects that can be used by downstream functions
}


## cacheSolve finds the inverse matrix.

cacheSolve<- function(x) {			# defining a function that will populate and retrieve values from makeCacheMatrix
	m<- x$getinverse ()			# retrieves the inverse matrix (or matrix) from makeCacheMatrix environment
	if(!is.null(m)) {
	message("getting cache data")		# if there is an inverse matrix in makeCacheMatrix, returns that matrix (no new calculation)
	return(m)} else {
	data<- x$get()				# if there is no inverse matrix in makeCacheMatrix, solves for the inverse matrix. 
	m<- solve(data)
	x$setinverse(m)
	m}						# prints inverse matrix.
}