## 此文件中包含两个方法
## 1. makeCacheMatrix：创建一个特殊的“矩阵”对象，可以计算和缓存矩阵的逆矩阵
## 2. cacheSolve：获取矩阵的逆矩阵，可以从缓存中获取
## 使用时，应该先调用makeCacheMatrix()函数，将原始矩阵作为参数传入，获取一个
##  “特殊”矩阵对象。然后使用cacheSolve()函数，将返回的“特殊”矩阵对象传入其中，
##  就会返回原始矩阵的逆矩阵。

## 创建一个特殊的“矩阵”对象，可以计算和缓存矩阵的逆矩阵
## 参数：
##      x ：要计算逆矩阵的原始矩阵对象
## 返回：
##      “特殊”矩阵对象，其中包括下面几个方法
##          1. get：获取原始矩阵对象
##          2. set：设置原始矩阵对象
##          3. getInverse：获取原始矩阵的逆矩阵
##          4. setInverse：设置原始矩阵的逆矩阵
makeCacheMatrix <- function(x = matrix()) {
    result <- NULL
    
    set <- function(y) {
        x <<- y
        result <<- NULL
    }
    get <- function() x
    
    setInverse <- function(inverse) result <<- inverse
    getInverse <- function() result
    
    list(set = set, get = get, setInverse = setInverse, 
         getInverse = getInverse)
}


## 获取矩阵的逆矩阵
##  如果缓存中存在，就从缓存中获取
##  如果缓存中不存在，则计算逆矩阵，并设置缓存
## 参数：
##      1. x：makeCacheMatrix()函数获取的“特殊”矩阵对象
##      2. ... ： sovle()可以使用的除第一个参数以外的其他参数
## 返回：
##      原始矩阵的逆矩阵
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        message("getting inverse data in memory")
        return(inverse)
    }
    
    message("can't get inverse in memory, compute now")
    inverse <- solve(x$get(), ...)
    x$setInverse(inverse)
    
    inverse
}
