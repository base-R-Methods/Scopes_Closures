# Preface
# Environments and Functions
# Environment Chains, Scope, and Function Calls
# Scopes, Lazy Evaluation, and Default Parameters
# Nested Functions and Scopes
# Closures
# Reaching Outside Your Innermost Scope
# Lexical and Dynamic Scope
# Conclusion

#################################################

# Preface

x + y # Error

x <- 2
y <- 3

x + y 

genv <- globalenv()
str(genv)

genv$x
genv$y

expr <- quote( x + y )
str(expr)

?eval
eval( expr )
eval( expr , genv )

env <- new.env()
str(env)

env$x <- 4
env$y <- 5

eval(expr , env )

# list2env() 

expr <- quote(x + y )
vals <- list(x = -1, y = -2)
str(vals)

eval( expr , vals )

e <- list2env(vals)
str(e)

eval( expr , e )

# Environments and Functions

x <- 1:10

f <- function(x){

  sqrt(sum(x))
  
}
# typeof(f)

f( x^2 )

x <- 10
f <- function(x){
  x+1
}

f(x = 5)

gx <- 1:10

f <- function(px){
  sqrt(sum(px))
}

f(gx^2)

gx <- 1:10

f <- function(px){
  
  as.list(environment())
  
}

f(px = gx^2)
f(gx)


f <- function(x){
  
  res <- sqrt(sum(x))
  as.list(environment())
  
}

x <- 1:10
f(x^2)

as.list(globalenv())

# Environment Chains, Scope, and Function Calls

## Ex 1 : 
?new.env

e1 <- new.env()
e2 <- new.env()

parent.env(e1)
parent.env(e2)

e1 <- new.env()
e2 <- new.env(parent = e1)

parent.env(e1)
parent.env(e2)

identical( parent.env(e2)  ,  e1 )

## Ex 2 : 

e1 <- new.env()

e1$x <- 10
as.list(e1)

e2 <- new.env( parent = e1)

e1$x
e2$x


get( "x"  , envir = e2 , inherits = TRUE )
get( "x"  , envir = e2 , inherits = FALSE )

exists("x", envir = e2, inherits = TRUE)
exists("x", envir = e2, inherits = FALSE)

eval( quote(x) , envir = e2)

# e2$x <- 20
# eval( quote(x) , envir = e2)

## Ex 3 :

f <- function(x){
  sqrt(sum(x))
}

x <- 1:10
y <- 1:10

f( x = x^2)

ls(baseenv())
ls(baseenv())[10:30]

baseenv()$mean
baseenv()$sum
baseenv()$sqrt

parent.env(baseenv())
parent.env(emptyenv())

# Scopes, Lazy Evaluation, and Default Parameters

## First_case_ study


f <- function(x, y = 2*x) {
  x + y
}

a <- 2
f( x = a )
# f( x = a, y = 2*x )

## Second_case_ study

f <- function(x, y = 2*x) {
  x + y
}

a <- 2

f( x = a, y = 2*x )


expr <- quote( x + 2 )
typeof(expr)
# str(expr)

x <- 3

e <- new.env()
e$x <- 100
x + 2 
eval( expr , e )

# f( g(x+y+z) , x =1  , y = 2 ,......)
# f( g(x+y+z) , ...)


f <- function(expr,...) {
  
  expr <- substitute(expr)
  lt <- list(...)
  env <- list2env(lt)
  # parent.env(env) <- parent.frame()
  eval(expr, env)
  
}

g <- function(x) {
  x
}


f( g( x + y + z), x = 1, y = 2, z = 3)

# Nested Functions and Scopes

f <- function() {
  environment()
}

f()

environment(f)

f <- function(x) {
  print(environment())
  
  g <- function(y) {
    x + y
  }
  g
  
}
environment(f)

g <- f(2)
environment(g)

g <- f(2)

g(3)

# Closures

h1 <- f(1)
h2 <- f(2)
h3 <- f(3)

sapply(1:5 , h1)
sapply(1:5 , h2)
sapply(1:5 , h3)


# Reaching Outside Your Innermost Scope

## Ex : 1

x <- 1
f <- function() {
  x <- 10
  print(x)
}

f()

f <- function() {
  # x <- 10
  print(x)
}
f()

## Ex : 2

make_counter <- function(){
  x <- 0
  x <- x + 1
  x
}
make_counter()

make_counter <- function(){
  
  x <- 0
  
  count <- function(){
    
  x <<- x + 1
  x
  }
}

counter <- make_counter()
counter()

## TREE + counter + <<-

make_node <- function(name, left = NULL, right = NULL ){
  list(name = name, left = left, right = right)
}

tree <- make_node(name = "root",
                  left = make_node(name = "C", 
                                   left = make_node(name = "A",
                                                    left = NULL,
                                                    right = NULL),
                                   right = make_node(name = "B",
                                                   left = NULL,
                                                   right = NULL)),
                  right = make_node(name = "D", 
                                    left = NULL,
                                    right = NULL))


tree


depth_first_numbers <- function(tree){
  
  counter <- make_counter()
  table <- c()
  
  travers_tree <- function(node) {
  if (is.null(node$left) && is.null(node$right)) {
    
    df <- counter()
    table[node$name] <<- df
    node$range <- c(df,df)
    node
  } else {
    
    left <- travers_tree(node$left)
    right <- travers_tree(node$right)
    
    new_node <- make_node(node$name, left,right)
    new_node$range <- c( left$range[1]  , right$range[2] )
    new_node
    
  }
  }
  
  new_tree <- travers_tree(tree)
  list(new_tree, table= table)
  
}

depth_first_numbers(tree)

# Lexical and Dynamic Scope


x <- 10
df <- data.frame(
  x = c(1,2,3),
  y = c(4,5,6)
)

df

with(df, x + y)
z <- 100
with(df, y + z )

df$y +z
df$x + df$y


f <- function(expr,dt) {
  
  expr <- substitute(expr)
  
  env <- list2env(dt)
  eval(expr, env)
  
}

f(x+y , df)

z <- 100
f(y + z , df)

?with

# Conclusion