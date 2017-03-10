#Paskal triangle by lapply TOO SLOW!
pascalTriangle <- function(h) {
  lapply(0:h, function(i) choose(i, 0:i))
}
pascalTriangle(10)


#Paskal triangle by for loop
x=1
for (i in 1:10) {x=c(0,x)+c(x,0); print(x)}

