import scala.collection.mutable

var ok = true
var square = 0
var rectangle = 0
var other = 0
while(ok){
  val input = readLine()
  if(input == null) ok=false
  val sides = input.split(" ").map(_.toInt)
  if(checkSquare(sides(0), sides(1), sides(2), sides(3)))
    square = square + 1
  else if(checkRectangle(sides(0), sides(1), sides(2), sides(3)))
    rectangle = rectangle + 1
  else other = other + 1
}

println(square + " " + rectangle + " " + other)

def checkSquare(a: Int, b: Int, c: Int, d: Int) = {
  a==b==c==d
}

def checkRectangle(a: Int, b: Int, c: Int, d: Int) = {
  a == c && b == d
}
