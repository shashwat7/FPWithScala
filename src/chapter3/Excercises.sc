val a = chapter3.List(1,2,3,4,5)
val b = chapter3.List(6,7,8,9,10)

chapter3.List.dropWhile[Int](a, x => x < 4)
