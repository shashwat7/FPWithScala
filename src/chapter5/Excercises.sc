import chapter5._

val s = chapter5.Stream(1,2,3,4,5)
s.headOption()
s.headOption_2()
s.take(2).toList
s.takeWhile(_<3).toList
s.takeWhile_2(_<3).toList
s.drop(3).toList
s.forAll(_<6)
s.exists_2(_ == 3)
s.map(_*2).toList
s.filter(_%2==0).toList
chapter5.Stream.append(s,6).toList

val ones: Stream[Int] = Stream.cons(1, ones)

Stream.startsWith(Stream(1,2,4),Stream(1,2))
chapter5.Stream(1,2,3).tails.map(_.toList).toList