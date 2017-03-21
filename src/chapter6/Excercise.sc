import chapter6._

SimpleRng.unit(10)(new SimpleRng(100))

SimpleRng.map[Int, Double]{
  _.nextInt
}{_ / 2.0}(new SimpleRng(1000))

SimpleRng.sequence(List.fill(10)(SimpleRng.unit(100)))(new SimpleRng(7))

chapter6.SimpleRng.rollDie(SimpleRng(0))