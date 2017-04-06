/*
{
    "name": "alpha",
    "age":  25,
    "location": {
        "country": "india",
        "city": "blr"
    },
    "books": [1,2,3]
}

[("name","alpha"),("age",25),("location.country","india"),("location.city","blr"),("book",[1,2,3])]

Object : int,float,string,long,byte,array,map
* */

case class KeyValuePair(k: String, v: Any)

def flatten(map: Map[String, Any]): List[KeyValuePair] = {

  map.foldLeft(List.empty[KeyValuePair]){case (list, (key, value)) =>
    value match{
      case innerMap: Map[String,Any] =>
        list ::: flatten(innerMap).map{ kvPair =>
          KeyValuePair(key + "." + kvPair.k,kvPair.v)
        }
      case _ => list ::: List(KeyValuePair(key, value))
    }
  }

}

val input: Map[String, Any] = Map(
  "name" -> "Shashwat",
  "age" -> 25,
  "location" -> Map(
    "address" -> Map(
      "streetName" -> "subhash marg",
      "locality" -> "raja bazaar"
    ),
    "country" -> "india",
    "city" -> "bangalore"
  ),
  "books" -> Array(1,2,3)
)

flatten(input).foreach(println)