package logic

import scala.collection.mutable.Buffer

class Matchmaker(personData: Array[Person]) {
  
  /**
   * Uses the people recorded in the matchmaker (personData) to create a map from each possible pairing
   * to the mutual matchmaking score of that pair.
   * 
   * When creating the map remember to take each pair of people *only once*, 
   * e.g. take only (Matt, Laura) or (Laura, Matt). If the person appears earlier in the list
   * you should put them as the first half o f the pair.
   */
  def matchMap: Map[(Person, Person), Int] = {
    var personsMap = Map[(Person, Person), Int]()
    for {
      person <- personData
      other <- personData.drop(personData.indexOf(person) + 1)
    } yield personsMap += ((person, other) -> person.bothMatch(other))
    return personsMap
    }
  
}