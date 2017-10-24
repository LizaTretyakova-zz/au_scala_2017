import org.scalatest.FunSuite

class MultisetTest extends FunSuite {

  case class Core(name: String, kind: String)

  val cores = Multiset(
    Core("Central", "Personality"),
    Core("Morality", "Personality"),
    Core("Party Escort", "Other"),
    Core("Curiosity", "Personality"),
    Core("Space", "Corrupted"),
    Core("Intelligence", "Personality"),
    Core("Anger", "Personality"),
    Core("Wheatley", "Personality"),
    Core("Adventure", "Corrupted"),
    Core("Fact", "Corrupted")
  )

  test("pattern matching") {
    assertResult(1) {
      Multiset(1, 2, 3) match {
        case Empty => None
        case Multiset(1, 2, 3) => 1
      }
    }
  }

  test("for-comprehension") {

    val personalityCores =
      Multiset((for (core <- cores.flatten() if core.kind == "Personality") yield core.name): _*)

    assertResult(true) {
      personalityCores match {
        case Multiset("Central", "Morality", "Curiosity", "Intelligence", "Anger", "Wheatley") => true
        case _ => false
      }
    }
  }

  test("search") {
    val corruptedCores = cores.filter(core => core.kind == "Corrupted")
    val otherCores = Multiset(cores.find(Core("Party Escort", "Other")).getOrElse(Core("So trash", "much garbage")))

    assertResult(true) {
      corruptedCores
        .flatMap(core => Multiset(core.name, core.kind))
        .map(str => str + str)("FactFact")
    }
    assertResult(true) {
      otherCores(Core("Party Escort", "Other"))
    }

    assertResult(Empty) {
      corruptedCores & otherCores
    }
    assertResult(true) {
      (corruptedCores | otherCores)(Core("Adventure", "Corrupted"))
    }
    assertResult(false) {
      (corruptedCores | otherCores)(Core("Wheatley", "Personality"))
    }
  }
}
