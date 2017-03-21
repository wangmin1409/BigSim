package org.bigraph.API

object GenerateAgent {

  def main(args: Array[String]): Unit = {

    val index = 200
    /*        println("""#Airplane
ap.boardingTime	Double	35	mins
ap.flightNo	String	CZ3265	unit""") */

    for (i <- index + 1 to index + 1) {
      print("p" + i + ":Passenger[isAdult:edge,isEconomy:edge].(t" + i + ":Ticket[idle] | c" + i + ":CheckinLuggage | d" + i + ":Danger) | ");
    }

    for (i <- index + 2 to index + 2) {
      print("p" + i + ":Passenger[isAdult:edge,isEconomy:edge].(t" + i + ":Ticket[idle] | c" + i + ":CheckinLuggage | d" + i + ":Danger) | ");
    }

    for (i <- index + 3 to index + 20) {
      print("p" + i + ":Passenger[isAdult:edge,isEconomy:edge].(t" + i + ":Ticket[idle] | c" + i + ":CheckinLuggage) | ");
    }

    for (i <- index + 21 to index + 21) {
      print("p" + i + ":Passenger[isAdult:edge,isEconomy:edge].(t" + i + ":Ticket[idle] | d" + i + ":Danger) | ");
    }

    for (i <- index + 22 to index + 53) {
      print("p" + i + ":Passenger[isAdult:edge,isEconomy:edge].(t" + i + ":Ticket[idle] | c" + i + ":CheckinLuggage) | ");
    }

    for (i <- index + 54 to index + 54) {
      print("p" + i + ":Passenger[isAdult:edge,isEconomy:edge].(t" + i + ":Ticket[idle] | d" + i + ":Danger) | ");
    }

    for (i <- index + 55 to index + 60) {
      print("p" + i + ":Passenger[isAdult:edge,isEconomy:edge].(t" + i + ":Ticket[idle]) | ");
    }

    for (i <- index + 61 to index + 76) {
      print("p" + i + ":Passenger[isAdult:edge,isEconomy:edge].(t" + i + ":Ticket[idle]) | ");
    }

    for (i <- index + 77 to index + 77) {
      print("p" + i + ":Passenger[isAdult:edge,isBusiness:edge].(t" + i + ":Ticket[idle] | c" + i + ":CheckinLuggage) | ");
    }

    for (i <- index + 78 to index + 78) {
      print("p" + i + ":Passenger[isAdult:edge,isBusiness:edge].(t" + i + ":Ticket[idle] | c" + i + ":CheckinLuggage) | ");
    }

    for (i <- index + 79 to index + 79) {
      print("p" + i + ":Passenger[isAdult:edge,isBusiness:edge].(t" + i + ":Ticket[idle]) | ");
    }

    for (i <- index + 80 to index + 80) {
      print("p" + i + ":Passenger[isAdult:edge,isBusiness:edge].(t" + i + ":Ticket[idle]) | ");
    }

    for (i <- index + 81 to index + 81) {
      print("p" + i + ":Passenger[isDisable:edge,isEconomy:edge].(t" + i + ":Ticket[idle] | c" + i + ":CheckinLuggage) | ");
    }

    for (i <- index + 82 to index + 82) {
      print("p" + i + ":Passenger[isChild:edge,isEconomy:edge].(t" + i + ":Ticket[idle]) | ");
    }

    for (i <- index + 83 to index + 83) {
      print("p" + i + ":Passenger[isChild:edge,isBusiness:edge].(t" + i + ":Ticket[idle] | c" + i + ":CheckinLuggage) | ");
    }

    for (i <- index + 84 to index + 84) {
      print("p" + i + ":Passenger[isAdultWithChild:edge,isBusiness:edge].(t" + i + ":Ticket[idle] | c" + i + ":CheckinLuggage) | ");
    }

    for (i <- index + 85 to index + 85) {
      print("p" + i + ":Passenger[isAdultChild:edge,isEconomy:edge].(t" + i + ":Ticket[idle] | c" + i + ":CheckinLuggage) | ");
    }

    for (i <- index + 86 to index + 86) {
      print("p" + i + ":Passenger[isOld:edge,isEconomy:edge].(t" + i + ":Ticket[idle] | c" + i + ":CheckinLuggage | d" + i + ":Danger) | ");
    }

    for (i <- index + 87 to index + 90) {
      print("p" + i + ":Passenger[isOld:edge,isEconomy:edge].(t" + i + ":Ticket[idle] | c" + i + ":CheckinLuggage) | ");
    }

    for (i <- index + 91 to index + 95) {
      print("p" + i + ":Passenger[isOld:edge,isBusiness:edge].(t" + i + ":Ticket[idle] | c" + i + ":CheckinLuggage) | ");
    }

    for (i <- index + 96 to index + 100) {
      print("p" + i + ":Passenger[isOld:edge,isEconomy:edge].(t" + i + ":Ticket[idle] | c" + i + ":CheckinLuggage) | ");
    }
  }

}