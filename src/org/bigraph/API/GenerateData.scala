package org.bigraph.API

object GenerateData {

  def main(args: Array[String]): Unit = {

    /*        println("""#Airplane
ap.boardingTime	Double	35	mins
ap.flightNo	String	CZ3265	unit""") */
    var index = 200;

    for (i <- index + 1 to index + 1) {
      var light = scala.util.Random.nextInt(11)
      if (light > 10)
        light = 10
      else if (light <= 0)
        light = 1

      print("""#Passenger p""" + i + """
p""" + i + """.traffic	String	true	boolean
p""" + i + """.flightNo	String	CZ3265	unit
p""" + i + """.fee	Double	0	yuan
p""" + i + """.light	Double	""" + light + """	degree
p""" + i + """.hasCheckinLuggage	String	true	boolean
p""" + i + """.hasDanger	String	true	boolean
p""" + i + """.updateLight	String	false	boolean
p""" + i + """.updateBillboard	String	false	boolean
""")
    }

    for (i <- index + 2 to index + 2) {
      var light = scala.util.Random.nextInt(11)
      if (light > 10)
        light = 10
      else if (light <= 0)
        light = 1

      print("""#Passenger p""" + i + """
p""" + i + """.traffic	String	false	boolean
p""" + i + """.flightNo	String	CZ3265	unit
p""" + i + """.fee	Double	0	yuan
p""" + i + """.light	Double	""" + light + """	degree
p""" + i + """.hasCheckinLuggage	String	true	boolean
p""" + i + """.hasDanger	String	true	boolean
p""" + i + """.updateLight	String	false	boolean
p""" + i + """.updateBillboard	String	false	boolean
""")
    }

    for (i <- index + 3 to index + 20) {
      var light = scala.util.Random.nextInt(11)
      if (light > 10)
        light = 10
      else if (light <= 0)
        light = 1

      print("""#Passenger p""" + i + """
p""" + i + """.traffic	String	true	boolean
p""" + i + """.flightNo	String	CZ3265	unit
p""" + i + """.fee	Double	0	yuan
p""" + i + """.light	Double	""" + light + """	degree
p""" + i + """.hasCheckinLuggage	String	true	boolean
p""" + i + """.hasDanger	String	false	boolean
p""" + i + """.updateLight	String	false	boolean
p""" + i + """.updateBillboard	String	false	boolean
""")
    }

    for (i <- index + 21 to index + 21) {
      var light = scala.util.Random.nextInt(11)
      if (light > 10)
        light = 10
      else if (light <= 0)
        light = 1

      print("""#Passenger p""" + i + """
p""" + i + """.traffic	String	true	boolean
p""" + i + """.flightNo	String	CZ3265	unit
p""" + i + """.fee	Double	0	yuan
p""" + i + """.light	Double	""" + light + """	degree
p""" + i + """.hasCheckinLuggage	String	false	boolean
p""" + i + """.hasDanger	String	true	boolean
p""" + i + """.updateLight	String	false	boolean
p""" + i + """.updateBillboard	String	false	boolean
""")
    }

    for (i <- index + 22 to index + 53) {
      var light = scala.util.Random.nextInt(11)
      if (light > 10)
        light = 10
      else if (light <= 0)
        light = 1

      print("""#Passenger p""" + i + """
p""" + i + """.traffic	String	false	boolean
p""" + i + """.flightNo	String	CZ3265	unit
p""" + i + """.fee	Double	0	yuan
p""" + i + """.light	Double	""" + light + """	degree
p""" + i + """.hasCheckinLuggage	String	true	boolean
p""" + i + """.hasDanger	String	false	boolean
p""" + i + """.updateLight	String	false	boolean
p""" + i + """.updateBillboard	String	false	boolean
""")
    }

    for (i <- index + 54 to index + 54) {
      var light = scala.util.Random.nextInt(11)
      if (light > 10)
        light = 10
      else if (light <= 0)
        light = 1

      print("""#Passenger p""" + i + """
p""" + i + """.traffic	String	false	boolean
p""" + i + """.flightNo	String	CZ3265	unit
p""" + i + """.fee	Double	0	yuan
p""" + i + """.light	Double	""" + light + """	degree
p""" + i + """.hasCheckinLuggage	String	false	boolean
p""" + i + """.hasDanger	String	true	boolean
p""" + i + """.updateLight	String	false	boolean
p""" + i + """.updateBillboard	String	false	boolean
""")
    }

    for (i <- index + 55 to index + 60) {
      var light = scala.util.Random.nextInt(11)
      if (light > 10)
        light = 10
      else if (light <= 0)
        light = 1

      print("""#Passenger p""" + i + """
p""" + i + """.traffic	String	true	boolean
p""" + i + """.flightNo	String	CZ3265	unit
p""" + i + """.fee	Double	0	yuan
p""" + i + """.light	Double	""" + light + """	degree
p""" + i + """.hasCheckinLuggage	String	false	boolean
p""" + i + """.hasDanger	String	false	boolean
p""" + i + """.updateLight	String	false	boolean
p""" + i + """.updateBillboard	String	false	boolean
""")
    }

    for (i <- index + 61 to index + 76) {
      var light = scala.util.Random.nextInt(11)
      if (light > 10)
        light = 10
      else if (light <= 0)
        light = 1

      print("""#Passenger p""" + i + """
p""" + i + """.traffic	String	false	boolean
p""" + i + """.flightNo	String	CZ3265	unit
p""" + i + """.fee	Double	0	yuan
p""" + i + """.light	Double	""" + light + """	degree
p""" + i + """.hasCheckinLuggage	String	false	boolean
p""" + i + """.hasDanger	String	false	boolean
p""" + i + """.updateLight	String	false	boolean
p""" + i + """.updateBillboard	String	false	boolean
""")
    }

    for (i <- index + 77 to index + 77) {
      var light = scala.util.Random.nextInt(11)
      if (light > 10)
        light = 10
      else if (light <= 0)
        light = 1

      print("""#Passenger p""" + i + """
p""" + i + """.traffic	String	true	boolean
p""" + i + """.flightNo	String	CZ3265	unit
p""" + i + """.fee	Double	0	yuan
p""" + i + """.light	Double	""" + light + """	degree
p""" + i + """.hasCheckinLuggage	String	true	boolean
p""" + i + """.hasDanger	String	false	boolean
p""" + i + """.updateLight	String	false	boolean
p""" + i + """.updateBillboard	String	false	boolean
""")
    }

    for (i <- index + 78 to index + 78) {
      var light = scala.util.Random.nextInt(11)
      if (light > 10)
        light = 10
      else if (light <= 0)
        light = 1

      print("""#Passenger p""" + i + """
p""" + i + """.traffic	String	false	boolean
p""" + i + """.flightNo	String	CZ3265	unit
p""" + i + """.fee	Double	0	yuan
p""" + i + """.light	Double	""" + light + """	degree
p""" + i + """.hasCheckinLuggage	String	true	boolean
p""" + i + """.hasDanger	String	false	boolean
p""" + i + """.updateLight	String	false	boolean
p""" + i + """.updateBillboard	String	false	boolean
""")
    }

    for (i <- index + 79 to index + 79) {
      var light = scala.util.Random.nextInt(11)
      if (light > 10)
        light = 10
      else if (light <= 0)
        light = 1

      print("""#Passenger p""" + i + """
p""" + i + """.traffic	String	true	boolean
p""" + i + """.flightNo	String	CZ3265	unit
p""" + i + """.fee	Double	0	yuan
p""" + i + """.light	Double	""" + light + """	degree
p""" + i + """.hasCheckinLuggage	String	false	boolean
p""" + i + """.hasDanger	String	false	boolean
p""" + i + """.updateLight	String	false	boolean
p""" + i + """.updateBillboard	String	false	boolean
""")
    }

    for (i <- index + 80 to index + 80) {
      var light = scala.util.Random.nextInt(11)
      if (light > 10)
        light = 10
      else if (light <= 0)
        light = 1

      print("""#Passenger p""" + i + """
p""" + i + """.traffic	String	false	boolean
p""" + i + """.flightNo	String	CZ3265	unit
p""" + i + """.fee	Double	0	yuan
p""" + i + """.light	Double	""" + light + """	degree
p""" + i + """.hasCheckinLuggage	String	false	boolean
p""" + i + """.hasDanger	String	false	boolean
p""" + i + """.updateLight	String	false	boolean
p""" + i + """.updateBillboard	String	false	boolean
""")
    }

    for (i <- index + 81 to index + 81) {
      var light = scala.util.Random.nextInt(11)
      if (light > 10)
        light = 10
      else if (light <= 0)
        light = 1

      print("""#Passenger p""" + i + """
p""" + i + """.traffic	String	true	boolean
p""" + i + """.flightNo	String	CZ3265	unit
p""" + i + """.fee	Double	0	yuan
p""" + i + """.light	Double	""" + light + """	degree
p""" + i + """.hasCheckinLuggage	String	true	boolean
p""" + i + """.hasDanger	String	false	boolean
p""" + i + """.updateLight	String	false	boolean
p""" + i + """.updateBillboard	String	false	boolean
""")
    }

    for (i <- index + 82 to index + 82) {
      var light = scala.util.Random.nextInt(11)
      if (light > 10)
        light = 10
      else if (light <= 0)
        light = 1

      print("""#Passenger p""" + i + """
p""" + i + """.traffic	String	true	boolean
p""" + i + """.flightNo	String	CZ3265	unit
p""" + i + """.fee	Double	0	yuan
p""" + i + """.light	Double	""" + light + """	degree
p""" + i + """.hasCheckinLuggage	String	false	boolean
p""" + i + """.hasDanger	String	false	boolean
p""" + i + """.updateLight	String	false	boolean
p""" + i + """.updateBillboard	String	false	boolean
""")
    }

    for (i <- index + 83 to index + 83) {
      var light = scala.util.Random.nextInt(11)
      if (light > 10)
        light = 10
      else if (light <= 0)
        light = 1

      print("""#Passenger p""" + i + """
p""" + i + """.traffic	String	false	boolean
p""" + i + """.flightNo	String	CZ3265	unit
p""" + i + """.fee	Double	0	yuan
p""" + i + """.light	Double	""" + light + """	degree
p""" + i + """.hasCheckinLuggage	String	true	boolean
p""" + i + """.hasDanger	String	false	boolean
p""" + i + """.updateLight	String	false	boolean
p""" + i + """.updateBillboard	String	false	boolean
""")
    }

    for (i <- index + 84 to index + 84) {
      var light = scala.util.Random.nextInt(11)
      if (light > 10)
        light = 10
      else if (light <= 0)
        light = 1

      print("""#Passenger p""" + i + """
p""" + i + """.traffic	String	true	boolean
p""" + i + """.flightNo	String	CZ3265	unit
p""" + i + """.fee	Double	0	yuan
p""" + i + """.light	Double	""" + light + """	degree
p""" + i + """.hasCheckinLuggage	String	true	boolean
p""" + i + """.hasDanger	String	false	boolean
p""" + i + """.updateLight	String	false	boolean
p""" + i + """.updateBillboard	String	false	boolean
""")
    }

    for (i <- index + 85 to index + 85) {
      var light = scala.util.Random.nextInt(11)
      if (light > 10)
        light = 10
      else if (light <= 0)
        light = 1

      print("""#Passenger p""" + i + """
p""" + i + """.traffic	String	false	boolean
p""" + i + """.flightNo	String	CZ3265	unit
p""" + i + """.fee	Double	0	yuan
p""" + i + """.light	Double	""" + light + """	degree
p""" + i + """.hasCheckinLuggage	String	true	boolean
p""" + i + """.hasDanger	String	false	boolean
p""" + i + """.updateLight	String	false	boolean
p""" + i + """.updateBillboard	String	false	boolean
""")
    }

    for (i <- index + 86 to index + 86) {
      var light = scala.util.Random.nextInt(11)
      if (light > 10)
        light = 10
      else if (light <= 0)
        light = 1

      print("""#Passenger p""" + i + """
p""" + i + """.traffic	String	true	boolean
p""" + i + """.flightNo	String	CZ3265	unit
p""" + i + """.fee	Double	0	yuan
p""" + i + """.light	Double	""" + light + """	degree
p""" + i + """.hasCheckinLuggage	String	true	boolean
p""" + i + """.hasDanger	String	true	boolean
p""" + i + """.updateLight	String	false	boolean
p""" + i + """.updateBillboard	String	false	boolean
""")
    }

    for (i <- index + 87 to index + 90) {
      var light = scala.util.Random.nextInt(11)
      if (light > 10)
        light = 10
      else if (light <= 0)
        light = 1

      print("""#Passenger p""" + i + """
p""" + i + """.traffic	String	false	boolean
p""" + i + """.flightNo	String	CZ3265	unit
p""" + i + """.fee	Double	0	yuan
p""" + i + """.light	Double	""" + light + """	degree
p""" + i + """.hasCheckinLuggage	String	true	boolean
p""" + i + """.hasDanger	String	false	boolean
p""" + i + """.updateLight	String	false	boolean
p""" + i + """.updateBillboard	String	false	boolean
""")
    }

    for (i <- index + 91 to index + 95) {
      var light = scala.util.Random.nextInt(11)
      if (light > 10)
        light = 10
      else if (light <= 0)
        light = 1

      print("""#Passenger p""" + i + """
p""" + i + """.traffic	String	true	boolean
p""" + i + """.flightNo	String	CZ3265	unit
p""" + i + """.fee	Double	0	yuan
p""" + i + """.light	Double	""" + light + """	degree
p""" + i + """.hasCheckinLuggage	String	true	boolean
p""" + i + """.hasDanger	String	false	boolean
p""" + i + """.updateLight	String	false	boolean
p""" + i + """.updateBillboard	String	false	boolean
""")
    }

    for (i <- index + 96 to index + 100) {
      var light = scala.util.Random.nextInt(11)
      if (light > 10)
        light = 10
      else if (light <= 0)
        light = 1

      print("""#Passenger p""" + i + """
p""" + i + """.traffic	String	true	boolean
p""" + i + """.flightNo	String	CZ3265	unit
p""" + i + """.fee	Double	0	yuan
p""" + i + """.light	Double	""" + light + """	degree
p""" + i + """.hasCheckinLuggage	String	true	boolean
p""" + i + """.hasDanger	String	false	boolean
p""" + i + """.updateLight	String	false	boolean
p""" + i + """.updateBillboard	String	false	boolean
""")
    }
  }

}