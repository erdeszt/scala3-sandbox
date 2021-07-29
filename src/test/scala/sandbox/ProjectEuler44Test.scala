package sandbox

import org.scalatest.funspec.AnyFunSpec
import sandbox.ProjectEuler44._

class ProjectEuler44Test extends AnyFunSpec:
  
  describe("Project Euler problem 44") {
    describe("Pentagon number calculator") {
      it("should calculate the Nth Pentagon number correctly") {
        assert(pentagon(0) == 0)
        assert(pentagon(1) == 1)
        assert(pentagon(2) == 5)
        assert(pentagon(5) == 35)
        assert(pentagon(10) == 145)
      } 
    }
    
    describe("Solver") {
      it("should calculate the correct solution with the firstImperative approach") {
        val solution = firstImperative()
        assert(solution.isDefined)
        assert(solution.get.diff == 5482660)
      }
      it("should calculate the correct solution with the max distance approach") {
        val solution = imperativeMaxDistance()
        assert(solution.isDefined)
        assert(solution.get.diff == 5482660)
      }
    }
  }
