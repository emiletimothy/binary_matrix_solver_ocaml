open Binmat

module AlgorithmX :
  functor (B : BinaryMatrix) ->
    sig 
      (* Attempt to solve the board.
         Return None if there is no solution.
         Return Some (set of indices) if there is a solution. *)
      val solve : B.t -> IntSet.t option
    end
