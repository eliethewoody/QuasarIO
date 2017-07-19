type rosenblatt = {
  rate         : float;
  bias         : float;
  weight       : float;
  errors       : int}

val initPerceptron: float -> float -> rosenblatt
val teachPerceptron: rosenblatt -> float list -> float list -> int -> rosenblatt
val predictValue: rosenblatt -> float -> float
val getNetInput: rosenblatt -> float -> float
val teach: rosenblatt -> float list -> float list -> rosenblatt
