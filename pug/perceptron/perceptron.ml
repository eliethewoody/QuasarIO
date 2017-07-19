(*open Core.Std*)

type rosenblatt = {
  rate         : float;
  bias         : float;
  weight       : float;
  errors       : int};;

let rec initPerceptron rate bias =
  if rate == 0.0 || rate >= 1.0 then initPerceptron 0.0153 bias else
   {rate   = rate;
    bias   = bias;
    weight = 0.0;
    errors = 0};;

let getNetInput perceptron priori =
  perceptron.bias +. priori *. perceptron.weight;;

let predictValue perceptron priori =
  if getNetInput perceptron priori >= 0.0 then 1.0 else -1.0;;

let rec teach perceptron (x::tlxs) (y::tlys) =
  match (tlxs, tlys) with
    | ([], []) -> perceptron
    | _        -> let delta = perceptron.rate *. (y -. (predictValue perceptron x)) in
      teach {rate   = perceptron.rate;
             bias   = perceptron.bias   +. delta;
             weight = perceptron.weight +. (x *. delta);
             errors = perceptron.errors +  (if delta == 0.0 then 0 else 1)} tlxs tlys;;

let rec teachPerceptron perceptron priori posteriori iters =
  match iters with
    | 0 -> perceptron
    | _ ->
      let partiallyTeachedPerceptron = teach perceptron priori posteriori in
         teachPerceptron partiallyTeachedPerceptron priori posteriori (iters - 1);;

let tp = teachPerceptron (initPerceptron 0.0  0.5) [1.0; 1.0; 16.0; 15.0] [0.0; 0.0; -2.0; 11.0] 1024 in
Format.printf "learning rate: %f\n bias: %f\n weight: %f\n errors: %d\n" tp.rate tp.bias tp.weight tp.errors;
Format.printf "test: 1024.121153 is %f\n" (getNetInput tp 1024.121153);;
