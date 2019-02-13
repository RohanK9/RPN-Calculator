open Core

let rpn (acc :float Stack.t) (toPrint : string): float Stack.t = 
  match toPrint with 
    |"+" -> begin
      match Core.Stack.pop acc with
      None -> acc
      |Some b ->
      match Core.Stack.pop acc with
      None -> acc
      |Some a -> Stack.push acc (a +. b); acc
    end
    |"-" -> begin
      match Core.Stack.pop acc with
      None -> acc
      |Some b ->
      match Core.Stack.pop acc with
      None -> acc
      |Some a -> Stack.push acc (a -. b); acc
    end
    |"*" -> begin
      match Core.Stack.pop acc with
      None -> acc
      |Some b ->
      match Core.Stack.pop acc with
      None -> acc
      |Some a -> Stack.push acc (a *. b); acc
    end
    |"/" -> begin
      match Core.Stack.pop acc with
      None -> acc
      |Some b ->
      match Core.Stack.pop acc with
      None -> acc
      |Some a -> Stack.push acc (a /. b); acc
    end
    |"^" -> begin
      match Core.Stack.pop acc with
      None -> acc
      |Some b ->
      match Core.Stack.pop acc with
      None -> acc
      |Some a -> Stack.push acc (a ** b); acc
    end

    |_ ->
      match Caml.Float.of_string_opt toPrint with
      None -> acc
      |Some y -> Core.Stack.push acc y; acc

let read () = 
  let userInput = Caml.read_line () in
  let inputTokens = Str.split (Str.regexp " +") userInput in
  let tokenStack = Stack.create () in
  let output = List.fold_left inputTokens ~init:(tokenStack) ~f:(rpn) in
  match Stack.top output with
  None -> ()
  | Some s -> Printf.printf "%g\n" s

let () = 
  read()