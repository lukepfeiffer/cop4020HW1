let evaluate mathExp =
  let regex = Str.regexp " " in
  let operations : string list = Str.split regex mathExp in
  let expStack = Stack.create() in 
  ignore(Stack.push "" expStack);
  ignore(Stack.pop expStack);
  let regexNum = Str.regexp "[0-9]+.[0-9]*" in

  for i = 0 to ((List.length operations) - 1) do
    let op = ref (List.nth operations i) in
    let isNum = Str.string_match regexNum !op 0 in
    Printf.printf "Operation val: %s\n" !op;
    Printf.printf "Boolean val: %b\n" isNum;
    if isNum then Stack.push !op expStack
    else begin
      let o2 = Stack.pop expStack in
      let o1 = Stack.pop expStack in
      let operand2 = float_of_string o2 in
      let operand1 = float_of_string o1 in
      let temp = ref 0.0 in

      if !op = "+" then temp := operand1 +. operand2
      else if !op = "-" then temp := operand1 -. operand2
      else if !op = "*" then temp := operand1 *. operand2      
      else if !op = "/" then temp := operand1 /. operand2
      else if !op = "^" then temp := operand1 ** operand2
      else temp := 0.0;
      let s = string_of_float !temp in
      Stack.push s expStack
    end;
    let curr = Stack.top expStack in
    Printf.printf "Solution so far: %s\n" curr
  done;

  let s = Stack.pop expStack in
  let sol = float_of_string s in
  sol;;
  

while true do
  print_string "> Enter Math Expression: ";
  let expr = read_line () in
  
  let ans = evaluate expr in
  print_string "Solution: ";
  print_float ans;
  print_string "\n";
done;;