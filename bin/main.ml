

let () = 
  let file = ref "" in
  let arg = "f", Arg.Set_string file, "Specify the file to execute" in 
  Arg.parse [arg] (Fun.const ()) "miniarm -f [FIlE]" ;
  let _result = Miniarm.Parser.from_file !file in 
  ()
