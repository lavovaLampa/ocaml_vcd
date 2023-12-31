let ( % ) f g x = f (g x)
let ( %% ) f g x y = f (g x y)
let ( %%% ) f g x y z = f (g x y z)

let ensure_one name ls =
  if List.length ls = 0 then
    raise (Failure (Printf.sprintf "%s not found/defined!" name))
  else if List.length ls > 1 then
    raise (Failure (Printf.sprintf "Multiple %s instances found!" name))
  else List.hd ls

let ensure_one_or_none name ls =
  if List.length ls > 1 then
    raise (Failure (Printf.sprintf "Multiple %s instances found!" name))
  else if List.length ls = 0 then None
  else Some (List.hd ls)
