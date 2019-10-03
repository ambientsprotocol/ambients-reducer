let string_of_list (list) = List.fold_left((a, b) => a ++ b, "", list);
let isLast (i, list) = (i == List.length(list) - 1);
let orElse (a, b) = {
  switch (a(b)) {
  | exception Not_found => b
  | x => x
  };
};
let generateId () = Random.int(10000);