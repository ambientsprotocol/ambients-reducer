let string_of_list (list) = List.fold_left((a, b) => a ++ b, "", list);
let isLast (i, list) = (i == List.length(list) - 1);
