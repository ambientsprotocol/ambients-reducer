open Ambient;

/*
   a[b[in_ c.open c.open_] | c[in b.open_ | d[]] | open b]
   : c -> b
-> a[b[open c.open_ | c[open_ | d[]]] | open b]
   : b -> c
   : c -> null
-> a[b[open_ | d[]] | open b]
   : a -> b
   : b -> null
-> a[b[d[]]
*/
let ambient = TestAmbients.create2();
let root = Ambient("", [ambient], [], []);

print_string("--------\n")
print_string("initial state:\n")
let o1 = toString(root);
print_string(o1);
let o1 = toString(List.nth(getChildren(root), 0))
print_string(o1);
let o2 = toString(List.nth(getChildren(List.nth(getChildren(root), 0)), 0))
print_string(o2);
let o3 = toString(List.nth(getChildren(List.nth(getChildren(root), 0)), 1))
print_string(o3);
let o4 = toString(List.nth(getChildren(List.nth(getChildren(List.nth(getChildren(root), 0)), 1)), 0))
print_string(o4);
print_string("--------\n")

let result = reduceFully(root);

print_string("final state:\n")
let o5 = toString(result);
print_string(o5);
let o6 = toString(List.nth(getChildren(result), 0))
print_string(o6);
let o7 = toString(List.nth(getChildren(List.nth(getChildren(result), 0)), 0))
print_string(o7);
