open Ambient;

let ambient = TestAmbients.create2();
let root = Ambient(-1, "", [ambient], [], [], []);

print_string("--------\n")
print_string("initial state:\n")
print_string(Ambient.treeToString(root));

let result = AmbientReducer.reduceFully(root);

print_string("--------\n")
print_string("final state:\n")
print_string(Ambient.treeToString(result));
