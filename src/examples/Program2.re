open Ambient;

let ambient = TestAmbients.create7();
let root = Ambient("", [ambient], [], []);

AmbientReducer.reduceFullyDebug(0, root);
