open Ambient;

let ambient = TestAmbients.create6();
let root = Ambient("", [ambient], [], []);

AmbientReducer.reduceFullyDebug(0, root);
