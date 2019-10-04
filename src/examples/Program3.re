open Ambient;

let jsonFile = "./__tests__/fixtures/003.json"

let json = Node.Fs.readFileAsUtf8Sync(jsonFile);
let root: ambient = Deserializer.fromJSON(json);

AmbientReducer.reduceFullyDebug(0, root);
