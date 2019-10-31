let jsonFile = "./__tests__/fixtures/001.json"

let result = jsonFile 
  |> Node.Fs.readFileAsUtf8Sync 
  |> Deserializer.fromJSON
  |> AmbientReducer.reduceToValue;
  /* |> AmbientReducer.reduceToValueDebug; */

switch (result) {
| String(x) => print_string(x ++ "\n")
| Int(x) => print_string(string_of_int(x) ++ "\n")
| _ => print_string("Unknown value\n")
};
