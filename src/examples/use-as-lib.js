const fs = require("fs")
const deserializer = require("./src/Deserializer.bs.js")
const vm = require("./src/AmbientReducer.bs.js")

const jsonFile = "./__tests__/fixtures/001.json"
const json = fs.readFileSync(jsonFile, "utf8")
const program = deserializer.fromJSON(json)

const result = vm.reduceFullyDebug(0, program)

//console.log(JSON.stringify(result, null, 2))
