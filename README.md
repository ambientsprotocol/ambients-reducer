# Ambients Reducer

*Experimental* Ambient Calculus reducer module. Give the reducer an ambient and it will reduce it to its final form.

Input: `a[open_] | open a`

Output: `a[]`

See [example input](https://github.com/haadcode/ambients-reducer/blob/master/__tests__/fixtures/002.json) format (JSON) which is produced by [ambc](https://github.com/aphelionz/ambc), an ambients compiler.

## Install

```
git clone https://github.com/haadcode/ambients-reducer.git
npm install
```

## Build
```
npm run build
```

## Build + Watch

```
npm run start
```

## Usage

Use as a module in a JavaScript program:

```js
const fs = require("fs")
const deserializer = require("./src/Deserializer.bs.js")
const vm = require("./src/AmbientReducer.bs.js")
const jsonFile = "./__tests__/fixtures/001.json"
const json = fs.readFileSync(jsonFile, "utf8")
const program = deserializer.fromJSON(json)
const result = vm.reduceFully(program)
console.log(JSON.stringify(result, null, 2))
```

## Examples

Run [Program1.re](https://github.com/haadcode/ambients-reducer/blob/master/src/examples/Program1.re):

```
node src/examples/Program1.bs.js
```

Output:
```
--------
initial state:
[]
└─ a[open b]
   ├─ b[in_ c.open c.open_]
   └─ c[in b.open_]
      └─ d[]
--------
final state:
[]
└─ a[]
   └─ d[]
```

Run [Program2.re](https://github.com/haadcode/ambients-reducer/blob/master/src/examples/Program2.re):

```
node src/examples/Program2.bs.js
```

See step-by-step state of the reductions by running [Program3.re](https://github.com/haadcode/ambients-reducer/blob/master/src/examples/Program3.re):

```
node src/examples/Program3.bs.js
```

*See the output of Program3 [here](https://github.com/haadcode/ambients-reducer/blob/master/src/examples/Program3.output.md)*
