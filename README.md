# Ambient Reductions

*Experimental* Ambient Calculus reducer module. Give the reducer an ambient and it will reduce it to its final form.

Input: `a[open_] | open a`

Output: `a[]`

# Install

```
git clone https://github.com/haadcode/ambient-reductions.git
npm install
```

# Build
```
npm run build
```

# Build + Watch

```
npm run start
```

# Examples

Run [Program1.re](https://github.com/haadcode/ambient-reductions/blob/master/src/examples/Program1.re):

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

Run [Program2.re](https://github.com/haadcode/ambient-reductions/blob/master/src/examples/Program2.re):

```
node src/examples/Program2.bs.js
```
