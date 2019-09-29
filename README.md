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
 | nested: [a],
 | caps:
 | transitions: 0
[a]
 | nested: [b], [c],
 | caps: open b.
 | transitions: 0
[b]
 | nested:
 | caps: in_ c.open c.open_.
 | transitions: 0
[c]
 | nested: [d],
 | caps: in b.open_.
 | transitions: 0
[d]
 | nested:
 | caps:
 | transitions: 0
--------
final state:
[]
 | nested: [a],
 | caps:
 | transitions: 0
[a]
 | nested: [d],
 | caps:
 | transitions: 0
[d]
 | nested:
 | caps:
 | transitions: 0
```
