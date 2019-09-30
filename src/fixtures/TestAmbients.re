type ambient = Ambient.ambient;

/* a[b[in_ c.open c.open_] | c[in b.open_] | open b] */
let create1 (): ambient = {
  Ambient("a", [
    Ambient("b", [], [], []),
    Ambient("c", [], [], [])
  ], [], []);
};

/*
   a[b[in_ c.open c.open_] | c[in b.open_ | d[]] | open b]
   : c -> b
-> a[b[open c.open_ | c[open_ | d[]]] | open b]
   : b -> c
   : c -> null
-> a[b[open_ | d[]] | open b]
   : a -> b
   : b -> null
-> a[d[]]
*/
let create2 (): ambient = {
  Ambient("a", [
    Ambient("b", [], [In_("c"), Open("c"), Open_], []),
    Ambient("c", [Ambient("d", [], [], [])], [In("b"), Open_], [])
  ], [
    Open("b")
  ], []);
};

/*
final state:
a[d[]]
*/
let create3 (): ambient = {
  Ambient("a", [
    Ambient("b", [], [Open_, Open("c")], []),
    Ambient("c", [
      Ambient("d", [], [], [])
    ], [
      Open_
    ], [])
  ], [
    Open("b")
  ], []);
};

/*
final state:
a[b[d[]]]
*/
let create4 (): ambient = {
  Ambient("a", [
    Ambient("b", [], [In_("c"), Open("c")], []),
    Ambient("c", [
      Ambient("d", [], [], [])
    ], [
      In("b"), Open_
    ], [])
  ], [], []);
};

/* 
Test: out/out_ 
Final state: a[b[]|c[]]
*/
let create5 (): ambient = {
  Ambient("a", [
    Ambient("b", [
      Ambient("c", [], [Out("b")], [])
    ], [Out_("c")], []),
  ], [], []);
};
