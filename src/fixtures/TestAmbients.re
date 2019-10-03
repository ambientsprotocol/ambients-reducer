type ambient = Ambient.ambient;

let newId = () => Random.int(10000);

/* a[b[in_ c.open c.open_] | c[in b.open_] | open b] */
let create1 (): ambient = {
  Ambient(0, "a", [
    Ambient(1, "b", [], [], []),
    Ambient(2, "c", [], [], [])
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
  Ambient(0, "a", [
    Ambient(1, "b", [], [In_("c"), Open("c"), Open_], []),
    Ambient(2, "c", [
      Ambient(3, "d", [], [], [])
    ], [In("b"), Open_], [])
  ], [
    Open("b")
  ], []);
};

/*
final state:
a[d[]]
*/
let create3 (): ambient = {
  Ambient(0, "a", [
    Ambient(1, "b", [], [Open_, Open("c")], []),
    Ambient(2, "c", [
      Ambient(3, "d", [], [], [])
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
  Ambient(0, "a", [
    Ambient(1, "b", [], [In_("c"), Open("c")], []),
    Ambient(2, "c", [
      Ambient(3, "d", [], [], [])
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
  Ambient(0, "a", [
    Ambient(1, "b", [
      Ambient(2, "c", [], [Out("b")], [])
    ], [Out_("c")], []),
  ], [], []);
};

/* 
Test: out/out_ 
Final state: a[c[]|d[]]
*/
let create6 (): ambient = {
  Ambient(0, "a", [
    Ambient(1, "b", [
      Ambient(2, "c", [
        Ambient(3, "d", [], [Out("c")], [])
      ], [Out("b"), Out_("d")], [])
    ], [Out_("c"), Open_], []),
  ], [Open("b")], []);
};

let create7 (): ambient = {
  Ambient(0, "a", [
    Ambient(1, "b", [
      Ambient(2, "c", [
        Ambient(3, "d", [], [Out("c"), Open_, Open("b")], [])
      ], [Out("b"), Out_("d")], [])
    ], [Out_("c"), Open_], []),
  ], [Open("d")], []);
};
