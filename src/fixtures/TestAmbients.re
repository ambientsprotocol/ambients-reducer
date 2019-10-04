type ambient = Ambient.ambient;
open Capability;

/* a[b[in_ c.open c.open_] | c[in b.open_] | open b] */
let create1 (): ambient = {
  Ambient(0, "a", [
    Ambient(1, "b", [], [], [], []),
    Ambient(2, "c", [], [], [], [])
  ], [], [], []);
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
    /* Ambient(1, "b", [], [In_("c"), Open("c"), Open_], [], []), */
    Ambient(1, "b", [], [In_("c", Open("c", Open_(None)))], [], []),
    Ambient(2, "c", [
      Ambient(3, "d", [], [], [], [])
    ], [In("b", Open_(None))], [], [])
  ], [
    Open("b", None)
  ], [], []);
};

/*
final state:
a[d[]]
*/
let create3 (): ambient = {
  Ambient(0, "a", [
    Ambient(1, "b", [], [Open_(Open("c", None))], [], []),
    Ambient(2, "c", [
      Ambient(3, "d", [], [], [], [])
    ], [
      Open_(None)
    ], [], [])
  ], [
    Open("b", None)
  ], [], []);
};

/*
final state:
a[b[d[]]]
*/
let create4 (): ambient = {
  Ambient(0, "a", [
    Ambient(1, "b", [], [In_("c", Open("c", None))], [], []),
    Ambient(2, "c", [
      Ambient(3, "d", [], [], [], [])
    ], [
      In("b", Open_(None))
    ], [], [])
  ], [], [], []);
};

/* 
Test: out/out_ 
Final state: a[b[]|c[]]
*/
let create5 (): ambient = {
  Ambient(0, "a", [
    Ambient(1, "b", [
      Ambient(2, "c", [], [Out("b", None)], [], [])
    ], [Out_("c", None)], [], []),
  ], [], [], []);
};

/* 
Test: out/out_ 
Final state: a[c[]|d[]]
*/
let create6 (): ambient = {
  Ambient(0, "a", [
    Ambient(1, "b", [
      Ambient(2, "c", [
        Ambient(3, "d", [], [Out("c", None)], [], [])
      ], [Out("b", Out_("d", None))], [], [])
    ], [Out_("c", Open_(None))], [], []),
  ], [Open("b", None)], [], []);
};

let create7 (): ambient = {
  Ambient(0, "a", [
    Ambient(1, "b", [
      Ambient(2, "c", [
        Ambient(3, "d", [], [Out("c", Open_(Open("b", None)))], [], [])
      ], [Out("b", Out_("d", None))], [], [])
    ], [Out_("c", Open_(None))], [], []),
  ], [Open("d", None)], [], []);
};
