open Jest;
open Expect;
open Ambient;

describe("Ambient", () => {
  test("create", () => {
    let amb: ambient = Ambient.empty(0, "a");
    expect(amb) |> toEqual(Ambient(0, "a", [], [], [], []))
  });

  /* test("createTestAmbient1", () => {
    let expected = Ambient(0, "a", [
      Ambient(1, "b", [], [], [], []),
      Ambient(2, "c", [], [], [], [])
    ], [], [], []);
    let amb: ambient = TestAmbients.create1();
    expect(amb) |> toEqual(expected)
  });

  test("createTestAmbient2", () => {
    let expected = Ambient(0, "a", [
        Ambient(1, "b", [], [In_("c", Open("c", Open_(None)))], [], []),
        Ambient(2, "c", [
          Ambient(3, "d", [], [], [], [])
        ], [In("b", Open_(None))], [], [])
      ], [
        Open("b", None)
      ], [], []);
    let amb: ambient = TestAmbients.create2();
    expect(amb) |> toEqual(expected)
  }); */
});
