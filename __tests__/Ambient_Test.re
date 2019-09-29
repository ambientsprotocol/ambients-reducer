open Jest;
open Expect;
open Ambient;

describe("Ambient", () => {
  test("create", () => {
    let amb: ambient = Ambient.empty("a");
    expect(amb) |> toEqual(Ambient("a", [], [], []))
  });

  test("createTestAmbient1", () => {
    let expected = Ambient("a", [
      Ambient("b", [], [], []),
      Ambient("c", [], [], [])
    ], [], []);
    let amb: ambient = TestAmbients.create1();
    expect(amb) |> toEqual(expected)
  });

  test("createTestAmbient2", () => {
    let expected = Ambient("a", [
        Ambient("b", [], [In_("c"), Open("c"), Open_], []),
        Ambient("c", [Ambient("d", [], [], [])], [In("b"), Open_], [])
      ], [
        Open("b")
      ], []);
    let amb: ambient = TestAmbients.create2();
    expect(amb) |> toEqual(expected)
  });
});
