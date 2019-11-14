open Jest;
open Expect;

let jsonFile1 = "./__tests__/fixtures/001.json" /* string monoid */
let jsonFile2 = "./__tests__/fixtures/002.json" /* function returns a string */
let jsonFile3 = "./__tests__/fixtures/003.json" /* function returns an int */
let jsonFile4 = "./__tests__/fixtures/004.json" /* int monoid */

let root1 = jsonFile1 |> Node.Fs.readFileAsUtf8Sync |> Deserializer.fromJSON;
let root2 = jsonFile2 |> Node.Fs.readFileAsUtf8Sync |> Deserializer.fromJSON;
let root3 = jsonFile3 |> Node.Fs.readFileAsUtf8Sync |> Deserializer.fromJSON;
let root4 = jsonFile4 |> Node.Fs.readFileAsUtf8Sync |> Deserializer.fromJSON;

describe("AmbientReducer", () => {
  describe("toValue", () => {
    test("returns a string of string monoid program", () => {
      let result = root1
      |> AmbientReducer.reduceToValue(_ => ignore)
      |> Value.toString
      expect(result) |> toEqual("helloworld")
    });

    test("returns a string from a function", () => {
      let result = root2
      |> AmbientReducer.reduceToValue(_ => ignore)
      |> Value.toString
      expect(result) |> toEqual("hello")
    });

    test("returns an int of int monoid program", () => {
      let result = root4
      |> AmbientReducer.reduceToValue(_ => ignore)
      |> Value.toInt
      expect(result) |> toEqual(3)
    });

    test("returns an int from a function", () => {
      let result = root3
      |> AmbientReducer.reduceToValue(_ => ignore)
      |> Value.toInt
      expect(result) |> toEqual(5)
    });
  });
});
