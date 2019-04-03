open Jest;
open Expect;
open Tests;

describe("Invalid sample", () =>
  test("Cannot be parsed", () => {
    testInvalidSample();
    expect(true) |> toEqual(true);
  })
);

describe("Sample 1", () =>
  test("Can be parsed", () => {
    testSample1();
    expect(true) |> toEqual(true);
  })
);

describe("RSS sample", () =>
  test("Can be parsed", () => {
    testRss();
    expect(true) |> toEqual(true);
  })
);

describe("Readme", () =>
  test("should work", () => {
    testReadme1();
    expect(true) |> toEqual(true);
  })
);

describe("Optionals", () =>
  test("should work", () => {
    testFloat();
    expect(true) |> toEqual(true);
  })
);

describe("HTML", () =>
  test("should work", () => {
    testHtml1();
    expect(true) |> toEqual(true);
  })
);

describe("Issue 1", () =>
  test("should work", () => {
    testIssue1();
    expect(true) |> toEqual(true);
  })
);
