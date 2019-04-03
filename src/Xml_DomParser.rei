type t;

[@bs.send]
external parseFromString: (t, string, string) => Dom.document =
  "parseFromString";

let parse: (t, string, string) => Belt.Result.t(Dom.element, string);

let parseXml: (t, string) => Belt.Result.t(Dom.element, string);

let parseHtml: (t, string) => Belt.Result.t(Dom.element, string);

// constructor, avoid external
let make: unit => t;