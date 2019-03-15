open Xml__;
open Belt;

type t;

[@bs.send]
external parseFromString_: (t, string, string) => Doc.t = "parseFromString";

let parseFromString = (self, text, type_) => {
  parseFromString_(self, text, type_)->Doc.asDom;
};

let parse = (self, text, type_): Result.t(Dom.element, string) => {
  let doc = self->parseFromString_(text, type_);

  switch (doc->Doc.querySelector("parsererror")) {
  | Some(errorElement) => Error(errorElement->Element_.nodeTextContent)
  | None =>
    let nodes = doc->Doc.childNodes->Js.Array.from;
    switch (nodes->Array.keepMap(Node.asElement)->Array.get(0)) {
    | Some(root) => Ok(root->Element_.asDom)
    | None => Error("root element missing")
    };
  };
};

let parseXml = (self, text) => parse(self, text, "text/xml");

let parseHtml = (self, text) => parse(self, text, "text/html");

[@bs.new] external make: unit => t = "DOMParser";
