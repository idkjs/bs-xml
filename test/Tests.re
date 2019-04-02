open Belt;
open Expect_;

let itunes = "http://www.itunes.com/dtds/podcast-1.0.dtd";

module Item = {
  type t = {
    title: string,
    itunesTitle: option(string),
    episodeType: string,
  };

  let decode = elem => {
    Xml.Decode.{
      title: elem |> child("title", ~namespace=None, text),
      itunesTitle:
        elem |> optional(child("title", ~namespace=Some(itunes), text)),
      episodeType: elem |> child("episodeType", text),
    };
  };
};

module Channel = {
  type t = {
    items: array(Item.t),
    title: string,
  };

  let decode = elem => {
    Xml.Decode.{
      items: elem |> children("item", Item.decode),
      title: elem |> child("title", text),
    };
  };
};

module Rss = {
  type t = {channel: Channel.t};

  let decode = elem => {
    Xml.Decode.(
      {
        if (elem |> name != "rss") {
          raise(DecodeError("invalid root"));
        };

        {channel: elem |> child("channel", Channel.decode)};
      }
    );
  };
};

let testRss = () => {
  let p = Xml.DomParser.make();

  let str = Samples.rss1;
  let res = p->Xml.DomParser.parseXml(str);
  let elem = res->Result.getExn;
  let rss = elem->Rss.decode;
  open Rss;
  open Channel;

  expectToEqual(rss.channel.title, "Windows Weekly (MP3)");
  expectToEqual(rss.channel.items->Array.length, 10);
  expectToEqual(
    rss.channel.items->Array.get(0)->Option.getExn.Item.title,
    "WW 588: Live from Ignite!",
  );
  expectToEqual(
    rss.channel.items->Array.get(0)->Option.getExn.Item.itunesTitle,
    Some("Live from Ignite!"),
  );
  expectToEqual(
    rss.channel.items->Array.get(0)->Option.getExn.Item.episodeType,
    "full",
  );
};

module Sample1 = {
  type t = {
    attr1: string,
    attr99: option(string),
    item1Text: string,
    item2Text: string,
    attr2: string,
    attr3: option(string),
    text: string,
  };

  let decode = elem => {
    Xml.Decode.{
      attr1: elem |> attribute("attr1"),
      attr99: elem |> optional(attribute("attr99")),
      item1Text: elem |> child("item1", text),
      item2Text: elem |> child("item2", e => text(e)->Js.String.trim),
      attr2: elem |> child("item2", attribute("attr2")),
      attr3: elem |> optional(child("item2", attribute("attr3"))),
      text: elem |> child("item3", e => text(e)->Js.String.trim),
    };
  };
};

let testSample1 = () => {
  open Sample1;

  let str = {|
            <root attr1="value1">
                <item1 />
                <item2 attr2="value2" attr3="value3">
                    Str 1
                </item2>
                <item3>
                  <![CDATA[ Str 2 ]]>
                </item3>
            </root>
        |};
  let p = Xml.DomParser.make();

  let res = p->Xml.DomParser.parseXml(str);

  let expected = {
    attr1: "value1",
    attr99: None,
    item1Text: "",
    item2Text: "Str 1",
    attr2: "value2",
    attr3: Some("value3"),
    text: "Str 2",
  };
  expectToEqual(res->Result.getExn->decode, expected);
};

let testInvalidSample = () => {
  let str = "<xml>";
  let p = Xml.DomParser.make();

  let xml = p->Xml.DomParser.parseXml(str);
  expectToEqual(xml->Result.isError, true);
};

type line = {
  start: point,
  end_: point,
  thickness: option(int),
}
and point = {
  x: int,
  y: int,
};

module Decode = {
  let point = elem =>
    Xml.Decode.{
      x:
        elem
        |> either(
             child("x", text)->andThen(int),
             attribute("x")->andThen(int),
           ),
      y:
        elem
        |> either(
             child("y", text)->andThen(int),
             attribute("y")->andThen(int),
           ),
    };

  let line = elem => {
    open Xml.Decode;
    let elem = elem->withName("line")->withNamespace(Some("geometry"));
    {
      start: elem |> child("start", point),
      end_: elem |> child("end", point),
      thickness: elem |> optional(child("thickness", text->andThen(int))),
    };
  };
};

let data = {|
<g:line xmlns:g="geometry">
    <start>
        <x>10</x>
        <y>20</y>
    </start>
    <end x="30">
        <y>40</y>
    </end>
</g:line>

|};

let p = Xml.DomParser.make();

let testReadme1 = () => {
  let line = p->Xml.DomParser.parseXml(data)->Belt.Result.getExn->Decode.line;
  expectToEqual(line.start.x, 10);
  expectToEqual(line.start.y, 20);
  expectToEqual(line.end_.x, 30);
  expectToEqual(line.end_.y, 40);
  expectToEqual(line.thickness, None);
};

module T1 = {
  type t = {
    a: float,
    b: option(float),
    c: bool,
    d: option(bool),
    e: option(string),
    f: Js.Date.t,
    g: string,
    h: string,
    i: float,
  };

  let decode = elem =>
    Xml.Decode.{
      a: elem |> attribute("a")->andThen(float),
      b: elem |> optional(attribute("b")->andThen(float)),
      c: elem |> child("c", text->andThen(bool)),
      d: elem |> optional(attribute("d")->andThen(bool)),
      e: elem |> optional(attribute("eee")),
      f: elem |> attribute("f")->andThen(date),
      g:
        elem |> oneOf([attribute("g"), attribute("gg"), attribute("ggg")]),
      h: elem |> child("h", text)->withDefault("default"),
      i: elem |> child("i", text)->andThen(float),
    };
};

let testFloat = () => {
  let line =
    p
    ->Xml.DomParser.parseXml(
        {|<line a="30" b="a" d="false" f="12-13-2015" gg="hello">
        <c>true</c>
        <i>25</i>
    </line>
    |},
      )
    ->Belt.Result.getExn
    ->T1.decode;
  expectToEqual(line.a, 30.0);
  expectToEqual(line.b, None);
  expectToEqual(line.c, true);
  expectToEqual(line.d, Some(false));
  expectToEqual(line.e, None);
  expectToEqual(line.f->Js.Date.getFullYear, 2015.0);
  expectToEqual(line.f->Js.Date.getMonth, 11.0);
  expectToEqual(line.f->Js.Date.getDate, 13.0);
  expectToEqual(line.g, "hello");
  expectToEqual(line.h, "default");
  expectToEqual(line.i, 25.0);
};

let testHtml1 = () => {
  let html = {|<html>
  <head>
    <title>the title</title>
  </head>
  <body>
  <div>
  <span>the body</span>
  </div>
  </body>
  </html>
  |};

  let res = p->Xml.DomParser.parseHtml(html);
  let root = res->Belt.Result.getExn;
  open Xml.Decode;

  let body = root |> child("body", text) |> Js.String.trim;
  let title = root |> child("head", child("title", text));
  expectToEqual(title, "the title");
  expectToEqual(body, "the body");
  expectToEqual(root->name, "html");
  expectToEqual(root->namespace, Some("http://www.w3.org/1999/xhtml"));
};

type subElements =
  | SubElementOne
  | SubElementTwo
  | SubElementThree;

let testIssue1 = () => {
  open Xml.Decode;

  let input = {|
  <parent-tag>
    <subelement-one/>
    <subelement-two/>
    <subelement-three/>
  </parent-tag>
  |};

  let input2 = {|
  <parent-tag>
    <subelement-two/>
  </parent-tag>
  |};

  let input3 = {|
  <parent-tag>
  </parent-tag>
  |};

  let parser = Xml.DomParser.make();

  let parseParent = parent =>
    parent
    ->childElements
    ->Belt.Array.map(elem =>
        switch (elem->name) {
        | "subelement-one" => SubElementOne
        | "subelement-two" => SubElementTwo
        | "subelement-three" => SubElementThree
        | _ => raise(DecodeError(""))
        }
      );

  let res = parser->Xml.DomParser.parseXml(input)->Result.getExn->parseParent;
  expectToEqual(res, [|SubElementOne, SubElementTwo, SubElementThree|]);

  let res =
    parser->Xml.DomParser.parseXml(input2)->Result.getExn->parseParent;
  expectToEqual(res, [|SubElementTwo|]);

  let res =
    parser->Xml.DomParser.parseXml(input3)->Result.getExn->parseParent;
  expectToEqual(res, [||]);
};