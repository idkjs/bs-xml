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
      title: elem |> child(select("title", ~namespace=None), text),
      itunesTitle:
        elem
        |> child(select("title", ~namespace=Some(itunes)), text)->optional,
      episodeType: elem |> child(select("episodeType", _), text),
    };
  };
  let decode = Xml.Decode.withName("item", decode);
};

module Channel = {
  type t = {
    items: array(Item.t),
    title: string,
  };

  let decode = elem => {
    Xml.Decode.{
      items: elem |> children(select("item", _), Item.decode),
      title: elem |> child(select("title", _), text),
    };
  };
};

module Rss = {
  type t = {channel: Channel.t};
  open Xml.Decode;

  let decode = elem => {
    {channel: elem |> child(select("channel", _), Channel.decode)};
  };
  let decode = Xml.Decode.withName("rss", decode);
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
      attr1: elem |> attribute("attr1", _),
      attr99: elem |> optional(attribute("attr99", _)),
      item1Text: elem |> child(select("item1", _), text),
      item2Text:
        elem |> child(select("item2", _), e => text(e)->Js.String.trim),
      attr2: elem |> child(select("item2", _), attribute("attr2", _)),
      attr3:
        elem |> child(select("item2", _), attribute("attr3", _))->optional,
      text: elem |> child(select("item3", _), e => text(e)->Js.String.trim),
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
             child(select("x", _), text->map(int)),
             attribute("x", _)->map(int),
           ),
      y:
        elem
        |> either(
             child(select("y", _), text->map(int)),
             attribute("y", _)->map(int),
           ),
    };

  let line =
    (
      elem => {
        Xml.Decode.{
          start: elem |> child(select("start", _), point),
          end_: elem |> child(select("end", _), point),
          thickness:
            elem
            |> child(select("thickness", _), text)
               ->optional
               ->mapOptional(int),
        };
      }
    )
    |> Xml.Decode.withName("line")
    |> Xml.Decode.withNamespace(Some("geometry"));
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

  let decode = elem => {
    {
      // Xml.Decode.

      a: elem |> Xml.Decode.(attribute("a", _)->map(Xml.Decode.float)),
      b:
        elem
        |> Xml.Decode.(attribute("b", _)->map(Xml.Decode.float)->optional),
      c:
        elem
        |> Xml.Decode.(child(Xml.Decode.select("c", _), text->map(bool))),
      d: elem |> Xml.Decode.(attribute("d", _)->map(bool)->optional),
      e: elem |> Xml.Decode.(attribute("eee", _)->optional),
      f: elem |> Xml.Decode.(attribute("f", _)->map(date)),
      g:
        elem
        |> Xml.Decode.(
             oneOf([
               attribute("g", _),
               attribute("gg", _),
               attribute("ggg", _),
             ])
           ),
      h:
        elem
        |> Xml.Decode.(child(select("h", _), text)->withDefault("default")),
      i:
        elem
        |> Xml.Decode.(child(select("i", _), text)->map(Xml.Decode.float)),
    };
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

  let body = root |> child(select("body", _), text) |> Js.String.trim;
  let title =
    root |> child(select("head", _), child(select("title", _), text));
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

  let input4 = {|
  <parent-tag>
    <other>123</other>
    <subelement-two/>
  </parent-tag>
  |};

  let parser = Xml.DomParser.make();

  let parseSubelements = elem => {
    Xml.Decode.(
      elem
      |> oneOf([
           ok(SubElementOne) |> withName("subelement-one"),
           ok(SubElementTwo) |> withName("subelement-two"),
           ok(SubElementThree) |> withName("subelement-three"),
         ])
    );
  };

  // let parseSubelements2 = elem =>
  //   switch (elem->name) {
  //   | "subelement-one" => SubElementOne
  //   | "subelement-two" => SubElementTwo
  //   | "subelement-three" => SubElementThree
  //   | _ => raise(DecodeError("fail"))
  //   };

  let parseOther = elem => {
    open Xml.Decode;
    let other = elem->childElements |> Js.Array.find(e => e->name == "other");
    let other = other->requireSome;
    other->text->int;
  };

  let parseOther2 = elem => {
    Xml.Decode.(elem |> child(select("other", _), text)->map(int));
  };

  let parseInput =
    (
      elem => {
        Xml.Decode.
          (elem |> children(selectAny, parseSubelements));
          // elem |> children(selectAny, parseSubelements2)
      }
    )
    |> Xml.Decode.withName("parent-tag");

  let parseInputOpt =
    (
      elem => {
        Xml.Decode.(elem |> children(selectAny, optional(parseSubelements)));
      }
    )
    |> Xml.Decode.withName("parent-tag");
  open Xml.DomParser;

  let res = parser->parseXml(input)->Result.getExn->parseInput;
  expectToEqual(res, [|SubElementOne, SubElementTwo, SubElementThree|]);

  let res = parser->parseXml(input2)->Result.getExn->parseInput;
  expectToEqual(res, [|SubElementTwo|]);

  let res = parser->parseXml(input3)->Result.getExn->parseInput;
  expectToEqual(res, [||]);

  let res = parser->parseXml(input4)->Result.getExn->parseInputOpt;
  expectToEqual(res, [|None, Some(SubElementTwo)|]);

  let res = parser->parseXml(input4)->Result.getExn->parseOther;
  expectToEqual(res, 123);

  let res = parser->parseXml(input4)->Result.getExn->parseOther2;
  expectToEqual(res, 123);
};
