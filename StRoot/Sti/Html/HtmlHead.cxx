#include "HtmlHead.h"
  
HtmlHead::HtmlHead()
: HtmlElement("head")
{
  HtmlElement * meta;
  meta = new HtmlElement("meta");
  meta->setAttribute("http-equiv","Content-Type");
  meta->setAttribute("content","text/html");
  meta->setAttribute("charset","windows-1252");
  add(meta);
  meta = new HtmlElement("meta");
  meta->setAttribute("name","GENERATOR");
  meta->setAttribute("content","PhysicsAnalysisToolkit");
  add(meta);
  meta = new HtmlElement("meta");
  meta->setAttribute("name","ProgId");
  meta->setAttribute("content","PhysicsAnalysisToolkit.Editor.Document");
  add(meta);
  
}

HtmlHead::~HtmlHead()
{}

