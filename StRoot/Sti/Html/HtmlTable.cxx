#include "HtmlTable.h"

HtmlTable::HtmlTable()
: HtmlElement("table")
{
  setAttribute("width","100%");
  setAttribute("border","0");
}

HtmlTable::~HtmlTable()
{}

