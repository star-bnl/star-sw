#include "HtmlTableCell.h"

HtmlTableCell::HtmlTableCell()
: HtmlElement("td")
{
  setAttribute("valign","middle");
  setAttribute("align","middle");
  setAttribute("bgcolor","#FFFFFF");
  setAttribute("bordercolor","#808000");
}

HtmlTableCell::~HtmlTableCell()
{}


