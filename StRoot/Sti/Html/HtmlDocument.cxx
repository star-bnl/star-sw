#include "HtmlDocument.h"
#include "HtmlBody.h"
#include "HtmlTitle.h"
#include "HtmlHead.h"
#include "HtmlHeading.h"
#include "Stiostream.h"
#include <fstream>

HtmlDocument::HtmlDocument(const string & fileName,
			   const string & titleText, 
			   const string & mainHeading)
  : HtmlElement("HTML"),
    _body(0),
    _fileName(fileName+".html")
{
  cout << "HtmlDocument::HtmlDocument(...) -I- Started"<<endl;
  HtmlHead*head=static_cast<HtmlHead *>(HtmlElement::add(new HtmlHead()));
  head->add(new HtmlTitle(titleText));
  _body = static_cast<HtmlBody *>(HtmlElement::add(new HtmlBody()));
  _body->add(new HtmlHeading("1",mainHeading));
  cout << "HtmlDocument::HtmlDocument(...) -I- Done"<<endl;
}

HtmlDocument::HtmlDocument()
: HtmlElement("HTML")
{
  HtmlHead*head=static_cast<HtmlHead *>(HtmlElement::add(new HtmlHead()));
  head->add(new HtmlTitle("noName"));
  _body = static_cast<HtmlBody *>(HtmlElement::add(new HtmlBody()));
  _body->add(new HtmlHeading("1","No Name"));}

HtmlDocument::~HtmlDocument()
{}

HtmlTitle   * HtmlDocument::addTitle(const string & text)
{
  return static_cast<HtmlTitle *>(add(new HtmlTitle(text)));
}

HtmlElement * HtmlDocument::add(HtmlElement * htmlElement)
{
  if (_body)
    _body->add(htmlElement);
  else
    HtmlElement::add(htmlElement);
  return htmlElement;
}

HtmlHeading * HtmlDocument::addHeading(const string & level, const string & text)
{
  return static_cast<HtmlHeading *>(_body->add(new HtmlHeading(level,text)));
}

HtmlTable   * HtmlDocument::addTable()
{
  _currentTable = static_cast<HtmlTable *>(_body->add(new HtmlTable()));
  return _currentTable;
}

HtmlTableRow   * HtmlDocument::addTableRow()
{
  _currentTableRow = static_cast<HtmlTableRow *>(_currentTable->add(new HtmlTableRow()));
  return _currentTableRow;
}

HtmlTableCell   * HtmlDocument::addTableCell()
{
  _currentTableCell = static_cast<HtmlTableCell *>(_currentTableRow->add(new HtmlTableCell()));
  _currentElement = _currentTableCell;
  return _currentTableCell;
}

HtmlTextElement * HtmlDocument::addText(const string & text)
{
  return static_cast<HtmlTextElement *>(_currentElement->add(new HtmlTextElement("p",text)));
}

HtmlParagraph   * HtmlDocument::addParagraph(const string & text)
{
  _currentElement = add(new HtmlParagraph(text));
  return static_cast<HtmlParagraph *>(_currentElement);
}

HtmlImage   * HtmlDocument::addImage(const string & image)
{
  return static_cast<HtmlImage *>(add(new HtmlImage(image)));
}

void HtmlDocument::save()
{
  ofstream out(_fileName.c_str());
  write(out);
}
