#ifndef HtmlDocument_H
#define HtmlDocument_H
#include "HtmlElement.h"
#include "HtmlTextElement.h"
#include "HtmlHead.h"
#include "HtmlHeading.h"
#include "HtmlTitle.h"
#include "HtmlParagraph.h"
#include "HtmlImage.h"
#include "HtmlTable.h"
#include "HtmlTableRow.h"
#include "HtmlTableCell.h"
#include "HtmlBody.h"

class HtmlDocument : public HtmlElement
{
public:
  HtmlDocument(const string & fileName,
	       const string & titleText,
	       const string & mainHeading);
  HtmlDocument();
  virtual ~HtmlDocument();

  virtual HtmlElement * add(HtmlElement * e);
          HtmlElement * add(const std::string& s){return HtmlElement::add(s);}
  virtual HtmlTitle   * addTitle(const string & text);
  virtual HtmlHeading * addHeading(const string & level, const string & text);
  virtual HtmlTable   * addTable();
  virtual HtmlTableRow    * addTableRow();
  virtual HtmlTableCell   * addTableCell();
  virtual HtmlParagraph   * addParagraph(const string & text);
  virtual HtmlImage       * addImage(const string & image);
  virtual HtmlTextElement * addText(const string & text);
  virtual void save();
protected:
  HtmlBody * _body;
  HtmlTable * _currentTable;
  HtmlTableRow * _currentTableRow;
  HtmlTableCell * _currentTableCell;
  HtmlElement   * _currentElement;
  string _fileName;
};


#endif
