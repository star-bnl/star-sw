#ifndef HtmlTextElement_H
#define HtmlTextElement_H
#include "HtmlElement.h"

class HtmlTextElement : public HtmlElement
{
public:
  HtmlTextElement(const string& name, const string &text);
  virtual ~HtmlTextElement();
  virtual void write(ofstream& target);
  virtual void setText(const string& title);
  virtual const string & getText();
protected:
  string _text;
};

#endif
