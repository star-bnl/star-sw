#ifndef HtmlString_H
#define HtmlString_H
#include "HtmlElement.h"

class HtmlString : public HtmlElement
{
public:
  HtmlString(const string & text);
  virtual ~HtmlString();
  void write(ofstream& target);
 protected: 
  string _text;
};

#endif

