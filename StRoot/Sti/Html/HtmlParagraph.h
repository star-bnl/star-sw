#ifndef HtmlParagraph_H
#define HtmlParagraph_H
#include "HtmlTextElement.h"

class HtmlParagraph : public HtmlTextElement
{
public:
  HtmlParagraph();
  HtmlParagraph(const string &text);
  virtual ~HtmlParagraph();
};


#endif
