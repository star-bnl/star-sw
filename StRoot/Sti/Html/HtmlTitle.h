#ifndef HtmlTitle_H
#define HtmlTitle_H
#include "HtmlTextElement.h"

class HtmlTitle : public HtmlTextElement
{
public:
  HtmlTitle(const string &title);
  virtual ~HtmlTitle();
};

#endif
