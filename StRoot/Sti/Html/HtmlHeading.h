#ifndef HtmlHeading_H
#define HtmlHeading_H
#include "HtmlTextElement.h"

class HtmlHeading : public HtmlTextElement
{
public:
  HtmlHeading(const string& level);
  HtmlHeading(const string& level, const string &text);
  virtual ~HtmlHeading();
};

#endif
