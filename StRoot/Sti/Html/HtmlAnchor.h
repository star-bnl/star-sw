#ifndef HtmlAnchor_H
#define HtmlAnchor_H
#include "HtmlTextElement.h"

class HtmlAnchor : public HtmlTextElement
{
public:
  HtmlAnchor(const string& url, const string &text);
  virtual ~HtmlAnchor();
  virtual void write(ofstream & target);
  void setUrl(const string&level);
  const string&getUrl() const;

protected:
  string _url;
};

#endif
