#include "HtmlAnchor.h"

HtmlAnchor::HtmlAnchor(const string& url, const string &text)
: HtmlTextElement("a",text),
  _url(url)
{
}

HtmlAnchor::~HtmlAnchor()
{}

void HtmlAnchor::write(ofstream & target)
{
  target << "<a href='"
         << _url
         << "'>" << endl
         << _text<<endl
         << "</a>" << endl;
}

void HtmlAnchor::setUrl(const string&url)
{
  _url = url;
}

const string& HtmlAnchor::getUrl() const
{
  return _url;
}

