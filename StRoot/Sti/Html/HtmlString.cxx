#include "HtmlString.h"
 
HtmlString::HtmlString(const string & text)
  : HtmlElement(" "),
    _text(text)
{}

HtmlString::~HtmlString()
{}

void HtmlString::write(ofstream& target)
{
  cout << "cell:"<< _text<<endl;
  target << _text;
}

