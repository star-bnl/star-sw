#include "HtmlElement.h"
#include "HtmlString.h"

HtmlElement::HtmlElement(const string &name)
: _name(name)
{}

HtmlElement::~HtmlElement()
{
  for (HtmlElementVector::iterator iter=begin();iter!=end();++iter)
  {
    delete (*iter);
  }
}

HtmlElement * HtmlElement::add(HtmlElement * htmlElement)
{
  _components.push_back(htmlElement);
  return htmlElement;
}

HtmlElement * HtmlElement::add(const string & text)
{
  HtmlString * s = new HtmlString(text);
  _components.push_back(s);
  return s;
}

void HtmlElement::setAttribute(const string & attribute, const string & value)
{
  _attributes[attribute] = value;
}

const string & HtmlElement::getAttribute(const string & attribute)
{
  return _attributes[attribute];
}

void HtmlElement::write(ofstream& target)
{
  target << "<"<<_name;
  map<string,string>::const_iterator i;
  for (i=_attributes.begin();i!=_attributes.end();++i)
    target << " " << (*i).first << "='"<<(*i).second<<"'";
  target<<">"<<endl;
  HtmlElementVector::iterator iter;
  for (iter=begin();iter!=end();++iter)
  {
    (*iter)->write(target); 
  }
  target << "</" << _name << ">" << endl;
}

vector<HtmlElement*>::iterator HtmlElement::begin()
{
  return _components.begin();
}

vector<HtmlElement*>::iterator HtmlElement::end()
{
  return _components.end();
}

vector<HtmlElement*>::const_iterator HtmlElement::begin() const
{
  return _components.begin();
}

vector<HtmlElement*>::const_iterator HtmlElement::end() const
{
  return _components.end();
}

