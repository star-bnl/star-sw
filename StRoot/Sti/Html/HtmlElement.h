#ifndef HtmlElement_HH
#define HtmlElement_HH
#include <iostream>
#include <fstream>
#include <string>
#include <math.h>
#include <stdexcept>
#include <vector>
#include <map>
using std::iostream;
using std::ofstream;
using std::cout;
using std::endl;
using std::string;
using std::vector;
using std::map;

class HtmlElement
{
public:
  typedef vector<HtmlElement*> HtmlElementVector;
  HtmlElement(const string &name);
  virtual ~HtmlElement();
  virtual void write(ofstream& target);
  virtual HtmlElement * add(HtmlElement * htmlElement);
  virtual HtmlElement * add(const string &  s);
  virtual void setAttribute(const string & attribute, const string & value);
  virtual const string & getAttribute(const string & attribute);
protected:
  vector<HtmlElement*>::iterator begin();
  vector<HtmlElement*>::iterator end();
  vector<HtmlElement*>::const_iterator begin() const;
  vector<HtmlElement*>::const_iterator end() const;
  vector<HtmlElement*> _components;
  string _name;
  map<string,string> _attributes;
};

#endif


