#ifndef HtmlAttribute_H
#define HtmlAttribute_H
#include <iostream>
#include <string>
#include <math.h>
#include <stdexcept>
#include <vector>

class HtmlAttribute
{
public:
  HtmlAttribute(const string &name, const string & defaultValue);
  ~HtmlAttribute();
  virtual void write(ofstream& target);
protected:
  string _name;
  string _value;
}


#endif
