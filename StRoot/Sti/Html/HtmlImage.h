#ifndef HtmlImage_H
#define HtmlImage_H
#include "HtmlElement.h"

class HtmlImage : public HtmlElement
{
public:
  HtmlImage();
  HtmlImage(const string& image);
  virtual ~HtmlImage();
  virtual void write(ofstream & target);
  void setImage(const string&level);
  const string&getImage() const;

protected:
  string _image;
};

#endif
