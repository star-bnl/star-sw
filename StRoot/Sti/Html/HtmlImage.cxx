#include "HtmlImage.h"
  
HtmlImage::HtmlImage()
: HtmlElement("img")
{}

HtmlImage::HtmlImage(const string& image)
: HtmlElement("img")
{
  _image = image;
}

HtmlImage::~HtmlImage()
{}

void HtmlImage::write(ofstream & target)
{
  target << "<img src='"<<_image<<"'>" << endl;
}

void HtmlImage::setImage(const string& image)
{
  _image = image;
}

const string & HtmlImage::getImage() const
{
  return _image;
}


