#include "StiRootDrawableHits.h"

StiRootDrawableHits::StiRootDrawableHits() 
  : StiDrawable(),
    _visible(true)
{}

StiRootDrawableHits::~StiRootDrawableHits()
{}

void StiRootDrawableHits::draw()
{ 
  _markers.SetPolyMarker(size()/3, &(this->operator[](0)),_style);
  _markers.SetMarkerColor(_color);
  _markers.Draw();
}

void StiRootDrawableHits::reset()
{
  clear();  
}

void StiRootDrawableHits::setColor(int color)
{
  _color = color;
}

void StiRootDrawableHits::setStyle(int style)
{
  _style = style;
}

void StiRootDrawableHits::setSize(double val)
{
  _markers.SetMarkerSize(val);
}

void StiRootDrawableHits::setVisible(bool val)
{
  _visible = val;
}

void StiRootDrawableHits::add(double x, double y, double z)
{
  push_back(x);
  push_back(y);
  push_back(z);
}
