#include "StiGui/StiRootDrawableTrack.h"
#include <algorithm>

StiRootDrawableTrack::StiRootDrawableTrack()
  : StiDrawable(),
    _visible(true)
{
  //_data = new deque<double>();
  _line = new StiTPolyLine3D();
  _line->SetLineColor(1);
  _line->ResetBit(kCanDelete);
}

StiRootDrawableTrack::~StiRootDrawableTrack()
{
  _line->Clear();
}

void StiRootDrawableTrack::draw()
{
 _line->SetPolyLine(size()/3, &(this->operator[](0)));
 _line->Draw();
}

void StiRootDrawableTrack::reset()
{
  //cout << "StiRootDrawableTrack::reset() -i- Started"<<endl;
  _rMax = 0.;
  _rMin = 100000.;
  clear();
  //_data->clear();
  _line->SetPolyLine(0);
  _line->ResetBit(kCanDelete);
}  

void StiRootDrawableTrack::setColor(int val)
{
  _line->SetLineColor(val);
}

void StiRootDrawableTrack::setSize(double val)
{
  // _line->setLineWidth(val);
}

void StiRootDrawableTrack::setVisible(bool val)
{
  _visible = val;
}

void StiRootDrawableTrack::setStyle(int val)
{
  _line->SetLineStyle(val);
}

void StiRootDrawableTrack::add(double x, double y, double z, int direction)
{
  //double r = sqrt(x*x+y*y+z*z);
  //if (direction>0)
  // {
      push_back(x);
      push_back(y);
      push_back(z);
      // }
      //else
      // {
      //}
  //cout << "2r:"<<r<<" rMin:"<<_rMin<<" rMax:"<<_rMax<<endl;
}
