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
  //cout <<"StiRootDrawableTrack::draw() -I- Started"<<endl;
  //if (_visible)
  //clear();
  //  if (!_data)
  //  {
  //    cout << "_data is NULL"<<endl;
  //    return;
  //  }
  //cout << "SRDT::draw() -I- copy deque.size()=="<< _data->size();
  // copy(_data->begin(),_data->end(),begin());
  //cout << " done"<<endl;
 _line->SetPolyLine(size()/3, &(this->operator[](0)));
 //cout << " and set"<<endl;
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

void StiRootDrawableTrack::add(double x, double y, double z)
{
  double r = sqrt(x*x+y*y+z*z);
  //cout << "1r:"<<r<<" rMin:"<<_rMin<<" rMax:"<<_rMax<<endl;
  if (r>_rMax) _rMax = r;
  if (r<_rMin) _rMin = r;  
  push_back(x);
  push_back(y);
  push_back(z);
  //cout << "2r:"<<r<<" rMin:"<<_rMin<<" rMax:"<<_rMax<<endl;
}
