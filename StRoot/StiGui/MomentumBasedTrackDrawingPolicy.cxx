#include "StiGui/MomentumBasedTrackDrawingPolicy.h"
#include "Sti/StiTrack.h"
#include "Sti/Base/EditableParameter.h"

MomentumBasedTrackDrawingPolicy::MomentumBasedTrackDrawingPolicy(const string& name, 
                             const string& description,
                             int color, 
                             int color0, 
                             int color1, 
                             int color2, 
                             int color3, 
                             int color4, 
                             int color5, 
                             int color6, 
                             int color7, 
                             int color8, 
                             int color9, 
                             int style, 
                             double size)
  : DefaultDrawingPolicy(name,description,color,style,size),
    _color1(_color1),
    _color2(_color2),
    _color3(_color3),
    _color4(_color4),
    _color5(_color5),
    _color6(_color6),
    _color7(_color7),
    _color8(_color8),
    _color9(_color9)
{ 
  initialize();
}

MomentumBasedTrackDrawingPolicy::MomentumBasedTrackDrawingPolicy(const MomentumBasedTrackDrawingPolicy& policy)
  : DefaultDrawingPolicy(policy),
    _color1(policy._color1),
    _color2(policy._color2),
    _color3(policy._color3),
    _color4(policy._color4),
    _color5(policy._color5),
    _color6(policy._color6),
    _color7(policy._color7),
    _color8(policy._color8),
    _color9(policy._color9)
{
  initialize();
}

const MomentumBasedTrackDrawingPolicy& MomentumBasedTrackDrawingPolicy::operator=(const MomentumBasedTrackDrawingPolicy& policy)
{
  DefaultDrawingPolicy::operator=(policy);
  _color1=policy._color1;
  _color2=policy._color2;
  _color3=policy._color3;
  _color4=policy._color4;
  _color5=policy._color5;
  _color6=policy._color6;
  _color7=policy._color7;
  _color8=policy._color8;
  _color9=policy._color9;
  clear();
  initialize();
  return *this;
}
  
MomentumBasedTrackDrawingPolicy::~MomentumBasedTrackDrawingPolicy()
{}

void MomentumBasedTrackDrawingPolicy::police(StiDrawable * object)
{
  object->setStyle(_style);
  object->setSize(_size);
  StiTrack * track = dynamic_cast<StiTrack *>(object);
  double p = track->getP();
  if (0<p<0.1)
    object->setColor(_color);
  else if (p<0.2)
    object->setColor(_color1);
  else if (p<0.3)
    object->setColor(_color2);
  else if (p<0.4)
    object->setColor(_color3);
  else if (p<0.6)
    object->setColor(_color4);
  else if (p<0.8)
    object->setColor(_color5);
  else if (p<1.0)
    object->setColor(_color6);
  else if (p<2.0)
    object->setColor(_color7);
  else if (p<5.0)
    object->setColor(_color8);
  else
    object->setColor(_color9);
}

void MomentumBasedTrackDrawingPolicy::initialize()
{
  add(new EditableParameter("Style","Style",&_style,_style,0,30,1,0));
  add(new EditableParameter("Size","Size",&_size,_size,0.1,2.,0.1,0));
  add(new EditableParameter("Color0","Color 0:0.0<p<0.1 GeV", &_color ,_color ,0,10,1,0));
  add(new EditableParameter("Color1","Color 1:0.1<p<0.2 GeV", &_color1,_color1,0,10,1,0));
  add(new EditableParameter("Color2","Color 2:0.2<p<0.3 GeV", &_color2,_color2,0,10,1,0));
  add(new EditableParameter("Color3","Color 3:0.3<p<0.4 GeV", &_color3,_color3,0,10,1,0));
  add(new EditableParameter("Color4","Color 4:0.4<p<0.6 GeV", &_color4,_color4,0,10,1,0));
  add(new EditableParameter("Color5","Color 5:0.6<p<0.8 GeV", &_color5,_color5,0,10,1,0));
  add(new EditableParameter("Color6","Color 6:0.8<p<1.0 GeV", &_color6,_color6,0,10,1,0));
  add(new EditableParameter("Color7","Color 7:1.0<p<2.0 GeV", &_color7,_color7,0,10,1,0));
  add(new EditableParameter("Color8","Color 8:2.0<p<5.0 GeV", &_color8,_color8,0,10,1,0));
  add(new EditableParameter("Color9","Color 9:5.0<p<10.0 GeV",&_color9,_color9,0,10,1,0));
}
