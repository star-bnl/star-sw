#ifndef MomentumBasedTrackDrawingPolicy_H_INCLUDED
#define MomentumBasedTrackDrawingPolicy_H_INCLUDED
#include "StiGui/DefaultDrawingPolicy.h"

///Concrete class implementing the DrawingPolicy interface  
///to draw tracks with colors reflecting their momentum.
///\author Claude A Pruneau, Wayne State U. 
///\date Jan 2003
class MomentumBasedTrackDrawingPolicy : public DefaultDrawingPolicy
{
  public:
  MomentumBasedTrackDrawingPolicy(const string& name, 
                             const string& descrition,
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
                             double size);
  MomentumBasedTrackDrawingPolicy(const MomentumBasedTrackDrawingPolicy& policy);
  const MomentumBasedTrackDrawingPolicy& operator=(const MomentumBasedTrackDrawingPolicy&);
  virtual ~MomentumBasedTrackDrawingPolicy();
  virtual void police(StiDrawable * object);
  virtual void initialize(); 
  protected:
    ///Colors 
  int _color0; 
  int _color1; 
  int _color2; 
  int _color3; 
  int _color4; 
  int _color5; 
  int _color6; 
  int _color7; 
  int _color8; 
  int _color9; 
 private:
  MomentumBasedTrackDrawingPolicy();//not implemented
};


#endif
