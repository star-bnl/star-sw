#ifndef DefaultDrawingPolicy_H_INCLUDED
#define DefaultDrawingPolicy_H_INCLUDED
#include "StiGui/StiDrawable.h"
#include "StiGui/DrawingPolicy.h"

///Concrete class implementing the DrawingPolicy interface  
///with a trivial uniform drawing policy, i.e. all objects 
///policed by policies of this class will be drawn with the 
///same size, line/marker type and color. This class is templated
///The template is the class of the object to be drawn.
///\author Claude A Pruneau, Wayne State U. 
///\date Jan 2003
class DefaultDrawingPolicy : public DrawingPolicy<StiDrawable>
{
  public:
  DefaultDrawingPolicy(const string& name, 
                             const string& descrition,
                             int color, 
                             int style, 
                             double size);
  DefaultDrawingPolicy(const DefaultDrawingPolicy& policy);
  const DefaultDrawingPolicy& operator=(const DefaultDrawingPolicy&);
  virtual ~DefaultDrawingPolicy();
  virtual void police(StiDrawable *object);
  virtual void initialize(); 
  protected:
    ///Color used to all objects
    int _color;
    ///Line/Marker style used to draw all objects
    int _style;
    ///Line width/marker size used to draw all objects
    double _size;
  private:
  DefaultDrawingPolicy();//not implemented
};



#endif
