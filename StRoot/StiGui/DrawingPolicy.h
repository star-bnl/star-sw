#ifndef DrawingPolicy_H_INCLUDED
#define DrawingPolicy_H_INCLUDED
#include "Sti/Base/EditableParameters.h"

///Abstract class defining an interface to a drawing policy.
///Implementations of the policy shall dictates how DRAWABLEs 
///are to be drawn on the canvas, i.e. what their size, color,
///line type shall be depending on external conditions associated
///with the DRAWABLE to be drawn. This is a templated class. The
///template corresponds to the class of DRAWABLE to be policed
///and subsequently drawn.
///\author Claude A Pruneau, Wayne State U. 
///\date Jan 2003
template<class DRAWABLE>
class DrawingPolicy : public EditableParameters
{
  public:
  DrawingPolicy(const string& name, const string& description);
  DrawingPolicy(const DrawingPolicy & policy);
  
  virtual ~DrawingPolicy();
  virtual void police(DRAWABLE * object)=0;

  private:
  DrawingPolicy();//not implemented
};

template<class DRAWABLE>
DrawingPolicy<DRAWABLE>::DrawingPolicy(const string& name, const string& description)
: EditableParameters(name,description)
{}

template<class DRAWABLE>
DrawingPolicy<DRAWABLE>::~DrawingPolicy()
{}

#endif
