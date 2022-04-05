#include "StiIsSvtActiveFunctor.h"
#include "StDbUtilities/St_svtRDOstrippedC.h"

/// Determines whether the object is considered active at the
/// given coordinates. Note that this base class implementation
/// is implemented as an "all active/ianactive" i.e. no partition 
/// of the object is defined.
bool StiIsSvtActiveFunctor::operator()(double y, double z) const {
  St_svtRDOstrippedC *svtRDOs = St_svtRDOstrippedC::instance();
  if (! svtRDOs) return _active;
  Int_t wafer = (Int_t) ((z/_dZ + _nWafers)/2)  + 1 ;
  return ! svtRDOs->svtRDOstrippedStatus(_Barrel,_Ladder,wafer);
} 
