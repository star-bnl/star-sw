#ifndef __STTPCT0I__
#define __STTPCT0I__
#include <TObject.h>
#include "StTpcPadPlaneI.h"

class StTpcT0I : public TObject {

  //Abstract base class defining accessors
public:

  virtual float getT0(int row, int pad)   const = 0;
  virtual void SetPadPlanePointer(StTpcPadPlaneI* ppin) = 0;

  ClassDef(StTpcT0I,0)

};

#endif














