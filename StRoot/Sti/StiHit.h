#ifndef StiHit_H
#define StiHit_H 1

#include <iostream.h>
#include <stdlib.h>
#include "TObject.h"

class StiHit : public TObject
{
 public:

  StiHit();
  virtual ~StiHit();

 protected:

  float x,y,z;
  ClassDef(StiHit, 1)
};

#endif
