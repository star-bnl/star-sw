#ifndef StiHitFactory_H
#define StiHitFactory_H 1

#include <iostream.h>
#include <stdlib.h>
#include "StiObjectFactory.h"
#include "StiHit.h"


class StiHitFactory : public StiObjectFactory
{
 public:

  StiHitFactory(int originalSize=10000,
		int incrementalSize=1000,
		int maxIncrementCount=5);
  virtual ~StiHitFactory();

  StiHit * getHit();
  
 protected:

  void createObjects(int n); 

  ClassDef(StiHitFactory, 1)
};

#endif
