#ifndef StiHitFactory_H
#define StiHitFactory_H 1

#include <iostream.h>
#include <stdlib.h>
#include "StiObjectFactory.h"
#include "StiHit.h"

class StiHitFactory : public StiObjectFactory
{
 public:

    //StiHitFactory(); //Not implemented
  StiHitFactory(int original=10000,
		int incremental=10000,
		int maxIncrement=100);
  virtual ~StiHitFactory();

  StiHit * getHit();

    //Diagnostic:
    void print() const;
    
 protected:

  virtual void createObjects(int n); 
};

#endif
