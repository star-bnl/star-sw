#ifndef StiTrackNodeFactory_H
#define StiTrackNodeFactory_H 1

#include <iostream.h>
#include <stdlib.h>
#include "StiObjectFactory.h"
#include "StiTrackNode.h"

class StiTrackNodeFactory : public StiObjectFactory
{
 public:

    //StiTrackNodeFactory(); //Not implemented
  StiTrackNodeFactory(int original=10000,
		int incremental=10000,
		int maxIncrement=100);
  virtual ~StiTrackNodeFactory();

  StiTrackNode * getTrackNode();

  //Diagnostic:
  void print() const;
  
 protected:

  virtual void createObjects(int n); 
};

#endif
