#ifndef StiObjectFactory_H
#define StiObjectFactory_H 1

#include <iostream.h>
#include <stdlib.h>
#include "StiFactory.h"
#include "TObjArray.h"

class StiObjectFactory : public StiFactory
{
 public:

  StiObjectFactory( const char *newName,
		    int originalSize, 
		    int incrementalSize=-1, 
		    int maxIncrementCount=-1);
  virtual ~StiObjectFactory();

  void reset();
  void setIncrementalSize(int increment);
  void setMaxIncrementCount(int maxCount);

  int getOriginalSize();
  int getIncrementalSize();
  int getMaxIncrementCount();
  int getCurrentSize();
  int getNextObjectIndex();
  

 protected:

  static int StiObjectFactory::defaultOriginalSize;
  static int StiObjectFactory::defaultIncrementSize;
  static int StiObjectFactory::defaultMaxIncrementCount;

  TObject * getObject();
  virtual void createObjects(int n)=0;

  int originalSize;
  int incrementalSize;
  int maxIncrementCount;
  int currentSize;
  int nextObjectIndex;
  int incrementCount;

  TObjArray * container;

  ClassDef(StiObjectFactory, 1)

};

#endif
