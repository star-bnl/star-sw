#ifndef StiFactory_H
#define StiFactory_H 1
#include "TObject.h"
#include "stdlib.h"

class StiFactory : public TObject
{
 public:

  StiFactory(const char * name);
  virtual ~StiFactory();

  const char * getName();
  void  setName(const char * newName);
 
 protected:

  char * name;
  
  ClassDef(StiFactory, 1)
};

#endif
