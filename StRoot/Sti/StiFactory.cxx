#include "StiFactory.h"

ClassImp(StiFactory) 


StiFactory::StiFactory(const char * newName)
{
  name = 0;
  setName(newName);
}

StiFactory::~StiFactory()
{
  delete name;
}

const char * StiFactory::getName()
{
  return name;
}
 

void  StiFactory::setName(const char * newName)
{
  if (name!=0 && name!=newName)
    delete name;
  int size = 1+strlen(newName);
  name = malloc(size);
  strcpy(newName,name);
}

