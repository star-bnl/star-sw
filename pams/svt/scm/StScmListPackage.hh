#ifndef STSCMLISTPACKAGE_HH
#define STSCMLISTPACKAGE_HH
#include <stdiostream.h>
#include <stdlib.h>
#include <math.h>
#include "StScmPackage.hh"
#include "scm_am.h"

class StScmListPackage
{
 public:
  StScmListPackage();
  ~StScmListPackage();

  StScmPackage* next(StScmPackage *ptr);
  StScmPackage* prev(StScmPackage *ptr);
  StScmPackage* first();
  StScmPackage* last();
  
  int           addNewPackage(StScmPackage *ptr);
  int           getSize();

 private:
  StScmListPackage(const StScmListPackage & originalListPackage);
  StScmListPackage& operator=(const StScmListPackage originalListPackage);

  int           mListLengthP;
  StScmPackage *mFirstP;
  StScmPackage *mLastP;
};
#endif
