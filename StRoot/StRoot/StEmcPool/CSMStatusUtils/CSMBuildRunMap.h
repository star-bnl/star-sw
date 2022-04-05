#ifndef CSMBUILDRUNMAP_H
#define CSMBUILDRUNMAP_H

#include "TObject.h"
#include "TH2.h"

#include <map>
#include <set>
#include <vector>
#include <string>

class CSMBuildRunMap : public TObject {
  public:
  // analysis functions
  Int_t buildRunMap(const Char_t *directory, const Char_t *filter);
  
  private:
  
  ClassDef(CSMBuildRunMap,0)
};

#endif
