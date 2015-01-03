#ifndef ___TMVArank__
#define ___TMVArank__
#include "TDataSet.h"
#include "TTable.h"
#include "Ttypes.h"
struct TMVArank_st {
  Char_t Method[64];      // BDT
  Char_t ActiveVars[128]; // colomn separated list like "postx:prompt:cross:tof:notof:BEMC:noBEMC:nWE:chi2"
  Char_t XmlFile[256];    // path to TMVArank4KFV.y2012.weights.xml
};
class St_TMVArank : public TTable {
 public:
  ClassDefTable(St_TMVArank,TMVArank_st)
  ClassDef(St_TMVArank,1) 
};
#endif
