#ifndef ___PVgadgets__
#define ___PVgadgets__
#include "TDataSet.h"
#include "TTable.h"
#include "Ttypes.h"
struct PVgadgets_st {
  Float_t postx; 
  Float_t prompt; 
  Float_t beam; 
  Float_t cross; 
  Float_t tof; 
  Float_t notof; 
  Float_t BEMC; 
  Float_t noBEMC; 
  Float_t EEMC; 
  Float_t noEEMC; 
  Float_t nWE; 
  Float_t iMc; 
  Float_t EMC; 
  Float_t noEMC; 
  Float_t chi2; 
  Float_t xV; 
  Float_t yV; 
  Float_t zV; 
  Float_t xMc; 
  Float_t yMc; 
  Float_t zMc; 
  Float_t zVpd; 
  Float_t vR; 
  Float_t Rank; 
  Float_t noTracks; 
  Float_t NoMcTracksWithHits; 
  Float_t l; 
  Float_t lBest; 
  Float_t lMcBest; 
  Float_t good; 
  Float_t timebucket;
};
class St_PVgadgets : public TTable {
 public:
  ClassDefTable(St_PVgadgets,PVgadgets_st)
  ClassDef(St_PVgadgets,1) 
};
#endif
