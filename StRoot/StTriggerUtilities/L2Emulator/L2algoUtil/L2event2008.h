#ifndef L2event2008_h
#define L2event2008_h
#include "L2EmcGeom.h"

class HitTower{
 public:
  int rdo;
  int adc; // ADC, pedestal corrected 
  float et,ene; // GeV
};

class L2btowCalAlgo08;
class L2event2008{
 private:
  friend class L2btowCalAlgo08;
  int btow_hitSize;
  HitTower btow_hit[BtowGeom::mxRdo]; 
  
  public:
  const int *get_btow_hitSize() {return &btow_hitSize;}
  const HitTower *  get_btow_hits(){ return btow_hit;}
};

extern L2event2008 globL2event2008;
#endif

