#ifndef L2eventStream2008_h
#define L2eventStream2008_h
#include "L2EmcGeom.h"


class L2btowCalAlgo08;
class L2etowCalAlgo08;


//........................................
class HitTower1{
 public:
  short rdo;
  short adc; // ADC, pedestal corrected 
  float et,ene; // GeV
};

//........................................
class L2BlockMonitor{// related to multi-token computation
  friend class L2btowCalAlgo08;
  int nTotal; // # of received blocks
};


//........................................
class L2BtowCalibData08{// Barrel calibrated data
 private:
  friend class L2btowCalAlgo08;
  friend class L2eventStream2008;
  int hitSize;
  HitTower1 hit[BtowGeom::mxRdo]; 
  L2BlockMonitor mon;
  public:
  const int get_hitSize()  const {return hitSize;}
  const HitTower1 *  get_hits() const { return hit;}
};

//........................................
class L2EtowCalibData08{// Endcap calibrated data
  int xxx;
  // nop, add it later
};

//========================================
class L2eventStream2008{ //   E V E N T     S T R E A M  
 public:
  enum {mxToken=4096, tokenZero=0};
 private:
  friend class L2btowCalAlgo08;
  friend class L2etowCalAlgo08;   // not implemented yet

  L2BtowCalibData08 btow[mxToken];  
  L2EtowCalibData08 etow[mxToken];// not implemented yet
  int nBadToken_btow;
  
  public:
  const L2BtowCalibData08 * get_btow() { return btow;}
};

extern L2eventStream2008 globL2eventStream2008;
#endif

