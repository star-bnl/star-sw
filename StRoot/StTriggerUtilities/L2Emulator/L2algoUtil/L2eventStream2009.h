#ifndef L2eventStream2009_h
#define L2eventStream2009_h
#include "L2EmcGeom.h"


class L2btowCalAlgo09;
class L2etowCalAlgo09;

//........................................
class HitTower1{// single tower container
 public:
  short rdo;
  short adc; // ADC, pedestal corrected 
  float et,ene; // GeV
  float low_noise_et;//GeV
};

//........................................
class L2BtowCalibData09{// Barrel calibrated data
 public:
  enum {mxListSize=500}; // keep this size small
 private:
  friend class L2btowCalAlgo09;
  friend class L2eventStream2009;
  int hitSize;
  HitTower1 hit[mxListSize]; 
  int nInputBlock; // counts seen blocks over the whole run
  public:
  const int get_hitSize()  const {return hitSize;}
  const HitTower1 *  get_hits() const { return hit;}
};


//........................................
class L2EtowCalibData09{// Endcap calibrated data
 public:
  enum {mxListSize=200}; // keep this size small
 private:
  friend class L2etowCalAlgo09;
  friend class L2eventStream2009;
  int hitSize;
  HitTower1 hit[mxListSize]; 
  int nInputBlock; // counts seen blocks over the whole run
  public:
  const int get_hitSize()  const {return hitSize;}
  const HitTower1 *  get_hits() const { return hit;}
};


//========================================
class L2eventStream2009{ //   E V E N T     S T R E A M  
 public:
  enum {mxToken=4096, tokenZero=0, tokenMask=0xfff};
 private:
  friend class L2btowCalAlgo09;
  friend class L2etowCalAlgo09;

  L2BtowCalibData09 btow[mxToken];  
  L2EtowCalibData09 etow[mxToken];
  
  public:
  const L2BtowCalibData09 * get_btow() { return btow;}
  const L2EtowCalibData09 * get_etow() { return etow;}

};

extern L2eventStream2009 globL2eventStream2009;
#endif

