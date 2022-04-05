#ifndef L2eventStream2012_h
#define L2eventStream2012_h
#include "L2EmcGeom2012.h"


class L2btowCalAlgo12;
class L2etowCalAlgo12;

//........................................
class HitTower1{// single tower container
 public:
  short rdo;
  short adc; // ADC, pedestal corrected 
  float et,ene; // GeV
  float low_noise_et;//GeV
};

//........................................
class L2BtowCalibData12{// Barrel calibrated data
 public:
  enum {mxListSize=500}; // keep this size small
 private:
  friend class L2btowCalAlgo12;
  friend class L2eventStream2012;
  int hitSize;
  HitTower1 hit[mxListSize]; 
  int nInputBlock; // counts seen blocks over the whole run
  public:
  const int get_hitSize()  const {return hitSize;}
  const HitTower1 *  get_hits() const { return hit;}
};


//........................................
class L2EtowCalibData12{// Endcap calibrated data
 public:
  enum {mxListSize=200}; // keep this size small
 private:
  friend class L2etowCalAlgo12;
  friend class L2eventStream2012;
  int hitSize;
  HitTower1 hit[mxListSize]; 
  int nInputBlock; // counts seen blocks over the whole run
  public:
  const int get_hitSize()  const {return hitSize;}
  const HitTower1 *  get_hits() const { return hit;}
};


//========================================
class L2eventStream2012{ //   E V E N T     S T R E A M  
 public:
  enum {mxToken=4096, tokenZero=0, tokenMask=0xfff};
 private:
  friend class L2btowCalAlgo12;
  friend class L2etowCalAlgo12;

  L2BtowCalibData12 btow[mxToken];  
  L2EtowCalibData12 etow[mxToken];
  
  public:
  const L2BtowCalibData12 * get_btow() { return btow;}
  const L2EtowCalibData12 * get_etow() { return etow;}

};

extern L2eventStream2012 globL2eventStream2012;
#endif

