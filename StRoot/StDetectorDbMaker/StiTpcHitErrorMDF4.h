#ifndef StiTpcHitErrorMDF4_h
#define StiTpcHitErrorMDF4_h

#include "Sti/StiNodePars.h"
#include "St_MDFCorrection4C.h"
#include "StDetectorDbMaker/St_tpcDriftVelocityC.h"
#include "StDetectorDbMaker/St_tpcPadConfigC.h"

class StiTpcHitErrorMDF4 : public St_MDFCorrection4C {
 public:
  virtual void  calculateError(Double_t _z,  Double_t _eta, Double_t _tanl, 
			       Double_t &ecross, Double_t &edip, 
			       Double_t fudgeFactor = 1, Double_t AdcL = 5.5, Double_t *dZ = 0, Double_t *dX = 0);
  virtual void  calculateError(const StiNodePars *pars,
			       Double_t &ecross, Double_t &edip, 
			       Double_t fudgeFactor = 1,  Double_t AdcL = 5.5, Double_t *dZ = 0, Double_t *dX = 0) {
    calculateError(pars->z(),  pars->eta(), pars->tanl(),  ecross, edip, fudgeFactor, AdcL, dZ, dX);
  }
 protected:
  StiTpcHitErrorMDF4(St_MDFCorrection4 *table=0) : St_MDFCorrection4C(table) {}
  virtual ~StiTpcHitErrorMDF4() {}
 private:
  void convert(Double_t _z,  Double_t _eta, Double_t _tanl, Double_t AdcL);
  Double_t fxx[4];
  virtual  Double_t padPitch() = 0;
  Double_t timePitch() {return St_tpcDriftVelocityC::instance()->timeBucketPitch();}
  ClassDefineChair(StiTpcHitErrorMDF4,St_MDFCorrection4, MDFCorrection4_st )
  ClassDef(StiTpcHitErrorMDF4,1) //C++ TChair for MDFCorrection4 table class
};
//________________________________________________________________________________
class StiTpcInnerHitErrorMDF4 : public StiTpcHitErrorMDF4 {
 public:
  static StiTpcInnerHitErrorMDF4 *instance();
 protected:
  StiTpcInnerHitErrorMDF4(St_MDFCorrection4 *table=0) : StiTpcHitErrorMDF4(table) {}
  virtual ~StiTpcInnerHitErrorMDF4() {fgInstance = 0;}
 private:
  static StiTpcInnerHitErrorMDF4* fgInstance;
  Double_t padPitch() {return St_tpcPadConfigC::instance()->innerSectorPadPitch();}
  ClassDef(StiTpcInnerHitErrorMDF4,1) //C++ TChair for MDFCorrection4 table class
};
//________________________________________________________________________________
class StiTpcOuterHitErrorMDF4 : public StiTpcHitErrorMDF4 {
 public:
  static StiTpcOuterHitErrorMDF4 *instance();
 protected:
  StiTpcOuterHitErrorMDF4(St_MDFCorrection4 *table=0) : StiTpcHitErrorMDF4(table) {}
  virtual ~StiTpcOuterHitErrorMDF4() {fgInstance = 0;}
 private:
  static StiTpcOuterHitErrorMDF4* fgInstance;
  Double_t padPitch() {return St_tpcPadConfigC::instance()->innerSectorPadPitch();}
  ClassDef(StiTpcOuterHitErrorMDF4,1) //C++ TChair for MDFCorrection4 table class
};

#endif
