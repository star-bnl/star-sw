#ifndef St_pidCorrectionC_h
#define St_pidCorrectionC_h

#include "TChair.h"
#include "tables/St_pidCorrection_Table.h"
#include "StEvent/StPidParticleDefinition.h"
#include "StEvent/StEnumerations.h"
#include "TF1.h"
class St_pidCorrectionC : public TChair {
 public:
 enum PiDStatusIDs { // from StdEdxY2Maker/StTrackCombPiD.h
    kUndef = kUndefinedMethodId,
    kI70   = kTruncatedMeanId,        
    kI70U  = kEnsembleTruncatedMeanId,
    kFit   = kLikelihoodFitId,        
    kFitU  = kWeightedTruncatedMeanId,
    kdNdx  = kOtherMethodId,          
    kdNdxU = kOtherMethodId2,         
    kBTof,   kETof,   kMtd, kBEmc, kTotal
  };
  static St_pidCorrectionC* 	instance();
  pidCorrection_st 	*Struct(Int_t i = 0) 	const {return ((St_pidCorrection*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()            const {return GetNRows();}
  Int_t         idx(Int_t i = 0)        const {return Struct(i)->idx;}      // row index > 0 if it is real
  Int_t         nrows(Int_t i = 0)      const {return Struct(i)->nrows;}    // total no. of real rows in the table; For Db interface (where nrows = 50)
  Int_t         type(Int_t i = 0)       const {return Struct(i)->type;}	    // type = 0 polymonical fit,                                        use only [min,max]
  Int_t         var(Int_t i = 0)        const {return Struct(i)->var;}	    // fit variable:  0 => pmomL10, 1 => bgL10
  Int_t         particle(Int_t i = 0)   const {return Struct(i)->particle;} // from StEvent/StPidParticleDefinition.h : kUndef = -1, kPidElectron = 0, Proton = 1, 
                                                                            // Kaon = 2, Pion = 3, Muon = 4, Deuteron = 5, Triton = 6,
									    // He3 = 7, Alpha = 8, He6 = 9, Li5 = 10, Li6,= 11, Li7 = 12, Be7 = 13, Be9 = 14, Be10 = 15, B11 = 16
  Int_t         charge(Int_t i = 0)     const {return Struct(i)->charge;}   // +/-1, 0 does not matter
  Int_t         pull(Int_t i = 0)       const {return Struct(i)->pull;}	    // != 0 calculated pull correction, == 0 to value
  Int_t         det(Int_t i = 0)        const {return Struct(i)->det;}	    // from StdEdxY2Maker/StTrackPiD.h : kUndef = 0, kI70 = 1, kI70U = 2, kFit = 3, kFitU = 4, kdNdx = 5, 
                                                                            // kdNdxU = 6, kBTof -7 , kETof = 8, kMtd = 9, kBEmc = 10
  Int_t         npar(Int_t i = 0)       const {return Struct(i)->npar;}	    // npar < 0, X = exp(x) paramterization; abs(npar) >= 100 cut on range [min.max]
  Double_t      OffSet(Int_t i = 0)     const {return Struct(i)->OffSet;}   // for future use
  Double_t      min(Int_t i = 0)        const {return Struct(i)->min;}	    // fit range
  Double_t      max(Int_t i = 0)        const {return Struct(i)->max;}	    // 
  Double_t*     a(Int_t i = 0)  const {return Struct(i)->a;}		    // a[npar]
  Char_t*       comment(Int_t i = 0)    const {return Struct(i)->comment;}
  Double_t      CalcCorrection(Int_t i, Double_t x, Double_t z = 0, Int_t NparMax = -1) const;
  Double_t      SumSeries(pidCorrection_st *cor, Double_t x, Double_t z = 0, Int_t NparMax = -1) const;
  Double_t      Correction(Double_t X, Int_t part = kPidPion, Int_t det = kFit, Int_t charge = 0, Int_t pull = 0, Int_t varT = 0);
  static Double_t func(Double_t *x, Double_t *p);
  TF1*          Func(Int_t part = kPidPion, Int_t det = kFit, Int_t charge = 0, Int_t pull = 0, Int_t varT = 0);
  Int_t         IsActiveChair() const;
 protected:
  St_pidCorrectionC(St_pidCorrection *table=0) : TChair(table) {}
  virtual ~St_pidCorrectionC() {fgInstance = 0;}
 private:
  static St_pidCorrectionC* fgInstance;
  ClassDefChair(St_pidCorrection, pidCorrection_st )
  ClassDef(St_pidCorrectionC,1) //C++ TChair for pidCorrection table class
};
#endif
