#ifndef St_tofCorrC_h
#define St_tofCorrC_h

#include "TChair.h"
#include "TArrayI.h"
#include "TArrayS.h"
class St_tofCorrC : public TChair {
 public:
  UInt_t     	getNumRows()            const {return GetNRows();}
  enum{
    mNTOF = 192,        // 192 for tof in Run 8++
    mNTDIG = 8,         // 8 per tray in Run 8++
    mNModule = 32,      // 32 for tofr5++ 
    mNVPD = 19,         // 19 tubes at each side
    mNCell = 6,         // 6 cells per module
    mNBinMax = 60,      // 60 bins for T-Tot, T-Z correction

    mNTray = 120,        // 120 trays in full
    mWestVpdTrayId = 121,
    mEastVpdTrayId = 122
  };
  enum calibtype {NOTSET=0, BOARDCALIB=mNTray*(mNModule/4), MODULECALIB=mNTray*mNModule, CELLCALIB=mNTray*mNModule*mNCell};
  Float_t Correction(Int_t N, Float_t *xArray, Float_t x, Float_t *yArray, Short_t &NN);
  Int_t   Index(Int_t tray, Int_t module, Int_t cell);
#if 0
  virtual Int_t NbinsMax()             const = 0;
  virtual Float_t *xarray(Int_t i = 0) const = 0;
  virtual Float_t *yarray(Int_t i = 0) const = 0;
#endif
 protected:
  St_tofCorrC(TTable *table=0);
  calibtype mCalibType;
  TArrayI mIndxArray;
  TArrayS mNusedArray;
  virtual ~St_tofCorrC() {}
 private:
  ClassDef(St_tofCorrC,1) //C++ TChair for tofCorr table class
};
#endif
