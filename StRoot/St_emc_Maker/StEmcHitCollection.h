// $Id: StEmcHitCollection.h,v 1.1 1999/02/12 19:15:39 akio Exp $
// $Log: StEmcHitCollection.h,v $
// Revision 1.1  1999/02/12 19:15:39  akio
// *** empty log message ***
//
// Revision 1.1  1998/12/15 22:39:55  akio
// Add emc_hit object and  adc_to_energy in here.
//
#ifndef STAR_StEmcHitCollection
#define STAR_StEmcHitCollection
#include "St_DataSet.h"
#include "St_DataSetIter.h"
#include "St_Table.h"
#include "St_TableSorter.h"
#include "StChain.h"
#include "TArrayS.h"
#include "TArrayF.h"
#include "TArrayI.h"
#include "St_ems_control_Table.h"
#include "St_emc_hits_Table.h"
#include "St_emc_pedestal_Table.h"
#include "St_emc_adcslope_Table.h"
#include "St_emc_calib_header_Table.h"
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StEmcHitCollection class for <FONT COLOR="RED">EMc Calibrated Hit</FONT>     //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
class StEmcHitCollection : public St_DataSet {
private:
  TString mName;
  Int_t mDetector;
  Int_t mMode;
  Int_t mNModule;
  Int_t mNEta;
  Int_t mNSub;
  Int_t mNes;
  Int_t mNRaw;
  Int_t mNHit;
  Float_t mEnergySum;
  Float_t mEtSum;
  TArrayS mID;
  TArrayS mModulePosition;
  TArrayF mEnergy;
  void init(Int_t);
  void getMemory(Int_t);
  Int_t ADCtoEnergy(St_emc_hits*, TArrayF*); // Get ADC from emc_hits and fill energy
  Int_t getID(Int_t);                        // Get Id from raw#;
  Int_t getID(Int_t, Int_t, Int_t);          // Get Id from bin;
protected:   
public: 
  StEmcHitCollection();
  StEmcHitCollection(const Char_t *);
  virtual ~StEmcHitCollection();
  const Char_t* getDetName(){return mName;}  // Get Detector Name
  Int_t   getDetector() {return mDetector;}  // Get Detector Number
  Int_t   getMode()     {return mMode;}      // Get mode
  Int_t   getNModule()  {return mNModule;}   // Get Number of Module
  Int_t   getNEta()     {return mNEta;}      // Get Number of Eta bin
  Int_t   getNSub()     {return mNSub;}      // Get Number of Sub bin
  Int_t   getNRaw()     {return mNRaw;}      // Get Number of Raws;
  Int_t   getNHit()     {return mNHit;}      // Get Number of Hits;
  Float_t getEnergySum(){return mEnergySum;} // Get total energy;
  Float_t getEtSum()    {return mEtSum;}     // Get total transverse energy;
  Int_t   getRaw(Int_t, Int_t, Int_t);       // Get raw# from bin, return 0 if no raw for the bin;
  Int_t   fill(St_emc_hits*);                // Fill energy from ADC table
  Float_t getEnergy(Int_t i) {return mEnergy[i];}          // Get energy in raw i;
  Float_t getEnergy(Int_t m, Int_t e, Int_t s){
    return mEnergy[getRaw(m,e,s)];}                         // Get energy in bin;
  void    getBin(Int_t, Int_t*, Int_t*, Int_t*);           // Get bin in the raw#; 
  void    getPos(Int_t, Float_t*, Float_t*);               // Get eta/phi in the raw#;
  void    getPos(Int_t, Int_t, Int_t, Float_t*, Float_t*); // Get eta/phi in the bin;
  void    getGrid(Int_t, Int_t, Int_t, Int_t*, Int_t*);    // Get grid(eta/phi) from bin(m,e,s);
  void    getGrid(Int_t, Int_t*, Int_t*);                  // Get grid(eta/phi) from raw#;
  St_emc_hits *copyToTable(const Char_t*);                 // Create emc_hit table and copy hits;
  St_TableSorter *getSortedID();                           // Create index sorted by energy
  TArrayI *getNeighborID(int, int*, int*);                 // Get neighbor hit id
  void    printHits(Int_t n=10, Int_t start=0);            // *MENU*
  void    printHitsAll();                                  // *MENU*
  void    Browse(TBrowser *b);                             // browser
  virtual Long_t HasData() const {return 1;}               // Non zero means it has data;
  virtual Bool_t IsFolder() {return kFALSE;}               // KTRUE means it is directory;
  ClassDef(StEmcHitCollection,1)                           // Standard Root macro;
};

#endif
