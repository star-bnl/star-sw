// $Id: StEmcHitCollection.h,v 1.3 1999/07/02 03:01:56 pavlinov Exp $
// $Log: StEmcHitCollection.h,v $
// Revision 1.3  1999/07/02 03:01:56  pavlinov
// Little corrections for Linux
//
// Revision 1.2  1999/07/01 16:17:57  pavlinov
// class StEmcGeom was created and maker was remade for new maker scheme
//
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
#include "StMaker.h"
#include "St_db_Maker/St_db_Maker.h"
#include "StEmcGeom.h"
#include "StChain.h"
#include "TArrayS.h"
#include "TArrayF.h"
#include "TArrayI.h"
#include "St_emc_hits_Table.h"
#include "St_emc_pedestal_Table.h"
#include "St_emc_adcslope_Table.h"
#include "St_emc_calib_header_Table.h"
//
//#include <vector>
//#include "StEvent/StEmcHit.hh"
//#include "StEvent/StEmcTowerHitCollection.hh"
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StEmcHitCollection class for <FONT COLOR="RED">EMc Calibrated Hit</FONT>     //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
class StEmcHitCollection : public St_DataSet , public StEmcGeom {
private:

  St_DataSet     *mEmcCalib;   //!  For StEmcCollection::ADCtoEnergy
  //  TString mName;  Delete => GetName()

  Int_t   mNHit;
  TArrayS mId;      // Id and Energy are the
  TArrayF mEnergy;  // hits collection !!!!

  Float_t mEnergySum;
  Float_t mEtSum;
  TArrayS mModulePosition;

  Int_t ADCtoEnergy(St_emc_hits*);   // Get ADC from emc_hits and fill energy
protected:   
public: 
  StEmcHitCollection();              // For comfort
  StEmcHitCollection(const Char_t* );
  virtual ~StEmcHitCollection();

  inline  void    setEmcCalib(St_DataSet *var) {mEmcCalib = var;}

  inline Int_t   NHit()     {return mNHit;}      // Get Number of Hits;
  inline Float_t EnergySum(){return mEnergySum;} // Get total energy;
  inline Float_t EtSum()    {return mEtSum;}     // Get total transverse energy;

  inline Float_t getHitEnergy(Int_t i) { return mEnergy[i];}  // Get hit energy in raw i;
  inline Int_t   getHitId(Int_t i) { return mId[i];}          // Get hit id     in raw i;
  St_emc_hits *copyToTable(const Char_t*);                 // Create emc_hit table and copy hits; ??
  const TArrayF *getmEnergy() {return &mEnergy;}           // Get array of energy
  void    printHits(Int_t n=10, Int_t start=0);            // *MENU*
  void    printHitsAll();                                  // *MENU*
  void    Browse(TBrowser *b);                             // browser
  //virtual Long_t HasData() const {return 1;}               // Non zero means it has data;
  //virtual Bool_t IsFolder() {return kFALSE;}               // KTRUE means it is directory;
  void    printNameTable();
  
  Int_t   fill(St_emc_hits*);             // Fill energy from ADC table
  ClassDef(StEmcHitCollection,1)                           // Standard Root macro;
};

#endif
