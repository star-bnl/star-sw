// $Id: St_emc_hit.h,v 1.1 1998/12/15 22:39:55 akio Exp $
// $Log: St_emc_hit.h,v $
// Revision 1.1  1998/12/15 22:39:55  akio
// Add emc_hit object and  adc_to_energy in here.
//
#ifndef STAR_St_emc_hit
#define STAR_St_emc_hit
#include "St_DataSet.h"
#include "St_DataSetIter.h"
#include "St_Table.h"
#include "StChain.h"
#include "TArrayS.h"
#include "TArrayF.h"
#include "St_ems_control_Table.h"
#include "St_emc_hits_Table.h"
#include "St_emc_pedestal_Table.h"
#include "St_emc_adcslope_Table.h"
#include "St_emc_calib_header_Table.h"
#include "St_emc_hit.h"
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_emc_hit class for <FONT COLOR="RED">EMc Calibrated Hit</FONT>     //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
class St_emc_hit : public St_DataSet {
private:
  TString name;
  Int_t detector;
  Int_t mode;
  Int_t nmodule;
  Int_t neta;
  Int_t nsub;
  Int_t nes;
  Int_t nraw;
  Int_t nhit;
  Float_t etot;
  TArrayS ID;
  TArrayS Mod;
  TArrayF energy;
  void Init(Int_t);
  void GetMem(Int_t);
  Int_t ADCtoEnergy(St_emc_hits*, TArrayF*); // Get ADC from emc_hits and fill energy
  Int_t GetId(Int_t);                        // Get Id from raw#;
  Int_t GetId(Int_t, Int_t, Int_t);          // Get Id from bin;
protected:   
public: 
  St_emc_hit();
  St_emc_hit(const Char_t *);
  virtual ~St_emc_hit();
  const Char_t* GetDetName(){return name;} // Get Detector Name
  Int_t   GetDetector(){return detector;}  // Get Detector Number
  Int_t   GetMode()    {return mode;}      // Get mode
  Int_t   GetNmodule() {return nmodule;}   // Get Number of Module
  Int_t   GetNeta()    {return neta;}      // Get Number of Eta bin
  Int_t   GetNsub()    {return nsub;}      // Get Number of Sub bin
  Int_t   GetNraw()    {return nraw;}      // Get Number of Raws;
  Int_t   GetNhit()    {return nhit;}      // Get Number of Hits;
  Float_t GetEtot()    {return etot;}      // Get total energy;
  Int_t   GetRaw(Int_t, Int_t, Int_t);     // Get raw# from bin, return 0 if no raw for the bin;
  Int_t   Fill(St_emc_hits*);              // Fill energy from ADC table
  Float_t GetEnergy(Int_t i) {return energy[i];}           // Get energy in raw i;
  Float_t GetEnergy(Int_t m, Int_t e, Int_t s){
    return energy[GetRaw(m,e,s)];}                         // Get energy in bin;
  void    GetBin(Int_t, Int_t*, Int_t*, Int_t*);           // Get bin in the raw#; 
  void    GetPos(Int_t, Float_t*, Float_t*);               // Get eta/phi in the raw#;
  void    GetPos(Int_t, Int_t, Int_t, Float_t*, Float_t*); // Get eta/phi in the bin;
  void    GetGrid(Int_t, Int_t, Int_t, Int_t*, Int_t*);    // Get grid(eta/phi) from bin(m,e,s);
  void    GetGrid(Int_t, Int_t*, Int_t*);                  // Get grid(eta/phi) from raw#;
  St_emc_hits *CopyToTable(const Char_t*);                 // Create emc_hit table and copy hits;
  void    PrintHitsAll();                                  // *MENU*
  void    PrintHits(Int_t n=10, Int_t start=0);            // *MENU*
  virtual Long_t HasData() const {return 1;}               // Non zero means it has data;
  virtual Bool_t IsFolder() {return kFALSE;}               // KTRUE means it is directory;
  ClassDef(St_emc_hit,1)                                   // Standard Root macro;
};

#endif





