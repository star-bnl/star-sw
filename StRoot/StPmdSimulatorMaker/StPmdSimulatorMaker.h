/*!
 * \class StPmdSimulatorMaker
 * \author
 */
/******************************************************************
 *
 * $Id: StPmdSimulatorMaker.h,v 1.6 2003/11/27 12:33:37 subhasis Exp $
 *
 * Author: Subhasis Chattopadhyay 
 *
 ******************************************************************
 *
 * Description: This is the Slow (fast) simulator for PMD
 ******************************************************************
 *
 * $Log: StPmdSimulatorMaker.h,v $
 * Revision 1.6  2003/11/27 12:33:37  subhasis
 * calib constant values updated
 *
 * Revision 1.5  2003/10/15 10:40:12  subhasis
 * Changes by Dipak (eg GeV to keV
 *
 ******************************************************************/
 
#ifndef STAR_StPmdSimulatorMaker
#define STAR_StPmdSimulatorMaker

#ifndef StMaker_H
#include "StMaker.h"
#endif
#include <TH2.h>
#include <TH1.h>
#include <TCanvas.h>
#include "StPmdUtil/StPmdGeom.h"    //! included geom class

class StPhmdCollection;
class StPmdCollection;
class StPmdHit;
class StPmdDetector;

class StPmdSimulatorMaker : public StMaker {
private:
  Float_t mlcon0;  
  Float_t mlcon1;
  Float_t mlcon2;
  Float_t mpcon0;  
  Float_t mpcon1;  
  Float_t mpcon2;
    Bool_t mResFlag;
protected:
  StPmdCollection *       mPmdCollection;   //!Pmd and CPV collections
  StPhmdCollection *      mevtPmdCollection;   //!Pmd and CPV collections for Stevent

  //!booking PMD histograms
  TH2F *m_pmdEdep2D;    //! 2D-Edep Display of Pmd
  TH2F *m_cpvEdep2D;    //! 2D-Edep Display of Cpv

  TH1F *mEdepPmd;      //! cell edep on Pmd
  TH1F *mEdepPmd_part; //! total edep on Pmd
  TH1F *mPmdAdc;       //! total ADC on PMD
  TH1F *mHitPmd;       //!  total no of hits on Pmd
  TH1F *mEdepCpv;      //!  cell edep on CPV
  TH1F *mEdepCpv_part; //!  Total edep on CPV
  TH1F *mCpvAdc;       //!  Total ADC on CPV
  TH1F *mHitCpv;       //!  total no of hits on Cpv
  TH1F *m_pmdsuper;    //!  total no of Supermodules for PMD
  TH1F *m_cpvsuper;    //!  total no of Supermodules for CPV
  TH2F *m_pmdrow;      //!  Pmd super vs row
  TH2F *m_cpvrow;      //!  Cpv super vs row
  TH2F *m_pmdcol;      //!  Pmd super vs col
  TH2F *m_cpvcol;      //!  Cpv super vs col


  void adcconstants();

public: 
 //! A constructor 
  StPmdSimulatorMaker(const char *name="PmdSimulator"); 
  ~StPmdSimulatorMaker();                               //! A destructor
  virtual Int_t Init(); //!
  virtual Int_t Make();   //! Getting GEANT tables from input file
  Int_t GetPmd();         //! Getting Pmdhits from geant
  Int_t makePmdHits();    //! Making Pmd hits after slow simulation
  Int_t Decode_VolId(Int_t&,Int_t&,Int_t&,Int_t&,Int_t&,Int_t&);  //! decoding
  StPmdHit* Exist(StPmdHit*,StPmdDetector*,Int_t);

  void FinalEdep(StPmdDetector*,Int_t);
  Float_t keV_ADC(Float_t,Float_t&);
  Float_t ADC_Readout(Float_t,Int_t&);
  void  bookHistograms();  //! Booking histograms
  void FillHistograms(StPmdDetector*,StPmdDetector*,Int_t);
  Int_t  fillStEvent(StPmdDetector*, StPmdDetector*);
  void SetResFlag(Bool_t);  //! Readout resolution is applied when Flag = 1

  void  Browse(TBrowser* b); //! StEvent staf will be visible in browser

  ClassDef(StPmdSimulatorMaker, 1)  //! Simulation maker for PMD
};

inline void StPmdSimulatorMaker::SetResFlag(Bool_t val){mResFlag=val;}

inline void StPmdSimulatorMaker::adcconstants()
{
/*
  mlcon0=7.12;
  mlcon1=20.7;
  mlcon2=0.029;
*/

  mlcon0=14.01;
  mlcon1=9.015;
  mlcon2=0.08203;
  
  mpcon0=127.13;
  mpcon1=-0.2182;
  mpcon2=.0001159;
}

#endif


