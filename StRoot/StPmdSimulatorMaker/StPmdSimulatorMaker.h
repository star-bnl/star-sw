/*!
 * \class StPmdSimulatorMaker
 * \author
 */
/******************************************************************
 *
 * $Id: StPmdSimulatorMaker.h,v 1.2 2002/09/05 06:20:58 subhasis Exp $
 *
 * Author: Subhasis Chattopadhyay 
 *         Premomoy Ghosh
 ******************************************************************
 *
 * Description: This is the Slow (fast) simulator for PMD
 ******************************************************************
 *
 * $Log: StPmdSimulatorMaker.h,v $
 * Revision 1.2  2002/09/05 06:20:58  subhasis
 * Calibration and readout resolution added
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

  
 protected:
  StPmdCollection *       mPmdCollection;   //!Pmd and CPV collections

  //!booking PMD histograms on cell (summing edep of hits on each cell) parameters

  TH1F *mEdepPmd;    //! total edep on Pmd
  TH1F *mHitPmd;     //!  total no of cells hit on Pmd
  TH1F *mPmdAdc;
  TH1F *mEdepCpv;    //!  Total edep on CPV
  TH1F *mHitCpv;     //!  total no of cells hit on Cpv
  TH1F *mCpvAdc;
  TH1F *m_pmdsuper;  //!  total no of Supermodules for PMD
  TH1F *m_cpvsuper;  //!  total no of Supermodules for CPV
  TH2F *m_pmdrow;    //!  Pmd super vs row
  TH2F *m_cpvrow;    //!  Cpv super vs row
  TH2F *m_pmdcol;    //!  Pmd super vs col
  TH2F *m_cpvcol;    //!  Cpv super vs col
  TH2F *m_pmdEdep2D;    //! 2D-Edep Display of Pmd
  TH2F *m_cpvEdep2D;    //! 2D-Edep Display of Cpv



  void adcconstants();


public: 
  StPmdSimulatorMaker(const char *name="PmdSimulator");  //! A constructor 
  ~StPmdSimulatorMaker();                               //! A destructor
  virtual Int_t Init();
  virtual Int_t Make();   //! Getting GEANT tables from input file
  Int_t GetPmd();         //! Getting Pmdhits from geant
  Int_t makePmdHits();    //! Making Pmd hits after slow simulation
  Int_t Decode_VolId(Int_t&,Int_t&,Int_t&,Int_t&,Int_t&,Int_t&);  //! decoding
  StPmdHit* Exist(StPmdHit*,StPmdDetector*,Int_t);




  void calAdc(StPmdDetector*,Int_t);
  Float_t keV_ADC(Float_t,Float_t&);
  Float_t ADC_Readout(Float_t,Int_t&);


  void  bookHistograms();  //! Booking histograms

  void  FillHistograms(StPmdDetector*, StPmdDetector*, Int_t);

  void  Browse(TBrowser* b); //! StEvent staf will be visible in browser

  ClassDef(StPmdSimulatorMaker, 1)  //! Simulation maker for PMD
};

inline void StPmdSimulatorMaker::adcconstants()
{
  mlcon0=7.12;
  mlcon1=20.7;
  mlcon2=0.029;
  mpcon0=127.13;
  mpcon1=-0.2182;
  mpcon2=.0001159;
}

#endif


