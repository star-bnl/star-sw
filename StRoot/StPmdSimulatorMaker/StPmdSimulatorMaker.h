/*!
 * \class StPmdSimulatorMaker
 * \author
 */
/******************************************************************
 *
 * $Id: StPmdSimulatorMaker.h,v 1.1 2002/08/27 12:02:56 subhasis Exp $
 *
 * Author: Subhasis Chattopadhyay 
 *
 ******************************************************************
 *
 * Description: This is the Slow (fast) simulator for PMD
 ******************************************************************
 *
 * $Log: StPmdSimulatorMaker.h,v $
 * Revision 1.1  2002/08/27 12:02:56  subhasis
 * First Version
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

protected:
  StPmdCollection *       mPmdCollection;   //!Pmd and CPV collections

  //!booking PMD histograms
  TH2F *m_pmdEdep2D;    //! 2D-Edep Display of Pmd
  TH2F *m_cpvEdep2D;    //! 2D-Edep Display of Cpv

  TH1F *mEdepPmd;    //! total edep on Pmd
  TH1F *mHitPmd;     //!  total no of hits on Pmd
  TH1F *mEdepCpv;    //!  Total edep on CPV
  TH1F *mHitCpv;     //!  total no of hits on Cpv
  TH1F *m_pmdsuper;  //!  total no of Supermodules for PMD
  TH1F *m_cpvsuper;  //!  total no of Supermodules for CPV
  TH2F *m_pmdrow;    //!  Pmd super vs row
  TH2F *m_cpvrow;    //!  Cpv super vs row
  TH2F *m_pmdcol;    //!  Pmd super vs col
  TH2F *m_cpvcol;    //!  Cpv super vs col


public: 
  StPmdSimulatorMaker(const char *name="PmdSimulator");  //! A constructor 
  ~StPmdSimulatorMaker();                               //! A destructor
  virtual Int_t Init();
  virtual Int_t Make();   //! Getting GEANT tables from input file
  Int_t GetPmd();         //! Getting Pmdhits from geant
  Int_t makePmdHits();    //! Making Pmd hits after slow simulation
  Int_t Decode_VolId(Int_t&,Int_t&,Int_t&,Int_t&,Int_t&,Int_t&);  //! decoding
  StPmdHit* Exist(StPmdHit*,StPmdDetector*,Int_t);
  void  bookHistograms();  //! Booking histograms
  void  Browse(TBrowser* b); //! StEvent staf will be visible in browser

  ClassDef(StPmdSimulatorMaker, 1)  //! Simulation maker for PMD
};

#endif

