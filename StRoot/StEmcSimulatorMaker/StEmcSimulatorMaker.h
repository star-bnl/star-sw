#ifndef STAR_StEmcSimulatorMaker
#define STAR_StEmcSimulatorMaker
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StEmcSimulatorMaker 
//
// It replaced St_ems_Maker. This is the clear C++ code. 
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif
#include <TH2.h>
#include <TH1.h>
#include <TCanvas.h>
#include "StEmcUtil/emcInternalDef.h"
#include "StEmcUtil/StEmcGeom.h"
#include "tables/St_emc_hits_Table.h"

class StMcEmcHitCollection;
class StEmcCollection;
class StEmcVirtualSimulator;

class StEmcSimulatorMaker : public StMaker {
private:

  UInt_t  mBEMC;            // Switch for BEMC; 0 => off; >0 => on
  UInt_t  mEEMC;            // Switch for EEMC; 0 => off; >0 => on
  UInt_t  mHistControl;     // Do histogramms (1) or no (0)

  StEmcGeom *mGeom[MAXDET]; //! Geometry 
  
  Bool_t  mCompare;
  TCanvas *mC1;             //!

protected:
  StMcEmcHitCollection*  mEmcMcHits[MAXDET];  //! For convinience 
  St_emc_hits*           mEmcRawHits[MAXDET]; //! For convinience 
  StEmcCollection*       mEmcCollection;      //! As in StEvent

  StEmcVirtualSimulator* mSimulator[MAXDET];  //!

  TH2F *m_nhit;           //! 
  TH2F *m_etot;           //!
  TH2F *m_hits[MAXDET];   //!
  TH2F *m_energy[MAXDET]; //!
  TH1F *m_adc[MAXDET];    //!
  TH1F *mEnergySum[MAXDET];    //!

  TH1F *mhModule[MAXDET];    //! For testing only
  TH1F *mhSub[MAXDET];
  TH1F *mhDiffNumHits[4];    //! 
  TH1F *mhDiffDe[4];         //!
public: 
  StEmcSimulatorMaker(const char *name="EmcSimulator"); 
  ~StEmcSimulatorMaker();
  virtual Int_t Init();
  virtual Int_t Make();
  Int_t fillStEvent();
  void  saveRunco();

  Int_t makeBemc();
  Int_t makeBemcAndBprsMcHits();
  Int_t makeBsmdeAndBsmdpMcHits();
  Int_t makeAllRawHitsForBemc();

  Int_t makeEemc();

  void  bookHistograms(const Int_t);
  void  makeHistograms(const Int_t); //! must be changed

  UInt_t  getBEMC(){return mBEMC;}
  UInt_t  getEEMC(){return mEEMC;}
  UInt_t  getHistControl(){return mHistControl;}
  StMcEmcHitCollection* getEmcMcHits(Int_t det) {return mEmcMcHits[det-1];}
  StMcEmcHitCollection* getBemcMcHits() {return getEmcMcHits(BEMC);}
  StMcEmcHitCollection* getBprsMcHits() {return getEmcMcHits(BPRS);}
  StMcEmcHitCollection* getBsmdeMcHits() {return getEmcMcHits(BSMDE);}
  StMcEmcHitCollection* getBsmdpMcHits() {return getEmcMcHits(BSMDP);}
  StEmcCollection*      getEmcCollection() {return  mEmcCollection;}
  void                  clearStEventStaf() {mEmcCollection = 0;}
  void                  Browse(TBrowser* b); // StEvent staf will be visible in browser
  void   pictureAllDetectors(Int_t print=0);          // *MENU* 
  void   pictureForDetector(Int_t det, Int_t logy=1, Int_t print=0);  // *MENU* 
  //  void   print(const char *filename, Option_t *option) {if(mC1) mC1->Print(filename,option);}

  void   compareOldSimulator();
  void   pictureCompareDe(Int_t print=0);             // *MENU* 

  void   printmBEMC();
  void   setBEMC(UInt_t  key){mBEMC = key; if (Debug()) printmBEMC();}
  void   setHistControl(UInt_t key) {mHistControl = key;}
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StEmcSimulatorMaker.h,v 1.6 2002/06/03 23:35:11 pavlinov Exp $ built "__DATE__" "__TIME__ ; return cvs;}

  ClassDef(StEmcSimulatorMaker, 1)  // Simulation maker for BEMC and EEMC
};

#endif
//////////////////////////////////////////////////////////////////////////////////
//
// $Id: StEmcSimulatorMaker.h,v 1.6 2002/06/03 23:35:11 pavlinov Exp $ 
// $Log: StEmcSimulatorMaker.h,v $
// Revision 1.6  2002/06/03 23:35:11  pavlinov
// Last correction without DB for ped and calib. coeff.
//
// Revision 1.5  2001/09/22 00:29:47  pavlinov
// No public constructor for StEmcGeom
//
// Revision 1.4  2001/03/22 22:04:45  pavlinov
// Clean up for mdc4
//
// Revision 1.3  2001/02/03 00:00:01  pavlinov
// New function Browse() and cleanup for new version of BFC
//
// Revision 1.2  2000/10/28 00:33:45  pavlinov
// added methods getEmcCollectin()
//
// Revision 1.1  2000/10/23 22:53:14  pavlinov
// First working C++ version
//
//
//////////////////////////////////////////////////////////////////////////////////
