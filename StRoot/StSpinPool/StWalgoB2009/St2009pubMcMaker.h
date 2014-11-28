// $Id: St2009pubMcMaker.h,v 1.5 2014/08/06 11:43:41 jeromel Exp $
//
//*-- Author :  Justin Stevens, IUCF


#ifndef STAR_St2009pubMcMaker
#define STAR_St2009pubMcMaker

/*!
 *                                                                     
 * \class  St2009pubMcMaker
 * \author Justin Stevens, IUCF
 * \date   September 2009
 * \brief  maker to retrieve info from geant.root files for 
 *         comparison with reco quantities from MC
 *
 * This commented block at the top of the header file is considered as
 * the class description to be present on the this class Web page. 
 *
 *
 */                                                                      

#ifndef StMaker_H
#include "StMaker.h"
#endif
#include <TH2.h>
#include <TVector3.h>
class St2009WMaker;
class St2009ZMaker;

class St2009pubMcMaker : public StMaker {
 private:
  
  St2009WMaker *wMK; // W-algo maker with all data
  St2009ZMaker *zMK; // Z-algo maker 

  // histograms
  TObjArray *HList;
  enum {mxHA=200}; TH1 * hA[mxHA];
  enum {mxHB=200}; TH2 * hB[mxHB];
    
  void initHistos();
  void doWanalysis();
  void doWefficiency();
  bool doWMCanalysis();
  void doZefficiency();
  bool doZMCanalysis();
   
  TVector3 mWP; 
  TVector3 mNeutrinoP;
  TVector3 mElectronP;
  TVector3 mElectronSmearP;
  TVector3 mElectronSmearTempP[10];
  TVector3 mVertex;
  //int mKeyElectron;
  //int mEveGenElectron;
  //int mIdElectron;
  float wRB;
  
  TVector3 mZP; 
  TVector3 mZpositronP;
  TVector3 mZelectronP;
  TVector3 mZvertex;
  
 public: 
  St2009pubMcMaker(const char *name="2009pubMc");
  virtual       ~St2009pubMcMaker();
  virtual Int_t  Init();
  virtual Int_t  Make();
  void setHList(TObjArray * x){HList=x;}

  void attachWalgoMaker(St2009WMaker *mk) { wMK=mk;}
  void attachZalgoMaker(St2009ZMaker *mk) { zMK=mk;}

  virtual Int_t InitRun  (int runumber){return 0;}; // Overload empty StMaker::InitRun 
  virtual Int_t FinishRun(int runumber){return 0;}; // Overload empty StMaker::FinishRun 



  /// Displayed on session exit, leave it as-is please ...
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: St2009pubMcMaker.h,v 1.5 2014/08/06 11:43:41 jeromel Exp $ built " __DATE__ " " __TIME__ ; 
    return cvs;
  }

  ClassDef(St2009pubMcMaker,0)   //StAF chain virtual base class for Makers
};

#endif


// $Log: St2009pubMcMaker.h,v $
// Revision 1.5  2014/08/06 11:43:41  jeromel
// Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
//
// Revision 1.4  2011/09/14 14:23:21  stevens4
// update used for cross section PRD paper
//
// Revision 1.3  2010/05/03 19:54:35  stevens4
// only try to calc effic if W->e+nu is found in McEvent
//
// Revision 1.2  2010/01/21 17:54:31  stevens4
// add effic histos and charge seperated background plots
//
// Revision 1.1  2009/11/23 23:00:18  balewski
// code moved spin-pool
//
// Revision 1.1  2009/11/23 21:11:18  balewski
// start
//
