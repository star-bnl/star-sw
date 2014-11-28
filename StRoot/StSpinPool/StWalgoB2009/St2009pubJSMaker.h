// $Id: St2009pubJSMaker.h,v 1.2 2014/08/06 11:43:41 jeromel Exp $
//
//*-- Author :  Justin Stevens, IUCF


#ifndef STAR_St2009pubJSMaker
#define STAR_St2009pubJSMaker

/*!
 *                                                                     
 * \class  St2009pubWanaMaker
 * \author Justin Stevens, IUCF
 * \date   September 2009
 * \brief  my own maker to do analysis outside of W selection
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

class St2009pubJSMaker : public StMaker {
 private:
  
  // parameters
  float  par_highEtow; //thresh for high tower for beambackground
  int par_awayNTrCut; // # of tracks thresh on away side

  float par_countTrPt,par_countTowEt,par_awayTotET; // params in W maker needed to call here to put in histo titles

  St2009WMaker *wMK; // W-algo maker with all data

  // histograms
  TObjArray *HList;
  enum {mxHA=50}; TH1 * hA[mxHA];
  enum {mxHB=32}; TH2 * hB[mxHB];
  
  void initHistos();
  void doWanalysis();
  void doMCanalysis();
  void etowQA(int whichCut, float zVert);
  
  TVector3 mWP; 
  TVector3 mNeutrinoP;
  TVector3 mLeptonP;

 public: 
  St2009pubJSMaker(const char *name="2009pubJS");
  virtual       ~St2009pubJSMaker();
  virtual Int_t  Init();
  virtual Int_t  Make();
  void setHList(TObjArray * x){HList=x;}

  void attachWalgoMaker(St2009WMaker *mk) { wMK=mk;}

  virtual Int_t InitRun  (int runumber){return 0;}; // Overload empty StMaker::InitRun 
  virtual Int_t FinishRun(int runumber){return 0;}; // Overload empty StMaker::FinishRun 



  /// Displayed on session exit, leave it as-is please ...
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: St2009pubJSMaker.h,v 1.2 2014/08/06 11:43:41 jeromel Exp $ built " __DATE__ " " __TIME__ ; 
    return cvs;
  }

  ClassDef(St2009pubJSMaker,0)   //StAF chain virtual base class for Makers
};

#endif


// $Log: St2009pubJSMaker.h,v $
// Revision 1.2  2014/08/06 11:43:41  jeromel
// Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
//
// Revision 1.1  2009/11/23 23:00:18  balewski
// code moved spin-pool
//
// Revision 1.1  2009/11/23 21:11:18  balewski
// start
//
