// $Id: St2011pubMcMaker.h,v 1.1.4.1 2016/05/23 18:33:22 jeromel Exp $
//
//*-- Author :  Justin Stevens, IUCF


#ifndef STAR_St2011pubMcMaker
#define STAR_St2011pubMcMaker

/*!
 *                                                                     
 * \class  St2011pubMcMaker
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
class St2011WMaker;

class St2011pubMcMaker : public StMaker {
 private:
  
  St2011WMaker *wMK; // W-algo maker with all data

  // histograms
  TObjArray *HList;
  enum {mxHA=128}; TH1 * hA[mxHA];
    
  void initHistos();
  void doWanalysis();
  void doWefficiency();
  bool doMCanalysis();
   
  TVector3 mWP; 
  TVector3 mNeutrinoP;
  TVector3 mElectronP;
  TVector3 mVertex;

 public: 
  St2011pubMcMaker(const char *name="2011pubMc");
  virtual       ~St2011pubMcMaker();
  virtual Int_t  Init();
  virtual Int_t  Make();
  void setHList(TObjArray * x){HList=x;}

  void attachWalgoMaker(St2011WMaker *mk) { wMK=mk;}

  virtual Int_t InitRun  (int runumber){return 0;}; // Overload empty StMaker::InitRun 
  virtual Int_t FinishRun(int runumber){return 0;}; // Overload empty StMaker::FinishRun 



  /// Displayed on session exit, leave it as-is please ...
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: St2011pubMcMaker.h,v 1.1.4.1 2016/05/23 18:33:22 jeromel Exp $ built "__DATE__" "__TIME__ ; 
    return cvs;
  }

  ClassDef(St2011pubMcMaker,0)   //StAF chain virtual base class for Makers
};

#endif


// $Log: St2011pubMcMaker.h,v $
// Revision 1.1.4.1  2016/05/23 18:33:22  jeromel
// Updates for SL12d / gcc44 embedding library - StDbLib, QtRoot update, new updated StJetMaker, StJetFinder, StSpinPool ... several cast fix to comply with c++0x and several cons related fixes (wrong parsing logic). Changes are similar to SL13b (not all ode were alike). Branch BSL12d_5_embed.
//
// Revision 1.1.2.2  2016/04/27 17:47:52  zchang
// *** empty log message ***
//
// Revision 1.1  2011/02/10 20:33:25  balewski
// start
//
