// $Id: St2011pubSpinMaker.h,v 1.9 2014/08/06 11:43:41 jeromel Exp $
//
//*-- Author : Jan Balewski, MIT


#ifndef STAR_St2011pubSpinMaker
#define STAR_St2011pubSpinMaker

/*!
 *                                                                     
 * \class  St2011pubSpinMaker

 * \author Jan Balewski, MIT
 * \date   August 2009
 * \brief  spin sorting of W's 
 *
 *
 *
 */                                                                      

#ifndef StMaker_H
#include "StMaker.h"
#endif
#include <TString.h>

class St2011WMaker;

class St2011pubSpinMaker : public StMaker {
 private:

  int par_QET2PTcut;

  float par_leptonEta1, par_leptonEta2; // narrow the range
  int par_useNoEEMC;

  float parE_leptonEta1, parE_leptonEta2; // narrow the range

  St2011WMaker *wMK; // W-algo maker with all data
  TString core; // name attached to all histos
  TString coreTitle; // eta bin name added title of key histos

  // histograms
  TObjArray *HList;
  enum {mxHA=35}; TH1 * hA[mxHA];
  enum {mxHE=35}; TH1 * hE[mxHE];
  
  void initHistos();
  void bXingSort();
  void bXingSortEndcap();
 public: 
  St2011pubSpinMaker(const char *name="2011pubSpin", const char* etaName="Eta7");
  virtual       ~St2011pubSpinMaker(){};
  virtual Int_t  Init();
  virtual Int_t  InitRun  (int runumber);
  virtual Int_t  Make();
  void setHList(TObjArray * x){HList=x;}
  void setEta(float x, float y) { par_leptonEta1=x; par_leptonEta2=y;};
  void setEtaE(float x, float y) { parE_leptonEta1=x; parE_leptonEta2=y;};
  void setQPT(int x){ par_QET2PTcut=x;};
  void setNoEEMC() {par_useNoEEMC=1;}

  void attachWalgoMaker(St2011WMaker *mk) { wMK=mk;}
  virtual Int_t FinishRun(int runumber);


  /// Displayed on session exit, leave it as-is please ...
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: St2011pubSpinMaker.h,v 1.9 2014/08/06 11:43:41 jeromel Exp $ built " __DATE__ " " __TIME__ ; 
    return cvs;
  }

  ClassDef(St2011pubSpinMaker,0)   //StAF chain virtual base class for Makers
};

#endif


// $Log: St2011pubSpinMaker.h,v $
// Revision 1.9  2014/08/06 11:43:41  jeromel
// Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
//
// Revision 1.8  2013/09/13 19:33:13  stevens4
// Updates to code for combined 2011+2012 result presented to spin PWG 9.12.13
//
// Revision 1.7  2013/06/14 21:08:51  jlzhang
// add histo Q/pT vs. nHitsFit and Q/pT vs. nHitsPos
//
// Revision 1.6  2012/09/17 03:29:30  stevens4
// Updates to Endcap algo and Q*ET/PT charge separation
//
// Revision 1.5  2012/08/28 14:28:27  stevens4
// add histos for barrel and endcap algos
//
// Revision 1.4  2012/08/21 21:28:22  stevens4
// Add spin sorting for endcap Ws
//
// Revision 1.3  2012/08/07 21:06:38  stevens4
// update to tree analysis to produce independent histos in a TDirectory for each eta-bin
//
// Revision 1.2  2012/07/12 20:49:21  balewski
// added spin info(star: bx48, bx7, spin4) and maxHtDSM & BTOW to Wtree
// removed dependence of spinSortingMaker from muDst
// Now Wtree can be spin-sorted w/o DB
// rdMu.C & readWtree.C macros modified
// tested so far on real data run 11
// lot of misc. code shuffling
//
// Revision 1.1  2011/02/10 20:33:25  balewski
// start
//
