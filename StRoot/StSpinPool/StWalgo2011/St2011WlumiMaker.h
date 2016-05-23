// $Id: St2011WlumiMaker.h,v 1.1.2.1 2016/05/23 18:33:22 jeromel Exp $
//
//*-- Author :  Jan Balewski, MIT


#ifndef STAR_St2011WlumiMaker
#define STAR_St2011WlumiMaker

/*!
 *                                                                     
 * \class  St2011WlumiMaker
 * \author Jan Balewski, MIT
 * \date   August 2009
 * \brief  accumulates alternative rel lumi monitors based on jet events
 *
 * Jan: this maker was written in 2009 by Ross - I completely changed its functionality and purpose
 *  The Ross's version is in CVS in St2009WB-algo sub dir.
 *
 */                                                                      

#ifndef StMaker_H
#include "StMaker.h"
#endif

class St2011WMaker;

class St2011WlumiMaker : public StMaker {
 private:
  St2011WMaker *wMK; // W-algo maker with all data
 
  // histograms
  TObjArray *HList;
  enum {mxHA=8}; TH1 * hA[mxHA];
  
  void initHistos();
  void bXingSort();
  
 public: 
  St2011WlumiMaker(const char *name="2011Wlumi");
  virtual       ~St2011WlumiMaker(){};
  virtual Int_t  Init();
  virtual Int_t  Make();
  void setHList(TObjArray * x){HList=x;}
  void attachWalgoMaker(St2011WMaker *mk) { wMK=mk;}

  virtual Int_t InitRun(int runumber); // Overload empty StMaker::InitRun 
  virtual Int_t FinishRun(int runumber); // Overload empty StMaker::FinishRun 


  /// Displayed on session exit, leave it as-is please ...
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: St2011WlumiMaker.h,v 1.1.2.1 2016/05/23 18:33:22 jeromel Exp $ built "__DATE__" "__TIME__ ; 
    return cvs;
  }

  ClassDef(St2011WlumiMaker,0)   //StAF chain virtual base class for Makers
};

#endif


// $Log: St2011WlumiMaker.h,v $
// Revision 1.1.2.1  2016/05/23 18:33:22  jeromel
// Updates for SL12d / gcc44 embedding library - StDbLib, QtRoot update, new updated StJetMaker, StJetFinder, StSpinPool ... several cast fix to comply with c++0x and several cons related fixes (wrong parsing logic). Changes are similar to SL13b (not all ode were alike). Branch BSL12d_5_embed.
//
// Revision 1.2.2.2  2016/04/27 17:47:52  zchang
// *** empty log message ***
//
// Revision 1.2  2012/09/14 21:02:29  balewski
// *lumi-maker re-written to accumulate alternative rel lumi monitors,
// * added spin sorting to Zs
//
// Revision 1.1  2011/02/10 20:33:24  balewski
// start
//
