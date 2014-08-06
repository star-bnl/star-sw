// $Id: St2009pubWanaMaker.h,v 1.3 2014/08/06 11:43:41 jeromel Exp $
//*-- Author : Jan Balewski, MIT


#ifndef STAR_St2009pubWanaMaker
#define STAR_St2009pubWanaMaker

/*!
 *                                                                     
 * \class  St2009pubWanaMaker
 * \author Jan Balewski, MIT
 * \date   August 2009
 * \brief  gathers all results from  W-analysis, Jan's analysis
 *
 *
 *
 */                                                                      

#ifndef StMaker_H
#include "StMaker.h"
#endif
class St2009WMaker;
class StEmcDecoder;

class St2009pubWanaMaker : public StMaker {
 private:
  
  // parameters
  float  par_highET; // cut of for W 2x2 cluster ET

  St2009WMaker *wMK; // W-algo maker with all data
  StEmcDecoder  *mMappB;

  // histograms
  TObjArray *HList;
  enum {mxHA=50}; TH1 * hA[mxHA];
  
  void initHistos();
  void evalWeleTrackSign();
  void scanCrateRate();
  void varyCuts4backgStudy();

 public: 
  St2009pubWanaMaker(const char *name="2009publWana");
  virtual       ~St2009pubWanaMaker(){};
  virtual Int_t  Init();
  virtual Int_t  Make();
  void setHList(TObjArray * x){HList=x;}

  void attachWalgoMaker(St2009WMaker *mk) { wMK=mk;}

  virtual Int_t InitRun  (int runumber);
  virtual Int_t FinishRun(int runumber){return 0;}; // Overload empty StMaker::FinishRun 


  /// Displayed on session exit, leave it as-is please ...
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: St2009pubWanaMaker.h,v 1.3 2014/08/06 11:43:41 jeromel Exp $ built " __DATE__ " " __TIME__ ; 
    return cvs;
  }

  ClassDef(St2009pubWanaMaker,0)   //StAF chain virtual base class for Makers
};

#endif


// $Log: St2009pubWanaMaker.h,v $
// Revision 1.3  2014/08/06 11:43:41  jeromel
// Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
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
