
// $Id: St2011ZMaker.h,v 1.5 2014/08/06 11:43:41 jeromel Exp $

#ifndef STAR_St2011ZMaker
#define STAR_St2011ZMaker

/*!
 *                                                                     
 * \class  St2011ZMaker
 * \author Ross
 * \date   December 2011
 * \brief  uses tree from W-algo to find Zs
 *
 *
 *
 */                                                                      

#ifndef StMaker_H
#include "StMaker.h"
#endif
class StMuDstMaker;
class St2011WMaker;
class WeventDisplay;
class WeveEleTrack;

class St2011ZMaker : public StMaker {
 private:
  // parameters
  float  par_nearTotEtFracZ;
  float  par_clusterEtZ;
  float  par_delPhi12;
  float par_minMassZ;
  float par_maxMassZ;

  St2011WMaker *wMK; // W-algo maker with all data
  StMuDstMaker *muMK;
  // histograms
  TObjArray *HList;
  enum {mxHA=100}; TH1 * hA[mxHA];
  
  void initHistos();
  void find_Z_boson();
  void findEndcap_Z_boson();

 public: 
  St2011ZMaker(const char *name="2011Zalgo");
  virtual       ~St2011ZMaker(){};
  virtual Int_t  Init();
  virtual Int_t  Make();
  void setHList(TObjArray * x){HList=x;}

  void attachWalgoMaker(St2011WMaker *mk) { wMK=mk;}
  void attachMuMaker(StMuDstMaker *mk) { muMK=mk;}
  void setNearEtFrac(float x) {par_nearTotEtFracZ=x; return;}
  void setClusterMinEt(float x) {par_clusterEtZ=x; return;}
  void setPhi12Min(float x) {par_delPhi12=x; return;}
  void setMinZMass(float x) {par_minMassZ=x; return;}
  void setMaxZMass(float x) {par_maxMassZ=x; return;}
  void printJan(WeveEleTrack *T);
  virtual Int_t InitRun(int runnumber); // Overload empty StMaker::InitRun 
  virtual Int_t FinishRun(int runnumber); // Overload empty StMaker::FinishRun 


  /// Displayed on session exit, leave it as-is please ...
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: St2011ZMaker.h,v 1.5 2014/08/06 11:43:41 jeromel Exp $ built " __DATE__ " " __TIME__ ; 
    return cvs;
  }

  ClassDef(St2011ZMaker,0)   //StAF chain virtual base class for Makers
};

#endif


// $Log: St2011ZMaker.h,v $
// Revision 1.5  2014/08/06 11:43:41  jeromel
// Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
//
// Revision 1.4  2012/08/21 17:40:09  stevens4
// Revert to previous version
//
// Revision 1.2  2012/06/26 20:30:23  stevens4
// Updates ZMaker for mixing barrel and endcap arms
//
// Revision 1.1  2011/02/10 20:33:24  balewski
// start
//
