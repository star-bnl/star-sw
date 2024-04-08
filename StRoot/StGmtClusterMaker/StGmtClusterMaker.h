//
// First Cluster Maker
// \class StGmtClusterMaker
// \authors K.S. Engle and Richard Witt (witt@usna.edu)
// based on StFgtClusterMaker
#ifndef STAR_StGmtClusterMaker_HH
#define STAR_StGmtClusterMaker_HH
#include "StMaker.h"
#include "TH1.h"
#include "TProfile.h"
#include "TROOT.h"
#include "TCanvas.h"
#include "TPolyMarker.h"
#include "StRoot/StChain/StRTSBaseMaker.h"
#include "Stypes.h"
#include "TSpectrum.h"
#include "TF1.h"
#include "TMath.h"
#include "TVirtualFitter.h"

class StGmtStripCollection;
class StGmtHitCollection;

class StGmtClusterMaker :  public StRTSBaseMaker {
  //omitted assignment operator and copy constructor on purpose
 public:
  StGmtClusterMaker( const Char_t* name="GmtCluster");
  ~StGmtClusterMaker() {}
  Int_t Init();
  Int_t Make();
  /**sets the clustering algorithm. Currently there is the simple Clustering algorithm and the max cluster algorithm. 
     The simple cluster algorithm is the default one. The max cluster only selects one hit stip per plane, the one with the highest charge
  */
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StGmtClusterMaker.h,v 1.1.1.1 2013/09/02 15:01:02 fisyak Exp $ built " __DATE__ " " __TIME__ ; return cvs;}
  static Int_t gmtStat;
 protected:
  
  void ClusterBuilder(ULong_t events, UInt_t module, StGmtStripCollection& strips, StGmtHitCollection& hits);
  TF1* FindPeaks(TH1F* hist);

  ClassDef(StGmtClusterMaker,1)
};
#endif
