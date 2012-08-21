// $Id: WtpcFilter.h,v 1.4 2012/08/21 17:40:09 stevens4 Exp $
//
//*-- Author : Jan Balewski, MIT


//----------------------------
//------- Sector dependent TPC track filter
//----------------------------
#ifndef W_TPC_FILTER_HH
#define W_TPC_FILTER_HH

#include <TString.h>

class TObjArray;
class TH1;
class StMuTrack ;

class WtpcFilter {
 public:
  WtpcFilter();
  void init(const char *core, int sec, TObjArray *HListX, bool barrel);
  void setCuts(int x, float y, float r1, float r2) {
    par_nFitPts=x; par_nHitFrac=y; par_Rmin=r1; par_Rmax=r2; }
  static int getTpcSec(float phiRad, float etaDet);
  bool accept( const StMuTrack  *prMuTrack);

 private:
  TString name;
  int secID;
  int par_nFitPts;
  float par_nHitFrac, par_Rmin, par_Rmax;

  // histograms
  TObjArray *HList;
  enum {mxHA=8}; TH1 * hA[mxHA];
  void initHistos(bool barrel);

};


#endif


// $Log: WtpcFilter.h,v $
// Revision 1.4  2012/08/21 17:40:09  stevens4
// Revert to previous version
//
// Revision 1.2  2012/06/18 18:28:01  stevens4
// Updates for Run 9+11+12 AL analysis
//
// Revision 1.1  2011/02/10 20:33:27  balewski
// start
//
