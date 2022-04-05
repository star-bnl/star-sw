// $Id: WeventDisplay.h,v 1.2 2010/01/06 04:22:15 balewski Exp $
//
//*-- Author : Jan Balewski, MIT


//----------------------------
//------- W-event Display container
//----------------------------
#ifndef W_EVENT_Diplay_HH
#define W_EVENT_Display_HH

#include <TVector3.h>

#include "Wevent2009.h"

class TH2F;
class St2009WMaker;
class TLine;
class TBox;

//---------------
class WeventDisplay {
 public:
  int maxEve;
  TH2F *hTpcET;
  TH2F *hEmcET;
  TH2F *hBsmdAdc[mxBSmd];
  TLine *etaBL_ln, *etaBR_ln; // barrel
  TLine *etaEL_ln, *etaER_ln; // endcap
  TBox  *bxT, *bxE,*bxS; // tpc, emc, bsmd
  St2009WMaker* wMK;

  WeventDisplay( St2009WMaker* mk, int mxEv);
  void clear();
  void exportEvent(  const char *tit,WeveVertex myV, WeveEleTrack myTr);
  void export2sketchup(  const char *tit, WeveVertex myV, WeveEleTrack myTr);
  void getPrimTracks(  int vertID);
  void draw( const char *tit,int eveID, int daqSeq, int runNo,  WeveVertex myV, WeveEleTrack myTr);

};

#endif


// $Log: WeventDisplay.h,v $
// Revision 1.2  2010/01/06 04:22:15  balewski
// added Q/PT plot for Zs, more cleanup
//
// Revision 1.1  2009/11/23 23:00:18  balewski
// code moved spin-pool
//
