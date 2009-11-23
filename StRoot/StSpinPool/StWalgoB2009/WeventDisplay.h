// $Id: WeventDisplay.h,v 1.1 2009/11/23 23:00:18 balewski Exp $
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
  void exportEvent(  WeveVertex myV, WeveEleTrack myTr);
  void export2sketchup(  WeveVertex myV, WeveEleTrack myTr);
  void getPrimTracks(  int vertID);
  void draw(int eveID, int daqSeq, int runNo,  WeveVertex myV, WeveEleTrack myTr);

};

#endif


// $Log: WeventDisplay.h,v $
// Revision 1.1  2009/11/23 23:00:18  balewski
// code moved spin-pool
//
