// $Id: WeventDisplay.h,v 1.3 2012/09/21 16:59:10 balewski Exp $
//
//*-- Author : Jan Balewski, MIT


//----------------------------
//------- W-event Display container
//----------------------------
#ifndef W_EVENT_Diplay_HH
#define W_EVENT_Display_HH

#include <TVector3.h>

#include "Wevent2011.h"

class TH2F;
class TH1F;
class St2011WMaker;
class TLine;
class TBox;

//---------------
class WeventDisplay {
 public:
  int maxEve;
  TH2F *hTpcET;
  TH2F *hEmcET;
  TH2F *hBsmdAdc[mxBSmd];
  TH1F *hEsmdShower[mxEsmdPlane];
  TH2F *hEsmdXpt;
  TLine *etaBL_ln, *etaBR_ln; // barrel
  TLine *etaEL_ln, *etaER_ln; // endcap
  TBox  *bxT, *bxE,*bxS; // tpc, emc, bsmd
  TBox  *bxEs[mxEsmdPlane]; // Esmd used sum range
  St2011WMaker* wMK;

  WeventDisplay( St2011WMaker* mk, int mxEv);
  void clear();
  void exportEvent(  const char *tit,WeveVertex myV, WeveEleTrack myTr, int vertexIndex);
  void export2sketchup(  const char *tit, WeveVertex myV, WeveEleTrack myTr);
  void getPrimTracks(  int vertID,int pointTowId);
  void getPrimTracksFromTree(  int vertID,int pointTowId);
  void draw( const char *tit,int eveID, int daqSeq, int runNo,  WeveVertex myV, WeveEleTrack myTr);

};

#endif


// $Log: WeventDisplay.h,v $
// Revision 1.3  2012/09/21 16:59:10  balewski
// added ESMD peak adjustement - partialy finished
//
// Revision 1.2  2012/06/18 18:28:01  stevens4
// Updates for Run 9+11+12 AL analysis
//
// Revision 1.1  2011/02/10 20:33:27  balewski
// start
//
