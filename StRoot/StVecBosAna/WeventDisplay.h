#ifndef WeventDisplay_h
#define WeventDisplay_h

#include "TBox.h"
#include "TH1F.h"
#include "TH2F.h"
#include "TLine.h"

#include "Globals.h"
#include "VecBosVertex.h"


class StVecBosMaker;


// W-event Display container
class WeventDisplay
{
public:
   int           maxEve;
   TH2F         *hTpcET;
   TH2F         *hEmcET;
   TH2F         *hBsmdAdc[mxBSmd];
   TH1F         *hEsmdShower[mxEsmdPlane];
   TH2F         *hEsmdXpt;
   TLine        *etaBL_ln;
   TLine        *etaBR_ln;                   // barrel
   TLine        *etaEL_ln;
   TLine        *etaER_ln;                   // endcap
   TBox         *bxT;
   TBox         *bxE;
   TBox         *bxS;                        // tpc;
   TBox          emc;
   TBox          bsmd;
   StVecBosMaker *wMK;

   WeventDisplay(StVecBosMaker *mk, int mxEv);
   void clear();
   void exportEvent(const char *tit, VecBosVertex &myV, VecBosTrack &myTr, int vertexIndex);
   void export2sketchup(const char *tit, VecBosVertex &myV, VecBosTrack &myTr);
   void getPrimTracks(int vertID, int pointTowId);
   void getPrimTracksFromTree(  int vertID, int pointTowId);
   void draw(const char *tit, int eveID, int daqSeq, int runNo, VecBosVertex &myV, VecBosTrack &myTr);

};

#endif
