/*
  root.exe 'lMuDst.C(-2,"","RpicoDst","PicoOut.root")' 'PicodEdx1.C+("/net/l404/data/fisyak/Pico/2016/125/17125034/st_physics_17125034_raw_5500079.picoDst.root")'
*/
#define DEBUG
//#define __SPARSE__
#if !defined(__CINT__) || defined(__MAKECINT__)
#include <assert.h>
#include "Riostream.h"
#include <stdio.h>
#include "TSystem.h"
#include "TMath.h"
#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "TProfile.h"
#include "TStyle.h"
#include "TF1.h"
#include "TTree.h"
#include "TChain.h"
#include "TFile.h"
#include "TNtuple.h"
#include "THnSparse.h"
#include "TCanvas.h"
#include "TMinuit.h"
#include "TSpectrum.h"
#include "TString.h"
#include "TLine.h"
#include "TText.h"
#include "TROOT.h"
#include "TList.h"
#include "TPolyMarker.h"
#include "StBichsel/Bichsel.h"
#include "StBichsel/StdEdxModel.h"
#include "BetheBloch.h"
#include "TDirIter.h"
#include "TTreeIter.h"
#include "TRandom.h"
#include "TFractionFitter.h"
#include "TLegend.h"
#include "StPicoEvent/StPicoEvent.h"
#include "StPicoEvent/StPicoTrack.h"
#define ClassStMessMgr
#define StMessMgr Int_t
#include "StPicoDstMaker/StPicoDstMaker.h"
#include "StPicoDstMaker/StPicoDst.h"
#include "StdEdxY2Maker/dEdxHist.h"
#include "StdEdxY2Maker/StPidStatus.h"
#undef  StMessMgr
#undef ClassStMessMgr
#else
class TSystem;
class TMath;
class TH1;
class TH2;
class TH3;
class TProfile;
class TStyle;
class TF1;
class TTree;
class TChain;
class TFile;
class TNtuple;
class TCanvas;
class TMinuit;
class TSpectrum;
class TString;
class TLine;
class TText;
class TROOT;
class TList;
class TPolyMarker;
class Bichsel;
class BetheBloch;
class TDirIter;
class TTreeIter;
#endif
#include "Ask.h"
StPicoDstMaker* maker = 0;
#ifndef DEBUG
static Int_t _debug = 0;
#else
static Int_t _debug = 1;
#endif
Bichsel *m_Bichsel = 0;
#include "Names.h"
//________________________________________________________________________________
Int_t Debug() {return _debug;}
//________________________________________________________________________________
void PicodEdx1(const Char_t *files ="./*.MuDst.root",
	    const Char_t *Out = "PicodEdx1.root"){
  maker = new StPicoDstMaker(StPicoDstMaker::IoRead,files);
  maker->Init();
  //         Now iterations
  Long64_t nevent = 9999999;
  for (Long64_t ev = 0; ev < nevent; ev++) {
    if (maker->Make()) break;
    // for Maxim ============================================================
    StPicoDst* pico = maker->picoDst();   // get a pointer to the StPicoDst class, the class that points to all the data
    if (ev%1000 == 0) cout << "Read event\t" << ev << endl;
    StPicoEvent* picoEvent = pico->event(); // get a pointer to the class holding event-wise information
    if (Debug()) {cout << " #" << ev;}
    Int_t NoGlobalTracks = pico->numberOfTracks( );  if (Debug()) {cout << "\tGlobalTracks " << NoGlobalTracks;}
    if (Debug())                                                               {cout << endl;}
    StThreeVectorF pRcVx = picoEvent->primaryVertex();
    Double_t zTpc = pRcVx.z();
    cout << "zTpc = " << zTpc << "\tzVpd = " << picoEvent->vzVpd() << endl;
    Int_t noGoodPrimTracks = 0;
    Bool_t HFTon = kFALSE;
    for (Int_t k = 0; k < NoGlobalTracks; k++) {
      StPicoTrack *gTrack = pico->track(k);
      if (!gTrack) continue;
      //      if (!gTrack->isPrimary()) continue;
      StDcaGeometry dcaG  = gTrack->dcaGeometry();
      KFParticle particle = dcaG.Particle(k);
#ifdef DEBUG
      cout << k << "\t" << particle <<endl;
      cout << "has Pxl1Hit:" <<  gTrack->hasPxl1Hit() << "\tPxl2Hit:" << gTrack->hasPxl2Hit() << "\tIstHit:" <<  gTrack->hasIstHit() << "\tSstHit:" << gTrack->hasSstHit() << endl;
#endif
    }
  }
}


