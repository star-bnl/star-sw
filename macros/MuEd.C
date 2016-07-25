// $Id: MuEd.C,v 1.1 2013/02/02 18:04:55 fisyak Exp $
// *-- Author :    Valery Fine(fine@bnl.gov)   25/02/2009

//  Additions to use BEMC towers (for display of upsilon events) by Manuel Calderon de la Barca. Feb 2010.
#if !defined(__CINT__)
// code that should be seen ONLY by the compiler
#else
#if !defined(__CINT__) || defined(__MAKECINT__)
// code that should be seen by the compiler AND rootcint
#else
// code that should always be seen
#endif
#endif
//________________________________________________________________________________
#if !defined(__CINT__) || defined(__MAKECINT__)
#include "Riostream.h"
#include <stdio.h>
#include "TROOT.h"
#include "TSystem.h"
#include "TMath.h"
#include "StMuDSTMaker/COMMON/StMuEmcHit.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StChain.h"
#include "St_db_Maker/St_db_Maker.h"
#include "StMuDSTMaker/COMMON/StuDraw3DMuEvent.h"
#include "StEmcUtil/database/StBemcTables.h"
#endif
using namespace std;
StMuDst* muDst = 0; //< The pointer to the current instanse fo the StMuDst object
StMuEvent* MuDstEvent = 0; //< The pointer to the \a StMuEvent object 
StMuDstMaker* muDstMaker = 0;
StuDraw3DMuEvent *gEd = 0; //< The pointer to the current instance of the Event Display. You may want it to change the the drawing option
StBemcTables* mBemcTables = 0;
Bool_t gRotationIsOn = kFALSE; //< Flag whether the animated rotation was initialized to do it at once
//____________________________________________________________________________________
void addEmcHits(Bool_t cuts=kFALSE) { 
  if (!cuts) gEd->Endcaps();
  //TClonesArray *emcs = muDst->emcArray(1);
  //TIter next(emcs);
  //StMuEmcHit *hit = 0;
  //while (hit = (StMuEmcHit *)next()  ) {
  for (Int_t softId = 1; softId<=4800; ++softId) {
    Int_t adc = muDst->muEmcCollection()->getTowerADC(softId);
    Color_t colorResponce=kWhite;
    Int_t status;
    mBemcTables->getStatus(1, softId, status);
    if (status != 1) continue;
    Float_t ped, rms;
    mBemcTables->getPedestal(1, softId, 0, ped, rms);
    if (adc < ped + 3 * rms) continue;
    Float_t calib;
    mBemcTables->getCalib(1, softId, 1, calib);
    Float_t energy = calib * (adc - ped);
    //energy = (adc/285.0*3.7);
    if (softId==1) {
      cout << "id =" << softId << "  energy =" << energy << endl;
      cout << "    adc    = " << adc << endl;
      cout << "    status = " << status << endl;
      cout << "    ped    = " << ped << endl;
      cout << "    rms    = " << rms << endl;
      cout << "    calib  = " << calib << endl;
    }
    if (energy > 0  && energy < 30) {
      // If edep less then MIP (~300 MeV), 60GeV <-> 4096 ADC counts
      if (  energy  < 0.3 )   {                colorResponce = kBlue; 
	// style = 4001;                 //wireframe 
	// If edep larger than MIP but less then 1 GeV 
      } else if (energy  < 1.0 )               colorResponce = kGreen;
      
      // If between 1 GeV and lowest HT threshold (4 GeV for Run7)
      else if (  energy  < 4.0 && energy>1.0)  colorResponce = kYellow;
      // If above lowest HT thershold
      else                                     colorResponce = kRed;
      
      const double maxSize =  400.; // (cm)
      const double scale   =  10.; // (cm/Gev)
      Float_t size =(scale)*energy;
      if (size > maxSize)  size = maxSize ;
      if (!cuts || (cuts && energy>1.0)) {
	gEd->EmcHit(softId,colorResponce, 0, size);
	gEd->AddComment(Form("energy =  %f size=%f ",energy,size));
      }
      //  gEd->EmcHit(softId, adc*0.1);	
    }
  }
}
//____________________________________________________________________________________
void addTracks(Bool_t cuts=kFALSE) {
  cout << "Adding tracks  ----------------------------- " << endl;
  cout << "Run / Event Number: " << MuDstEvent->runNumber() << " / " << MuDstEvent->eventNumber() << endl;
  
  size_t n_prim=muDst->GetNPrimaryTrack();
  
  Int_t counter = 0;
  for (size_t i_track=0; i_track < n_prim; ++i_track) {
    StMuTrack &track = *(muDst->primaryTracks(i_track));
    double pt =track.pt();
    short charge= track.charge();
    double nSigmaE = track.nSigmaElectron();
    if (!cuts || (cuts && (pt>2 && nSigmaE>-0.5))) {
      Style_t sty = gEd->Style(kPrimaryTrack).Sty();
      Size_t  siz = gEd->Style(kPrimaryTrack).Siz();
      gEd->Track(track,StDraw3DStyle::Pt2Color(pt),sty,siz);
      gEd->AddComment(Form("pT =  %f charge=%d ",pt,charge));
      ++counter;
    }
  }
  cout << counter << " primary tracks have been rendered" << endl;
}
//____________________________________________________________________________________
void mrd(Bool_t doTowerCuts=kFALSE, Bool_t doTrackCuts=kFALSE, Bool_t clear=kTRUE) 
{  
  // redraw the event
  if (MuDstEvent) {
    if (clear) gEd->Clear();
    addEmcHits(doTowerCuts);
    addTracks(doTrackCuts);
    
  }
}
//____________________________________________________________________________________
void mae(Bool_t rotation=kFALSE, Bool_t doTowerCuts=kFALSE, Bool_t doTrackCuts=kFALSE) 
{
  // Advance till next "good" event
  // One may want to replace the "plain"  "if" clause below
  // with the full-flegded filter
  gEd->Clear();
  mBemcTables->loadTables(StMaker::GetChain());
  muDst = ((StMuDstMaker *)StMaker::GetChain()->Maker("MuDst"))->muDst();
  if (! muDst) return;
  if ((MuDstEvent = muDst->event())) {
    cout << "Run / Event Number: " << MuDstEvent->runNumber() << " / " << MuDstEvent->eventNumber() << endl;
    mrd(doTowerCuts,doTrackCuts);     // Draw the tracks and towers
    // The file rotation.iv should be in the StRoot/macros/graphics directory 
    // It can be used to set up a rotation of the coordinates.  The orientation,
    // rotation angles and the angular velocity are set in that file. MCBS.
    if (rotation && !gRotationIsOn ) {
      gEd->SetDrawOption("{file:rotation.iv}"); 
      gRotationIsOn = kTRUE;
    }
  } else {
    printf(" MuDstEvent is empty\n");
    return;
  }
}
//____________________________________________________________________________________
void MuEd(const char * detectorNames="TPC,StarBeam") {
  mBemcTables = new StBemcTables;  
  delete gEd; // destroy the built-in display
  gEd = new StuDraw3DMuEvent(detectorNames); // create our own one (with no detector geometry)
  gEd->SetBkColor(kBlack);
  printf("\n The display is ready!\n");
  printf(" calling:\n");   
  printf("\t---\n");
  printf("\tmae()\n");
  printf("\t---\n");
  printf("method to read and show the next event\n");
  mae();
  printf(" call:\n");   
  printf("\t---\n");
  printf("\tmae() - to  advance the event \n");
  printf("\tmrd() - to redraw the event\n");
  printf("\t---\n");
  gEd->SetDrawOption("{view:all}");
}
