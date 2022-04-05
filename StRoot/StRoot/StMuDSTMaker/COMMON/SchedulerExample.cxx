/***************************************************************************
 *
 * $Id: SchedulerExample.cxx,v 1.2 2003/09/02 17:58:44 perev Exp $
 * Author: Frank Laue, BNL, laue@bnl.gov
 *
 ***************************************************************************/

#include "SchedulerExample.h"

#include "Stiostream.h"

#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "TChain.h"
#include "TSystem.h"
#include "TFile.h"

#include "StMuDSTMaker/COMMON/StMuDebug.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuDbReader.h"
#include "StMuDSTMaker/COMMON/StMuTimer.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"

#include "StarClassLibrary/SystemOfUnits.h"
#include "StarClassLibrary/StHelixD.hh"
#include "StarClassLibrary/StPhysicalHelixD.hh"
#include "StarClassLibrary/StThreeVectorF.hh"
#include "StarClassLibrary/StThreeVectorD.hh"


#define __ZPOS__ 25.

ClassImp(SchedulerExample)

SchedulerExample::SchedulerExample(const char* outputFile) {
  mOutputFile = string(outputFile);
  mPt  = new TH1D("mPt","pt-distribution",100,0.,2.);
  mEta = new TH1D("mEta","eta-distribution",100,-2.,2.);
  mRefMultPos = new TH1D("mRefMultPos", "ref mult pos",  400,0.,400.);
  mRefMultNeg = new TH1D("mRefMultNeg", "ref mult neg",  400,0.,400.);
  mRefMult    = new TH1D("mRefMult",    "ref mult",      400,0.,400.);
  mVertex     = new TH3D("mVertex",     "vertex", 100,-5.,+5., 100,-5.,+5., 100,-50.,+50.); 
}


SchedulerExample::~SchedulerExample(){
  delete mPt;     
  delete mEta;
  delete mRefMultPos; 
  delete mRefMultNeg; 
  delete mRefMult;    
  delete mVertex;     
}


Int_t  SchedulerExample::Init() {
  mMuDstMaker = (StMuDstMaker*)GetMaker("MuDstMaker");
  if (!mMuDstMaker) return 1;
  return 0;
}


Int_t  SchedulerExample::Finish() {
  cout << " SchedulerExample::Finish() " << endl;
  TFile f(mOutputFile.c_str(),"recreate");
  mPt->Write();
  mEta->Write();
  mRefMultPos->Write(); 
  mRefMultNeg->Write();
  mRefMult->Write();    
  mVertex->Write();     
  f.Close();
  cout << " SchedulerExample::Finish() file written" << endl;
  return 0;
}

Int_t SchedulerExample::Make() {
  mMuDst = mMuDstMaker->muDst();
  if (!mMuDst) return 1;

  // here is the actual analysis code

  StMuEvent* ev = mMuDst->event();
  // fill vertex histogram
  StThreeVectorF pos = ev->primaryVertexPosition();
  mVertex->Fill( pos.x(),pos.y(),pos.z() );

  if ( fabs(pos.z())<__ZPOS__ ) {
    mRefMultPos->Fill( ev->refMultPos() );
    mRefMultNeg->Fill( ev->refMultNeg() );
    mRefMult->Fill( ev->refMult() );

    int numberOfPrimaries= mMuDst->primaryTracks()->GetEntries();
    StMuTrack* t;
    for (int l=0; l<numberOfPrimaries; l++) {
      t = mMuDst->primaryTracks(l);
      mPt->Fill( t->pt() );   
      mEta->Fill( t->eta() );   
    }
  }


  return 0;
}


/***************************************************************************
 *
 * $Log: SchedulerExample.cxx,v $
 * Revision 1.2  2003/09/02 17:58:44  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.1  2003/01/23 21:59:50  laue
 * Modification to compile on Solaris.
 *
 *
 **************************************************************************/





