/***************************************************************************
 *
 * $Id: StMuMomentumShiftMaker.cxx,v 1.11 2013/07/23 11:02:59 jeromel Exp $
 * Author: Marco van Leeuwen, LBNL
 *
 * This class is used to correct the momenta of tracks on MicroDst after 
 * production, in case the magnetic field that was used during production 
 * was wrong. It was used for the SL04k year-4 HalfField productions.
 * 
 ***************************************************************************/
#include "StMuMomentumShiftMaker.h"
#include "StMuDstMaker.h"
#include "StMuDst.h"
#include "StMuEvent.h"
#include "StMuTrack.h"
#include "StEvent/StRunInfo.h"
#ifndef __NO_STRANGE_MUDST__
#include "StStrangeMuDstMaker/StKinkMuDst.hh"
#include "StStrangeMuDstMaker/StV0MuDst.hh"
#include "StStrangeMuDstMaker/StXiMuDst.hh"
#include "StStrangeMuDstMaker/StStrangeEvMuDst.hh"
#include "StMessMgr.h"
#endif
#include "TChain.h"
#include "THack.h"
#include "TFile.h"

ClassImp(StMuMomentumShiftMaker)

StMuMomentumShiftMaker::StMuMomentumShiftMaker() : StMaker("MomentumShiftMaker"), mMomentumScale(1), mWriteMuDst(1), mOutDir("."), mOutFile(0), mOutTree(0) { }

StMuMomentumShiftMaker::StMuMomentumShiftMaker(const char *outDir) : StMaker("MomentumShiftMaker"), mMomentumScale(1), mWriteMuDst(1), mOutFile(0), mOutTree(0) {
  mOutDir=outDir;
}

void StMuMomentumShiftMaker::ScaleMomentum(StMuTrack *track) {
  track->mP *= mMomentumScale;
  track->mPt = mMomentumScale*track->pt();

  track->mHelix.mP *= mMomentumScale;
  track->mOuterHelix.mP *= mMomentumScale;
}
#ifndef __NO_STRANGE_MUDST__
void StMuMomentumShiftMaker::ScaleMomentum(StKinkMuDst *kink) {
  kink->mParentMomentum *= mMomentumScale;
  kink->mParentPrimMomentum *= mMomentumScale;
  kink->mDaughterMomentum *= mMomentumScale;
  kink->mTransverseMomentum *= mMomentumScale;
}

void StMuMomentumShiftMaker::ScaleMomentum(StV0MuDst *v0) {
  v0->mMomPosX *= mMomentumScale;
  v0->mMomPosY *= mMomentumScale;
  v0->mMomPosZ *= mMomentumScale;
  v0->mMomNegX *= mMomentumScale;
  v0->mMomNegY *= mMomentumScale;
  v0->mMomNegZ *= mMomentumScale;
}

void StMuMomentumShiftMaker::ScaleMomentum(StXiMuDst *xi) {
  xi->mMomPosX *= mMomentumScale;
  xi->mMomPosY *= mMomentumScale;
  xi->mMomPosZ *= mMomentumScale;
  xi->mMomNegX *= mMomentumScale;
  xi->mMomNegY *= mMomentumScale;
  xi->mMomNegZ *= mMomentumScale;
  xi->mMomBachelorX *= mMomentumScale;
  xi->mMomBachelorY *= mMomentumScale;
  xi->mMomBachelorZ *= mMomentumScale;
}
#endif  
int StMuMomentumShiftMaker::Make() {

  StMuDstMaker *mudstMaker = (StMuDstMaker*) GetMaker("MuDst");
  if (!mudstMaker) {
    LOG_ERROR << "ERROR: cannot find MuDstMaker" << endm;
    return kStErr;
  }
  if (mWriteMuDst) {
    const Char_t *inBaseName= strrchr(mudstMaker->chain()->GetFile()->GetName(),'/');
    if ( ! inBaseName ) inBaseName = (Char_t *) mudstMaker->chain()->GetFile()->GetName();
    else                inBaseName = inBaseName+1;

    if (mOutFile==0 || strstr(mOutFile->GetName(),inBaseName)==0) {
      if (mOutFile) {
	mOutFile->Write();
	mOutFile->Close();
	delete mOutFile;
	mOutFile=0;
	mOutTree=0;
      }
      mOutFile=new TFile(mOutDir+inBaseName,"RECREATE");
      if (!mOutFile->IsOpen()) {
         LOG_ERROR << "ERROR in StMuMomentumShiftMaker::Make: cannot open output file: " << mOutDir+inBaseName << endm;
         delete mOutFile; mOutFile = 0;
      }
    }
  }
  StMuDst *mudst=(StMuDst*) GetInputDS("MuDst");
  
  StMuEvent *event=mudst->event();
  event->eventSummary().setMagneticField(event->eventSummary().magneticField());
  event->runInfo().setMagneticField(mMomentumScale * event->runInfo().magneticField());
#ifndef __NO_STRANGE_MUDST__
  StStrangeEvMuDst *strange_event=mudst->strangeEvent();
  strange_event->mMagneticField *= mMomentumScale;
#endif
  if (mOutTree==0) {
    mOutTree=mudstMaker->chain()->GetTree()->CloneTree(0);
  }

  // Scale momenta of different track types
  Int_t n_prim=mudst->numberOfPrimaryTracks();
  for (Int_t i_prim=0; i_prim < n_prim; i_prim++) {
    StMuTrack *track= mudst->primaryTracks(i_prim);
    ScaleMomentum(track);
  }
  Int_t n_glob=mudst->numberOfGlobalTracks();
  for (Int_t i_glob=0; i_glob < n_glob; i_glob++) {
    StMuTrack *track= mudst->globalTracks(i_glob);
    ScaleMomentum(track);
  }
  Int_t n_other=mudst->numberOfOtherTracks();
  for (Int_t i_other=0; i_other < n_other; i_other++) {
    StMuTrack *track= mudst->otherTracks(i_other);
    ScaleMomentum(track);
  }
  /* Skip L3 tracks for now, they were reconstructed seperately
  Int_t n_l3=mudst->numberOfL3Tracks();
  for (Int_t i_l3=0; i_l3 < n_l3; i_l3++) {
    StMuTrack *track= mudst->l3Tracks(i_l3);
    ScaleMomentum(track);
  }
  */
#ifndef __NO_STRANGE_MUDST__
  // Scale momenta of V0 et al
  Int_t n_kink=mudst->numberOfKinks();
  for (Int_t i_kink=0; i_kink < n_kink; i_kink++) {
    StKinkMuDst *kink= mudst->kinks(i_kink);
    ScaleMomentum(kink);
  }
  Int_t n_v0=mudst->numberOfV0s();
  for (Int_t i_v0=0; i_v0 < n_v0; i_v0++) {
    StV0MuDst *v0= mudst->v0s(i_v0);
    ScaleMomentum(v0);
  }
  Int_t n_xi=mudst->numberOfXis();
  for (Int_t i_xi=0; i_xi < n_xi; i_xi++) {
    StXiMuDst *xi= mudst->xis(i_xi);
    ScaleMomentum(xi);
  }
#endif
  if (mWriteMuDst) {
    mOutTree->Fill(); THack::IsTreeWritable(mOutTree); 
  }
  return kStOk;
}

int StMuMomentumShiftMaker::Finish() {
  if (mWriteMuDst && mOutFile) {
    mOutFile->Write();
    mOutFile->Close();
  }
  return kStOk;
}
