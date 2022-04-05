/***************************************************************************
 *
 * $Id: Trk2CorrFctn.cxx,v 1.1 2001/12/14 23:11:25 fretiere Exp $
 *
 * Author: 
 *
 * $Log: Trk2CorrFctn.cxx,v $
 * Revision 1.1  2001/12/14 23:11:25  fretiere
 * Add class HitMergingCut. Add class fabricesPairCut = HitMerginCut + pair purity cuts. Add TpcLocalTransform function which convert to local tpc coord (not pretty). Modify StHbtTrack, StHbtParticle, StHbtHiddenInfo, StHbtPair to handle the hit information and cope with my code
 *
 *
 *
 **************************************************************************/

#include "TMath.h"
#include "StHbtMaker/CorrFctn/Trk2CorrFctn.h"
#include <cstdio>

#ifdef __ROOT__ 
ClassImp(Trk2CorrFctn)
#endif

//____________________________
Trk2CorrFctn::Trk2CorrFctn(char* title){
  mNumFracRowClosestRow = new StHbt2DHisto("NumFC","NumFC",45,1.,46.,
					   20,0.,1.);
  mDenFracRowClosestRow = new StHbt2DHisto("DenFC","DenFC",45,1.,46.,
					   20,0.,1.);
  mRatFracRowClosestRow = new StHbt2DHisto("RatFC","RatFC",45,1.,46.,
					   20,0.,1.);
}

//____________________________
Trk2CorrFctn::~Trk2CorrFctn(){
  delete mNumFracRowClosestRow;
  delete mDenFracRowClosestRow;
  delete mRatFracRowClosestRow;
}
//_________________________
void Trk2CorrFctn::Finish(){
  mRatFracRowClosestRow->Divide(mNumFracRowClosestRow,
				mDenFracRowClosestRow,
				mDenFracRowClosestRow->Integral(),
				mNumFracRowClosestRow->Integral());
}

void Trk2CorrFctn::Write(){
  mNumFracRowClosestRow->Write();
  mDenFracRowClosestRow->Write();
  mRatFracRowClosestRow->Write();
}

//____________________________
StHbtString Trk2CorrFctn::Report(){
  string stemp = "Qinv Correlation Function Report:\n";
  char ctemp[100];
  sprintf(ctemp,"Number of entries in numerator:\t%E\n",mNumFracRowClosestRow->GetEntries());
  stemp += ctemp;
  sprintf(ctemp,"Number of entries in denominator:\t%E\n",mDenFracRowClosestRow->GetEntries());
  stemp += ctemp;
  sprintf(ctemp,"Number of entries in ratio:\t%E\n",mRatFracRowClosestRow->GetEntries());
  stemp += ctemp;
  StHbtString returnThis = stemp;
  return returnThis;
}
//____________________________
void Trk2CorrFctn::AddRealPair(const StHbtPair* pair){
  mNumFracRowClosestRow->Fill(pair->getClosestRowAtDCA(),
			      pair->getFracOfMergedRow(),1.);
}

//____________________________
void Trk2CorrFctn::AddMixedPair(const StHbtPair* pair){
  mDenFracRowClosestRow->Fill(pair->getClosestRowAtDCA(),
			      pair->getFracOfMergedRow(),1.);
}


