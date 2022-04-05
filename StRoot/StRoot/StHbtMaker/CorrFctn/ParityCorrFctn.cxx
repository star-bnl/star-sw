/***************************************************************************
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 * this is a special correlation function for Parity Violation studies.
 * this CorrFctn is special because it interacts with the ParityEventCut
 * closely.  This correlation function has no histograms... the histos
 * are in the EventCut!!!
 *
 ***************************************************************************/

#include "StHbtMaker/CorrFctn/ParityCorrFctn.h"
//#include "StHbtMaker/Infrastructure/StHbtHisto.hh"
#include <cstdio>

#ifdef __ROOT__
ClassImp(ParityCorrFctn)
#endif

//____________________________
ParityCorrFctn::ParityCorrFctn(ParityEventCut* PEC){
  mParityEventCut = PEC;
}

//____________________________
ParityCorrFctn::~ParityCorrFctn(){ 
  /* noop */
} 
//_________________________
void ParityCorrFctn::Finish(){ 
  /* noop */
}

//____________________________
StHbtString ParityCorrFctn::Report(){ 
  string stemp = "Parity Correlation Function Report:\n Hello there\n"; 
  StHbtString returnThis = stemp; 
  return returnThis; 
} 
//____________________________
void ParityCorrFctn::AddRealPair(const StHbtPair* pair){ 
  // this should increment the RealQuantity of the EventCut by (p1 x p2) dot z
  StHbtThreeVector CrossProd = pair->track1()->FourMomentum().vect().cross(pair->track2()->FourMomentum().vect());
  mParityEventCut->RealQuantity += CrossProd.z();
  mParityEventCut->nReals++;
} 
//____________________________
void ParityCorrFctn::AddMixedPair(const StHbtPair* pair){
  StHbtThreeVector CrossProd = pair->track1()->FourMomentum().vect().cross(pair->track2()->FourMomentum().vect());
  mParityEventCut->MixedQuantity += CrossProd.z();
  mParityEventCut->nMixed++;
} 


