/**********************************************************
 * $Id: StRichTDrawableRings.cxx,v 1.2 2000/05/22 15:14:44 horsley Exp $
 *
 * Description:
 *  
 *
 *  $Log: StRichTDrawableRings.cxx,v $
 *  Revision 1.2  2000/05/22 15:14:44  horsley
 *  modified StRichRings, StRichTDrawableRings to comply with sun compiler
 *
 *  Revision 1.1  2000/05/19 19:06:11  horsley
 *  many revisions here, updated area calculation ring calc, ring, tracks , etc...
 *
  *
 **********************************************************/

#include "StRichTDrawableRings.h"
#include "StRichRings.h"
#include "TPolyLine.h"
#include "StRichTrack.h"

#include "StThreeVectorD.hh"
#include "StThreeVector.hh"

#include <vector>


#ifndef ST_NO_NAMESPACES
using namespace units;
#endif

StRichTDrawableRings::StRichTDrawableRings() {}

StRichTDrawableRings::StRichTDrawableRings(StRichRings& ring) {
  
  mTrack = ring.getTrack();
  mParticle = ring.getParticle();

  if (!mTrack) {
    cout << "StRichTDrawableRings:: passed null pointer! " << endl;
    abort();
  } 
 
  const Int_t innerSize = 360; 
  const Int_t outerSize = 360;
 
  vector<StThreeVector<double> > in  = ring.getInnerPoints(innerSize);
  vector<StThreeVector<double> > out = ring.getOuterPoints(outerSize);

  Float_t ix[innerSize];
  Float_t iy[innerSize];
  
  Float_t ox[outerSize];
  Float_t oy[outerSize];
  
  for (int hh=0;hh<innerSize;hh++) {
    ix[hh] = in[hh].x();
    iy[hh] = in[hh].y();
  }

  for (int jj=0;jj<outerSize;jj++) {
    ox[jj] = out[jj].x();
    oy[jj] = out[jj].y();
  }

  mInnerRing = new TPolyLine(innerSize,ix,iy);
  mOuterRing = new TPolyLine(outerSize,ox,oy);
  
  mInnerRing->SetLineStyle(1); // solid
  mOuterRing->SetLineStyle(1);
  
  if (mParticle->name() == "e-" || mParticle->name() == "e+") {
    mInnerRing->SetLineColor(5);
    mOuterRing->SetLineColor(5);
  }
  
  if (mParticle->name() == "pi-" || mParticle->name() == "pi+") {
    mInnerRing->SetLineColor(2);
    mOuterRing->SetLineColor(2);
  }
  
  if (mParticle->name() == "kaon-" || mParticle->name() == "kaon+") {
    mInnerRing->SetLineColor(1);
    mOuterRing->SetLineColor(1);
  }
  
  if (mParticle->name() == "anti_proton" || mParticle->name() == "proton") {
    mInnerRing->SetLineColor(3);
    mOuterRing->SetLineColor(3);
  }
}

StRichTDrawableRings::~StRichTDrawableRings() {
  mInnerRing->Clear();
  mOuterRing->Clear();

  delete mInnerRing;
  delete mOuterRing;
}


TPolyLine* StRichTDrawableRings::getInnerRing() {
  return mInnerRing;
}

TPolyLine* StRichTDrawableRings::getOuterRing() {
  return mOuterRing;
}

StRichTrack* StRichTDrawableRings::getTrack() {
  return mTrack;
}

StParticleDefinition* StRichTDrawableRings::getParticle() {
  return mParticle;
}


