/**********************************************************
 * $Id: StRichDrawableTRings.cxx,v 2.2 2000/11/01 16:55:26 lasiuk Exp $
 *
 * Description:
 *  
 *
 *  $Log: StRichDrawableTRings.cxx,v $
 *  Revision 2.2  2000/11/01 16:55:26  lasiuk
 *  add hilite() members which utilize the flags defined in
 *  StEvent.  draw() member also added.  Hits are hilited only
 *  by demand.  Not by default
 *
 *  Revision 2.1  2000/09/29 17:36:58  gans
 *  Modified addHit(), StThreeVector<double> -> StThreeVectorF,other minor stuff
 *
 *  Revision 2.0  2000/08/09 16:28:03  gans
 *  Created New Maker for all drawable objects.
 *
 *  Revision 1.3  2000/06/16 02:37:12  horsley
 *  many additions, added features to pad plane display (MIPS, rings, etc)
 *  along with Geant info. Added StMcRichTrack. Modified access to hit collection.
 *
 *  Revision 1.2  2000/05/22 15:14:44  horsley
 *  modified StRichRings, StRichDrawableTRings to comply with sun compiler
 *
 *  Revision 1.1  2000/05/19 19:06:11  horsley
 *  many revisions here, updated area calculation ring calc, ring, tracks , etc...
 *
 *
 **********************************************************/

#include "StRichDrawableTRings.h"

#include <vector>
#include "TRandom.h"
#include "TPolyLine.h"

#include "StThreeVectorD.hh"
#include "StThreeVectorF.hh"

#include "StRichHit.h"

#include "StRichPIDMaker/StRichRings.h"
#include "StRichPIDMaker/StRichTrack.h"

#ifndef ST_NO_NAMESPACES
using namespace units;
#endif

StRichDrawableTRings::StRichDrawableTRings() {}

StRichDrawableTRings::StRichDrawableTRings(StRichRings& ring) {

    // no need to set a new seed
    
    time_t t1 = time(0);   // to be used as a seed  
    mRand = new TRandom();
    mRand->SetSeed(t1);
    
    mTrack = ring.getTrack();
    mParticle = ring.getParticle();
    
    if (!mTrack) {
	cout << "StRichDrawableTRings:: passed null pointer! " << endl;
	abort();
    } 
    
  const Int_t maxInnerSize = 3600;    // each ring <= 3600 line segments 
  const Int_t maxOuterSize = 3600;

  //
  // Copy into temp vector (should make a vector<pair<double>>)
  //
  vector<StThreeVectorF > in  = ring.getInnerPoints(maxInnerSize);
  vector<StThreeVectorF > out = ring.getOuterPoints(maxOuterSize);

  //
  // Set Up Arrays for TPolyLine
  // inner points
  //
  Float_t ix[maxInnerSize];
  Float_t iy[maxInnerSize];

  //
  //  outer
  //
  Float_t ox[maxOuterSize];
  Float_t oy[maxOuterSize];

  Float_t tempX,tempY;
  int innerSize = 0;

  //
  // loop over inner ring points
  //
  for (unsigned int hh=0;hh<in.size();hh++) {
      // grab X and Y
      tempX = in[hh].x();
      tempY = in[hh].y();

      //
      // if not both at 0,0 (ie, on pad plane)
      //
      
      if(tempX || tempY) {
	  // add variables to array
	  // for TPolyLine and
	  // incriment size counter of arrays
	  ix[innerSize]   = tempX;
	  iy[innerSize] = tempY;
	  innerSize++;       
      }
  }

  // size counter fro Outer Ring
  int outerSize = 0;

  //
  // same as above for Outer Ring
  //
  for (unsigned int hh=0;hh<out.size();hh++) {
      tempX = out[hh].x();
      tempY = out[hh].y();
      
      if(tempX || tempY) {
	  ox[outerSize]   = tempX;
	  oy[outerSize] = tempY;
	  outerSize++;
      }
	  
  }
  
  mInnerRing = new TPolyLine(innerSize,ix,iy); // Make T drawable Ring
  mOuterRing = new TPolyLine(outerSize,ox,oy); // Only use valid points
  
  mInnerRing->SetLineStyle(1); // solid
  mOuterRing->SetLineStyle(1);
  
  // Set Colors for rings
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

StRichDrawableTRings::~StRichDrawableTRings() {

    mInnerRing->Clear();
    mOuterRing->Clear();
    
    delete mInnerRing;
    delete mOuterRing;
    delete mRand;

   for (unsigned int i=0;i<mHits.size();i++) {
       delete mHits[i];
   }
   mHits.clear();
   mHits.resize(0);
}


void StRichDrawableTRings::draw() {

    mInnerRing->Draw();
    mOuterRing->Draw();

    //
    // used to draw the hits here as well
    //
//     for(unsigned int i=0;i<mHits.size();i++)
//      	mHits[i]->Draw();
}

void StRichDrawableTRings::clear() {

    // This is messy, but alot cleaner than righting a deep copy
    // for StRichDrawableTRings as no deepcopy for StRichHit
    
    TPolyLine *tempInner = new TPolyLine(*mInnerRing);
    TPolyLine *tempOuter = new TPolyLine(*mOuterRing);
    vector<StRichDrawableTHit*> tempHits;
    
    mInnerRing->Clear();
    mOuterRing->Clear();
    
    delete mInnerRing;
    delete mOuterRing;
    
    for (unsigned int i=0;i<mHits.size();i++)
	tempHits.push_back(new StRichDrawableTHit(*mHits[i]));

    int size = mHits.size();
    for (int i=0;i<size;i++)
	delete mHits[i];
	
    
    mHits.clear();
    mHits.resize(0);
    
    for (unsigned int i=0;i<tempHits.size();i++)
	mHits.push_back(new StRichDrawableTHit(*tempHits[i]));

    size = tempHits.size();
    for (int i=0;i<size;i++)
	delete tempHits[i];
	
    tempHits.clear();
    tempHits.resize(0);
    
    
    mInnerRing = new TPolyLine(*tempInner);
    mOuterRing = new TPolyLine(*tempOuter);
    
    tempInner->Clear();
    tempOuter->Clear();
    delete tempInner;
    delete tempOuter;
}


StRichTrack* StRichDrawableTRings::getTrack() {
  return mTrack;
}


StParticleDefinition* StRichDrawableTRings::getParticle() {
  return mParticle;
}


void StRichDrawableTRings::addHit(StRichHit* hit) {

    if (!hit) {
	cout << "StRichDrawableTRings::addHit()\n";
	cout << "\t ERROR: null pointer" << endl;
	return;
    }

    //
    // Make a drawable hit and save it
    //

    //
    // 4 == Circle
    //
    StRichDrawableTHit* tempHit = new StRichDrawableTHit(hit,4);

    // Make Same Color as Ring
    tempHit->SetMarkerColor(mInnerRing->GetLineColor());

    //
    // Size == 2+ (num between 0 and 1)
    //
    tempHit->SetMarkerSize(2+ mRand->Rndm());
    mHits.push_back(tempHit);
}

void StRichDrawableTRings::hilite() {

    cout << "StRichDrawableTRings::hilite() size => " << mHits.size() << endl;
    // loop over all the Hits
    for(size_t ii=0; ii<mHits.size(); ii++) {
	//
	// Make Same Color as Ring
	//
	mHits[ii]->SetMarkerColor(mInnerRing->GetLineColor());
	mHits[ii]->SetMarkerSize(2+ mRand->Rndm());
	mHits[ii]->Draw();
    }

}

void StRichDrawableTRings::hilite(const StRichHitFlag& f) {

    // loop over all the DrawableTHits
    for(size_t ii=0; ii<mHits.size(); ii++) {
	//
	// Make Same Color as Ring
	//
	if(!mHits[ii]->getRichHit()) {
	    cout << "StRichDrawableTRings::hilite(flag)\n";
	    cout << "\tptr problem" << endl;
	    continue;
	}

	if(mHits[ii]->getRichHit()->isSet(f)) {
	    mHits[ii]->SetMarkerColor(mInnerRing->GetLineColor());
	    mHits[ii]->SetMarkerSize(2+ mRand->Rndm());
	    mHits[ii]->Draw();
	}

    }
}

Int_t StRichDrawableTRings::numberOfHits(){
    return mHits.size();
}

StRichDrawableTHit* StRichDrawableTRings::getHit(unsigned int i){

    if(i < mHits.size())
	return mHits[i];
    else
	return 0;
}
