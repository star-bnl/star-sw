/**********************************************************
 * $Id: StRichDrawableTRings.cxx,v 2.0 2000/08/09 16:28:03 gans Exp $
 *
 * Description:
 *  
 *
 *  $Log: StRichDrawableTRings.cxx,v $
 *  Revision 2.0  2000/08/09 16:28:03  gans
 *  Created New Maker for all drawable objects.
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
#include "StRichPIDMaker/StRichRings.h"
#include "TPolyLine.h"
#include "StThreeVector.hh"

#include "TRandom.h"
#include "StRichHit.h"

#include <vector>


#ifndef ST_NO_NAMESPACES
using namespace units;
#endif

StRichDrawableTRings::StRichDrawableTRings() {}

StRichDrawableTRings::StRichDrawableTRings(StRichRings& ring) {

    // no need to set a new seed
    
    time_t t1 = time(0);   // to be used as a seed  
    rand = new TRandom();
    rand->SetSeed(t1);
    
    mTrack = ring.getTrack();
    mParticle = ring.getParticle();
    
    if (!mTrack) {
	cout << "StRichDrawableTRings:: passed null pointer! " << endl;
	abort();
    } 
    
  const Int_t maxInnerSize = 3600;    // each ring <= 3600 line segments 
  vector<StThreeVector<double> > in  = ring.getInnerPoints(maxInnerSize);
  vector<StThreeVector<double> > out = ring.getOuterPoints(maxOuterSize);
  // Copy into temp vector (should make a vector<pair<double>>)
  vector<StThreeVectorF > in  = ring.getInnerPoints(maxInnerSize);
  vector<StThreeVectorF > out = ring.getOuterPoints(maxOuterSize);

  // Set Up Arrays for TPolyLine
  Float_t ix[maxInnerSize];        // inner X points
  Float_t iy[maxInnerSize];        // inner Y points
  
  Float_t ox[maxOuterSize];        // outer
  Float_t oy[maxOuterSize];

  Float_t tempX,tempY;        // temp variables
  int innerSize = 0;
  
  for (unsigned int hh=0;hh<in.size();hh++) // loop over inner ring points
      {
	  tempX = in[hh].x();      // grab X and Y
	  tempY = in[hh].y();
	  
	  if(tempX || tempY)      // if not both at 0,0 (ie, on pad plane)
	      {
		  ix[innerSize]   = tempX; // add variables to array
		  iy[innerSize] = tempY;   // for TPolyLine
		  innerSize++;             // incriment size counter of arrays
	      }
      }
  
  int outerSize = 0;                    // size counter fro Outer Ring
		    
  for (unsigned int hh=0;hh<out.size();hh++)        // same as above for Outer Ring
      {
	  tempX = out[hh].x();
	  tempY = out[hh].y();
	  
	  if(tempX || tempY)
	      {
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

   for (unsigned int i=0;i<mHits.size();i++) {
   mHits.resize(0);
   
   }
   mHits.clear();
   mHits.resize(0);   
}


void StRichDrawableTRings::draw() {

    mInnerRing->Draw();
    mOuterRing->Draw();
   
    for(unsigned int i=0;i<mHits.size();i++)
	mHits[i]->Draw();
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
    delete tempInner;
}
    delete tempOuter;
}


StRichTrack* StRichDrawableTRings::getTrack() {
}


StParticleDefinition* StRichDrawableTRings::getParticle() {
void StRichDrawableTRings::addHit(double x,double y){
    StRichDrawableTHit * tempHit = new StRichDrawableTHit(x,y,4); // 4 == Circle
    tempHit->SetMarkerColor(mInnerRing->GetLineColor());  // Make Same Color as Ring
    tempHit->SetMarkerSize(2+ rand->Rndm());              // Size == 2+ (num between 0 and 1)
    mHits.push_back(tempHit);                             // add hit to list
  tempHit->SetMarkerColor(mInnerRing->GetLineColor());  // Make Same Color as Ring
    
  mHits.push_back(tempHit);                             // add hit to list
}

Int_t StRichDrawableTRings::numberOfHits(){
    return mHits.size();}

StRichDrawableTHit* StRichDrawableTRings::getHit(unsigned int i){
    if(i < mHits.size())
	return mHits[i];
    else
	return 0;}
