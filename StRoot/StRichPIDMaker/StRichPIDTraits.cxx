/***************************************************************************
 *
 * $Id: StRichPIDTraits.cxx,v 1.2 2000/05/19 19:06:10 horsley Exp $
 *
 * Author: Matt Horsley, March 30, 2000
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StRichPIDTraits.cxx,v $
 * Revision 1.2  2000/05/19 19:06:10  horsley
 * many revisions here, updated area calculation ring calc, ring, tracks , etc...
 *
 * Revision 1.1  2000/04/03 19:40:51  horsley
 * initial revision
 **************************************************************************/

#include "StRichPIDTraits.h"
#include "StParticleDefinition.hh"
#include <math.h>

ClassImp(StRichPIDTraits)

static const char rcsid[] = "$Id: StRichPIDTraits.cxx,v 1.2 2000/05/19 19:06:10 horsley Exp $";

StRichPIDTraits::StRichPIDTraits() { }

StRichPIDTraits::StRichPIDTraits(StDetectorId det, 
				            StParticleDefinition* particle) :
  StTrackPidTraits(det), mParticle(particle) {

  mAreaArray.Set(0);
  mHitArray.Set(0);
  
  mAreaArray.Reset();
  mHitArray.Reset();

  mNewRingArea = 0.0;
  mRichHits = 0;
  mCut = 0.0;

}

StRichPIDTraits::~StRichPIDTraits() { /* noop */ }

void 
StRichPIDTraits::setCut(double c)
{
  mCut = c;
}

double 
StRichPIDTraits::getCut() {
  return mCut;
}

void 
StRichPIDTraits::addNewRingArea(double a) {
  mNewRingArea = a;
}

double 
StRichPIDTraits::getNewRingArea() {
  return mNewRingArea;
}

void
StRichPIDTraits::addRHits(int h) {
  mRichHits = h;
}

int 
StRichPIDTraits::getRHits() {
  return mRichHits;
}

void
StRichPIDTraits::addHitArray(TArrayD& hits) {
  mHitArray = hits;  
}


void
StRichPIDTraits::addAreaArray(TArrayD& array) {
  mAreaArray = array;
}


TArrayD
StRichPIDTraits::getDensityArray() {
  
  TArrayD dens;

  if (mAreaArray.GetSize() != mHitArray.GetSize() ) {
    cout << "StRichPIDTraits::getDensityArray---> bad indexes!" << endl;
    abort();
  }
  
  dens.Set(mHitArray.GetSize());
  
  for (int k=0;k<mAreaArray.GetSize();k++) {
    
    double tempArea = mAreaArray[k];
    double tempHits = mHitArray[k];
    
    if (tempArea < 0 || tempHits < 0) {
      cout << "StRichPIDTraits::getDensityArray() ----> problem!" << endl;
      cout << "index = " << k << "   area = " << tempArea << "  cm^2    " 
	   << "    hits = " << tempHits << endl;
      abort();
    }
    
    if (tempArea>0) {
      dens[k] = tempHits/tempArea;
    }
    else {
      dens[k] = 0.0;
    }

  } 
  
  return dens;
}


TArrayD&
StRichPIDTraits::getHitArray() {
  return mHitArray;
}


TArrayD&
StRichPIDTraits::getAreaArray() {
  return mAreaArray;
}


StParticleDefinition*
StRichPIDTraits::particle() const { return mParticle; }

StObject*
StRichPIDTraits::clone() { return new StRichPIDTraits(*this); }
