/***************************************************************************
 *
 * $Id: StRichPIDTraits.cxx,v 1.1 2000/04/03 19:40:51 horsley Exp $
 *
 * Author: Matt Horsley, March 30, 2000
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StRichPIDTraits.cxx,v $
 * Revision 1.1  2000/04/03 19:40:51  horsley
 * initial revision
 *
 * 
 *
 **************************************************************************/

#include "StRichPIDTraits.h"
#include "StParticleDefinition.hh"

ClassImp(StRichPIDTraits)

static const char rcsid[] = "$Id: StRichPIDTraits.cxx,v 1.1 2000/04/03 19:40:51 horsley Exp $";

StRichPIDTraits::StRichPIDTraits() :
    mNumberOfPoints(0), mAreaOnPadPlane(0), mTotalArea(0) { /* noop */ }

StRichPIDTraits::StRichPIDTraits(StDetectorId det, 
				            StParticleDefinition* particle, 
				            UShort_t nPoints, 
				            Float_t areaOnPadPlane, 
                                            Float_t areaTot) :
    StTrackPidTraits(det),
    mNumberOfPoints(nPoints), 
    mAreaOnPadPlane(areaOnPadPlane), 
    mTotalArea(areaTot),
    mName(particle->name())
   { /* noop */ }

StRichPIDTraits::~StRichPIDTraits() { /* noop */ }

Float_t
StRichPIDTraits::photonDensity() const { 
  if (mAreaOnPadPlane>0) {
    return mNumberOfPoints/mAreaOnPadPlane;
  }
  
  else 
  return 0;
}

string
StRichPIDTraits::particleName() const { return mName; }

UShort_t
StRichPIDTraits::numberOfPoints() const { return mNumberOfPoints; }

Float_t
StRichPIDTraits::areaOnPadPlane() const { return mAreaOnPadPlane; }

Float_t
StRichPIDTraits::totalArea() const { return mTotalArea; }

StObject*
StRichPIDTraits::clone() { return new StRichPIDTraits(*this); }
