/**********************************************************
 * $Id: StRichRings.cxx,v 1.1 2000/05/19 19:06:11 horsley Exp $
 *
 * Description:
 *  
 *
 *  $Log: StRichRings.cxx,v $
 *  Revision 1.1  2000/05/19 19:06:11  horsley
 *  many revisions here, updated area calculation ring calc, ring, tracks , etc...
 *
 *
 *  Revision 1.1  2000/05/19 19:06:11  horsley
 *
 *  
 *
 *  many revisions here, updated area calculation ring calc, ring, tracks , etc...
 *
 *  Revision 1.1  2000/04/03 19:36:09  horsley
 *  initial revision
 **********************************************************/

#include "StRichRings.h"
#include "TPolyLine.h"
#include "StRichRingCalculator.h"
#include "StRichTrack.h"
#include "StThreeVector.hh"
#include "StRichRingDefinition.h"

#include "StThreeVectorD.hh"
#include "StThreeVectorF.hh"

#include "StParticleDefinition.hh"

#include <vector>

#ifndef ST_NO_NAMESPACES
  
  myGeometryDb = StRichGeometryDb::getDb();
using namespace units;
  mTrack = track;
  mParticle = particle;
  StRichRingCalculator ringCalc(track);
  
  if (!particle || !track) {

StRichRings::StRichRings(StRichTrack* track, StParticleDefinition* particle) {

  if (!particle || !track) {
  ringCalc.setParticleType(particle);

  for (int i=0; i<360; i+=1) {
    double psi = ((double) i)*2.0*M_PI/360.0;

    StThreeVector<double> temp;
    if (ringCalc.getRing(eInnerRing)->getPoint(psi,temp)) {
      if (inBounds(temp) ) {mInnerPoints.push_back(temp);}
	  if (inBounds(temp) )

    if (ringCalc.getRing(eOuterRing)->getPoint(psi,temp)) {
      if (inBounds(temp) ) mOuterPoints.push_back(temp);
    }
	    mInnerPoints.push_back(temp); // put on vector
  
  
      }
    }
StRichRings::~StRichRings() {}

vector<StThreeVector<double> > StRichRings::getInnerPoints() {
  return mInnerPoints;
}

vector<StThreeVector<double> > StRichRings::getOuterPoints() {
	}
      }
    }
  }
  return mOuterPoints;
}


StRichTrack* StRichRings::getTrack() {
  return mTrack;
}

StParticleDefinition* StRichRings::getParticle() {
  return mParticle;
StRichRings::inBounds(StThreeVector<double>& xy) {


bool 
StRichRings::inBounds(StThreeVectorF& xy) {
  
  if ( (xy.x() > -myGeometryDb->radiatorDimension().x() && 
	xy.x() <  myGeometryDb->radiatorDimension().x() ) && 
       
       (xy.y() > -myGeometryDb->radiatorDimension().y() && 
	xy.y() <  myGeometryDb->radiatorDimension().y() ) ) { 
    return true;
  }
  
  return false;
}
