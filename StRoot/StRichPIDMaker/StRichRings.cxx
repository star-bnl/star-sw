/**********************************************************
 * $Id: StRichRings.cxx,v 1.3 2000/06/16 02:37:12 horsley Exp $
 *
 * Description:
 *  
 *
 *  $Log: StRichRings.cxx,v $
 *  Revision 1.3  2000/06/16 02:37:12  horsley
 *  many additions, added features to pad plane display (MIPS, rings, etc)
 *  along with Geant info. Added StMcRichTrack. Modified access to hit collection.
 *
 *  Revision 1.3  2000/06/16 02:37:12  horsley
 *  many additions, added features to pad plane display (MIPS, rings, etc)
 *  along with Geant info. Added StMcRichTrack. Modified access to hit collection.
 *
 *  Revision 1.2  2000/05/22 15:14:44  horsley
 *  modified StRichRings, StRichTDrawableRings to comply with sun compiler
 *
 *  Revision 1.1  2000/05/19 19:06:11  horsley
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
using namespace units;
#endif

StRichRings::StRichRings(StRichTrack* track, StParticleDefinition* particle) {

  if (!particle || !track) {
    cout << "StRichRings:: passed null pointer! " << endl;
    abort();
  } 

  myGeometryDb = StRichGeometryDb::getDb();
  mTrack    = track;
  mParticle = particle;
vector<StThreeVector<double> > StRichRings::getInnerPoints(int points/* number of points*/) {

    StRichRingCalculator ringCalc(mTrack);  // Calculates inner points
  ringCalc.setParticleType(mParticle);
StRichRings::~StRichRings() {}

vector<StThreeVectorF > StRichRings::getInnerPoints(int points/* number of points*/) {
  for (int i=0; i<points; i+=1) {          // goes from 0 to 360 degrees
    double psi = ((double) i)*2.0*M_PI/points;

    StThreeVector<double> temp(0,0,0);
    if (ringCalc.getRing(eInnerRing)->getPoint(psi,temp)) {
      if (inBounds(temp) )
	  mInnerPoints.push_back(temp); // put on vector
	  if (inBounds(temp) )

	    mInnerPoints.push_back(temp); // put on vector
  
	}
      }
    }
vector<StThreeVector<double> > StRichRings::getOuterPoints(int points) {
  
  StRichRingCalculator ringCalc(mTrack);
  ringCalc.setParticleType(mParticle);
  return mInnerPoints;
}

  
  for (int i=0; i<points; i+=1) {
    double psi = ((double) i)*2.0*M_PI/points;

    StThreeVector<double> temp;
    if (ringCalc.getRing(eOuterRing)->getPoint(psi,temp)) {
      if (inBounds(temp) ) mOuterPoints.push_back(temp);
	if (ringCalc.getRing(eOuterRing) && ringCalc.getRing(eOuterRing)->getPoint(psi,temp)) {

	  if (inBounds(temp) ) mOuterPoints.push_back(temp);
  
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
