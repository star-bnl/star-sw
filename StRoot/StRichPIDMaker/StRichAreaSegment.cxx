/**********************************************************
 * $Id: StRichAreaSegment.cxx,v 2.2 2009/08/25 22:50:09 fine Exp $
 *
 * Description:
 **********************************************************/

#include "StRichAreaSegment.h"
#include "StThreeVectorF.hh"
#include "StMessMgr.h"

/*
#ifndef ST_NO_NAMESPACES
using namespace units;
#endif
*/


StRichAreaSegment::StRichAreaSegment() {
  mPoints.resize(12);
  for (int i=0;i<12;i++) {mPoints[i] = StThreeVectorF(-999,-999,0);}
  
  mAngle.resize(2);
  mAngle[0] = -999;
  mAngle[1] = -999;
  mAngle_0 = -999;
  mAngle_1 = -999;
  mType = -1;
}

StRichAreaSegment::~StRichAreaSegment() {
 /* nopt */
}

StThreeVectorF& StRichAreaSegment::getPoint(int i) {
  if (i<0 || i>11) {
    LOG_FATAL << "StRichAreaSegment::getPoint() passed " << i 
	 << "  which is out of bounds." << endm;
  }
  return mPoints[i];
}

void StRichAreaSegment::addPoint(int i, StThreeVectorF p) {
  if (i<0 || i>11) {
    LOG_FATAL << "StRichAreaSegment::addPoint() passed " << i 
	 << "  which is out of bounds." << endm;
  }
  mPoints[i] = p;
}


void StRichAreaSegment::addAngle(int i, double ang) {
  if (i<0 || i>1) {
    LOG_FATAL << "StRichAreaSegment::addAngle() passed " << i 
	 << "  which is out of bounds." << endl;
  } 
  mAngle[i] = ang;
  if (i==0) mAngle_0 = ang;
  if (i==1) mAngle_1 = ang;
}


double StRichAreaSegment::getAngle(int i) {
  if (i<0 || i>1) {
    LOG_FATAL << "StRichAreaSegment::getAngle() passed " << i 
	 << "  which is out of bounds." << endl;
  } 
  if (i==0) return mAngle_0;
  if (i==1) return mAngle_1;
  return -999;
}

void StRichAreaSegment::setType(int i) {
  mType=i;
}

int StRichAreaSegment::getType() {
  return mType;
}


