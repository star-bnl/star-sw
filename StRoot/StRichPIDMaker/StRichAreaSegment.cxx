/**********************************************************
 * $Id: StRichAreaSegment.cxx,v 2.1 2000/11/21 16:24:22 horsley Exp $
 *
 * Description:
 **********************************************************/

#include "StRichAreaSegment.h"
#include "StThreeVectorF.hh"

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
    cout << "StRichAreaSegment::getPoint() passed " << i 
	 << "  which is out of bounds." << endl;
    abort();
  }
  return mPoints[i];
}

void StRichAreaSegment::addPoint(int i, StThreeVectorF p) {
  if (i<0 || i>11) {
    cout << "StRichAreaSegment::addPoint() passed " << i 
	 << "  which is out of bounds." << endl;
    abort();
  }
  mPoints[i] = p;
}


void StRichAreaSegment::addAngle(int i, double ang) {
  if (i<0 || i>1) {
    cout << "StRichAreaSegment::addAngle() passed " << i 
	 << "  which is out of bounds." << endl;
    abort(); 
  } 
  mAngle[i] = ang;
  if (i==0) mAngle_0 = ang;
  if (i==1) mAngle_1 = ang;
}


double StRichAreaSegment::getAngle(int i) {
  if (i<0 || i>1) {
    cout << "StRichAreaSegment::getAngle() passed " << i 
	 << "  which is out of bounds." << endl;
    abort(); 
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


