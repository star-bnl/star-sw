#include "StSectorAligner.h"
#include "StThreeVectorF.hh"

#include "Stiostream.h"
#include <math.h>
#include <assert.h>

// from ben
// global X & Y coordinates of Y unit vector, by sector
// (Y => normal to padrow in XY plane, radially outward from Z axis)
// note.  index 0 is empty.

static const float sectorY[] = {
  0,0,
  0.5, 0.866025403784439,
  0.866025403784439, 0.5,
  1, 0,
  0.866025403784439, -0.5,
  0.5, -0.866025403784439,
  0, -1,
  -0.5, -0.866025403784439,
  -0.866025403784439, -0.5,
  -1, 0,
  -0.866025403784439, 0.5,
  -0.5, 0.866025403784438,
  0, 1,
  -0.5, 0.866025403784439,
  -0.866025403784439, 0.5,
  -1, 0,
  -0.866025403784439, -0.5,
  -0.5, -0.866025403784439,
  0, -1,
  0.5, -0.866025403784439,
  0.866025403784438, -0.5,
  1, 0,
  0.866025403784438, 0.5,
  0.5, 0.866025403784439,
  0, 1
};

ClassImp(StSectorAligner)

static const float deg_2_rad= M_PI/180.;


//__________ CONSTRUCTORS AND DESTRUCTORS_________

StSectorAligner::StSectorAligner(StTpcDb* dbin) :
  mTranslateOuterSector(true), mVecHit(0)
{
  mVecHit = new StThreeVectorF;   assert(mVecHit);

  thedb = dbin;
  lastInnerSectorRow=13;
  innerSectorRotatePoint=123;
  outerSectorRotatePoint=123;
}

//___________

StSectorAligner::~StSectorAligner()
{
  delete mVecHit;  mVecHit=0;

}

//____________ ACTIONS ____________
/*
  what the public uses to move the hits ('align' sectors).
  input: float x[], int sector, int row.
  output: float xprime[]
*/

void
StSectorAligner::moveHit(const float x[],float xprime[],int sector, int row)
{
  if(sector<1||sector>24||row<1||row>45){
    cout << "Wrong input value: row=" <<row << ",sector=" <<sector<< endl;
    xprime[0]=x[0]; xprime[1]=x[1]; xprime[2]=x[2];
    return;
  }

  // set the vector with the input coordinates
  mVecHit->setX(x[0]); mVecHit->setY(x[1]); mVecHit->setZ(x[2]);

  float angle(0),offset(0),rotatePoint(0);

  // always rotate the hits
  if(row<=lastInnerSectorRow){ //inner
    angle  = 
      thedb->SectorPosition(sector)->innerRotation()*deg_2_rad;
    offset = 
      thedb->SectorPosition(sector)->innerPositionOffsetX();
    rotatePoint = innerSectorRotatePoint;
  }
  else{ // outer
    angle  = 
      thedb->SectorPosition(sector)->outerRotation()*deg_2_rad;
    offset = 
      thedb->SectorPosition(sector)->outerPositionOffsetX();
    rotatePoint = outerSectorRotatePoint;
  }
  
  // translate
  if((row>lastInnerSectorRow && mTranslateOuterSector) ||
     (row<=lastInnerSectorRow && !mTranslateOuterSector)) 
    translateHit(offset,sector);

  // rotate
  rotateHit(angle,sector,rotatePoint);

  // set xprime (z coordinate should remain the same)
  xprime[0] = mVecHit->x(); xprime[1] = mVecHit->y(); xprime[2] = x[2];
}

/*
  rotate
 */
void 
StSectorAligner::rotateHit(float angle, int sector, float radius)
{
  StThreeVectorF origin(radius*sectorY[2*sector],
			radius*sectorY[2*sector+1],0);

  // hit in new coordinate system
  StThreeVectorF hitPrime = (*mVecHit - origin);
  
  // now rotate the hitPrime cw w.r.t to origin
  StThreeVectorF hitRot;
  rotateCW(hitPrime,hitRot,angle);

  // reset the hit position
  mVecHit->setX(hitRot.x()+origin.x()); 
  mVecHit->setY(hitRot.y()+origin.y());
}

/*
  rotate point cw in xy plane. doesnt change the z coordinate.
 */
void
StSectorAligner::rotateCW(const StThreeVectorF& point,
			  StThreeVectorF& pointRot,float angle)
{
  pointRot.setX(point.x()*cos(angle)+point.y()*sin(angle));
  pointRot.setY(-point.x()*sin(angle)+point.y()*cos(angle));
  pointRot.setZ(point.z());

}

/*
  rotate the point to the sector angular position.
  new values set in pointRot.
 */

void
StSectorAligner::rotateToSector(const StThreeVectorF& point,
				StThreeVectorF& pointRot,int sector)
{
  pointRot.setX(point.x()*sectorY[2*sector]-point.y()*sectorY[2*sector+1]);
  pointRot.setY(point.x()*sectorY[2*sector+1]+point.y()*sectorY[2*sector]);
  pointRot.setZ(point.z());
}

/*
  translate
 */
void
StSectorAligner::translateHit(float offset, int sector)
{
  StThreeVectorF offsetVec(0,offset,0);

  StThreeVectorF offsetRot;
  rotateToSector(offsetVec,offsetRot,sector);

  mVecHit->setX(mVecHit->x()+offsetRot.x());
  mVecHit->setY(mVecHit->y()+offsetRot.y());
}


