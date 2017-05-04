/*!
 * \class StRnDHit 
 * \author Mike Miller and Andrew Rose, Jan 2006
 */
/***************************************************************************
 *
 * $Id: StRnDHit.cxx,v 2.3 2017/05/04 01:07:34 perev Exp $
 *
 * Author: Mike Miller and Andrew Rose, Jan 2006
 ***************************************************************************
 *
 * Description:  This is an experimental class and not final yet
 *
 ***************************************************************************
 *
 * $Log: StRnDHit.cxx,v $
 * Revision 2.3  2017/05/04 01:07:34  perev
 * Own err matrix added
 *
 * Revision 2.2  2016/02/25 17:10:20  ullrich
 * Implemented detector() which is now a pure abstract method in StHit.
 *
 * Revision 2.1  2006/01/19 21:42:06  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#include "StRnDHit.h"

ClassImp(StRnDHit)

StMemoryPool StRnDHit::mPool(sizeof(StRnDHit));
  
StRnDHit::~StRnDHit() { /* noop */ }

StRnDHit::StRnDHit() :
  mLayer(-1),
  mLadder(-1),
  mWafer(-1),
  mExtraByte0(-1),
  mExtraByte1(-1),
  mKey(-1),
  mVolumeId(-1),
  mDouble0(0),
  mDouble1(0),
  mDouble2(0),
  mDouble3(0),
  mDouble4(0),
  mDetectorId(kUnknownId),
  mErrorMatrix{0,0,0, 0,0,0, 0,0,0}
{
    mLayer = mLadder = mWafer = -1;
    mExtraByte1 = mExtraByte0 = 0;
    mKey = mVolumeId = -1;
    mDouble0 = mDouble1 = mDouble2 = mDouble3 = mDouble4 = 0.;
    mDetectorId = kUnknownId;
}

void StRnDHit::setErrorMatrix( const float* M )
{
  for ( int i=0;i<9;i++ )
    {
      mErrorMatrix[i] = M[i];
    }
}

StMatrixF StRnDHit::covariantMatrix() const {

  StMatrixF M(3,3);

  M(1,1) = mErrorMatrix[0];
  M(1,2) = mErrorMatrix[1];
  M(1,3) = mErrorMatrix[2];

  M(2,1) = mErrorMatrix[3];
  M(2,2) = mErrorMatrix[4];
  M(2,3) = mErrorMatrix[5];

  M(3,1) = mErrorMatrix[6];
  M(3,2) = mErrorMatrix[7];
  M(3,3) = mErrorMatrix[8];

  return M;

}


StRnDHit::StRnDHit(const StThreeVectorF& p,
                   const StThreeVectorF& e,
                   unsigned int hw, float q, unsigned char c,
	         unsigned short idTruth,  unsigned short quality,
	         unsigned short id, StDetectorId dId)
    : StHit(p, e, hw, q, c, idTruth, quality, id)
{ 
    mLayer = mLadder = mWafer = -1;
    mExtraByte0 = mExtraByte1 = 0;
    
    mKey = mVolumeId = -1;
    
    mDouble0 = mDouble1 = mDouble2 = mDouble3 = mDouble4 = 0.;
    mDetectorId = dId;
}

StDetectorId StRnDHit::detector() const {return mDetectorId;}

void StRnDHit::setDetectorId(StDetectorId id) {mDetectorId = id;}

ostream& operator<<(ostream& os, const StRnDHit& hit)
{
    return os << "HFT Hit -I- \tLayer:"<<hit.mLayer<<" ladder: "<<hit.mLadder
	    << " wafer: "<< hit.mWafer<<"\n\t\t"<< hit
	    <<" \n\t\tExtraByte0: "<<hit.mExtraByte0<<" ExtraByte1: "<<hit.mExtraByte1
	    <<"\n\t\tDoubles: "<< hit.mDouble0<< " "<<hit.mDouble1<<" "<<hit.mDouble2
	    << hit.mDouble3 << " " << hit.mDouble4<<endl;
}
