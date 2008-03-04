/***************************************************************************
 *
 * $Id: StDcaGeometry.cxx,v 2.2 2008/03/04 01:03:36 perev Exp $
 *
 * Author: Victor Perevoztchikov, Thomas Ullrich, May 2006
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StDcaGeometry.cxx,v $
 * Revision 2.2  2008/03/04 01:03:36  perev
 * remove redundant mHz
 *
 * Revision 2.1  2006/05/24 17:27:43  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#include "StDcaGeometry.h"

ClassImp(StDcaGeometry)
    
static const char rcsid[] = "$Id: StDcaGeometry.cxx,v 2.2 2008/03/04 01:03:36 perev Exp $";

StDcaGeometry::StDcaGeometry()
{
    memset(mBeg,0,mEnd-mBeg+1);
}

StDcaGeometry::~StDcaGeometry() {/* noop */}

StThreeVectorF StDcaGeometry::origin() const
{
    float x = -mImp*sin(mPsi);
    float y =  mImp*cos(mPsi);
    return StThreeVectorF(x,y,mZ);
}

StThreeVectorF StDcaGeometry::momentum() const
{
    float ptt = pt();
    float x   = ptt*cos(mPsi);
    float y   = ptt*sin(mPsi);
    float z   = ptt*mTan;
    return StThreeVectorF(x,y,z);
}

void StDcaGeometry::set(const float pars[7],const float errs[15])
{
    if (pars) memcpy(&mImp   ,pars,sizeof(float)*6 );
    if (pars) memcpy(&mImpImp,errs,sizeof(float)*15);
}

StPhysicalHelixD StDcaGeometry::helix() const
{
    //    double curvature = fabs(mCurv);
    int  h = (mCurv>=0) ? 1:-1;
    
    double phase = mPsi-h*M_PI/2;
    
    return StPhysicalHelixD(fabs(mCurv),   // 1/cm
                            atan(mTan),    // radian
                            phase,         // radian
                            origin(),      // cm
                            h);
}

THelixTrack StDcaGeometry::thelix() const
{
    enum {kImp,kZ,kPsi,kPti,kTan};
    
    StThreeVectorD pos = origin();
    StThreeVectorD dir = momentum().unit();
    THelixTrack myHelx(&(pos.x()),&(dir.x()),mCurv);
    double errXY[6],errSZ[6];
    const float *myErr = &mImpImp;
    int jjx=0,jjz=0;
    for (int i=0,li=0;i<5; li+=++i) {
        for (int j=0;j<=i;j++)        {
	  do {// select XY part
	      if(i==kZ || i==kTan)         break;
	      if(j==kZ || j==kTan)         break;
	      errXY[jjx++]=myErr[li+j];
	  }
	  while(0);
	  do {// select SZ part
	      if(i!=kZ && i!=kTan)         break;
	      if(j!=kZ && j!=kTan)         break;
	      errSZ[jjz++]=myErr[li+j];
	  }
	  while(0);
        } }
    errXY[3]*=hz();errXY[4]*=hz();errXY[5]*=hz()*hz();
    myHelx.SetEmx(errXY,errSZ);
    return myHelx;
}

