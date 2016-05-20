/***************************************************************************
 *
 * $Id: StExtGeometry.cxx,v 1.1 2016/05/20 18:40:41 perev Exp $
 *
 * Author: Victor Perevoztchikov, Apr 2016
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StExtGeometry.cxx,v $
 * Revision 1.1  2016/05/20 18:40:41  perev
 * Xtended tracks maker added
 *
 * Initial Revision.
 *
 **************************************************************************/
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "StExtGeometry.h"
#include "TCernLib.h"
ClassImp(StExtGeometry)
    
static const char rcsid[] = "$Id: StExtGeometry.cxx,v 1.1 2016/05/20 18:40:41 perev Exp $";

//_____________________________________________________________________________
StExtGeometry::StExtGeometry(const char *name)
{
    memset(mName,0,mEnd-mName+1);
    set(name);
}

//_____________________________________________________________________________
StExtGeometry::~StExtGeometry() {/* noop */}
//_____________________________________________________________________________
StThreeVectorF StExtGeometry::origin() const
{
    float x = mRxy*cos(mPhi);
    float y = mRxy*sin(mPhi);
    return StThreeVectorF(x,y,mZ);
}

//_____________________________________________________________________________
StThreeVectorF StExtGeometry::momentum() const
{
    float ptt = pt();
    float x   = ptt*cos(mPsi);
    float y   = ptt*sin(mPsi);
    float z   = ptt*mTan;
    return StThreeVectorF(x,y,z);
}
//_____________________________________________________________________________
void StExtGeometry::set(const char *name)
{
    assert(strlen(name)<=3);
    strcpy(mName,name);
}

//_____________________________________________________________________________
void StExtGeometry::set(const float pars[7],const float errs[15])
{
  if (pars) {TCL::ucopy(pars, &mPhi, 6);}
  if (errs) {TCL::ucopy(errs,  mG,  15);} else {TCL::vzero(mG,15);}
}
//_____________________________________________________________________________
void StExtGeometry::set(const double pars[7],const double errs[15])
{
  if (pars) {TCL::ucopy(pars, &mPhi, 6);}
  if (errs) {TCL::ucopy(errs,  mG,  15);} else {TCL::vzero(mG,15);}
}

//_____________________________________________________________________________
StPhysicalHelixD StExtGeometry::helix() const
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

//_____________________________________________________________________________
THelixTrack StExtGeometry::thelix() const
{
    enum {kImp,kZ,kPsi,kPti,kTan};
    
    StThreeVectorD pos = origin();
    StThreeVectorD dir = momentum().unit();
    THelixTrack myHelx(&(pos.x()),&(dir.x()),mCurv);
    return myHelx;
}
//_____________________________________________________________________________
void StExtGeometry::add(StExtGeometry **top)
{
  StExtGeometry **kadd = top;
  StExtGeometry *lExt=0;
  for(;(lExt=*kadd);kadd = &(lExt->mNext)) {if (mRxy<lExt->rxy()) break;}
  *kadd = this; this->mNext = lExt;
}
