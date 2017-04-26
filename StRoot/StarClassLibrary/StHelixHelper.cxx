/*!
 * \class StHelixHelper 
 * \author Valeri Fine, Sep 2009
 */
/***************************************************************************
 *
 * $Id: StHelixHelper.cxx,v 1.2 2017/04/26 21:08:28 perev Exp $
 *
 * Author: Valeri Fine, July 2009
 ***************************************************************************/
#include "StHelixHelper.h"
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#if ROOT_VERSION_CODE < 331013
#  include "TCL.h"
#else
#  include "TCernLib.h"
#endif
#include "TMath.h"
#include "TVirtualPad.h"
#include "TView.h"

#include "StObject.h"
#include "THelixTrack.h"
#include "StPhysicalHelixD.hh"

/// StHelixHelper is to convert the track object defined 
/// by 2 StHelix objects and length
/// into the array of 3D points
//______________________________________________________________________________
ClassImp(StHelixHelper)
StHelixHelper::StHelixHelper(const StPhysicalHelix &helix
        ,const StPhysicalHelix &outerHelix, double length)
      :fLength(length)
{ 
  fHelx[0]=new StPhysicalHelixD(helix); 
  fHelx[1]=new StPhysicalHelixD(outerHelix);
  fTHlx[0]=0; fTHlx[1]=0; 
}

//______________________________________________________________________________
StHelixHelper::StHelixHelper() : TObject(),fLength(-1)
{
  fHelx[0] = fHelx[1] = 0;
  fTHlx[0] = fTHlx[1] = 0;
}

//______________________________________________________________________________
StHelixHelper::StHelixHelper(const StHelixHelper &helper) : TObject (helper)
    ,fLength(helper.fLength)
{
  fHelx[0]=new StPhysicalHelixD(*helper.fHelx[0]); 
  fHelx[1]=new StPhysicalHelixD(*helper.fHelx[1]);

  fTHlx[0] = helper.fTHlx[0] ? new THelixTrack(*helper.fTHlx[0])  : 0;
  fTHlx[1] = helper.fTHlx[1] ? new THelixTrack(*helper.fTHlx[1])  : 0;
  
}

//______________________________________________________________________________
StHelixHelper::~StHelixHelper()
{  
  delete fHelx[0];delete fHelx[1];
  delete fTHlx[0];delete fTHlx[1];
}
//______________________________________________________________________________
float     StHelixHelper::GetLength() const {return fLength;}
//______________________________________________________________________________
StPhysicalHelixD *StHelixHelper::GetHelix(int idx) const
{
   return fHelx[idx];
}
//______________________________________________________________________________
THelixTrack *StHelixHelper::MyHelix(THelixTrack *myHlx,const StHelixD *evHlx)
{
  if (!myHlx)    myHlx= new THelixTrack;
  double curv =  evHlx->curvature();
  double phase = evHlx->phase();
  double dip   = evHlx->dipAngle();
  int h = evHlx->h();

  double myDir[3];
  myDir[0]= -sin(phase)*cos(dip);	
  myDir[1]=  cos(phase)*cos(dip);
  myDir[2]=             sin(dip);
  if (h<0) {myDir[0]=-myDir[0]; myDir[1]=-myDir[1];}
  double myX[3];
  myX[0]= evHlx->x(0.);
  myX[1]= evHlx->y(0.);
  myX[2]= evHlx->z(0.);
  
  myHlx->Set(myX,myDir,curv*h);
  return myHlx;		
}
//______________________________________________________________________________
THelixTrack *StHelixHelper::GetTHelix(int idx) const
{
   StPhysicalHelixD *hlx = GetHelix(idx);
   fTHlx[idx] = StHelixHelper::MyHelix(fTHlx[idx],hlx);
   return fTHlx[idx];
}		

/// Create the Float_t array of the npoints point 
/// The end-use code is repsonsible to destroy 
//  the output arry to free the memory
//______________________________________________________________________________
Float_t  *StHelixHelper::GetPoints(int &npoints) const
{
  static int ndebug=0; ndebug++;
  npoints=0;
  double len,len0,len1;
  len =  GetLength();
  if (len <= 0.0001) {
   // Warning("GetPoints","Zero length %s(%p), IGNORED",fTrk->ClassName(),(void*)fTrk);
    return 0;
  }
  
  GetHelix(0); GetHelix(1);
  for (int i=0;i<2;i++) {fTHlx[i] = StHelixHelper::MyHelix(fTHlx[i],fHelx[i]);}

  len0 = fTHlx[0]->Path(fTHlx[1]->Pos());
  double rho0 = fTHlx[0]->GetRho();
//  double rho1 = fTHlx[1]->GetRho();
//   double drho = (rho1-rho0)/(len0*fTHlx[0]->GetCos());
//   fTHlx[0]->Set(rho0,drho);
//   fTHlx[1]->Set(rho1,drho);
  fTHlx[1]->Backward();
  npoints  = abs(int(len*fTHlx[0]->GetCos()*rho0*90))+2;   
  double step = 1./(npoints-1);
  len0 = fTHlx[0]->Path(fTHlx[1]->Pos());
  len1 = fTHlx[1]->Path(fTHlx[0]->Pos());
  float *arr = new Float_t[npoints*3];
  double xyz[3][3];
  for (int i =0;i<npoints;i++)
  {
     double s0 = i*step;
     double s1 = 1.-s0;
     fTHlx[0]->Eval(s0*len0,xyz[0]);
     fTHlx[1]->Eval(s1*len1,xyz[1]);
     s0 = s0*s0*s0; s1 = s1*s1*s1;
     double tmp = s0+s1;
     s0 /=tmp;      s1 /=tmp;
     TCL::vlinco(xyz[0],s1,xyz[1],s0,xyz[2],3);
     TCL::ucopy(xyz[2],arr+i*3,3);
  }
  return arr;
}
