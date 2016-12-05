// $Id: StvFtsHitLoader.cxx.C,v 1.1 2016/11/29 16:58:51 perev Exp $
/*!
\author V Perev 2015  

A StvFtsHitLoader loads  Stv hits using StFtsHits.
</ul>
 */
#include "StEvent/StEnumerations.h"
#include "StvFtsHitLoader.h"
#include "StEvent/StFtsHit.h"
#include "Stv/StvHit.h"
#include "Stv/StvToolkit.h"
#include "TCernLib.h"

static const double kFtsAccu = 0.9;

ClassImp(StvFtsHitLoader)
//_____________________________________________________________________________
int StvFtsHitLoader::MakeStvHit(const StHit *stHit,UInt_t upath, int &sure
                               ,StvHit *stvHit)
{
   int num = 0;
#ifdef kFtsIdentifier
static StvToolkit  *kit = StvToolkit::Inst();
   auto detId = stHit->detector();
   if (detId != kFtsId) return StvHitLoader::MakeStvHit(stHit,upath,sure,stvHit);
   assert(!stvHit);
   
   StThreeVectorF dRdPdZ = stHit->positionError();
   StThreeVectorF fx = stHit->position();
   double Rxy = sqrt(fx[0]*fx[0]+fx[1]*fx[1]);
   double dRxy = dRdPdZ[0]*sqrt(12.);
   double dPhi = dRdPdZ[1]*sqrt(12.);

   int   nStp = dRxy/kFtsAccu+1;
   double stp = dRxy/nStp;
   double r = Rxy+0.5*(-dRxy+stp);
   StFtsHit hit(*(StFtsHit*)stHit);
   for (int iStp=0;iStp<nStp;iStp++,r+=stp) {
     StThreeVectorF v3(fx[0]*(r/Rxy),fx[1]*(r/Rxy),fx[2]); 
     hit.setPosition(v3);
     StvHit *stvHit = kit->GetHitRr();
     int ans = StvHitLoader::MakeStvHit(&hit,upath,sure,stvHit);
     stvHit->setStHit(stHit);
     if (!ans) continue;
     num += ans;
     double cFi = fx[0]/Rxy,sFi = fx[1]/Rxy;
     double T[2][2]= {{cFi,-fx[1]},{sFi,+fx[0]}};
     double gRFi[3] = {stp*stp/12,0,dPhi*dPhi/12};
     double gMtx[3];
     TCL::trasat(T[0],gRFi,gMtx,2,2);
     float *e = stvHit->errMtx();
     memset(e+3,0,sizeof(e[0])*3);
     TCL::ucopy(gMtx,e,3); e[3]=0; e[4]= 0; 
     e[5] = dRdPdZ[2]*dRdPdZ[2];
   }  
#endif
   return num;
}
