#include "St_dst_trackC.h"
#if ROOT_VERSION_CODE < 331013
#include "TCL.h"
#else
#include "TCernLib.h"
#endif

ClassImp(St_dst_trackC);

//_____________________________________________________________________________
St_dst_trackC::St_dst_trackC() : TChair() {
   TCL::vzero(mMomentum,3);
}
//_____________________________________________________________________________
St_dst_trackC::St_dst_trackC(St_dst_track *table) : TChair(table) 
{
   TCL::vzero(mMomentum,3);
}

//_____________________________________________________________________________
const Double_t *St_dst_trackC::momentum(Int_t i){
   if ( i != -1 && UInt_t(i) != fLastIndx) {
     dst_track_st &t = *GetTable(i);
     double pt = t.curvature > 0 ? 1./t.invpt : 0;
     double pz = pt*t.tanl;
     mMomentum[0] = pt*cos(t.psi);
     mMomentum[1] = pt*sin(t.psi);
     mMomentum[2] = pz;
   }
   return mMomentum;
}
