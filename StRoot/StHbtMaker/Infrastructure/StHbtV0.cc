#include "StHbtV0.hh"
#include "phys_constants.h"
void StHbtV0::UpdateV0(){
  //Calc. derived memebers of the v0 class
  float MomNegAlongV0, MomPosAlongV0;

   mmomV0     = mmomPos + mmomNeg;
   mptV0   = mmomV0.perp();
   mptotV0 = mmomV0.mag();
   mptPos  = mmomPos.perp();
   mptotPos= mmomPos.mag();
   mptNeg  = mmomNeg.perp();
   mptotNeg= mmomNeg.mag();
   meLambda= sqrt(mptotV0*mptotV0+M_LAMBDA*M_LAMBDA);
   meK0Short= sqrt(mptotV0*mptotV0+M_KAON_0_SHORT*M_KAON_0_SHORT);
   mePosProton = sqrt(mptotPos*mptotPos+M_PROTON*M_PROTON);
   meNegProton = sqrt(mptotNeg*mptotNeg+M_PROTON*M_PROTON);
   mePosPion = sqrt(mptotPos*mptotPos+M_PION_PLUS*M_PION_PLUS);
   meNegPion = sqrt(mptotNeg*mptotNeg+M_PION_MINUS*M_PION_MINUS);
  
   MomNegAlongV0 =  mmomNeg*mmomV0 / sqrt(pow(mptotV0,2));
   MomPosAlongV0 =  mmomPos*mmomV0 / sqrt(pow(mptotV0,2));

   malphaV0 = (MomPosAlongV0-MomNegAlongV0)/(MomPosAlongV0-MomNegAlongV0);
   mptArmV0 =  sqrt(mptotPos*mptotPos - MomPosAlongV0*MomPosAlongV0);
   mmassLambda = sqrt(pow(mePosProton+meNegPion,2)-pow(mptotV0,2));
   mmassAntiLambda = sqrt(pow(meNegProton+mePosPion,2)-pow(mptotV0,2));
   mmassK0Short = sqrt(pow(meNegPion+mePosPion,2)-pow(mptotV0,2));
}



