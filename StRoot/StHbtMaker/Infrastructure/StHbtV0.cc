#include "StHbtV0.hh"
#include "phys_constants.h"

// -----------------------------------------------------------------------
StHbtV0::StHbtV0(const StHbtV0& v){ // copy constructor
  mdecayLengthV0 = v.mdecayLengthV0;
  mdecayVertexV0 = v.mdecayVertexV0;
  mdcaV0Daughters = v.mdcaV0Daughters;
  mdcaV0ToPrimVertex = v.mdcaV0ToPrimVertex;
  mdcaPosToPrimVertex = v.mdcaPosToPrimVertex;
  mdcaNegToPrimVertex = v.mdcaNegToPrimVertex;
  mmomPos = v.mmomPos;
  mmomNeg = v.mmomNeg;


  mTrackTopologyMapPos[0] = v.mTrackTopologyMapPos[0];
  mTrackTopologyMapNeg[0] = v.mTrackTopologyMapNeg[0];
  mTrackTopologyMapPos[1] = v.mTrackTopologyMapPos[1];
  mTrackTopologyMapNeg[1] = v.mTrackTopologyMapNeg[1];
       
  mmomV0 = v.mmomV0;
  malphaV0 = v.malphaV0;
  mptArmV0 = v.mptArmV0;
  meLambda = v.meLambda;
  meK0Short = v.meK0Short;
  mePosProton = v.mePosProton;
  mePosPion = v.mePosPion;
  meNegProton = v.meNegProton;
  meNegPion = v.meNegPion;
  mmassLambda = v.mmassLambda;
  mmassAntiLambda = v.mmassAntiLambda;
  mmassK0Short = v.mmassK0Short;
  mrapLambda = v.mrapLambda;
  mrapK0Short = v.mrapK0Short;
  mcTauLambda = v.mcTauLambda;
  mcTauK0Short = v.mcTauK0Short;
  mptV0 = v.mptV0;
  mptotV0 = v.mptotV0;
  mptPos = v.mptPos;
  mptotPos = v.mptotPos;
  mptNeg = v.mptNeg;
  mptotNeg = v.mptotNeg;

  midNeg = v.midNeg;
  midPos = v.midPos;
  
  mtpcHitsPos = v.mtpcHitsPos;
  mtpcHitsNeg = v.mtpcHitsNeg;


}
// -----------------------------------------------------------------------
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

   malphaV0 = (MomPosAlongV0-MomNegAlongV0)/(MomPosAlongV0+MomNegAlongV0);
   mptArmV0 =  sqrt(mptotPos*mptotPos - MomPosAlongV0*MomPosAlongV0);
   mmassLambda = sqrt(pow(mePosProton+meNegPion,2)-pow(mptotV0,2));
   mmassAntiLambda = sqrt(pow(meNegProton+mePosPion,2)-pow(mptotV0,2));
   mmassK0Short = sqrt(pow(meNegPion+mePosPion,2)-pow(mptotV0,2));
}



