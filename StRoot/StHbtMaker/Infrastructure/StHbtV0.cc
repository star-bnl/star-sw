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

   mrapLambda = 0.5*log( (meLambda+mmomV0.z()) / (meLambda-mmomV0.z()) );
   mcTauLambda = M_LAMBDA*(mdecayLengthV0) / sqrt( pow((double)mmomV0.mag(),2.) );
   
   mrapK0Short = 0.5*log( (meK0Short+mmomV0.z()) / (meK0Short-mmomV0.z()) );
   mcTauK0Short = M_KAON_0_SHORT*(mdecayLengthV0) / sqrt( pow((double)mmomV0.mag(),2.) );
}
// -----------------------------------------------------------------------
#ifdef __ROOT__
#include "StStrangeMuDstMaker/StV0MuDst.hh"
StHbtV0::StHbtV0( StV0MuDst& v0FromMuDst){ // from strangess micro dst structure
    mdecayLengthV0 = v0FromMuDst.decayLengthV0();
    mdecayVertexV0 = StHbtThreeVector( v0FromMuDst.decayVertexV0X(), v0FromMuDst.decayVertexV0Y(), v0FromMuDst.decayVertexV0Z() );
  mdcaV0Daughters = v0FromMuDst.dcaV0Daughters();
  mdcaV0ToPrimVertex = v0FromMuDst.dcaV0ToPrimVertex();
  mdcaPosToPrimVertex = v0FromMuDst.dcaPosToPrimVertex();
  mdcaNegToPrimVertex = v0FromMuDst.dcaNegToPrimVertex();
  mmomPos = StHbtThreeVector( v0FromMuDst.momPosX(), v0FromMuDst.momPosY(), v0FromMuDst.momPosZ() );
  mmomNeg = StHbtThreeVector( v0FromMuDst.momNegX(), v0FromMuDst.momNegY(), v0FromMuDst.momNegZ() ); 
#ifdef STHBTDEBUG
  cout << " hist pos ";
  cout << v0FromMuDst.topologyMapPos().numberOfHits(kTpcId); 
  cout << " hist neg ";
  cout << v0FromMuDst.topologyMapNeg().numberOfHits(kTpcId) << endl;
#endif
    mtpcHitsPos = ( v0FromMuDst.topologyMapPos().numberOfHits(kTpcId) );
    mtpcHitsNeg = ( v0FromMuDst.topologyMapNeg().numberOfHits(kTpcId) );
    mTrackTopologyMapPos[0] = ( v0FromMuDst.topologyMapPos().data(0) );
    mTrackTopologyMapPos[1] = ( v0FromMuDst.topologyMapPos().data(1) );
    mTrackTopologyMapNeg[0] = ( v0FromMuDst.topologyMapNeg().data(0) );
    mTrackTopologyMapNeg[1] = ( v0FromMuDst.topologyMapNeg().data(1) );
    midPos = v0FromMuDst.keyPos();
    midNeg = v0FromMuDst.keyNeg();
#ifdef STHBTDEBUG
    cout << " keyPos " << v0FromMuDst.keyPos() << endl;
    cout << " keyNeg " << v0FromMuDst.keyNeg() << endl;
#endif
    mmomV0 = StHbtThreeVector( v0FromMuDst.momV0X(), v0FromMuDst.momV0Y(), v0FromMuDst.momV0Z() );
#ifdef STHBTDEBUG
    cout << " alpha  ";
    cout << v0FromMuDst.alphaV0();
    cout << " ptArm  ";
    cout << v0FromMuDst.ptArmV0() << endl;
#endif
    malphaV0 = v0FromMuDst.alphaV0();
    mptArmV0 = v0FromMuDst.ptArmV0();
    meLambda = v0FromMuDst.eLambda();
    meK0Short = v0FromMuDst.eK0Short();
    mePosProton = v0FromMuDst.ePosProton();
    mePosPion = v0FromMuDst.ePosPion();
    meNegPion = v0FromMuDst.eNegPion();
    meNegProton = v0FromMuDst.eNegProton();
    mmassLambda = v0FromMuDst.massLambda();
    mmassAntiLambda = v0FromMuDst.massAntiLambda();
    mmassK0Short = v0FromMuDst.massK0Short();
    mrapLambda = v0FromMuDst.rapLambda();
    mrapK0Short = v0FromMuDst.rapK0Short();
    mcTauLambda = v0FromMuDst.cTauLambda();
    mcTauK0Short = v0FromMuDst.cTauK0Short();
    mptV0 = v0FromMuDst.ptV0();
    mptotV0 = v0FromMuDst.ptotV0();
    mptPos = v0FromMuDst.ptPos();
    mptotPos = v0FromMuDst.ptotPos();
    mptNeg = v0FromMuDst.ptNeg();
    mptotNeg = v0FromMuDst.ptotNeg();
}
#endif // __ROOT__

