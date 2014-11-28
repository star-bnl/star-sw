#include "StHbtMaker/Infrastructure/StHbtXi.hh"
#include "StHbtMaker/Infrastructure/StHbtTTreeXi.h"
#include "phys_constants.h"

// -----------------------------------------------------------------------
void StHbtXi::UpdateXi(){
  //Calc. derived members of the xi class
  float MomV0AlongXi, MomBacAlongXi;

   mMomXi  = momV0() + momBac(); 
   mPtXi   = mMomXi.perp();
   mPtotXi = mMomXi.mag();
   mPtBac  = momBac().perp();
   mPtotBac= momBac().mag();
   mEXi= ::sqrt(mPtotXi*mPtotXi+M_XI_MINUS*M_XI_MINUS);
   mEOmega= ::sqrt(mPtotXi*mPtotXi+M_OMEGA_MINUS*M_OMEGA_MINUS);
   mEBacPion = ::sqrt(ptotBac()*ptotBac()+M_PION_MINUS*M_PION_MINUS);
   mEBacKaon = ::sqrt(ptotBac()*ptotBac()+M_KAON_MINUS*M_KAON_MINUS);

   MomV0AlongXi  =  momV0()*mMomXi / ::sqrt(::pow(mPtotXi,2));
   MomBacAlongXi =  momBac()*mMomXi / ::sqrt(::pow(mPtotXi,2));

   mAlphaXi = (MomBacAlongXi-MomV0AlongXi)/(MomBacAlongXi+MomV0AlongXi);
   mPtArmXi =  ::sqrt(ptotBac()*ptotBac() - MomBacAlongXi*MomBacAlongXi);
   mMassXi = ::sqrt(::pow(eBacPion()+eLambda(),2)-::pow(mPtotXi,2));
   mMassOmega = ::sqrt(::pow(eBacKaon()+eLambda(),2)-::pow(mPtotXi,2));

   mRapXi = 0.5*::log( (eXi()+mMomXi.z()) / (eXi()-mMomXi.z()) );
   mCTauXi = M_XI_MINUS*(mDecayLengthXi) / ::sqrt( ::pow((double)mMomXi.mag(),2.) );
   
   mRapOmega = 0.5*::log( (eOmega()+mMomXi.z()) / (eOmega()-mMomXi.z()) );// eO,
   mCTauOmega = M_OMEGA_MINUS*(mDecayLengthXi) / ::sqrt( ::pow((double)mMomXi.mag(),2.) );
}
// -----------------------------------------------------------------------
#ifdef __ROOT__
#include "StStrangeMuDstMaker/StXiMuDst.hh"
StHbtXi::StHbtXi( StXiMuDst& xiFromMuDst)  : StHbtV0(xiFromMuDst) { // from strangess micro dst structure
  UpdateV0(); // the v0 stuff


  mCharge = xiFromMuDst.charge();
  mDecayLengthXi = xiFromMuDst.decayLengthXi(); // 12/07/2001 Gael
  mDecayVertexXi.setX(xiFromMuDst.decayVertexXiX());
  mDecayVertexXi.setY(xiFromMuDst.decayVertexXiY());
  mDecayVertexXi.setZ(xiFromMuDst.decayVertexXiZ());
  mDcaXiDaughters = xiFromMuDst.dcaXiDaughters();
  mDcaBachelorToPrimVertex = xiFromMuDst.dcaBachelorToPrimVertex();
  mDcaXiToPrimVertex = xiFromMuDst.dcaXiToPrimVertex();
  mMomBachelor.setX(xiFromMuDst.momBachelorX());
  mMomBachelor.setY(xiFromMuDst.momBachelorY());
  mMomBachelor.setZ(xiFromMuDst.momBachelorZ());
  
  mKeyBachelor = xiFromMuDst.keyBachelor();
  mTopologyMapBachelor[0] = xiFromMuDst.topologyMapBachelor().data(1);
  mTopologyMapBachelor[1] = xiFromMuDst.topologyMapBachelor().data(2);
  mTpcHitsBac = xiFromMuDst.topologyMapBachelor().numberOfHits(kTpcId); // 12/07/2001 Gael

  mChi2Xi = xiFromMuDst.chi2Xi();//nulle
  mClXi = xiFromMuDst.clXi();//nulle
  mChi2Bachelor = xiFromMuDst.chi2Bachelor();
  mClBachelor = xiFromMuDst.clBachelor();
  
  mDedxBachelor = xiFromMuDst.dedxBachelor();
  mNumDedxBachelor = xiFromMuDst.numDedxBachelor();

  UpdateXi(); // the xi stuff
  
}

StHbtXi::StHbtXi(const StHbtTTreeEvent* ev, const StHbtTTreeXi* xi) : StHbtV0(ev, xi) {
  mCharge = xi->mCharge;                
  mDecayVertexXi.setX(xi->mDecayVertexXiX);
  mDecayVertexXi.setY(xi->mDecayVertexXiY);
  mDecayVertexXi.setZ(xi->mDecayVertexXiZ);
  mDcaXiDaughters = xi->mDcaXiDaughters;
  mDcaBachelorToPrimVertex = xi->mDcaBachelorToPrimVertex;
  mDcaXiToPrimVertex = xi->mDcaXiToPrimVertex;
  mMomBachelor.setX(xi->mMomBachelorX);
  mMomBachelor.setY(xi->mMomBachelorY);
  mMomBachelor.setZ(xi->mMomBachelorZ);
  
  mKeyBachelor = xi->mKeyBachelor;
  mTopologyMapBachelor[0] = xi->mTopologyMapBachelor[0];
  mTopologyMapBachelor[1] = xi->mTopologyMapBachelor[1];
  
  mChi2Xi = xi->mChi2Xi;// nulle
  mClXi = xi->mClXi;// nulle
  mChi2Bachelor = xi->mChi2Bachelor;
  mClBachelor = xi->mClBachelor;
  
  mDedxBachelor = xi->mDedxBachelor;
  mNumDedxBachelor = xi->mNumDedxBachelor;

  UpdateXi();
}
#endif // __ROOT__

