#include "StHbtMaker/Infrastructure/StHbtXi.hh"
#include "StHbtMaker/Infrastructure/StHbtTTreeXi.h"
#include "phys_constants.h"

// -----------------------------------------------------------------------
void StHbtXi::UpdateXi(){
  //Calc. derived memebers of the v0 class
  float MomNegAlongXi, MomPosAlongXi;

   mMomXi  = momPos() + momNeg();
   mPtXi   = mMomXi.perp();
   mPtotXi = mMomXi.mag();
   mPtPos  = momPos().perp();
   mPtotPos= momPos().mag();
   mPtNeg  = momNeg().perp();
   mPtotNeg= momNeg().mag();
   mELambda= sqrt(mPtotXi*mPtotXi+M_LAMBDA*M_LAMBDA);
   mEK0Short= sqrt(mPtotXi*mPtotXi+M_KAON_0_SHORT*M_KAON_0_SHORT);
   mEPosProton = sqrt(ptotPos()*ptotPos()+M_PROTON*M_PROTON);
   mENegProton = sqrt(ptotNeg()*ptotNeg()+M_PROTON*M_PROTON);
   mEPosPion = sqrt(ptotPos()*ptotPos()+M_PION_PLUS*M_PION_PLUS);
   mENegPion = sqrt(ptotNeg()*ptotNeg()+M_PION_MINUS*M_PION_MINUS);
  
   MomNegAlongXi =  momNeg()*mMomXi / sqrt(pow(mPtotXi,2));
   MomPosAlongXi =  momPos()*mMomXi / sqrt(pow(mPtotXi,2));

   mAlphaXi = (MomPosAlongXi-MomNegAlongXi)/(MomPosAlongXi+MomNegAlongXi);
   mPtArmXi =  sqrt(ptotPos()*ptotPos() - MomPosAlongXi*MomPosAlongXi);
   mMassLambda = sqrt(pow(ePosProton()+eNegPion(),2)-pow(mPtotXi,2));
   mMassAntiLambda = sqrt(pow(eNegProton()+ePosPion(),2)-pow(mPtotXi,2));
   mMassK0Short = sqrt(pow(eNegPion()+ePosPion(),2)-pow(mPtotXi,2));

   mRapLambda = 0.5*log( (eLambda()+mMomXi.z()) / (eLambda()-mMomXi.z()) );
   mCTauLambda = M_LAMBDA*(mDecayLengthXi) / sqrt( pow((double)mMomXi.mag(),2.) );
   
   mRapK0Short = 0.5*log( (eK0Short()+mMomXi.z()) / (eK0Short()-mMomXi.z()) );
   mCTauK0Short = M_KAON_0_SHORT*(mDecayLengthXi) / sqrt( pow((double)mMomXi.mag(),2.) );
}
// -----------------------------------------------------------------------
#ifdef __ROOT__
#include "StStrangeMuDstMaker/StXiMuDst.hh"
StHbtXi::StHbtXi( StXiMuDst& v0FromMuDst)  : StHbtV0(v0FromMuDst) { // from strangess micro dst structure
  UpdateV0(); // the v0 stuff

  mCharge = v0FromMuDst.charge();                
  mDecayVertexXiX = v0FromMuDst.decayVertexXiX();
  mDecayVertexXiY = v0FromMuDst.decayVertexXiY();
  mDecayVertexXiZ = v0FromMuDst.decayVertexXiZ();
  mDcaXiDaughters = v0FromMuDst.dcaXiDaughters();
  mDcaBachelorToPrimVertex = v0FromMuDst.dcaBachelorToPrimVertex();
  mDcaXiToPrimVertex = v0FromMuDst.dcaXiToPrimVertex();
  mMomBachelorX = v0FromMuDst.momBachelorX();
  mMomBachelorY = v0FromMuDst.momBachelorY();
  mMomBachelorZ = v0FromMuDst.momBachelorZ();
  
  mKeyBachelor = v0FromMuDst.keyBachelor();
  mTopologyMapBachelor[0] = v0FromMuDst.topologyMapBachelor().data(1);
  mTopologyMapBachelor[1] = v0FromMuDst.topologyMapBachelor().data(2);
  
  mChi2Xi = v0FromMuDst.chi2Xi();
  mClXi = v0FromMuDst.clXi();
  mChi2Bachelor = v0FromMuDst.chi2Bachelor();
  mClBachelor = v0FromMuDst.clBachelor();
  
  mDedxBachelor = v0FromMuDst.dedxBachelor();
  mNumDedxBachelor = v0FromMuDst.numDedxBachelor();

  UpdateXi(); // the v0 stuff
  
}

StHbtXi::StHbtXi(const StHbtTTreeEvent* ev, const StHbtTTreeXi* xi) : StHbtV0(ev, xi) {
  mCharge = xi->mCharge;                
  mDecayVertexXiX = xi->mDecayVertexXiX;
  mDecayVertexXiY = xi->mDecayVertexXiY;
  mDecayVertexXiZ = xi->mDecayVertexXiZ;
  mDcaXiDaughters = xi->mDcaXiDaughters;
  mDcaBachelorToPrimVertex = xi->mDcaBachelorToPrimVertex;
  mDcaXiToPrimVertex = xi->mDcaXiToPrimVertex;
  mMomBachelorX = xi->mMomBachelorX;
  mMomBachelorY = xi->mMomBachelorY;
  mMomBachelorZ = xi->mMomBachelorZ;
  
  mKeyBachelor = xi->mKeyBachelor;
  mTopologyMapBachelor[0] = xi->mTopologyMapBachelor[0];
  mTopologyMapBachelor[1] = xi->mTopologyMapBachelor[1];
  
  mChi2Xi = xi->mChi2Xi;
  mClXi = xi->mClXi;
  mChi2Bachelor = xi->mChi2Bachelor;
  mClBachelor = xi->mClBachelor;
  
  mDedxBachelor = xi->mDedxBachelor;
  mNumDedxBachelor = xi->mNumDedxBachelor;

  UpdateXi();
}
#endif // __ROOT__

