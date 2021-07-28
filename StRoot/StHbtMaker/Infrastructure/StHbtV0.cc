#include "StHbtV0.hh"
#include "StHbtTTreeV0.h"
#include "phys_constants.h"
#include "StHbtTTreeEvent.h"

// -----------------------------------------------------------------------
StHbtV0::StHbtV0(const StHbtV0& v){ // copy constructor
  mDecayLengthV0 = v.mDecayLengthV0;
  mDecayVertexV0 = v.mDecayVertexV0;
  mDcaV0Daughters = v.mDcaV0Daughters;
  mDcaV0ToPrimVertex = v.mDcaV0ToPrimVertex;
  mDcaPosToPrimVertex = v.mDcaPosToPrimVertex;
  mDcaNegToPrimVertex = v.mDcaNegToPrimVertex;
  mMomPos = v.mMomPos;
  mMomNeg = v.mMomNeg;

  mTrackTopologyMapPos[0] = v.mTrackTopologyMapPos[0];
  mTrackTopologyMapPos[1] = v.mTrackTopologyMapPos[1];
  mTrackTopologyMapNeg[0] = v.mTrackTopologyMapNeg[0];
  mTrackTopologyMapNeg[1] = v.mTrackTopologyMapNeg[1];
   
  mKeyPos = v.mKeyPos;
  mKeyNeg = v.mKeyNeg;
     
  mTpcHitsPos = v.mTpcHitsPos;
  mTpcHitsNeg = v.mTpcHitsNeg;

  mChi2V0 = v.mChi2V0;
  mClV0 = v.mClV0;
  mChi2Pos = v.mChi2Pos;
  mClPos = v.mClPos;
  mChi2Neg = v.mChi2Neg;
  mClNeg = v.mClNeg;
  mDedxPos = v.mDedxPos;
  mErrDedxPos = v.mErrDedxPos;//Gael 04Fev2002
  mLenDedxPos = v.mLenDedxPos;//Gael 04Fev2002
  mDedxNeg = v.mDedxNeg;
  mErrDedxNeg = v.mErrDedxNeg;//Gael 04Fev2002
  mLenDedxNeg = v.mLenDedxNeg;//Gael 04Fev2002

  mNumDedxPos = v.mNumDedxPos;
  mNumDedxNeg = v.mNumDedxNeg;

  mHelixPos = v.mHelixPos;// Gael 12 Sept
  mHelixNeg = v.mHelixNeg;// Gael 12 Sept
  mHiddenInfo = v.mHiddenInfo? v.mHiddenInfo->clone() : 0;// GR 11 DEC 02
  UpdateV0();
}
// -----------------------------------------------------------------------
void StHbtV0::UpdateV0(){
  //Calc. derived memebers of the v0 class
  float MomNegAlongV0, MomPosAlongV0;

   mMomV0  = mMomPos + mMomNeg;
   mPtV0   = mMomV0.perp();
   mPtotV0 = mMomV0.mag();
   mPtPos  = mMomPos.perp();
   mPtotPos= mMomPos.mag();
   mPtNeg  = mMomNeg.perp();
   mPtotNeg= mMomNeg.mag();
   mELambda= ::sqrt(mPtotV0*mPtotV0+M_LAMBDA*M_LAMBDA);
   mEK0Short= ::sqrt(mPtotV0*mPtotV0+M_KAON_0_SHORT*M_KAON_0_SHORT);
   mEPosProton = ::sqrt(mPtotPos*mPtotPos+M_PROTON*M_PROTON);
   mENegProton = ::sqrt(mPtotNeg*mPtotNeg+M_PROTON*M_PROTON);
   mEPosPion = ::sqrt(mPtotPos*mPtotPos+M_PION_PLUS*M_PION_PLUS);
   mENegPion = ::sqrt(mPtotNeg*mPtotNeg+M_PION_MINUS*M_PION_MINUS);
  
   MomNegAlongV0 =  mMomNeg*mMomV0 / ::sqrt(::pow(mPtotV0,2));
   MomPosAlongV0 =  mMomPos*mMomV0 / ::sqrt(::pow(mPtotV0,2));

   mAlphaV0 = (MomPosAlongV0-MomNegAlongV0)/(MomPosAlongV0+MomNegAlongV0);
   mPtArmV0 =  ::sqrt(mPtotPos*mPtotPos - MomPosAlongV0*MomPosAlongV0);
   mMassLambda = ::sqrt(::pow(mEPosProton+mENegPion,2)-::pow(mPtotV0,2));
   mMassAntiLambda = ::sqrt(::pow(mENegProton+mEPosPion,2)-::pow(mPtotV0,2));
   mMassK0Short = ::sqrt(::pow(mENegPion+mEPosPion,2)-::pow(mPtotV0,2));

   mRapLambda = 0.5*::log( (mELambda+mMomV0.z()) / (mELambda-mMomV0.z()) );
   mCTauLambda = M_LAMBDA*(mDecayLengthV0) / ::sqrt( ::pow((double)mMomV0.mag(),2.) );
   
   mRapK0Short = 0.5*::log( (mEK0Short+mMomV0.z()) / (mEK0Short-mMomV0.z()) );
   mCTauK0Short = M_KAON_0_SHORT*(mDecayLengthV0) / ::sqrt( ::pow((double)mMomV0.mag(),2.) );

}
// -----------------------------------------------------------------------
#ifdef __ROOT__
#include "StStrangeMuDstMaker/StV0MuDst.hh"
StHbtV0::StHbtV0( StV0MuDst& v){ // from strangess micro dst structure
  mDecayLengthV0 = v.decayLengthV0();
  mDecayVertexV0 = StHbtThreeVector( v.decayVertexV0X(), v.decayVertexV0Y(), v.decayVertexV0Z() );
  mDcaV0Daughters = v.dcaV0Daughters();
  mDcaV0ToPrimVertex = v.dcaV0ToPrimVertex();
  mDcaPosToPrimVertex = v.dcaPosToPrimVertex();
  mDcaNegToPrimVertex = v.dcaNegToPrimVertex();
  mMomPos = StHbtThreeVector( v.momPosX(), v.momPosY(), v.momPosZ() );
  mMomNeg = StHbtThreeVector( v.momNegX(), v.momNegY(), v.momNegZ() ); 
#ifdef STHBTDEBUG
  cout << " hist pos ";
  cout << v.topologyMapPos().numberOfHits(kTpcId); 
  cout << " hist neg ";
  cout << v.topologyMapNeg().numberOfHits(kTpcId) << endl;
#endif
  mTpcHitsPos = ( v.topologyMapPos().numberOfHits(kTpcId) );
  mTpcHitsNeg = ( v.topologyMapNeg().numberOfHits(kTpcId) );
  mTrackTopologyMapPos[0] = ( v.topologyMapPos().data(0) );
  mTrackTopologyMapPos[1] = ( v.topologyMapPos().data(1) );
  mTrackTopologyMapNeg[0] = ( v.topologyMapNeg().data(0) );
  mTrackTopologyMapNeg[1] = ( v.topologyMapNeg().data(1) );
  mKeyPos = v.keyPos();
  mKeyNeg = v.keyNeg();
  mChi2V0 = v.chi2V0();
  mClV0 = v.clV0();
  mChi2Pos = v.chi2Pos();
  mClPos = v.clPos();
  mChi2Neg = v.chi2Neg();
  mClNeg = v.clNeg();
  mDedxPos = v.dedxPos();
  mErrDedxPos = v.errDedxPos();//Gael 04Fev2002
  mLenDedxPos = v.lenDedxPos();//Gael 04Fev2002
  mDedxNeg = v.dedxNeg();
  mErrDedxNeg = v.errDedxNeg();//Gael 04Fev2002
  mLenDedxNeg = v.lenDedxNeg();//Gael 04Fev2002
  mNumDedxPos = v.numDedxPos();
  mNumDedxNeg = v.numDedxNeg();
  mHiddenInfo =  0;//GR 11 DEC 02

#ifdef STHBTDEBUG
  cout << " keyPos " << v.keyPos() << endl;
  cout << " keyNeg " << v.keyNeg() << endl;
#endif
  mMomV0 = StHbtThreeVector( v.momV0X(), v.momV0Y(), v.momV0Z() );
#ifdef STHBTDEBUG
  cout << " alpha  ";
  cout << v.alphaV0();
  cout << " ptArm  ";
  cout << v.ptArmV0() << endl;
#endif
  mAlphaV0 = v.alphaV0();
  mPtArmV0 = v.ptArmV0();
  mELambda = v.eLambda();
  mEK0Short = v.eK0Short();
  mEPosProton = v.ePosProton();
  mEPosPion = v.ePosPion();
  mENegPion = v.eNegPion();
  mENegProton = v.eNegProton();
  mMassLambda = v.massLambda();
  mMassAntiLambda = v.massAntiLambda();
  mMassK0Short = v.massK0Short();
  mRapLambda = v.rapLambda();
  mRapK0Short = v.rapK0Short();
  mCTauLambda = v.cTauLambda();
  mCTauK0Short = v.cTauK0Short();
  mPtV0 = v.ptV0();
  mPtotV0 = v.ptotV0();
  mPtPos = v.ptPos();
  mPtotPos = v.ptotPos();
  mDedxPos = v.dedxPos();
  mPtNeg = v.ptNeg();
  mPtotNeg = v.ptotNeg();
  mDedxNeg = v.dedxNeg();
}


StHbtV0::StHbtV0(const StHbtTTreeEvent* ev, const StHbtTTreeV0* v) {
  mDecayVertexV0 = StHbtThreeVector(v->mDecayVertexV0X,v->mDecayVertexV0Y,v->mDecayVertexV0Z);

  mPrimaryVertex.setX(ev->mVertexX);
  mPrimaryVertex.setY(ev->mVertexY);
  mPrimaryVertex.setZ(ev->mVertexZ);

  mDecayLengthV0 = v->mDecayLengthV0;
  mDcaV0Daughters = v->mDcaV0Daughters;
  mDcaV0ToPrimVertex = v->mDcaV0ToPrimVertex;
  mDcaPosToPrimVertex = v->mDcaPosToPrimVertex;
  mDcaNegToPrimVertex = v->mDcaNegToPrimVertex;

  mMomPos = StHbtThreeVector( v->mMomPosX, v->mMomPosY, v->mMomPosZ);
  mMomNeg = StHbtThreeVector( v->mMomNegX, v->mMomNegY, v->mMomNegZ);
  
  mTrackTopologyMapPos[0] = v->mTrackTopologyMapPos[0];
  mTrackTopologyMapPos[1] = v->mTrackTopologyMapPos[1];
  mTrackTopologyMapNeg[0] = v->mTrackTopologyMapNeg[0];
  mTrackTopologyMapNeg[1] = v->mTrackTopologyMapNeg[1];
  
  mKeyPos = v->mKeyPos;
  mKeyNeg = v->mKeyNeg;

  mTpcHitsPos = v->mTpcHitsPos;
  mTpcHitsNeg = v->mTpcHitsNeg;
  
  mChi2V0 = v->mChi2V0;
  mClV0 = v->mClV0;
  mChi2Pos = v->mChi2Pos;
  mClPos = v->mClPos;
  mChi2Neg = v->mChi2Neg;
  mClNeg = v->mClNeg;

  mDedxPos = v->mDedxPos;
  mErrDedxPos = v->mErrDedxPos;//Gael 04Fev2002
  mLenDedxPos = v->mLenDedxPos;//Gael 04Fev2002
  mDedxNeg = v->mDedxNeg;
  mErrDedxNeg = v->mErrDedxNeg;//Gael 04Fev2002
  mLenDedxNeg = v->mLenDedxNeg;//Gael 04Fev2002

  mNumDedxPos = v->mNumDedxPos;
  mNumDedxNeg = v->mNumDedxNeg;

  mHiddenInfo = 0;

  UpdateV0();
}

#endif // __ROOT__
void StHbtV0::SetHelixPos(const StPhysicalHelixD& h){mHelixPos = h;}// Gael 12 Sept 02
const StPhysicalHelixD& StHbtV0::HelixPos() const {return mHelixPos;}// Gael 12 Sept 02
void StHbtV0::SetHelixNeg(const StPhysicalHelixD& h){mHelixNeg = h;}// Gael 12 Sept 02
const StPhysicalHelixD& StHbtV0::HelixNeg() const {return mHelixNeg;}// Gael 12 Sept 02

void StHbtV0::SetHiddenInfo(StHbtHiddenInfo* aHiddenInfo) {mHiddenInfo=aHiddenInfo;}
bool StHbtV0::ValidHiddenInfo() const { if (mHiddenInfo) return true; else return false; }
StHbtHiddenInfo* StHbtV0::getHiddenInfo() const {return mHiddenInfo;}

