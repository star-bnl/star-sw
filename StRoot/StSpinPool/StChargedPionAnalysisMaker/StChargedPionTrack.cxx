#include "StChargedPionTrack.h"

#include "TClass.h"

ClassImp(StChargedPionTrack)

StChargedPionTrack::StChargedPionTrack() : TObject(),
	mId(0),mFlag(0),mVertexIndex(0),mNHits(0),mNHitsPoss(0),mNHitsDedx(0),mNHitsFit(0),
	mPidProbElectron(0),mPidProbPion(0),mPidProbKaon(0),mPidProbProton(0),mNSigmaElectron(0),
	mNSigmaPion(0),mNSigmaKaon(0),mNSigmaProton(0),mdEdx(0),mChi2(0),mChi2Prob(0),mPt(0),mPhi(0),
	mEta(0),mCharge(0),mP(0.,0.,0.),mFirstPoint(0.,0.,0.),mLastPoint(0.,0.,0.),
	mHelix(StThreeVectorF(0.,0.,0.),StThreeVectorF(0.,0.,0.),0.,0.),
	mOuterHelix(StThreeVectorF(0.,0.,0.),StThreeVectorF(0.,0.,0.),0.,0.),
	mDCA(0.,0.,0.),mSigmaDcaD(0),mSigmaDcaZ(0)
{ }

StChargedPionTrack::StChargedPionTrack(const StChargedPionTrack& t) : TObject()
{
	this->mId				= t.id();
	this->mFlag				= t.flag();
	this->mVertexIndex		= t.vertexIndex();
	this->mNHits			= t.nHits();
	this->mNHitsPoss		= t.nHitsPoss();
	this->mNHitsDedx		= t.nHitsDedx();
	this->mNHitsFit			= t.nHitsFit();
	this->mPidProbElectron	= pack2UnsignedShort(t.pidProbElectron(),__PROB_SCALE__);
	this->mPidProbPion		= pack2UnsignedShort(t.pidProbPion(),__PROB_SCALE__);
	this->mPidProbKaon		= pack2UnsignedShort(t.pidProbKaon(),__PROB_SCALE__);
	this->mPidProbProton	= pack2UnsignedShort(t.pidProbProton(),__PROB_SCALE__);
	this->mNSigmaElectron	= pack2Int( fabsMin(t.nSigmaElectron(),__SIGMA_SCALE__), __SIGMA_SCALE__ );
	this->mNSigmaPion		= pack2Int( fabsMin(t.nSigmaPion(),__SIGMA_SCALE__), __SIGMA_SCALE__ );
	this->mNSigmaKaon		= pack2Int( fabsMin(t.nSigmaKaon(),__SIGMA_SCALE__), __SIGMA_SCALE__ );
	this->mNSigmaProton		= pack2Int( fabsMin(t.nSigmaProton(),__SIGMA_SCALE__), __SIGMA_SCALE__ );
	this->mdEdx				= t.dEdx();
	this->mChi2				= t.chi2();
	this->mChi2Prob			= t.chi2prob();
	this->mPt				= t.pt();
	this->mPhi				= t.phi();
	this->mEta				= t.eta();
	this->mCharge			= t.charge();
	this->mP				= t.p();
	this->mFirstPoint		= t.firstPoint();
	this->mLastPoint		= t.lastPoint();
	this->mHelix			= t.helix();
	this->mOuterHelix		= t.outerHelix();
	this->mProbPidTraits	= t.probPidTraits();
	this->mDCA				= t.dca();
	this->mSigmaDcaD		= t.sigmaDcaD();
	this->mSigmaDcaZ		= t.sigmaDcaZ();
}

double StChargedPionTrack::length() const 
{ 
	return fabs( helix().pathLength(StThreeVectorD(mLastPoint)) ); 
}

double StChargedPionTrack::lengthMeasured() const 
{ 
	return fabs( helix().pathLength(StThreeVectorD(mLastPoint)) - helix().pathLength(StThreeVectorD(mFirstPoint)) ); 
}

float StChargedPionTrack::dcaD() const
{
	return 0.;
}

float StChargedPionTrack::dcaZ() const
{
	return 0.;
}