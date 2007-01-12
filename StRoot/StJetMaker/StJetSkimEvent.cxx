
//StJetSkimEvent.cxx
//M.L. Miller (MIT)
//12/06

#include <iostream>
#include <string>
using namespace std;

//local
#include "StJetSkimEvent.h"

ClassImp(StJetSkimTrig)
ClassImp(StJetSkimEvent)
ClassImp(StJetSkimVert)

StJetSkimTrig::StJetSkimTrig(const StJetSkimTrig& rhs) 
: trigId(rhs.trigId), prescale(rhs.prescale), isSatisfied(rhs.isSatisfied)
{
}

StJetSkimVert::StJetSkimVert()
{
	for (int i=0; i<3; ++i) {
		mPosition[i] = 0.;
		mPosError[i] = 0.;
	}
	mVertexFinderId  = mNTracksUsed = mNCTBMatch = mNBEMCMatch = mNEEMCMatch = mNCrossCentralMembrane = mRefMultNeg = mRefMultPos = mRefMultFtpcWest = mRefMultFtpcEast = 0;
	mRanking =  mSumTrackPt = mMeanDip = mChiSquared = 0.;
}

void StJetSkimVert::setPosition(float* x)
{
	mPosition[0] = x[0];
	mPosition[1] = x[1];
	mPosition[2] = x[2];
}
void StJetSkimVert::setError(float* x)
{
	mPosError[0] = x[0];
	mPosError[1] = x[1];
	mPosError[2] = x[2];
}



void StJetSkimVert::clear()
{
	for (int i=0; i<3; ++i) {
		mPosition[i] = 0.;
		mPosError[i] = 0.;
	}	
	mVertexFinderId  = mNTracksUsed = mNCTBMatch = mNBEMCMatch = mNEEMCMatch = mNCrossCentralMembrane = mRefMultNeg = mRefMultPos = mRefMultFtpcWest = mRefMultFtpcEast = 0;
	mRanking =  mSumTrackPt = mMeanDip = mChiSquared = 0.;
}

StJetSkimEvent::StJetSkimEvent() 
: TObject(), mTriggers(new TClonesArray("StJetSkimTrig",100)), mVertices(new TClonesArray("StJetSkimVert",100)),
mBestVert(new StJetSkimVert())
{
	mMudstFileName = "Undefined";
	mFill = mRunId = mEventId = mbx7 = mbx48 = mSpinBits = 0;
	mEbbc = mWbbc = mBbcTimeBin = 0;
	mIsValid = mIsPolLong = mIsPolTrans = mIsMaskedUsingBx48 = mOffsetBx48minusBX7 = mSpin4usingBx48 = 0;
	//for (int i=0; i<32; ++i) {
	for (int i=0; i<mL2Result.GetSize(); ++i) {
		mL2Result[i] = 0;
	}	
}

StJetSkimEvent::~StJetSkimEvent()
{
}

void StJetSkimEvent::clear()
{
	mMudstFileName = "Undefined";
	mFill = mRunId = mEventId =  mbx7 = mbx48 = mSpinBits = 0;
	mEbbc = mWbbc = mBbcTimeBin = 0;
	mIsValid = mIsPolLong = mIsPolTrans = mIsMaskedUsingBx48 = mOffsetBx48minusBX7 = mSpin4usingBx48 = 0;
	mTriggers->Clear();
	mVertices->Clear();
	mBestVert->clear();
	//for (int i=0; i<32; ++i) {
	for (int i=0; i<mL2Result.GetSize(); ++i) {
		mL2Result[i] = 0;
	}
	//mL2Result.Clear();
	
}
/*
void StJetSkimEvent::setL2Result(int* vals)
{
	assert(vals);
	for (int i=0; i<32; ++i) {
		mL2Result[i] = vals[i];
	}
}
*/

void StJetSkimEvent::setTrig(const StJetSkimTrig& t)
{
	int addAt = mTriggers->GetLast()+1;
	
	//and here's the crazy syntax
	TClonesArray& arrayRef = *(mTriggers);
	new ( arrayRef[addAt]) StJetSkimTrig( t );
}

void StJetSkimEvent::setVert(const StJetSkimVert& t)
{
	int addAt = mVertices->GetLast()+1;
	
	//and here's the crazy syntax
	TClonesArray& arrayRef = *(mVertices);
	new ( arrayRef[addAt]) StJetSkimVert( t );
}
