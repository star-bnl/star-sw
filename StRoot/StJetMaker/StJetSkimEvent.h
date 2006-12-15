//StJetSkimEvent
//M.L. Miller (MIT)
//12/06

#ifndef StJetSkimEvent_HH
#define StJetSkimEvent_HH

#include "TObject.h"
#include "TClonesArray.h"

class StJetSkimTrig : public TObject
{
public:
	StJetSkimTrig() : trigId(0), prescale(0.), isSatisfied(0) {};
	StJetSkimTrig(const StJetSkimTrig& rhs);
	virtual ~StJetSkimTrig() {};
	
	int trigId;
	float prescale;
	int isSatisfied;
	
private:
	ClassDef(StJetSkimTrig,1)
};

class StJetSkimVert : public TObject {
	
public:
	StJetSkimVert();
	virtual ~StJetSkimVert() {};
	
	//action
	void clear();
	
	//sets
	void setPosition(float* x);
	void setError(float* y);
	
	void setVertexFinderId(int i) {mVertexFinderId = i;}
	void setRanking(Float_t i) { mRanking = i;}
	void setNTracksUsed(UShort_t i) {mNTracksUsed = i;}
	void setNCTBMatch(UShort_t i) { mNCTBMatch = i;}
	void setNBEMCMatch(UShort_t i) { mNBEMCMatch = i;}
	void setNEEMCMatch(UShort_t i) { mNEEMCMatch = i;}
	void setNCrossingCentralMembrane(UShort_t i) { mNCrossCentralMembrane = i;}
	void setSumTrackPt(Float_t i) { mSumTrackPt = i;}
	void setMeanDip(Float_t i) { mMeanDip = i;}
	void setChiSquared(Float_t i) { mChiSquared = i;}
	
	// RefMult fields
	void setRefMultNeg( UShort_t i) { mRefMultNeg = i;}
	void setRefMultPos(UShort_t i) { mRefMultPos = i;}
	void setRefMultFtpcWest(UShort_t i) { mRefMultFtpcWest = i;}
	void setRefMultFtpcEast( UShort_t i) { mRefMultFtpcEast = i;}
	
	//gets
	const float* position() const { return mPosition;}
	const float*  posError() const { return mPosError;}
	int vertexFinderId() const { return mVertexFinderId;} 
	Float_t ranking()  const { return mRanking;}
	UShort_t nTracksUsed() const { return mNTracksUsed;}
	UShort_t nCTBMatch() const { return mNCTBMatch;}
	UShort_t nBEMCMatch() const { return mNBEMCMatch;}
	UShort_t nEEMCMatch() const { return mNEEMCMatch;}
	UShort_t nCrossCentralMembrane() const  {return mNCrossCentralMembrane;}
	Float_t sumTrackPt() const { return mSumTrackPt; }
	Float_t meanDip() const { return mMeanDip; }
	Float_t chiSquared() const { return mChiSquared; }
	
	UShort_t refMultPos() const { return mRefMultPos; }
	UShort_t refMultNeg() const { return mRefMultNeg; }
	UShort_t refMult() const { return refMultPos() + refMultNeg(); }
	UShort_t refMultFtpcEast() const { return mRefMultFtpcEast; }
	UShort_t refMultFtpcWest() const { return mRefMultFtpcWest; }
	UShort_t refMultFtpc() const { return refMultFtpcEast() + refMultFtpcWest(); }
	
private:

	float mPosition[3];
	float mPosError[3];
	
	int mVertexFinderId;
	Float_t mRanking;
	UShort_t mNTracksUsed;
	UShort_t mNCTBMatch;
	UShort_t mNBEMCMatch;
	UShort_t mNEEMCMatch;
	UShort_t mNCrossCentralMembrane;
	Float_t mSumTrackPt;
	Float_t mMeanDip;
	Float_t mChiSquared;
	
	// RefMult fields
	UShort_t mRefMultNeg;
	UShort_t mRefMultPos;
	UShort_t mRefMultFtpcWest;
	UShort_t mRefMultFtpcEast;
	
	ClassDef(StJetSkimVert,1)

};

class StJetSkimEvent : public TObject
{
public:
	
	StJetSkimEvent();
	virtual ~StJetSkimEvent();
	
	//action
	void clear();
	
	//sets
	void setTrig(const StJetSkimTrig& );
	void setVert(const StJetSkimVert& );
	
	void setFill(float i) {mFill = i;}
	void setRunId(int i) {mRunId = i;}
	void setEventId(int i) {mEventId = i;}
	
	void setBx7(int i) {mbx7 = i;}
	void setBx48(int i) {mbx48 = i;}
	void setSpinBits(int i) {mSpinBits = i;}	
	
	void setEbbc(int i) {mEbbc = i;}
	void setWbbc(int i) {mWbbc = i;}
	void setBbcTimeBin(int i) {mBbcTimeBin = i;}
	
	void setIsValid(int i) {mIsValid = i;}
	void setIsPolLong(int i) {mIsPolLong = i;}
	void setIsPolTrans(int i) {mIsPolTrans = i;}
	void setIsMaskedUsingBx48(int i) {mIsMaskedUsingBx48 = i;}
	void setOffsetBx48minusBX7(int i) {mOffsetBx48minusBX7 = i;}
	void setSpin4UsingBx48(int i) {mSpin4usingBx48 = i;}
	
	//gets
	float fill() const {return mFill;}
	int runId() const {return mRunId;}
	int eventId() const {return mEventId;}
	
	int bx7() const {return mbx7;}
	int bx48() const {return mbx48;}
	int spinBits() const {return mSpinBits;}
	
	
	int eBbc() const {return mEbbc;}
	int wBbc() const {return mWbbc;}
	int bbcTimeBin() const {return mBbcTimeBin;}
	
	int isValid() const {return mIsValid;}
	int isPolLong() const {return mIsPolLong;}
	int isPolTrans() const {return mIsPolTrans;}
	int isMaskedUsingBx48() const {return mIsMaskedUsingBx48;}
	int offsetBx48minusBX7() const {return mOffsetBx48minusBX7;}
	int spin4usingBx48() const {return mSpin4usingBx48;}


	const TClonesArray* triggers() const {return mTriggers;}
	const TClonesArray* vertices() const {return mVertices;}
	StJetSkimVert* bestVert() {return mBestVert;}

private:
	
	float mFill;
	int mRunId;
	int mEventId;
	
	TClonesArray* mTriggers;
	TClonesArray* mVertices;
	StJetSkimVert* mBestVert;
	
	//bunch x-ing info from MuDSt
	int mbx7;
	int mbx48;
	int mSpinBits;
	
	//bbc info:
	int mEbbc; //E-bbc summed ADC
	int mWbbc; //W-bbc summed ADC
	int mBbcTimeBin;
	
	//spin db
	int mIsValid;
	int mIsPolLong;
	int mIsPolTrans;
	int mIsMaskedUsingBx48;
	int mOffsetBx48minusBX7;
	int mSpin4usingBx48;
	
	ClassDef(StJetSkimEvent,1)
};

	
	
#endif
