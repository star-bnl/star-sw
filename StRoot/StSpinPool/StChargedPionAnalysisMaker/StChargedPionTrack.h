#ifndef ST_CHARGED_PION_TRACK_HH
#define ST_CHARGED_PION_TRACK_HH

#include "StPhysicalHelixD.hh"
#include "StThreeVectorF.hh"

#include "StMuDSTMaker/COMMON/StMuProbPidTraits.h"
#include "StMuDSTMaker/COMMON/StMuUtilities.h"

#include "TObject.h"

#define __PROB_SCALE__  1000.
#define __SIGMA_SCALE__ 1000.

class StChargedPionTrack : public TObject
{
public:
	StChargedPionTrack() { /*no-op*/ };
	
	short id() const; ///< Returns the track id(or key), is unique for a track node, i.e. global and primary tracks have the same id.
    short flag() const; ///< Returns flag, (see StEvent manual for type information) 
    int vertexIndex() const; ///< Returns index of associated primary vertex.
    unsigned short nHits() const;     ///< Return total number of hits on track.
    unsigned short nHitsPoss() const; ///< Return number of possible hits on track.
    unsigned short nHitsDedx() const; ///< Return number of hits used for dEdx. 
    unsigned short nHitsFit() const;  ///< Return total number of hits used in fit. 
    double pidProbElectron() const; ///< Returns Aihong's probability of being an electron.
    double pidProbPion() const;     ///< Returns Aihong's probability of being a pion.
    double pidProbKaon() const;     ///< Returns Aihong's probability of being a kaon.
    double pidProbProton() const;   ///< Returns Aihong's probability of being a proton.
    double nSigmaElectron() const;  ///< Returns Craig's distance to the calculated dE/dx band for electrons in units of sigma.
    double nSigmaPion() const;      ///< Returns Craig's distance to the calculated dE/dx band for pions in units of sigma.
    double nSigmaKaon() const;      ///< Returns Craig's distance to the calculated dE/dx band for kaons in units of sigma.
    double nSigmaProton() const;    ///< Returns Craig's distance to the calculated dE/dx band for protons in units of sigma.
    double dEdx() const; ///< Returns measured dE/dx value.
    double chi2() const;     ///< Returns chi2 of fit.       
    double chi2prob() const; ///< Returns probability of fit.
    double pt() const;   ///< Returns pT at point of dca to primary vertex.
    double phi() const;  ///< Returns phi at point of dca to primary vertex.
    double eta() const;  ///< Returns pseudo rapidity at point of dca to primary vertex.
    double length() const; ///< Returns length of track (cm) from primary vertex to last measured point.
    double lengthMeasured() const;  ///< Returns length of track (cm) from first to last measured point.
    short charge() const;  ///< Returns charge. 
    StThreeVectorF p() const; ///< Returns 3-momentum at dca to primary vertex.
    StThreeVectorF firstPoint() const; ///< Returns positions of first measured point.
    StThreeVectorF lastPoint() const; ///< Returns positions of last measured point.
	StPhysicalHelixD helix() const; ///< Returns inner helix (first measured point)
	StPhysicalHelixD outerHelix() const; ///< Returns outer helix (last measured point)
	StMuProbPidTraits probPidTraits() const; ///< Returns Yuri Fisyak new pid probabilities. 
	
	StThreeVectorF dca() const; ///< Returns 3D distance of closest approach to primary vertex.
    float dcaD() const; ///< Signed radial component of global DCA (projected)
    float dcaZ() const; ///< Z component of global DCA
    float sigmaDcaD() const; ///< Error on signed radial component of global DCA (projected)
    float sigmaDcaZ() const; ///< Error on Z component of global DCA
	
	void setId(short aId);
	void setFlag(short aFlag);
	void setVertexIndex(int aVertexIndex);
	void setNHits(unsigned short aNHits);
	void setNHitsPoss(unsigned short aNHitsPoss);
	void setNHitsDedx(unsigned short aNHitsDedx);
	void setNHitsFit(unsigned short aNHitsFit);
	void setPidProbElectron(double aPidProbElectron);
	void setPidProbPion(double aPidProbPion);
	void setPidProbKaon(double aPidProbKaon);
	void setPidProbProton(double aPidProbProton);
	void setNSigmaElectron(double aNSigmaElectron);
	void setNSigmaPion(double aNSigmaPion);
	void setNSigmaKaon(double aNSigmaKaon);
	void setNSigmaProton(double aNSigmaProton);
	void setDedx(double aDedx);
	void setChi2(double aChi2);
	void setChi2prob(double aChi2Prob);
	void setPt(double aPt);
	void setPhi(double aPhi);
	void setEta(double aEta);
	void setCharge(short aCharge);
	void setP(StThreeVectorF aP);
	void setFirstPoint(StThreeVectorF aFirstPoint);
	void setLastPoint(StThreeVectorF aLastPoint);
	void setHelix(StPhysicalHelixD aHelix);
	void setOuterHelix(StPhysicalHelixD aOuterHelix);
	void setDca(StThreeVectorF aDCA);
	void setSigmaDcaD(float aSigma);
	void setSigmaDcaZ(float aSigma);
	void setProbPidTraits(StMuProbPidTraits aTraits);
	
protected:
	Short_t mId;
	Short_t mFlag;
	Int_t mVertexIndex;
	UChar_t mNHits;           
	UChar_t mNHitsPoss;      
	UChar_t mNHitsDedx;       
	UChar_t mNHitsFit;       
	UShort_t mPidProbElectron;
	UShort_t mPidProbPion;
	UShort_t mPidProbKaon;
	UShort_t mPidProbProton;
	Int_t mNSigmaElectron;
	Int_t mNSigmaPion;
	Int_t mNSigmaKaon;
	Int_t mNSigmaProton;
	Float_t mdEdx;
	Float_t mChi2;
	Float_t mChi2Prob;
	Float_t mPt;
	Float_t mPhi;
	Float_t mEta;
	Short_t mCharge;
	StThreeVectorF mP;
	StThreeVectorF mFirstPoint;
	StThreeVectorF mLastPoint;
	StPhysicalHelixD mHelix;
	StPhysicalHelixD mOuterHelix;
	StThreeVectorF mDCA;
	Float_t mSigmaDcaD;
	Float_t mSigmaDcaZ;
	StMuProbPidTraits mProbPidTraits; ///< Class holding the new Yuri Fisyak pid probabilities.
	
	ClassDef(StChargedPionTrack,1)
};

inline short StChargedPionTrack::id() const {return mId;}
inline short StChargedPionTrack::flag() const {return mFlag;}
inline int StChargedPionTrack::vertexIndex() const {return mVertexIndex;}
inline unsigned short StChargedPionTrack::nHits() const {return mNHits;}
inline unsigned short StChargedPionTrack::nHitsDedx() const {return mNHitsDedx;}
inline unsigned short StChargedPionTrack::nHitsFit() const {return mNHitsFit;}
inline double StChargedPionTrack::pidProbElectron() const {return unPack(mPidProbElectron,__PROB_SCALE__);}
inline double StChargedPionTrack::pidProbPion() const     {return unPack(mPidProbPion,    __PROB_SCALE__);}
inline double StChargedPionTrack::pidProbKaon() const     {return unPack(mPidProbKaon,    __PROB_SCALE__);}
inline double StChargedPionTrack::pidProbProton() const   {return unPack(mPidProbProton,  __PROB_SCALE__);}
inline double StChargedPionTrack::nSigmaElectron() const  {return unPack(mNSigmaElectron, __SIGMA_SCALE__);}
inline double StChargedPionTrack::nSigmaPion() const      {return unPack(mNSigmaPion,     __SIGMA_SCALE__);}
inline double StChargedPionTrack::nSigmaKaon() const      {return unPack(mNSigmaKaon,     __SIGMA_SCALE__);}
inline double StChargedPionTrack::nSigmaProton() const    {return unPack(mNSigmaProton,   __SIGMA_SCALE__);}
inline double StChargedPionTrack::dEdx() const {return mdEdx;}
inline double StChargedPionTrack::chi2() const {return mChi2;}
inline double StChargedPionTrack::chi2prob() const {return mChi2Prob;}
inline short StChargedPionTrack::charge() const {return mCharge;}
inline double StChargedPionTrack::pt() const {return mPt;}
inline double StChargedPionTrack::eta() const {return mEta;}
inline double StChargedPionTrack::phi() const {return mPhi;}
inline StThreeVectorF StChargedPionTrack::p() const {return mP;}
inline StThreeVectorF StChargedPionTrack::dca() const {return mDCA;}
inline float StChargedPionTrack::sigmaDcaD() const {return mSigmaDcaD;}
inline float StChargedPionTrack::sigmaDcaZ() const {return mSigmaDcaZ;}
inline StThreeVectorF StChargedPionTrack::firstPoint() const {return mFirstPoint;}
inline StThreeVectorF StChargedPionTrack::lastPoint() const {return mLastPoint;}
inline StPhysicalHelixD StChargedPionTrack::helix() const {return mHelix;}
inline StPhysicalHelixD StChargedPionTrack::outerHelix() const {return mOuterHelix;}

inline void StChargedPionTrack::setId(short aId) {mId = aId;}
inline void StChargedPionTrack::setFlag(short aFlag) {mFlag = aFlag;}
inline void StChargedPionTrack::setVertexIndex(int aVertexIndex) {mVertexIndex = aVertexIndex;}
inline void StChargedPionTrack::setNHits(unsigned short aNHits) {mNHits = aNHits;}
inline void StChargedPionTrack::setNHitsPoss(unsigned short aNHitsPoss) {mNHitsPoss = aNHitsPoss;}
inline void StChargedPionTrack::setNHitsDedx(unsigned short aNHitsDedx) {mNHitsDedx = aNHitsDedx;}
inline void StChargedPionTrack::setNHitsFit(unsigned short aNHitsFit) {mNHitsFit = aNHitsFit;}
inline void StChargedPionTrack::setPidProbElectron(double aPidProbElectron) {pack2UnsignedShort(aPidProbElectron,__PROB_SCALE__);}
inline void StChargedPionTrack::setPidProbPion(double aPidProbPion) {pack2UnsignedShort(aPidProbPion,__PROB_SCALE__);}
inline void StChargedPionTrack::setPidProbKaon(double aPidProbKaon) {pack2UnsignedShort(aPidProbKaon,__PROB_SCALE__);}
inline void StChargedPionTrack::setPidProbProton(double aPidProbProton) {pack2UnsignedShort(aPidProbProton,__PROB_SCALE__);}
inline void StChargedPionTrack::setNSigmaElectron(double aNSigmaElectron) {mNSigmaElectron = pack2Int( fabsMin(aNSigmaElectron,__SIGMA_SCALE__), __SIGMA_SCALE__ );}
inline void StChargedPionTrack::setNSigmaPion(double aNSigmaPion) {mNSigmaPion = pack2Int( fabsMin(aNSigmaPion,__SIGMA_SCALE__), __SIGMA_SCALE__ );}
inline void StChargedPionTrack::setNSigmaKaon(double aNSigmaKaon) {mNSigmaKaon = pack2Int( fabsMin(aNSigmaKaon,__SIGMA_SCALE__), __SIGMA_SCALE__ );}
inline void StChargedPionTrack::setNSigmaProton(double aNSigmaProton) {mNSigmaProton = pack2Int( fabsMin(aNSigmaProton,__SIGMA_SCALE__), __SIGMA_SCALE__ );}
inline void StChargedPionTrack::setDedx(double adEdx) {mdEdx = adEdx;}
inline void StChargedPionTrack::setChi2(double aChi2) {mChi2 = aChi2;}
inline void StChargedPionTrack::setChi2prob(double aChi2Prob) {mChi2Prob = aChi2Prob;}
inline void StChargedPionTrack::setPt(double aPt) {mPt = aPt;}
inline void StChargedPionTrack::setPhi(double aPhi) {mPhi = aPhi;}
inline void StChargedPionTrack::setEta(double aEta) {mEta = aEta;}
inline void StChargedPionTrack::setCharge(short aCharge) {mCharge = aCharge;}
inline void StChargedPionTrack::setP(StThreeVectorF aP) {mP = aP;}
inline void StChargedPionTrack::setFirstPoint(StThreeVectorF aFirstPoint) {mFirstPoint = aFirstPoint;}
inline void StChargedPionTrack::setLastPoint(StThreeVectorF aLastPoint) {mLastPoint = aLastPoint;}
inline void StChargedPionTrack::setHelix(StPhysicalHelixD aHelix) {mHelix = aHelix;}
inline void StChargedPionTrack::setOuterHelix(StPhysicalHelixD aOuterHelix) {mOuterHelix = aOuterHelix;}
inline void StChargedPionTrack::setDca(StThreeVectorF aDCA) {mDCA = aDCA;}
inline void StChargedPionTrack::setSigmaDcaD(float aSigma) {mSigmaDcaD = aSigma;}
inline void StChargedPionTrack::setSigmaDcaZ(float aSigma) {mSigmaDcaZ = aSigma;}
inline void StChargedPionTrack::setProbPidTraits(StMuProbPidTraits aTraits) {mProbPidTraits = aTraits;}

#endif
