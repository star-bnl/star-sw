/***************************************************************************
 *
 * $Id: StMuTrack.h,v 1.1 2002/03/08 17:04:18 laue Exp $
 * Author: Frank Laue, BNL, laue@bnl.gov
 *
 ***************************************************************************/

#ifndef StMuTrack_h
#define StMuTrack_h

#include "StMuDst.h"
#include "StMuEvent.h"
#include "StMuHelix.h"
#include "StMuUtilities.h"


#include "StEvent/StTrackTopologyMap.h"
#include "StEvent/StRunInfo.h"
#include "StarClassLibrary/StPhysicalHelixD.hh"
#include "StarClassLibrary/StThreeVectorD.hh"
#include "StarClassLibrary/StThreeVectorF.hh"

#include "TObject.h"

#define __NOVALUE__ -999
#define __PROB_SCALE__ 1000.
#define __SIGMA_SCALE__ 1000.

class StRichSpectra;
class StEvent;
class StTrack;


class StuProbabilityPidAlgorithm;


class StMuTrack : public TObject {
public:
  StMuTrack(){/* no-op*/};
  StMuTrack(const StEvent*, const StTrack*, int index2Global=-2, int index2RichSpectra=-2, bool l3=false);
  
  Short_t id();
  Short_t type();
  Short_t flag();
  Int_t index2Global();
  Int_t index2RichSpectra();
  StMuTrack* globalTrack();
  StRichSpectra* richSpectra();
  UChar_t nHits();
  UChar_t  nHitsPoss();
  UChar_t  nHitsDedx();
  UChar_t  nHitsFit();
  Float_t pidProbElectron();
  Float_t pidProbPion();
  Float_t pidProbKaon();
  Float_t pidProbProton();
  Float_t nSigmaElectron();
  Float_t nSigmaPion();
  Float_t nSigmaKaon();
  Float_t nSigmaProton();
  Float_t dEdx();
  Float_t chi2xy();
  Float_t chi2z();
  Float_t pt();
  Float_t phi();
  Float_t eta();
  StTrackTopologyMap topologyMap();
  Float_t charge();
  StThreeVectorF p();
  StThreeVectorF momentum();
  StThreeVectorF dca();
  StThreeVectorF dcaGlobal();
  StThreeVectorF firstPoint();
  StThreeVectorF lastPoint();
  StPhysicalHelixD helix();  
  StPhysicalHelixD outerHelix();
  static void setProbabilityPidAlgorithm(StuProbabilityPidAlgorithm*);
  static void setProbabilityPidCentrality(double cent);
private:
  Short_t mId;
  Short_t mType;
  Short_t mFlag;
  Int_t mIndex2Global;
  Int_t mIndex2RichSpectra;
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
  Float_t mChiSqXY;
  Float_t mChiSqZ;
  Float_t mPt;
  Float_t mEta;
  Float_t mPhi;
  StTrackTopologyMap mTopologyMap;
  StThreeVectorF mP;
  StThreeVectorF mDCA;
  StThreeVectorF mDCAGlobal;
  StThreeVectorF mFirstPoint;
  StThreeVectorF mLastPoint;
  StMuHelix mHelix;
  StMuHelix mOuterHelix;

  void setIndex2Global(size_t i) {mIndex2Global=i;}
  void setIndex2RichSpectra(size_t i) {mIndex2RichSpectra=i;}
  StThreeVectorD dca(const StEvent*, const StTrack*);
  StThreeVectorD momentumAtPrimaryVertex(const StEvent* event, const StTrack* track);

  static StuProbabilityPidAlgorithm* mProbabilityPidAlgorithm;
  static double mProbabilityPidCentrality;

  friend class StMuDst;
  ClassDef(StMuTrack,3)
};

inline Short_t StMuTrack::id() {return mId;}
inline Short_t StMuTrack::type() {return mType;}
inline Short_t StMuTrack::flag() {return mFlag;}
inline Int_t StMuTrack::index2Global() {return mIndex2Global;}
inline Int_t StMuTrack::index2RichSpectra() {return mIndex2RichSpectra;}
inline UChar_t StMuTrack::nHits() {return mNHits;}
inline UChar_t  StMuTrack::nHitsPoss() {return mNHitsPoss;}
inline UChar_t  StMuTrack::nHitsDedx() {return mNHitsDedx;}
inline UChar_t  StMuTrack::nHitsFit() {return mNHitsFit;}
inline Float_t StMuTrack::pidProbElectron() {return unPack(mPidProbElectron,__PROB_SCALE__);}
inline Float_t StMuTrack::pidProbPion()     {return unPack(mPidProbPion,    __PROB_SCALE__);}
inline Float_t StMuTrack::pidProbKaon()     {return unPack(mPidProbKaon,    __PROB_SCALE__);}
inline Float_t StMuTrack::pidProbProton()   {return unPack(mPidProbProton,  __PROB_SCALE__);}
inline Float_t StMuTrack::nSigmaElectron()  {return unPack(mNSigmaElectron, __SIGMA_SCALE__);}
inline Float_t StMuTrack::nSigmaPion()      {return unPack(mNSigmaPion,     __SIGMA_SCALE__);}
inline Float_t StMuTrack::nSigmaKaon()      {return unPack(mNSigmaKaon,     __SIGMA_SCALE__);}
inline Float_t StMuTrack::nSigmaProton()    {return unPack(mNSigmaProton,   __SIGMA_SCALE__);}
inline Float_t StMuTrack::dEdx() {return mdEdx;}
inline Float_t StMuTrack::chi2xy() {return mChiSqXY;}
inline Float_t StMuTrack::chi2z() {return mChiSqZ;}
inline StTrackTopologyMap StMuTrack::topologyMap() {return mTopologyMap;}
inline Float_t StMuTrack::charge() {return mHelix.q();}
inline Float_t StMuTrack::pt() {return mPt;}
inline Float_t StMuTrack::eta() {return mEta;}
inline Float_t StMuTrack::phi() {return mPhi;}
inline StThreeVectorF StMuTrack::p() {return mP;}
inline StThreeVectorF StMuTrack::momentum() {return mP;}
inline StThreeVectorF StMuTrack::dca() {return mDCA;}
inline StThreeVectorF StMuTrack::dcaGlobal() {return mDCAGlobal;}
inline StThreeVectorF StMuTrack::firstPoint() {return mFirstPoint;}
inline StThreeVectorF StMuTrack::lastPoint() {return mLastPoint;}
inline StPhysicalHelixD StMuTrack::helix() {return StPhysicalHelixD(mHelix.p(),mHelix.origin(), mHelix.b(), mHelix.q());}  
inline StPhysicalHelixD StMuTrack::outerHelix() {return StPhysicalHelixD(mOuterHelix.p(),mOuterHelix.origin(), mOuterHelix.b(), mOuterHelix.q());}  
//!inline StPhysicalHelixD StMuTrack::helix() {return mHelix;}
//!inline StPhysicalHelixD StMuTrack::outerHelix() {return mOuterHelix;}
inline void StMuTrack::setProbabilityPidAlgorithm(StuProbabilityPidAlgorithm* p) { mProbabilityPidAlgorithm=p;}
inline void StMuTrack::setProbabilityPidCentrality(double cent) { mProbabilityPidCentrality = cent;}

inline StMuTrack* StMuTrack::globalTrack() { return (mIndex2Global>=0) ? (StMuTrack*)StMuDst::array(muGlobal)->UncheckedAt(mIndex2Global) :0;}
inline StRichSpectra* StMuTrack::richSpectra() { return (mIndex2RichSpectra>=0) ? (StRichSpectra*)StMuDst::array(muRich)->UncheckedAt(mIndex2RichSpectra) : 0;}


#endif

/***************************************************************************
 *
 * $Log: StMuTrack.h,v $
 * Revision 1.1  2002/03/08 17:04:18  laue
 * initial revision
 *
 *
 **************************************************************************/
