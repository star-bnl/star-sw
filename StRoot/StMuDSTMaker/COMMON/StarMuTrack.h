/***************************************************************************
 *
 * $Id: StarMuTrack.h,v 1.1 2002/03/05 15:41:10 jeromel Exp $
 * Author: Frank Laue, BNL, laue@bnl.gov
 *
 ***************************************************************************/

#ifndef StarMuTrack_h
#define StarMuTrack_h

#include "StarMuDst.h"
#include "StarMuEvent.h"
#include "StarMuHelix.h"
#include "StEvent/StTrackTopologyMap.h"
#include "StEvent/StRunInfo.h"
#include "StarClassLibrary/StPhysicalHelixD.hh"
#include "StarClassLibrary/StThreeVectorD.hh"
#include "StarClassLibrary/StThreeVectorF.hh"

#include "TObject.h"

#define __NOVALUE__ -999

class StRichSpectra;
class StEvent;
class StTrack;


class StuProbabilityPidAlgorithm;


class StarMuTrack : public TObject {
public:
  StarMuTrack(){/* no-op*/};
  StarMuTrack(const StEvent*, const StTrack*, int index2Global=-2, int index2RichSpectra=-2, bool l3=false);
  
  Short_t id();
  Short_t type();
  Short_t flag();
  Int_t index2Global();
  Int_t index2RichSpectra();
  StarMuTrack* globalTrack();
  StRichSpectra* richSpectra();
  Short_t nHits();
  Short_t nHitsPoss();
  Short_t nHitsDedx();
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
  Short_t mNHits;
  Short_t mNHitsPoss; 
  Short_t mNHitsDedx;
  Float_t mPidProbElectron;
  Float_t mPidProbPion;
  Float_t mPidProbKaon;
  Float_t mPidProbProton;
  Float_t mNSigmaElectron;
  Float_t mNSigmaPion;
  Float_t mNSigmaKaon;
  Float_t mNSigmaProton;
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
  StarMuHelix mHelix;
  StarMuHelix mOuterHelix;

  void setIndex2Global(size_t i) {mIndex2Global=i;}
  void setIndex2RichSpectra(size_t i) {mIndex2RichSpectra=i;}
  StThreeVectorD dca(const StEvent*, const StTrack*);
  StThreeVectorD momentumAtPrimaryVertex(const StEvent* event, const StTrack* track);

  static StuProbabilityPidAlgorithm* mProbabilityPidAlgorithm;
  static double mProbabilityPidCentrality;
  ClassDef(StarMuTrack,1)
};

inline Short_t StarMuTrack::id() {return mId;}
inline Short_t StarMuTrack::type() {return mType;}
inline Short_t StarMuTrack::flag() {return mFlag;}
inline Int_t StarMuTrack::index2Global() {return mIndex2Global;}
inline Int_t StarMuTrack::index2RichSpectra() {return mIndex2RichSpectra;}
inline Short_t StarMuTrack::nHits() {return mNHits;}
inline Short_t StarMuTrack::nHitsPoss() {return mNHitsPoss;}
inline Short_t StarMuTrack::nHitsDedx() {return mNHitsDedx;}
inline Float_t StarMuTrack::pidProbElectron() {return mPidProbElectron;}
inline Float_t StarMuTrack::pidProbPion() {return mPidProbPion;}
inline Float_t StarMuTrack::pidProbKaon() {return mPidProbKaon;}
inline Float_t StarMuTrack::pidProbProton() {return mPidProbProton;}
inline Float_t StarMuTrack::nSigmaElectron() {return mNSigmaElectron;}
inline Float_t StarMuTrack::nSigmaPion() {return mNSigmaPion;}
inline Float_t StarMuTrack::nSigmaKaon() {return mNSigmaKaon;}
inline Float_t StarMuTrack::nSigmaProton() {return mNSigmaProton;}
inline Float_t StarMuTrack::dEdx() {return mdEdx;}
inline Float_t StarMuTrack::chi2xy() {return mChiSqXY;}
inline Float_t StarMuTrack::chi2z() {return mChiSqZ;}
inline StTrackTopologyMap StarMuTrack::topologyMap() {return mTopologyMap;}
inline Float_t StarMuTrack::pt() {return mPt;}
inline Float_t StarMuTrack::eta() {return mEta;}
inline Float_t StarMuTrack::phi() {return mPhi;}
inline StThreeVectorF StarMuTrack::p() {return mP;}
inline StThreeVectorF StarMuTrack::momentum() {return mP;}
inline StThreeVectorF StarMuTrack::dca() {return mDCA;}
inline StThreeVectorF StarMuTrack::dcaGlobal() {return mDCAGlobal;}
inline StThreeVectorF StarMuTrack::firstPoint() {return mFirstPoint;}
inline StThreeVectorF StarMuTrack::lastPoint() {return mLastPoint;}
inline StPhysicalHelixD StarMuTrack::helix() {return StPhysicalHelixD(mHelix.p(),mHelix.origin(), mHelix.b(), mHelix.q());}  
inline StPhysicalHelixD StarMuTrack::outerHelix() {return StPhysicalHelixD(mOuterHelix.p(),mOuterHelix.origin(), mOuterHelix.b(), mOuterHelix.q());}  
//!inline StPhysicalHelixD StarMuTrack::helix() {return mHelix;}
//!inline StPhysicalHelixD StarMuTrack::outerHelix() {return mOuterHelix;}
inline void StarMuTrack::setProbabilityPidAlgorithm(StuProbabilityPidAlgorithm* p) { mProbabilityPidAlgorithm=p;}
inline void StarMuTrack::setProbabilityPidCentrality(double cent) { mProbabilityPidCentrality = cent;}

inline StarMuTrack* StarMuTrack::globalTrack() { return (mIndex2Global>=0) ? (StarMuTrack*)StarMuDst::array(muGlobal)->UncheckedAt(mIndex2Global) :0;}
inline StRichSpectra* StarMuTrack::richSpectra() { return (mIndex2RichSpectra>=0) ? (StRichSpectra*)StarMuDst::array(muRich)->UncheckedAt(mIndex2RichSpectra) : 0;}


#endif

/***************************************************************************
 *
 * $Log: StarMuTrack.h,v $
 * Revision 1.1  2002/03/05 15:41:10  jeromel
 * First version of Frank's Commone MicroDST.
 *
 *
 **************************************************************************/
