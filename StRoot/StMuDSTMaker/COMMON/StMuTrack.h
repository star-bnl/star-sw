/***************************************************************************
 *
 * $Id: StMuTrack.h,v 1.5 2002/09/19 21:54:01 laue Exp $
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
#include "StarClassLibrary/SystemOfUnits.h"

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
  
  short id() const;
  short type() const;
  short flag() const;
  int index2Global() const;
  int index2RichSpectra() const;
  StMuTrack* globalTrack() const;
  StRichSpectra* richSpectra() const;
  unsigned short nHits() const;
  unsigned short  nHitsPoss() const;
  unsigned short  nHitsDedx() const;
  unsigned short  nHitsFit() const;
  double pidProbElectron() const;
  double pidProbPion() const;
  double pidProbKaon() const;
  double pidProbProton() const;
  double nSigmaElectron() const;
  double nSigmaPion() const;
  double nSigmaKaon() const;
  double nSigmaProton() const;
  double dEdx() const;
  double chi2() const;
  double chi2prob() const;
  double chi2xy() const;
  double chi2z() const;
  double pt() const;
  double phi() const;
  double eta() const;
  double length() const;
  double lengthMeasured() const;
  StTrackTopologyMap topologyMap() const;
  Short_t charge() const;
  StThreeVectorF p() const;
  StThreeVectorF momentum() const;
  StThreeVectorF dca() const;
  StThreeVectorF dcaGlobal() const;
  StThreeVectorF firstPoint() const;
  StThreeVectorF lastPoint() const;
  StPhysicalHelixD helix() const;  
  StPhysicalHelixD outerHelix() const;
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

inline short StMuTrack::id() const {return mId;}
inline short StMuTrack::type() const {return mType;}
inline short StMuTrack::flag() const {return mFlag;}
inline int StMuTrack::index2Global() const {return mIndex2Global;}
inline int StMuTrack::index2RichSpectra() const {return mIndex2RichSpectra;}
inline unsigned short StMuTrack::nHits() const {return mNHits;}
inline unsigned short  StMuTrack::nHitsPoss() const {return mNHitsPoss;}
inline unsigned short  StMuTrack::nHitsDedx() const {return mNHitsDedx;}
inline unsigned short  StMuTrack::nHitsFit() const {return mNHitsFit;}
inline double StMuTrack::pidProbElectron() const {return unPack(mPidProbElectron,__PROB_SCALE__);}
inline double StMuTrack::pidProbPion() const     {return unPack(mPidProbPion,    __PROB_SCALE__);}
inline double StMuTrack::pidProbKaon() const     {return unPack(mPidProbKaon,    __PROB_SCALE__);}
inline double StMuTrack::pidProbProton() const   {return unPack(mPidProbProton,  __PROB_SCALE__);}
inline double StMuTrack::nSigmaElectron() const  {return unPack(mNSigmaElectron, __SIGMA_SCALE__);}
inline double StMuTrack::nSigmaPion() const      {return unPack(mNSigmaPion,     __SIGMA_SCALE__);}
inline double StMuTrack::nSigmaKaon() const      {return unPack(mNSigmaKaon,     __SIGMA_SCALE__);}
inline double StMuTrack::nSigmaProton() const    {return unPack(mNSigmaProton,   __SIGMA_SCALE__);}
inline double StMuTrack::dEdx() const {return mdEdx;}
inline double StMuTrack::chi2xy() const {return mChiSqXY;}
inline double StMuTrack::chi2z() const {return mChiSqZ;}
inline double StMuTrack::chi2() const {return mChiSqXY;}
inline double StMuTrack::chi2prob() const {return mChiSqZ;}
inline StTrackTopologyMap StMuTrack::topologyMap() const {return mTopologyMap;}
inline short StMuTrack::charge() const {return mHelix.q();}
inline double StMuTrack::pt() const {return mPt;}
inline double StMuTrack::eta() const {return mEta;}
inline double StMuTrack::phi() const {return mPhi;}
inline StThreeVectorF StMuTrack::p() const {return mP;}
inline StThreeVectorF StMuTrack::momentum() const {return mP;}
inline StThreeVectorF StMuTrack::dca() const {return mDCA;}
inline StThreeVectorF StMuTrack::dcaGlobal() const {return mDCAGlobal;}
inline StThreeVectorF StMuTrack::firstPoint() const {return mFirstPoint;}
inline StThreeVectorF StMuTrack::lastPoint() const {return mLastPoint;}
//!inline StPhysicalHelixD StMuTrack::helix() const {return mHelix;}
//!inline StPhysicalHelixD StMuTrack::outerHelix() const {return mOuterHelix;}
inline void StMuTrack::setProbabilityPidAlgorithm(StuProbabilityPidAlgorithm* p) { mProbabilityPidAlgorithm=p;}
inline void StMuTrack::setProbabilityPidCentrality(double cent) { mProbabilityPidCentrality = cent;}

inline StMuTrack* StMuTrack::globalTrack() const { return (mIndex2Global>=0) ? (StMuTrack*)StMuDst::array(muGlobal)->UncheckedAt(mIndex2Global) :0;}
inline StRichSpectra* StMuTrack::richSpectra() const { return (mIndex2RichSpectra>=0) ? (StRichSpectra*)StMuDst::array(muRich)->UncheckedAt(mIndex2RichSpectra) : 0;}


#endif

/***************************************************************************
 *
 * $Log: StMuTrack.h,v $
 * Revision 1.5  2002/09/19 21:54:01  laue
 * fix bug in length() method
 *
 * Revision 1.4  2002/08/20 19:55:49  laue
 * Doxygen comments added
 *
 * Revision 1.3  2002/04/01 22:42:30  laue
 * improved chain filter options
 *
 * Revision 1.2  2002/03/20 16:04:12  laue
 * minor changes, mostly added access functions
 *
 * Revision 1.1  2002/03/08 17:04:18  laue
 * initial revision
 *
 *
 **************************************************************************/
