/***************************************************************************
 *
 * $Id: StMuTrack.h,v 1.8 2004/04/14 03:27:30 jeromel Exp $
 * Author: Frank Laue, BNL, laue@bnl.gov
 *
 ***************************************************************************/

/** @class StMuTrack
 * Class holding the track properties
 * All unitis are in standard STAR units: GeV,cm
 * Please keep in mind, for primary tracks the first measured point is the primary vertex.
 * This affects dca, length calculations, inner helix.
*/


#ifndef StMuTrack_h
#define StMuTrack_h

#include "StMuDst.h"
#include "StMuEvent.h"
#include "StMuHelix.h"
#include "StMuUtilities.h"
#include "StMuProbPidTraits.h"


#include "StEvent/StTrackTopologyMap.h"
#include "StEvent/StRunInfo.h"
#include "StarClassLibrary/StPhysicalHelixD.hh"
#include "StarClassLibrary/StThreeVectorD.hh"
#include "StarClassLibrary/StThreeVectorF.hh"
#include "StarClassLibrary/SystemOfUnits.h"

#include "TObject.h"

#define __PROB_SCALE__  1000.
#define __SIGMA_SCALE__ 1000.
#define __NOVALUE__     -999

class StRichSpectra;
class StEvent;
class StTrack;


class StuProbabilityPidAlgorithm;


class StMuTrack : public TObject {
 public:
    StMuTrack(){/* no-op*/}; ///< default constructor
    StMuTrack(const StEvent*, const StTrack*, int index2Global=-2, int index2RichSpectra=-2, bool l3=false); ///< constructor from StEvent and StTrack
    short id() const; ///< Returns the track id(or key), is unique for a track node, i.e. global and primary tracks have the same id.
    short type() const; ///< Returns the track type: 0=global, 1=primary, etc (see StEvent manual for type information) 
    short flag() const; ///< Returns flag, (see StEvent manual for type information) 
    int   bad () const; // track is bad
    /// Returns index of associated global track. If not in order can be set with StMuDst::fixTrackIndeces() (but is taken care of in StMuDstReader.)  
    int index2Global() const;
    int index2RichSpectra() const; ///< Returns index of associated rich spectra.
    StMuTrack* globalTrack() const; ///< Returns pointer to associated global track. Null pointer if no global track available.
    StRichSpectra* richSpectra() const; ///< Returns pointer to associated rich spectra. Null pointer if no global track available.
    unsigned short nHits() const;      ///< Return number of hits on track.
    unsigned short  nHitsPoss() const; ///< Return number of possible hits on track.
    unsigned short  nHitsDedx() const; ///< Return number of hits used for dEdx. 
    unsigned short  nHitsFit() const;  ///< Return number of hits used in fit. 
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
    double chi2xy() const;   ///< Returns chi2 of fit.         ATTENTIONS: does not return xy chi2 (historic)        
    double chi2z() const;    ///< Returns probability of fit.  ATTENTIONS: does not return z chi2 (historic)        
    double pt() const;   ///< Returns pT at point of dca to primary vertex.
    double phi() const;  ///< Returns phi at point of dca to primary vertex.
    double eta() const;  ///< Returns pseudo rapidity at point of dca to primary vertex.
    double length() const; ///< Returns length of track (cm) from primary vertex to last measured point.
    double lengthMeasured() const;  ///< Returns length of track (cm) from first to last measured point.
    StTrackTopologyMap topologyMap() const; ///< Returns topology map.
    Short_t charge() const;  ///< Returns charge. 
    StThreeVectorF p() const; ///< Returns 3-momentum at dca to primary vertex.
    StThreeVectorF momentum() const; ///< Returns 3-momentum at dca to primary vertex.
    StThreeVectorF dca() const; ///< Returns 3D distance of closest approach to primary vertex.
    StThreeVectorF dcaGlobal() const; ///< Returns 3D distance of closest approach to primary vertex of associated global track.
    StThreeVectorF firstPoint() const; ///< Returns positions of first measured point.
    StThreeVectorF lastPoint() const; ///< Returns positions of last measured point.
    StPhysicalHelixD helix() const; ///< Returns inner helix (first measured point)
    StPhysicalHelixD outerHelix() const; ///< Returns outer helix (last measured point)
    StMuProbPidTraits probPidTraits() const; ///< Returns Yuri Fisyac new pid probabilities. 
    static void setProbabilityPidAlgorithm(StuProbabilityPidAlgorithm*); ///< Sets the StuProbabilityPidAlgorithm. Important in order to calculate Aihong's pids.
    static void setProbabilityPidCentrality(double cent); ///< Sets the centrality for calculating Aihong's pid.
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
  StMuProbPidTraits mProbPidTraits; ///< Class holding the new Yuri Fisyac pid probabilities.

  void setIndex2Global(size_t i) {mIndex2Global=i;} ///< Set index of associated global track.
  void setIndex2RichSpectra(size_t i) {mIndex2RichSpectra=i;} ///< Set index of associated rich spectra.
  StThreeVectorD dca(const StEvent*, const StTrack*); ///< Helper function: Calculates dca from a given StTrack and the primary vertex taken from StEvent
  StThreeVectorD momentumAtPrimaryVertex(const StEvent* event, const StTrack* track); ///< Helper function: Calculates the momentum at dca a given StTrack and the primary vertex taken from StEvent.
  void fillMuProbPidTraits(const StEvent*, const StTrack*); ///< Helper function to fill all the different pid values 
  static StuProbabilityPidAlgorithm* mProbabilityPidAlgorithm; ///< StuProbabilityPidAlgorithm, we will use the same algorithm for all tracks
  static double mProbabilityPidCentrality; ///< Centrality for Aihong's pid prob calculations. Will set when new StMuEvent is made from StEvent

  friend class StMuDst;
  ClassDef(StMuTrack,4)
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
inline StMuProbPidTraits StMuTrack::probPidTraits() const { return mProbPidTraits;} ///< Returns Yuri Fisyac new pid probabilities. 
inline void StMuTrack::setProbabilityPidAlgorithm(StuProbabilityPidAlgorithm* p) { mProbabilityPidAlgorithm=p;}
inline void StMuTrack::setProbabilityPidCentrality(double cent) { mProbabilityPidCentrality = cent;}

inline StMuTrack* StMuTrack::globalTrack() const { return (mIndex2Global>=0) ? (StMuTrack*)StMuDst::array(muGlobal)->UncheckedAt(mIndex2Global) :0;}
inline StRichSpectra* StMuTrack::richSpectra() const { return (mIndex2RichSpectra>=0) ? (StRichSpectra*)StMuDst::array(muRich)->UncheckedAt(mIndex2RichSpectra) : 0;}


#endif

/***************************************************************************
 *
 * $Log: StMuTrack.h,v $
 * Revision 1.8  2004/04/14 03:27:30  jeromel
 * Change init of mNSigma
 *
 * Revision 1.7  2003/10/28 18:57:56  perev
 * BadData protection added
 *
 * Revision 1.6  2002/11/18 14:29:32  laue
 * update for Yuri's new StProbPidTraits
 *
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
