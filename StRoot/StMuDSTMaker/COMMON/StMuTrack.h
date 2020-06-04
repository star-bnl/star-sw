/***************************************************************************
 *
 * $Id: StMuTrack.h,v 1.54 2019/02/21 14:00:02 jdb Exp $
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
#include "StMuBTofPidTraits.h" /// dongx
#include "StMuETofPidTraits.h" /// fseck
#include "StMuMtdPidTraits.h" /// Bingchu
#include "StMuPrimaryTrackCovariance.h"


#include "StEvent/StTrackTopologyMap.h"
#include "StEvent/StRunInfo.h"
#include "StEvent/StDcaGeometry.h"
#include "StarClassLibrary/StPhysicalHelixD.hh"
#include "StarClassLibrary/StThreeVectorD.hh"
#include "StarClassLibrary/StThreeVectorF.hh"
#include "StarClassLibrary/SystemOfUnits.h"

#include "TObject.h"
#include "TVector.h"

#define __PROB_SCALE__  1000.
#define __SIGMA_SCALE__ 1000.
#define __NOVALUE__     -999

class StRichSpectra;
class StEvent;
class StTrack;
class StVertex;
class StEmcGeom;
class StEmcPosition;
class StuProbabilityPidAlgorithm;

class TObjArray;

class StMuTrack : public TObject {
 public:
  StMuTrack(): mVertexIndex(0), mNHitsPossInner(0), mNHitsFitInner(0), mNHitsPossTpc(255), mNHitsFitTpc(255), mIndex2Cov(-1),
    mIdTruth(0), mQuality(0), mIdParentVx(0) {/* no-op*/}; ///< default constructor
    StMuTrack(const StEvent*, const StTrack*, const StVertex*, Int_t index2Global=-2, Int_t index2RichSpectra=-2, Bool_t l3=false, TObjArray *vtx_list=0); ///< constructor from StEvent and StTrack
    short id() const; ///< Returns the track id(or key), is unique for a track node, i.e. global and primary tracks have the same id.
    short type() const; ///< Returns the track type: 0=global, 1=primary, etc (see StEvent manual for type information)
    short flag() const; ///< Returns flag, (see StEvent manual for type information) 
    UInt_t                         flagExtension() const { return mFlagExtension; }
    Int_t   bad () const; // track is bad
    /// Returns index of associated global track. If not in order can be set with StMuDst::fixTrackIndeces() (but is taken care of in StMuDstReader.)
    Int_t index2Global() const;
    Int_t index2Cov() const;
    Int_t index2RichSpectra() const; ///< Returns index of associated rich spectra.
    Int_t index2BTofHit() const; /// dongx
    Int_t index2ETofHit() const;
    Int_t index2MtdHit() const; ///
    Int_t vertexIndex() const; ///< Returns index of associated primary vertex.
    void setVertexIndex(Int_t i) { mVertexIndex=i; } ///< Set index of primary vertex for which dca is stored
    const StMuTrack* globalTrack() const; ///< Returns pointer to associated global track. Null pointer if no global track available.
    const StMuTrack* primaryTrack() const; ///< Returns pointer to associated primary track. Null pointer if no global track available.
    const StRichSpectra* richSpectra() const; ///< Returns pointer to associated rich spectra. Null pointer if no global track available.
    const StMuBTofHit* tofHit() const;  /// dongx
    const StMuETofHit* etofHit() const;
    const StMuMtdHit* mtdHit() const;  /// Bingchu
    UShort_t nHits() const;     ///< Return total number of hits on track.
    UShort_t nHitsPoss() const; ///< Return number of possible hits on track.
    UShort_t nHitsPoss(StDetectorId) const; ///< Return number of possible hits on track.
    UShort_t nHitsDedx() const; ///< Return number of hits used for dEdx. 
    UShort_t nHitsFit() const;  ///< Return total number of hits used in fit. 
    UShort_t nHitsFit(StDetectorId) const;  ///< Return number of hits used in fit. 
    Double_t pidProbElectron() const; ///< Returns Aihong's probability of being an electron.
    Double_t pidProbPion() const;     ///< Returns Aihong's probability of being a pion.
    Double_t pidProbKaon() const;     ///< Returns Aihong's probability of being a kaon.
    Double_t pidProbProton() const;   ///< Returns Aihong's probability of being a proton.
    Double_t nSigmaElectron() const;  ///< Returns Craig's distance to the calculated dE/dx band for electrons in units of sigma.
    Double_t nSigmaPion() const;      ///< Returns Craig's distance to the calculated dE/dx band for pions in units of sigma.
    Double_t nSigmaKaon() const;      ///< Returns Craig's distance to the calculated dE/dx band for kaons in units of sigma.
    Double_t nSigmaProton() const;    ///< Returns Craig's distance to the calculated dE/dx band for protons in units of sigma.
    Double_t dEdxPull(Double_t mass, Bool_t fit = kFALSE, Int_t charge=1) const;
    Double_t dEdxPullElectronI70() const {return dEdxPull(0.51099907e-3);}
    Double_t dEdxPullPionI70()     const {return dEdxPull(0.13956995);}
    Double_t dEdxPullKaonI70()     const {return dEdxPull(0.493677);}
    Double_t dEdxPullProtonI70()   const {return dEdxPull(0.93827231);}
    Double_t dEdxPullElectronFit() const {return dEdxPull(0.51099907e-3,kTRUE);}
    Double_t dEdxPullPionFit()     const {return dEdxPull(0.13956995,kTRUE);}
    Double_t dEdxPullKaonFit()     const {return dEdxPull(0.493677,kTRUE);}
    Double_t dEdxPullProtonFit()   const {return dEdxPull(0.93827231,kTRUE);}
    Double_t nSigmaElectron70()    const {return dEdxPullElectronI70() ;}
    Double_t nSigmaPion70()        const {return dEdxPullPionI70()     ;}
    Double_t nSigmaKaon70()        const {return dEdxPullKaonI70()     ;}
    Double_t nSigmaProton70()      const {return dEdxPullProtonI70()   ;}
    Double_t nSigmaElectronFit()   const {return dEdxPullElectronFit() ;}
    Double_t nSigmaPionFit()       const {return dEdxPullPionFit()     ;}
    Double_t nSigmaKaonFit()       const {return dEdxPullKaonFit()     ;}
    Double_t nSigmaProtonFit()     const {return dEdxPullProtonFit()   ;}
    Double_t dEdx() const; ///< Returns measured dE/dx value.
    Double_t chi2() const;     ///< Returns chi2 of fit.
    Double_t chi2prob() const; ///< Returns probability of fit.
    Double_t chi2xy() const;   ///< Returns chi2 of fit.         ATTENTIONS: does not return xy chi2 (historic)
    Double_t chi2z() const;    ///< Returns probability of fit.  ATTENTIONS: does not return z chi2 (historic)
    Double_t pt() const;   ///< Returns pT at point of dca to primary vertex.
    Double_t phi() const;  ///< Returns phi at point of dca to primary vertex.
    Double_t eta() const;  ///< Returns pseudo rapidity at point of dca to primary vertex.
    Double_t length() const; ///< Returns length of track (cm) from primary vertex to last measured point.
    Double_t lengthMeasured() const;  ///< Returns length of track (cm) from first to last measured point.
    StTrackTopologyMap topologyMap() const; ///< Returns topology map.
    Short_t charge() const;  ///< Returns charge. 
    const StThreeVectorF &p() const; ///< Returns 3-momentum at dca to primary vertex.
    const StThreeVectorF &momentum() const; ///< Returns 3-momentum at dca to primary vertex.
    StThreeVectorF dca(Int_t vtx_id=-1) const; ///< Returns 3D distance of closest approach to primary vertex.
    StThreeVectorF dcaGlobal(Int_t vtx_id=-1) const; ///< Returns 3D distance of closest approach to primary vertex of associated global track.
    Float_t dcaD(Int_t vtx_id=-1) const; ///< Signed radial component of global DCA (projected)
    Float_t dcaZ(Int_t vtx_id=-1) const; ///< Z component of global DCA
    const StThreeVectorF &firstPoint() const; ///< Returns positions of first measured point.
    const StThreeVectorF &lastPoint() const; ///< Returns positions of last measured point.
    StPhysicalHelixD helix() const;       ///< Returns inner helix (first measured point)
    StPhysicalHelixD outerHelix() const;  ///< Returns outer helix (last measured point)
    const StMuHelix &muHelix()      const {return mHelix;}      ///< Returns inner helix (first measured point)
    const StMuHelix &muOuterHelix() const {return mOuterHelix;} ///< Returns outer helix (last measured point)
    const StMuProbPidTraits &probPidTraits() const; ///< Returns Yuri Fisyak new pid probabilities. 
    const StMuBTofPidTraits &btofPidTraits() const; /// dongx
    const StMuETofPidTraits &etofPidTraits() const;
    const StMuMtdPidTraits &mtdPidTraits() const; ///
    void setBTofPidTraits(const StMuBTofPidTraits& pid); /// dongx
    void setETofPidTraits(const StMuETofPidTraits& pid);
    void setMtdPidTraits(const StMuMtdPidTraits& pid); ///
    static void setProbabilityPidAlgorithm(StuProbabilityPidAlgorithm*); ///< Sets the StuProbabilityPidAlgorithm. Important in order to calculate Aihong's pids.
    static void setProbabilityPidCentrality(Double_t cent); ///< Sets the centrality for calculating Aihong's pid.
    virtual void Print(Option_t* option = "") const;  ///< Print track info
    void setIndex2BTofHit(Int_t i) {mIndex2BTofHit=i;} /// dongx
    void setIndex2ETofHit(Int_t i) {mIndex2ETofHit=i;}
    void setIndex2MtdHit(Int_t i) {mIndex2MtdHit=i;} /// Bingchu
    void setIndex2Cov(Int_t i) {mIndex2Cov=i;}    ///< Set index of associated DCA geoemtry for the global track.

    //Matching to BEMC related functions
    TArrayI getTower(Bool_t useExitRadius=false, Int_t det=1) const; //Get Tower track is pointing too -  1=BTOW, 3=BSMDE, 4=BSMDP //1=BTOW, 3=BSMDE, 4=BSMDP Returns TVector tower. tower[0] is module, tower[1] is eta, tower[2] is sub, and tower[3] is id
    Double_t energyBEMC() const;
    Bool_t matchBEMC() const;
    Int_t            idTruth() const { return mIdTruth;}
    Int_t            qaTruth() const { return mQuality; }
    Int_t           idParentVx() const {return mIdParentVx;}
    void            setType(short type) { mType = type;}
    void            setIdTruth(Int_t idtru,Int_t qatru=0) {mIdTruth = (UShort_t) idtru; mQuality = (UShort_t) qatru;}
    void         setIdParentVx(Int_t Id) {mIdParentVx = Id;}

protected:
  Int_t mId;
  Short_t mType;
  Short_t mFlag;
  UInt_t  mFlagExtension; // bit wise fast detector matching status
  Int_t mIndex2Global;
  Int_t mIndex2RichSpectra;
  Int_t mIndex2BTofHit;     // dongx
  Int_t mIndex2MtdHit;
  Int_t mVertexIndex;       // Primary vertex id for this track's dca
  UChar_t mNHits;           // Total number of points (was (F)tpc only)
  UChar_t mNHitsPoss;       // Total possible points (was (F)tpc only)
  UChar_t mNHitsDedx;       
  UChar_t mNHitsFit;        // Total fitted points (was (F)tpc only)
  UChar_t mNHitsPossInner;  // Svt/Ist (3 bit) and Ssd (2 bit) possible hits, Pixel (3 bits)
  UChar_t mNHitsFitInner;   // Svt/Ist (3 bit) and Ssd (2 bit) fitted hits, Pixel (3 bits)
  UChar_t mNHitsPossTpc;    // Possible (F)tpc hits (Ftpc flagged with first 2 bits)
  UChar_t mNHitsFitTpc;     // Fitted (F)tpc hits (Ftpc flagged with first 2 bits)
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
  StMuProbPidTraits mProbPidTraits; ///< Class holding the new Yuri Fisyak pid probabilities.
  StMuBTofPidTraits mBTofPidTraits; /// dongx
  StMuMtdPidTraits  mMtdPidTraits; /// Bingchu
  Int_t mIndex2Cov;
  Int_t mIndex2ETofHit;
  StMuETofPidTraits mETofPidTraits;
  // IdTruth
  UShort_t         mIdTruth; // MC track id if any 
  UShort_t         mQuality; // quality of this information (percentage of hits coming the above MC track)
  Int_t         mIdParentVx;
  void setIndex2Global(Int_t i) {mIndex2Global=i;} ///< Set index of associated global track.
  void setIndex2RichSpectra(Int_t i) {mIndex2RichSpectra=i;} ///< Set index of associated rich spectra.
  StThreeVectorF dca(const StThreeVectorF &pos) const; ///< Calculate dca to a given point
  StThreeVectorD dca(const StTrack*, const StVertex *vertex) const; ///< Helper function: Calculates dca from a given StTrack and the primary vertex taken from StEvent
  StThreeVectorD momentumAtPrimaryVertex(const StEvent *event, const StTrack* track, const StVertex *vertex) const; ///< Helper function: Calculates the momentum at dca a given StTrack and the primary vertex taken from StEvent.
  void fillMuProbPidTraits(const StEvent*, const StTrack*); ///< Helper function to fill all the different pid values 
  void fillMuBTofPidTraits(const StTrack*); /// dongx
  void fillMuETofPidTraits(const StTrack*);
  void fillMuMtdPidTraits(const StTrack*); /// Bingchu
  static StuProbabilityPidAlgorithm* mProbabilityPidAlgorithm; ///< StuProbabilityPidAlgorithm, we will use the same algorithm for all tracks
  static Double_t mProbabilityPidCentrality; ///< Centrality for Aihong's pid prob calculations. Will set when new StMuEvent is made from StEvent
  friend class StMuDst;
  friend class StMuDstFilterMaker;
  friend class StMuMomentumShiftMaker;
  ClassDef(StMuTrack,16)
};

inline short StMuTrack::id() const {return mId;}
inline short StMuTrack::type() const {return mType;}
inline short StMuTrack::flag() const {return mFlag;}
inline Int_t StMuTrack::index2Global() const {return mIndex2Global;}
inline Int_t StMuTrack::index2Cov() const {return mIndex2Cov;}
inline Int_t StMuTrack::index2RichSpectra() const {return mIndex2RichSpectra;}
inline Int_t StMuTrack::index2BTofHit() const {return mIndex2BTofHit;}  /// dongx
inline Int_t StMuTrack::index2ETofHit() const {return mIndex2ETofHit;}
inline Int_t StMuTrack::index2MtdHit() const {return mIndex2MtdHit;}  ///
inline UShort_t StMuTrack::nHits() const {return mNHits;}
inline UShort_t  StMuTrack::nHitsDedx() const {return mNHitsDedx;}
inline UShort_t  StMuTrack::nHitsFit() const {return mNHitsFit;}
inline Double_t StMuTrack::pidProbElectron() const {return unPack(mPidProbElectron,__PROB_SCALE__);}
inline Double_t StMuTrack::pidProbPion() const     {return unPack(mPidProbPion,    __PROB_SCALE__);}
inline Double_t StMuTrack::pidProbKaon() const     {return unPack(mPidProbKaon,    __PROB_SCALE__);}
inline Double_t StMuTrack::pidProbProton() const   {return unPack(mPidProbProton,  __PROB_SCALE__);}
inline Double_t StMuTrack::nSigmaElectron() const  {return unPack(mNSigmaElectron, __SIGMA_SCALE__);}
inline Double_t StMuTrack::nSigmaPion() const      {return unPack(mNSigmaPion,     __SIGMA_SCALE__);}
inline Double_t StMuTrack::nSigmaKaon() const      {return unPack(mNSigmaKaon,     __SIGMA_SCALE__);}
inline Double_t StMuTrack::nSigmaProton() const    {return unPack(mNSigmaProton,   __SIGMA_SCALE__);}
inline Double_t StMuTrack::dEdx() const {return mdEdx;}
inline Double_t StMuTrack::chi2xy() const {return mChiSqXY;}
inline Double_t StMuTrack::chi2z() const {return mChiSqZ;}
inline Double_t StMuTrack::chi2() const {return mChiSqXY;}
inline Double_t StMuTrack::chi2prob() const {return mChiSqZ;}
inline void     StMuTrack::setMtdPidTraits(const StMuMtdPidTraits& pid) { mMtdPidTraits = pid; }
inline StTrackTopologyMap StMuTrack::topologyMap() const {return mTopologyMap;}
inline short StMuTrack::charge() const {return mHelix.q();}
inline Double_t StMuTrack::pt() const {return mPt;}
inline Double_t StMuTrack::eta() const {return mEta;}
inline Double_t StMuTrack::phi() const {return mPhi;}
inline const StThreeVectorF &StMuTrack::p() const {return mP;}
inline const StThreeVectorF &StMuTrack::momentum() const {return mP;}
inline const StThreeVectorF &StMuTrack::firstPoint() const {return mFirstPoint;}
inline const StThreeVectorF &StMuTrack::lastPoint() const {return mLastPoint;}
inline const StMuProbPidTraits &StMuTrack::probPidTraits() const { return mProbPidTraits;} ///< Returns Yuri Fisyak new pid probabilities. 
inline const StMuBTofPidTraits &StMuTrack::btofPidTraits() const { return mBTofPidTraits;} /// dongx
inline const StMuETofPidTraits &StMuTrack::etofPidTraits() const { return mETofPidTraits;}
inline const StMuMtdPidTraits &StMuTrack::mtdPidTraits() const { return mMtdPidTraits;} /// Bingchu
inline void StMuTrack::setProbabilityPidAlgorithm(StuProbabilityPidAlgorithm* p) { mProbabilityPidAlgorithm=p;}
inline void StMuTrack::setProbabilityPidCentrality(Double_t cent) { mProbabilityPidCentrality = cent;}
inline void StMuTrack::setBTofPidTraits(const StMuBTofPidTraits& pid) { mBTofPidTraits = pid; }
inline void StMuTrack::setETofPidTraits(const StMuETofPidTraits& pid) { mETofPidTraits = pid; }

inline const StMuTrack* StMuTrack::globalTrack() const { return (mIndex2Global>=0) ? (StMuTrack*)StMuDst::array(muGlobal)->UncheckedAt(mIndex2Global) :0;}
inline const StRichSpectra* StMuTrack::richSpectra() const { return (mIndex2RichSpectra>=0) ? (StRichSpectra*)StMuDst::array(muRich)->UncheckedAt(mIndex2RichSpectra) : 0;}
inline const StMuBTofHit* StMuTrack::tofHit() const { return (mIndex2BTofHit>=0) ? (StMuBTofHit*)StMuDst::btofArray(muBTofHit)->UncheckedAt(mIndex2BTofHit) :0;} /// dongx
inline const StMuETofHit* StMuTrack::etofHit() const { return (mIndex2ETofHit>=0) ? (StMuETofHit*)StMuDst::etofArray(muETofHit)->UncheckedAt(mIndex2ETofHit) :0;}
inline const StMuMtdHit* StMuTrack::mtdHit() const { return (mIndex2MtdHit>=0) ? (StMuMtdHit*)StMuDst::mtdArray(muMTDHit)->UncheckedAt(mIndex2MtdHit) :0;} ///
ostream&              operator<<(ostream& os, StMuTrack const & v);
#endif

/***************************************************************************
 *
 * $Log: StMuTrack.h,v $
 * Revision 1.54  2019/02/21 14:00:02  jdb
 * Bumped the ClassDef versions in MuDst where eTOF was added. I also added the etofTypes to the LinkDef file
 *
 * Revision 1.53  2019/02/21 13:32:54  jdb
 * Inclusion of ETOF MuDst code. This code adds support for the full set of ETOF data which includes EtofDigi, EtofHit, EtofHeader. The code essentially copies similar structures from StEvent and additionally rebuilds the maps between Digis and Hits. Accessor methods are added based on the pattern from BTOF to provide access to data at various levels. The code for accessing the PID traits provided by ETOF is also provided
 *
 * Revision 1.52  2017/04/17 19:19:51  smirnovd
 * StMuTrack: Make setVertexIndex() public
 *
 * Associated vertex can change when running a vertex finder as an afterburner.
 *
 * Revision 1.51  2017/04/17 19:19:44  smirnovd
 * [Cosmetic] Whitespace adjustments
 *
 * Revision 1.50  2017/01/19 23:03:04  smirnovd
 * StMuTrack: Let users change track type e.g. global/primary/etc...
 *
 * Revision 1.49  2014/02/28 05:40:09  jdb
 * jdb StMuTrack: Added setMtdPidTraits, StMuMtdPidTraits: removed mtdHit(), MtdHit(), setMtdHit()
 *
 * Revision 1.48  2014/01/15 22:00:04  fisyak
 * Add method to calculate dE/dx pulls for I70 and Ifit
 *
 * Revision 1.4  2014/01/15 21:11:08  fisyak
 * Add dE/dx pulls calculations
 *
 * Revision 1.3  2013/08/19 15:03:16  fisyak
 * Add calculation dE/dx pulls
 *
 * Revision 1.2  2013/08/07 12:58:40  fisyak
 * Add access to StMuHelix
 *
 * Revision 1.1.1.1  2013/07/23 14:14:48  fisyak
 *
 *
 * Revision 1.44  2013/07/16 14:30:30  fisyak
 * Restore mass fit tracks
 *
 * Revision 1.42  2013/04/08 18:07:55  fisyak
 * Add branches for KFParticles, fix problem with zero cov. matrix for primary tracks
 *
 * Revision 1.41  2012/05/07 14:47:06  fisyak
 * Add handles for track to fast detector matching
 *
 * Revision 1.40  2011/10/17 00:19:14  fisyak
 * Active handing of IdTruth
 *
 * Revision 1.39  2011/10/11 20:35:43  fisyak
 * Make idTruth public
 *
 * Revision 1.38  2011/04/08 01:25:51  fisyak
 * Add branches for MC track and vertex information, add IdTruth to  tracks and vertices, reserve a possiblity to remove Strange MuDst
 *
 * Revision 1.37  2010/04/27 20:47:17  tone421
 * Added extra functions for BEMC matching. See this post for more details:
 *
 * http://www.star.bnl.gov/HyperNews-star/get/starsofi/7816.html
 *
 * Revision 1.36  2009/12/08 23:24:46  fine
 * Fix issue  #1748 http://www.star.bnl.gov/rt2/Ticket/Display.html?id=1748
 *
 * Revision 1.35  2009/12/01 21:56:35  tone421
 * Implemented changes as per http://www.star.bnl.gov/rt2/Ticket/Display.html?id=1734
 *
 * Revision 1.31  2009/09/01 16:37:11  tone421
 * Fixed in a bug for setBTofPidTraits
 *
 * Revision 1.30  2009/02/27 19:54:25  tone421
 * Iterated Class Def by 1 to accommodate new BTOF data member
 *
 * Revision 1.29  2009/02/20 16:37:44  tone421
 * *** empty log message ***
 *
 * Revision 1.27  2008/03/19 17:22:39  fisyak
 * Increase Version number
 *
 * Revision 1.26  2008/03/19 14:51:04  fisyak
 * Add two clone arrays for global and primary track covariance matrices, remove mSigmaDcaD and mSigmaDcaZ
 *
 * Revision 1.25  2007/10/18 03:44:24  mvl
 * Added Ist and Pixel hits to mNPossInner and mNFitInner
 *
 * Revision 1.24  2007/09/18 02:29:58  mvl
 * Added basic printing functionality. For convenience and to assist data consistency checks
 *
 * Revision 1.23  2007/04/20 06:23:46  mvl
 * Removed Q-vector flag (will implement utility class)
 *
 * Revision 1.21  2007/01/29 18:34:44  mvl
 * Updates to use StDcaGeometry for global DCA and momentum.
 * Added helper functions for radial and Z component: dcaD and dcaZ.
 * Uncertainties on those are stored in sigmaDcaD and sigmaDcaZ.
 * dcaD and dcaZ only work for the primary vertex to which the track belongs (avoid long extrapolations).
 *
 * Revision 1.20  2006/07/27 18:55:42  fisyak
 * Remove DCA hack used in SSD+SVT test production (P06id)
 *
 * Revision 1.17  2005/12/13 03:12:13  mvl
 * Changes to StMuDst2StEventMaker (code in StMuDst) and StMuDstFilterMaker
 * to no longer rely on track keys for matching global and primary tracks.
 * This was needed because track keys are not guaranteed to be unique anymore.
 *
 * Revision 1.16  2005/08/19 19:46:06  mvl
 * Further updates for multiple vertices. The main changes are:
 * 1) StMudst::primaryTracks() now returns a list (TObjArray*) of tracks
 *    belonging to the 'current' primary vertex. The index number of the
 *    'current' vertex can be set using StMuDst::setCurrentVertex().
 *    This also affects StMuDst::primaryTracks(Int_t i) and
 *    StMuDst::numberOfprimaryTracks().
 * 2) refMult is now stored for all vertices, in StMuPrimaryVertex. The
 *    obvious way to access these numbers is from the StMuprimaryVertex structures,
 *    but for ebakcward compatibility a function is provided in StMuEvent as well
 *    (this is the only function taht works for existing MuDst)
 *
 * As an aside, I've also changes the internals of StMuDst::createStEvent and
 * StMuDst::fixTrackIndices() to be able to deal with a larger range of index numbers for tracks as generated by Ittf.
 *
 * BIG FAT WARNING: StMudst2StEventMaker and StMuDstFilterMaker
 * do not fully support the multiple vertex functionality yet.
 *
 * Revision 1.15  2005/07/15 21:45:08  mvl
 * Added support for multiple primary vertices (StMuPrimaryVertex). Track Dcas are now calculated with repect to the first vertex in the list (highest rank), but another vertex number can be specified. Tarcks also store the index of the vertex they belong to (StMuTrack::vertexIndex())
 *
 * Revision 1.14  2005/07/06 21:40:18  fisyak
 * use template version of StPhysicalHelixD
 *
 * Revision 1.13  2005/03/17 21:55:00  mvl
 * Added StMuMomentumShiftMaker for applying a magnetic field scaling to the reconstructed MuDst. This class accesses StMuTrack, StMuEvent and StMuHelix and some Strangeness MuDst data members as 'friend'
 *
 * Revision 1.12  2004/10/22 23:44:16  mvl
 * Fixed StMuDst::fixTrackIndices()
 *
 * Revision 1.11  2004/08/14 00:53:42  mvl
 * Added 1 to possibel points for primary tracks, like in StEvent
 *
 * Revision 1.10  2004/08/07 02:44:06  mvl
 * Added support for fitted and possible points in different detectors, for ITTF
 *
 * Revision 1.9  2004/05/02 04:10:14  perev
 * private => protected
 *
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
