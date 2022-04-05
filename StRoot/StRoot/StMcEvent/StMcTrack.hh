/*!
 * \class StMcTrack
 * \brief Monte Carlo Track class
 * \author Manuel Calderon de la Barca Sanchez
 * \date   July 1999
 *
 ***************************************************************************
 *
 * $Id: StMcTrack.hh,v 2.30 2018/03/15 22:00:35 smirnovd Exp $
 *
 ***************************************************************************
 *
 * $Log: StMcTrack.hh,v $
 * Revision 2.30  2018/03/15 22:00:35  smirnovd
 * Fix linker error by removing declared but undefined functions
 *
 * Revision 2.29  2013/03/25 23:47:08  perev
 * Mustafa.Pxl corrs
 *
 * Revision 2.28  2012/03/22 01:03:21  perev
 * Etr add
 *
 * Revision 2.27  2011/10/11 01:22:24  perev
 * Not used anymore or ever
 *
 * Revision 2.26  2011/07/20 17:36:09  perev
 * Fsc added
 *
 * Revision 2.25  2011/01/26 19:46:24  perev
 * FPD ==> STAR Soft
 *
 * Revision 2.24  2007/10/16 19:49:20  fisyak
 * rename Hft => Pxl, remove Hpd, Igt and Fst
 *
 * Revision 2.23  2006/09/25 14:20:43  fisyak
 * Add Hpd Hits
 *
 * Revision 2.21  2005/11/22 21:44:52  fisyak
 * Add compress Print for McEvent, add Ssd collections
 *
 * Revision 2.20  2005/09/28 21:30:15  fisyak
 * Persistent StMcEvent
 *
 * Revision 2.19  2005/07/07 18:20:49  calderon
 * Added support for IGT detector.
 *
 * Revision 2.18  2005/05/27 23:37:25  calderon
 * Update for EEMC, add eprs, esmdu esdmv hits to StMcEvent.
 *
 * Revision 2.17  2005/04/18 20:11:33  calderon
 * Addition of Fgt and Fst files.  Modified other files to accomodate changes.
 *
 * Revision 2.16  2005/01/27 23:40:49  calderon
 * Adding persistency to StMcEvent as a step for Virtual MonteCarlo.
 *
 * Revision 2.15  2004/09/14 05:00:30  calderon
 * Added support for Ist, Ssd and changes to Pixel, from "El Kai".
 *
 * Revision 2.14  2004/01/13 21:03:34  fisyak
 * Replace iostream by Stiostream.h (for icc)
 *
 * Revision 2.13  2003/12/04 05:56:47  calderon
 * Inclusion of Endcap EMC hit collection in StMcEvent and
 * of the Endcap hit vector in StMcTrack.
 * fix const of StMcVertex::parent() to avoid warnings in user code
 *
 * Revision 2.12  2003/10/08 20:17:55  calderon
 * -using <iostream>, std::cout, std::ostream.
 * -changes in FTPC volume Id.
 *   o Causes changes in decoding of plane().
 *   o sector() is added.
 *   o print volumeId and sector() in the operator<<.
 *
 * Revision 2.11  2003/09/02 17:58:41  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 2.10  2003/08/20 18:50:21  calderon
 * Addition of Tof classes and Pixel classes.  Modified track, event, and
 * container code to reflect this.
 * Fix bug in StMcVertex and in clearing of some hit collections.
 *
 * Revision 2.9  2003/02/19 03:16:05  calderon
 * Introduction of Ctb Hit Class and Ctb Hit Collection class, modified
 * StMcTrack, and StMcEvent accordingly.  Clearing of hits in StMcSvtWaferHitCollection.
 *
 * Revision 2.8  2000/06/06 02:58:42  calderon
 * Introduction of Calorimeter classes.  Modified several classes
 * accordingly.
 *
 * Revision 2.7  2000/04/06 23:29:10  calderon
 * Store the parent track for all tracks.
 *
 * Revision 2.6  2000/04/06 08:34:56  calderon
 * Version using the particle table:
 * 1) Constructor for particle_st*
 * 2) Pointer to parent track from particle table
 * 3) PDG encoding when track is from particle table
 * 4) Generator label, used to index entries in the table for debugging
 *
 * Revision 2.5  2000/04/04 22:25:24  calderon
 * add inline function to return primary key from g2t table
 *
 * Revision 2.4  2000/03/06 18:05:23  calderon
 * 1) Modified SVT Hits storage scheme from layer-ladder-wafer to
 * barrel-ladder-wafer.
 * 2) Added Rich Hit class and collection, and links to them in other
 * classes.
 *
 * Revision 2.3  2000/01/18 20:52:31  calderon
 * Works with CC5
 *
 * Revision 2.2  1999/12/03 00:51:53  calderon
 * Tested with new StMcEventMaker.  Added messages for
 * diagnostics.
 *
 * Revision 2.1  1999/11/19 19:06:34  calderon
 * Recommit after redoing the files.
 *
 * Revision 2.0  1999/11/17 02:12:16  calderon
 * Completely revised for new StEvent
 *
 * Revision 1.4  1999/09/24 01:23:17  fisyak
 * Reduced Include Path
 *
 * Revision 1.3  1999/09/23 21:25:54  calderon
 * Added Log & Id
 * Modified includes according to Yuri
 *
 *
 **************************************************************************/
#ifndef StMcTrack_hh
#define StMcTrack_hh 
#include "Stiostream.h"
#include "StLorentzVectorF.hh"
#include "StMcContainers.hh"
#include "StObject.h"
#include "StEnumerations.h"
class StParticleDefinition;
class g2t_track_st;
class particle_st;
class StMcTrack;
ostream&  operator<<(ostream& os, const StMcTrack&);

class StMcTrack : public StObject {
public:
    StMcTrack();
    virtual ~StMcTrack();
    StMcTrack(g2t_track_st* trk);
    StMcTrack(particle_st* trk);
    
    // StMcTrack(const StMcTrack&);                     use default
    // const StMcTrack & operator=(const StMcTrack&);   use default
    
    int operator==(const StMcTrack&) const;
    int operator!=(const StMcTrack&) const;

    void initToZero();
  // "Get" Methods
  const StLorentzVectorF& fourMomentum() const { return mFourMomentum; }
  const StThreeVectorF& momentum() const { return mFourMomentum.vect(); }
  float energy() const { return mFourMomentum.e(); }
  float pt() const { return mFourMomentum.perp(); }
  float rapidity() const { return mFourMomentum.rapidity(); }
  float pseudoRapidity() const { return mFourMomentum.pseudoRapidity(); }
  StMcVertex* startVertex() { return mStartVertex; }
  const StMcVertex* startVertex() const { return mStartVertex; }
  StMcVertex* stopVertex() { return mStopVertex; }
  const StMcVertex* stopVertex() const { return mStopVertex; }
  StMcTrack* parent() { return mParent; }
  const StMcTrack* parent() const { return mParent; }
  StPtrVecMcVertex& intermediateVertices() { return mIntermediateVertices; }
  const StPtrVecMcVertex& intermediateVertices() const { return mIntermediateVertices; }
  StPtrVecMcTpcHit& tpcHits() { return mTpcHits; }
  const StPtrVecMcTpcHit& tpcHits() const { return mTpcHits; }
  StPtrVecMcSvtHit& svtHits() { return mSvtHits; }
  const StPtrVecMcSvtHit& svtHits() const { return mSvtHits; }
  StPtrVecMcSsdHit& ssdHits() { return mSsdHits; }
  const StPtrVecMcSsdHit& ssdHits() const { return mSsdHits; }
  StPtrVecMcFtpcHit& ftpcHits() { return mFtpcHits; }
  const StPtrVecMcFtpcHit& ftpcHits() const { return mFtpcHits; }
  StPtrVecMcRichHit& richHits() { return mRichHits; }
  const StPtrVecMcRichHit& richHits() const { return mRichHits; }
  StPtrVecMcCtbHit& ctbHits() { return mCtbHits; }
  const StPtrVecMcCtbHit& ctbHits() const { return mCtbHits; }
  StPtrVecMcCalorimeterHit& bemcHits() { return mBemcHits; }
  const StPtrVecMcCalorimeterHit& bemcHits() const { return mBemcHits; }
  StPtrVecMcCalorimeterHit& bprsHits() { return mBprsHits; }
  const StPtrVecMcCalorimeterHit& bprsHits() const { return mBprsHits; }
  StPtrVecMcCalorimeterHit& bsmdeHits() { return mBsmdeHits; }
  const StPtrVecMcCalorimeterHit& bsmdeHits() const { return mBsmdeHits; }
  StPtrVecMcCalorimeterHit& bsmdpHits() { return mBsmdpHits; }
  const StPtrVecMcCalorimeterHit& bsmdpHits() const { return mBsmdpHits; }
  StPtrVecMcTofHit& tofHits() { return mTofHits; }
  const StPtrVecMcTofHit& tofHits() const { return mTofHits; }
  StPtrVecMcMtdHit& mtdHits() { return mMtdHits; }
  const StPtrVecMcMtdHit& mtdHits() const { return mMtdHits; }
  StPtrVecMcCalorimeterHit& eemcHits() { return mEemcHits; }
  const StPtrVecMcCalorimeterHit& eemcHits() const { return mEemcHits; }
  StPtrVecMcCalorimeterHit& eprsHits() { return mEprsHits; }
  const StPtrVecMcCalorimeterHit& eprsHits() const { return mEprsHits; }
  StPtrVecMcCalorimeterHit& esmduHits() { return mEsmduHits; }
  const StPtrVecMcCalorimeterHit& esmduHits() const { return mEsmduHits; }
  StPtrVecMcCalorimeterHit& esmdvHits() { return mEsmdvHits; }
  const StPtrVecMcCalorimeterHit& esmdvHits() const { return mEsmdvHits; }
  StPtrVecMcCalorimeterHit& fpdHits() { return mFpdHits; }
  const StPtrVecMcCalorimeterHit& fpdHits() const { return mFpdHits; }
  StPtrVecMcCalorimeterHit& fscHits() { return mFscHits; }
  const StPtrVecMcCalorimeterHit& fscHits() const { return mFscHits; }
  StPtrVecMcPxlHit& pxlHits() { return mPxlHits; }
  const StPtrVecMcPxlHit& pxlHits() const { return mPxlHits; }
  StPtrVecMcIstHit& istHits() { return mIstHits; }
  const StPtrVecMcIstHit& istHits() const { return mIstHits; }
  StPtrVecMcFgtHit& fgtHits() { return mFgtHits; }
  const StPtrVecMcFgtHit& fgtHits() const { return mFgtHits; }
  StPtrVecMcEtrHit& etrHits() { return mEtrHits; }
  const StPtrVecMcEtrHit& etrHits() const { return mEtrHits; }
  StParticleDefinition* particleDefinition();
  const StParticleDefinition* particleDefinition() const { return mParticleDefinition; }
  const StPtrVecMcHit *Hits(StDetectorId Id) const;
  const StPtrVecMcCalorimeterHit *CalorimeterHits(StDetectorId Id) const;
  int isShower() const { return mIsShower; }
  long geantId() const { return mGeantId; }
  long pdgId() const { return mPdgId; }
  long key() const { return mKey; }
  long eventGenLabel() const { return mEventGenLabel; }

  // "Set" Methods
    void setFourMomentum(const StLorentzVectorF&); 
    void setStartVertex(StMcVertex*); 
    void setStopVertex(StMcVertex*); 
    void setIntermediateVertices(StPtrVecMcVertex&); 
    void setTpcHits(StPtrVecMcTpcHit&); 
    void setSvtHits(StPtrVecMcSvtHit&); 
    void setSsdHits(StPtrVecMcSsdHit&); 
    void setFtpcHits(StPtrVecMcFtpcHit&); 
    void setRichHits(StPtrVecMcRichHit&); 
    void setCtbHits(StPtrVecMcCtbHit&); 
    void setBemcHits(StPtrVecMcCalorimeterHit&); 
    void setBprsHits(StPtrVecMcCalorimeterHit&); 
    void setBsmdeHits(StPtrVecMcCalorimeterHit&); 
    void setBsmdpHits(StPtrVecMcCalorimeterHit&); 
    void setTofHits(StPtrVecMcTofHit&); 
    void setMtdHits(StPtrVecMcMtdHit&); 
    void setEemcHits(StPtrVecMcCalorimeterHit&); 
    void setEprsHits(StPtrVecMcCalorimeterHit&); 
    void setEsmduHits(StPtrVecMcCalorimeterHit&); 
    void setEsmdvHits(StPtrVecMcCalorimeterHit&); 
    void setFscHits(StPtrVecMcCalorimeterHit&); 
    void setPxlHits(StPtrVecMcPxlHit&); 
    void setIstHits(StPtrVecMcIstHit&); 
    void setFgtHits(StPtrVecMcFgtHit&); 
    void setEtrHits(StPtrVecMcEtrHit&); 

    void setShower(char); 
    void setGeantId(long); 
    void setPdgId(long); 
    void setKey(long);     
    void setEventGenLabel(long);     
    void setParent(StMcTrack*);     
  void setPrimary(Bool_t val) {mIsPrimary = val;}

    void addTpcHit(StMcTpcHit*); 
    void addSvtHit(StMcSvtHit*); 
    void addSsdHit(StMcSsdHit*); 
    void addFtpcHit(StMcFtpcHit*); 
    void addRichHit(StMcRichHit*); 
    void addCtbHit(StMcCtbHit*); 
    void addBemcHit(StMcCalorimeterHit*); 
    void addBprsHit(StMcCalorimeterHit*); 
    void addBsmdeHit(StMcCalorimeterHit*); 
    void addBsmdpHit(StMcCalorimeterHit*); 
    void addTofHit(StMcTofHit*); 
    void addMtdHit(StMcMtdHit*); 
    void addEemcHit(StMcCalorimeterHit*); 
    void addEprsHit(StMcCalorimeterHit*); 
    void addEsmduHit(StMcCalorimeterHit*); 
    void addEsmdvHit(StMcCalorimeterHit*); 
    void addFpdHit(StMcCalorimeterHit*);
    void addFscHit(StMcCalorimeterHit*);
    void addPxlHit(StMcPxlHit*); 
    void addIstHit(StMcIstHit*);
    void addFgtHit(StMcFgtHit*); 
    void addEtrHit(StMcEtrHit*); 
    void removeTpcHit(StMcTpcHit*); 
    void removeSvtHit(StMcSvtHit*); 
    void removeSsdHit(StMcSsdHit*); 
    void removeFtpcHit(StMcFtpcHit*); 
    void removeRichHit(StMcRichHit*); 
    void removeCtbHit(StMcCtbHit*); 
    void removeCalorimeterHit(StPtrVecMcCalorimeterHit&, StMcCalorimeterHit*); 
    void removeBemcHit(StMcCalorimeterHit*); 
    void removeBprsHit(StMcCalorimeterHit*); 
    void removeBsmdeHit(StMcCalorimeterHit*); 
    void removeBsmdpHit(StMcCalorimeterHit*); 
    void removeTofHit(StMcTofHit*); 
    void removeMtdHit(StMcMtdHit*); 
    void removeEemcHit(StMcCalorimeterHit*); 
    void removeEprsHit(StMcCalorimeterHit*); 
    void removeEsmduHit(StMcCalorimeterHit*); 
    void removeEsmdvHit(StMcCalorimeterHit*); 
    void removePxlHit(StMcPxlHit*); 
    void removeIstHit(StMcIstHit*); 
    void removeFgtHit(StMcFgtHit*); 
    void removeEtrHit(StMcEtrHit*); 
  void Print(Option_t *option="") const;
  Bool_t IsPrimary() const {return mIsPrimary;}
    //    void setTopologyMap(StTrackTopologyMap&); 
    
protected:
    StLorentzVectorF         mFourMomentum;  
    StMcVertex*              mStartVertex; 
    StMcVertex*              mStopVertex; 
    StPtrVecMcVertex         mIntermediateVertices; 
    StPtrVecMcTpcHit         mTpcHits; 
    StPtrVecMcSvtHit         mSvtHits; 
    StPtrVecMcSsdHit         mSsdHits; 
    StPtrVecMcFtpcHit        mFtpcHits; 
    StPtrVecMcRichHit        mRichHits; 
    StPtrVecMcCtbHit         mCtbHits; 
    StPtrVecMcCalorimeterHit mBemcHits; 
    StPtrVecMcCalorimeterHit mBprsHits; 
    StPtrVecMcCalorimeterHit mBsmdeHits; 
    StPtrVecMcCalorimeterHit mBsmdpHits; 
    StPtrVecMcTofHit         mTofHits; 
    StPtrVecMcMtdHit         mMtdHits; 
    StPtrVecMcCalorimeterHit mEemcHits; 
    StPtrVecMcCalorimeterHit mEprsHits; 
    StPtrVecMcCalorimeterHit mEsmduHits; 
    StPtrVecMcCalorimeterHit mEsmdvHits; 
    StPtrVecMcCalorimeterHit mFpdHits; 
    StPtrVecMcCalorimeterHit mFscHits; 
    StPtrVecMcPxlHit         mPxlHits; 
    StPtrVecMcIstHit         mIstHits;
    StPtrVecMcFgtHit         mFgtHits; 
    StPtrVecMcEtrHit         mEtrHits; 
  StParticleDefinition*    mParticleDefinition; //!
    StMcTrack*               mParent;
    char                     mIsShower; 
    long                     mGeantId; 
    long                     mPdgId; 
    long                     mKey;     
    long                     mEventGenLabel; 
  Bool_t                   mIsPrimary;
    ClassDef(StMcTrack,2)
};
#endif




