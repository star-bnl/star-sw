/***************************************************************************
 *
 * $Id: StMuDst.h,v 1.54 2018/02/27 04:11:57 jdb Exp $
 * Author: Frank Laue, BNL, laue@bnl.gov
 *
 ***************************************************************************/

#ifndef StMuDst_h
#define StMuDst_h

#include "TObject.h"
#include "TClonesArray.h"
#include <map>
#include <vector>
#include <utility>
class StMuDstMaker;
class StMuEvent;
class StMuPrimaryVertex;
class StMuTrack;
class StMuMcVertex;
class StMuMcTrack;
class KFParticle;
class StRichSpectra;
class StDetectorState;
class StL3AlgorithmInfo;
#ifndef __NO_STRANGE_MUDST__
class StStrangeEvMuDst;
class StV0MuDst;
class StXiMuDst;
class StKinkMuDst;
class StV0Mc;
class StXiMc;
class StKinkMc;
class StStrangeAssoc;
class TCut;
#endif
class StMuEmcCollection;
class StMuFmsCollection;
class StMuPmdCollection;

class StEvent;
class StTriggerData;
class StTrack;
class StTrackGeometry;
class StEmcCollection;
class StFmsCollection;

class StMtdCollection;

class StMuTofHit;
class StTofData;
// run 5 - dongx
class StTofRawData;
// dongx
class StBTofCollection;
class StMuBTofHit;
class StBTofRawHit;
class StBTofHeader;
class StMuEpdHitCollection;  // MALisa
class StMuEpdHit;            // MALisa
class EztEventHeader;
class EztTrigBlob;
class EztFpdBlob;
class EztEmcRawData;

class StDcaGeometry;
class StMuPrimaryTrackCovariance;

class StMuRpsCollection;
class StMuMtdCollection;

class StMuMtdHit;
class StMuMtdRawHit;
class StMuMtdHeader;
class KFParticle;
class KFVertex;
#include "StPhysicalHelixD.hh"

#include "TObject.h"
#include "StMuArrays.h"
#include "StMuException.hh"



#define ARRAY(NAME)  TClonesArray* (NAME)##s() { return tca_##NAME##s;}
#define OBJECT(TYPE,FUNC) TYPE FUNC##(UInt_t i=0) { if (FUNC##s() && (i<(UInt_t)FUNC##s()->GetEntriesFast()) ) return (##TYPE##)FUNC##s()->UncheckedAt(i); return 0;}

#define DO(TYPE,NAME) ARRAY(NAME)    OBJECT(TYPE,NAME)
typedef multimap<StMuPrimaryVertex*,KFParticle*>::iterator RcVx2KFVxIter;


/** 
    @class StMuDst
    Top class of the 'dataformat'. This class exists only in memory and is not 
    written/read to/from disk.
    However, this class is used to hold the pointers to all the TClonesArrays that have 
    been read from disk.
    The class is used to navigate within a 'physics' event (to access tracks, 
    detector info, etc).  
    
*/
  enum PicoVtxMode {NotSet=0, Default=1, Vpd=2, VpdOrDefault=3};

class StMuDst : public TObject {
public:
  /// constructor
  StMuDst(); 
  /// set the pointers to the TClonesArrays
  void set(StMuDstMaker* maker);
  /// set the pointers to the TClonesArrays
  /// dongx
    void set(TClonesArray** /* Arrays */, 
#ifndef __NO_STRANGE_MUDST__
		    TClonesArray** /* StrangeArrays */, 
#endif
		    TClonesArray** mc_ptca=0, 
		    TClonesArray** emc_ptca=0, 
		    TClonesArray** fms_ptca=0, 
		    TClonesArray** pmd_ptca=0, 
		    TClonesArray** tof_ptca=0, 
		    TClonesArray** btof_ptca=0,
		    TClonesArray**  epd_col=0,  // MALisa
		    TClonesArray** mtd_ptca=0,
		    TClonesArray** fgt_ptca=0,
		    TClonesArray** ezt_ptca=0,
		    TClonesArray *emc_tca=0, 
		    StMuEmcCollection *emc_col=0, 
		    StMuFmsCollection *fms_col=0, 
		    TClonesArray *pmd_tca=0, 
		    StMuPmdCollection *pmd_col=0
);
  /// set pointer to current StEmcCollection
  static void setEmcCollection(StEmcCollection *emc_coll) { instance()->mEmcCollection=emc_coll; }
  
  static void setFmsCollection(StFmsCollection *fms_coll) { instance()->mFmsCollection=fms_coll; }
  void  ResetMaps();
  /// resets the pointers to the TClonesArrays to 0
  void unset();
  /// checks and if necessary corrects the indecies of elements pointing to each other (e.g., a primary track's index to the corresponding global track)
  static void fixTrackIndices(TClonesArray* primary, TClonesArray* global);
  /// checks and if necessary corrects the indecies of elements pointing to each other (e.g., a primary track's index to the corresponding global track)
  static void fixTrackIndices();
  //fills gloabl track's mIndex2Global with the index to the respective primary track
  static void fixTrackIndicesG(Int_t mult=1);
  /// creates a StEvent from the StMuDst (this) and returns a pointer to it. (This function is not yet finished)  
  StEvent* createStEvent();
  /// helper function to create a StTrackGeometry
  static StTrackGeometry* trackGeometry(int q, StPhysicalHelixD* h);
  /// creates a StTrack from an StMuTrack and return pointer to it
  static StTrack* createStTrack(const StMuTrack*);
  /// dongx
  static void fixTofTrackIndices(TClonesArray* btofHit, TClonesArray* primary, TClonesArray* global);
  static void fixMtdTrackIndices(TClonesArray* mtdHit, TClonesArray* primary, TClonesArray* global);
  ///
  void fixTofTrackIndices();
  void fixMtdTrackIndices();

  void setMtdArray(StMtdCollection *mtd_coll); 
  static StMuDst *instance() {return fgMuDst;}
  void SetInstance() {fgMuDst = this;}
  static PicoVtxMode vtxMode() {return mVtxMode;}
  static Double_t dca3Dmax() {return fgdca3Dmax;}
 protected:
  static Double_t  fgerMax;
  static Double_t  fgdca3Dmax; 
  static Double_t  fgVxXmin, fgVxXmax, fgVxYmin, fgVxYmax;
  static Double_t  fgVxZmin, fgVxZmax, fgVxRmax;
  static PicoVtxMode mVtxMode;
  static Float_t   mTpcVpdVzDiffCut;
  static Float_t   TpcVpdVzDiffCut() {return mTpcVpdVzDiffCut;}

  static StMuDst *fgMuDst; //!
  /// array of TClonesArrays
  TClonesArray** arrays;
#ifndef __NO_STRANGE_MUDST__
  /// array of TClonesArrays for the stuff inherited from the StStrangeMuDst
  TClonesArray** strangeArrays;
#endif
  TClonesArray** mcArrays;
  /// array of TClonesArrays for the stuff inherited from the Emc
  TClonesArray** emcArrays;
  /// array of TClonesArrays for the stuff inherited from the Fms
  TClonesArray** fmsArrays;
  /// array of TClonesArrays for the stuff inherited from the Pmd 
  TClonesArray** pmdArrays;
  /// array of TClonesArrays for the stuff inherited from the TOF
  TClonesArray** tofArrays;
  /// array of TClonesArrays for the stuff inherited from the BTOF // dongx
  TClonesArray** btofArrays;  
  /// array of TClonesArrays for Epd
  TClonesArray** epdArrays;
  /// array of TClonesArrays for the stuff inherited from the Mtd
  TClonesArray** mtdArrays;  
  /// array of TClonesArrays for the stuff inherited from the Fgt
  TClonesArray** fgtArrays;
  // pointer to array with MuEmcCollection (for backward compatible mode)
  TClonesArray *mMuEmcCollectionArray;
  /// pointer to EmcCollection (manages the EmcArrays)
  StMuEmcCollection *mMuEmcCollection;
  // pointer to array with MuPmdCollection (for backward compatible mode)
  TClonesArray *mMuPmdCollectionArray;
  /// pointer to FmsCollection (manages the FmsArrays)
  StMuFmsCollection *mMuFmsCollection; 
  /// pointer to PmdCollection (manages the PmdArrays)
  StMuPmdCollection *mMuPmdCollection;
  /// pointer to EmcCollecion (for Emc clusterfinding etc)
  StEmcCollection *mEmcCollection;
  /// pointer to FmsCollecion (for Fms clusterfinding etc)
  StFmsCollection *mFmsCollection;

  /// array of TClonesArrays for the stuff inherited from the EZT (ezTree)
  TClonesArray** eztArrays;

  /// Index number of current primary vertex
  Int_t     mCurrVertexId;
  /// Temporary array to collect tracks from currect primary vertex
  TObjArray *mCurrPrimaryTracks;
  /// Helper function to collect tracks for the current prim vertex
  void collectVertexTracks();
  
public:
  /// Set the index number of the current primary vertex (used by both primaryTracks() functions and for StMuEvent::refMult())
  static void setVertexIndex(Int_t vtx_id);
  /// Get the index number of the current primary vertex 
  static Int_t currentVertexIndex() {return instance()->mCurrVertexId; }
  /// returns pointer to the n-th TClonesArray 
  static TClonesArray* array(Int_t type) { return instance()->arrays[type]; }
#ifndef __NO_STRANGE_MUDST__
  /// returns pointer to the n-th TClonesArray from the strangeness arrays
  static TClonesArray* strangeArray(Int_t type) { return instance()->strangeArrays[type]; }
#endif
  static TClonesArray* mcArray(Int_t type) { return instance()->mcArrays[type]; }
  static TClonesArray* mcVertices()      { return instance()->mcArray(0);}
  static TClonesArray* mcTracks()        { return instance()->mcArray(1);}
  /// returns pointer to the n-th TClonesArray from the emc arrays
  static TClonesArray* emcArray(Int_t type) { return instance()->emcArrays[type]; }
   /// returns pointer to the n-th TClonesArray from the fms arrays
  static TClonesArray* fmsArray(Int_t type) { return instance()->fmsArrays[type]; }
    /// returns pointer to the n-th TClonesArray from the pmd arrays
  static TClonesArray* pmdArray(Int_t type) { return instance()->pmdArrays[type]; }
  /// returns pointer to the n-th TClonesArray from the tof arrays
  static TClonesArray* tofArray(Int_t type) { return instance()->tofArrays[type]; }
  /// returns pointer to the n-th TClonesArray from the btof arrays // dongx
  static TClonesArray* btofArray(Int_t type) { return instance()->btofArrays[type]; }
  /// returns pointer to the n-th TClonesArray from the mtd arrays
  static TClonesArray* mtdArray(Int_t type) { return instance()->mtdArrays[type]; }
  /// returns pointer to the n-th TClonesArray from the fgt arrays
  static TClonesArray* fgtArray(Int_t type) { return instance()->fgtArrays[type]; }
  /// returns pointer to the n-th TClonesArray from the ezt arrays
  static TClonesArray* eztArray(int type) { return instance()->eztArrays[type]; }
  /// returns pointer to the EpdHitCollection
  static TClonesArray* epdHits() { return instance()->epdArrays[muEpdHit]; }  // MALisa
  /// returns pointer to the primary vertex list
  static TClonesArray* primaryVertices() { return instance()->arrays[muPrimaryVertex]; }
  static TClonesArray* allPrimaryTracks() { return instance()->arrays[muPrimary]; } 
  /// returns pointer to a list of tracks belonging to the selected primary vertex
  static TObjArray* primaryTracks() { return instance()->mCurrPrimaryTracks; } 
  /// returns pointer to the global tracks list
  static TObjArray* globalTracks() { return instance()->arrays[muGlobal]; }
  /// returns pointer to the other tracks list (all tracks that are not flagged as primary of global)
  static TClonesArray* otherTracks() { return instance()->arrays[muOther]; }
  /// returns pointer to the l3Tracks list
  static TClonesArray* l3Tracks() { return instance()->arrays[muL3]; }
  /// returns pointer to the list of rich spectra
  static TClonesArray* richSpectra() { return instance()->arrays[muRich]; }
  /// returns pointer to the list of detector states
  static TClonesArray* detectorStates() { return instance()->arrays[muState]; }
  /// returns pointer to list of accepted l3 algorithms 
  static TClonesArray* l3AlgoAccept() { return instance()->arrays[muAccept]; }
  /// returns pointer to list rejected l3 algorithms 
  static TClonesArray* l3AlgoReject() { return instance()->arrays[muReject]; }
  static TClonesArray* covGlobTrack() {return instance()->arrays[muCovGlobTrack];}
  static TClonesArray* covPrimTrack() {return instance()->arrays[muCovPrimTrack];}
  static TClonesArray* KFTracks() {return instance()->arrays[muKFTracks];}
  static TClonesArray* KFVertices() {return instance()->arrays[muKFVertices];}

  /// returns pointer to current StMuEvent (class holding the event wise information, e.g. event number, run number)
  static StMuEvent* event() { return (StMuEvent*)instance()->arrays[muEvent]->UncheckedAt(0); }
  static Int_t      eventId();
  /// return pointer to current primary vertex
    static StMuPrimaryVertex* primaryVertex() { return (StMuPrimaryVertex*)instance()->arrays[muPrimaryVertex]->UncheckedAt(instance()->mCurrVertexId); }
  /// return pointer to i-th primary vertex
  static StMuPrimaryVertex* primaryVertex(Int_t i) { return (StMuPrimaryVertex*)instance()->arrays[muPrimaryVertex]->UncheckedAt(i); }
  /// return pointer to i-th primary track 
  static StMuTrack* primaryTracks(Int_t i) { return (StMuTrack*)instance()->mCurrPrimaryTracks->UncheckedAt(i); }
  /// return pointer to i-th global track 
  static StMuTrack* globalTracks(Int_t i) { return (StMuTrack*)instance()->arrays[muGlobal]->UncheckedAt(i); }
  /// return pointer to i-th other track  (track that is not flagged as primary of global)
  static StMuTrack* otherTracks(Int_t i) { return (StMuTrack*)instance()->arrays[muOther]->UncheckedAt(i); }
  /// return pointer to i-th l3 track
  static StMuTrack* l3Tracks(Int_t i) { return (StMuTrack*)instance()->arrays[muL3]->UncheckedAt(i); }
  /// returns pointer to i-th StRichSpectra
  static StRichSpectra* richSpectra(Int_t i) { return (StRichSpectra*)instance()->arrays[muRich]->UncheckedAt(i); }
  /// returns pointer to i-th StDetectorState
  static StDetectorState* detectorStates(Int_t i) { return (StDetectorState*)instance()->arrays[muState]->UncheckedAt(i); }
  /// returns pointer to i-th accepted StL3AlgorithmInfo
  static StL3AlgorithmInfo* l3AlgoAccept(Int_t i) { return (StL3AlgorithmInfo*)instance()->arrays[muAccept]->UncheckedAt(i); }
  /// returns pointer to i-th rejected StL3AlgorithmInfo
  static StL3AlgorithmInfo* l3AlgoReject(Int_t i) { return (StL3AlgorithmInfo*)instance()->arrays[muReject]->UncheckedAt(i); }
  //returns pp2pp infomation
  static StMuRpsCollection* RpsCollection() { return (StMuRpsCollection*)instance()->arrays[mupp2pp]->UncheckedAt(0); }
  static StMuMtdCollection* MtdCollection() { return (StMuMtdCollection*)instance()->arrays[muMtd]->UncheckedAt(0); }

  static StDcaGeometry* covGlobTracks(Int_t i) { return (StDcaGeometry*)instance()->arrays[muCovGlobTrack]->UncheckedAt(i); }
  static StMuPrimaryTrackCovariance* covPrimTracks(Int_t i) { return (StMuPrimaryTrackCovariance*)instance()->arrays[muCovPrimTrack]->UncheckedAt(i); }
  static KFParticle*                 KFtrack(Int_t i)  { return (KFParticle*) KFTracks()->UncheckedAt(i); }
  static KFVertex*                   KFvertex(Int_t i) { return (KFVertex*)   KFVertices()->UncheckedAt(i); }
  static StMuMcTrack*                MCtrack(Int_t i)  { return (StMuMcTrack*) mcTracks()->UncheckedAt(i); }
  static StMuMcVertex*               MCvertex(Int_t i) { return (StMuMcVertex*)   mcVertices()->UncheckedAt(i); }
 
#ifndef __NO_STRANGE_MUDST__
  /// returns pointer to current StStrangeEvMuDst (class holding the event wise information, e.g. event number, run number)
  static StStrangeEvMuDst* strangeEvent() { return (StStrangeEvMuDst*)instance()->strangeArrays[smuEv]->UncheckedAt(0); }
  /// returns pointer to MC version of current StStrangeEvMuDst
  static StStrangeEvMuDst* strangeEventMc() { return (StStrangeEvMuDst*)instance()->strangeArrays[smuEvMc]->UncheckedAt(0); }
  /// returns pointer to the v0 list
  static TClonesArray* v0s() { return instance()->strangeArrays[smuV0]; }
  /// returns pointer to the mc v0 list
  static TClonesArray* v0sMc() { return instance()->strangeArrays[smuV0Mc]; }
  /// returns pointer to the v0 association list
  static TClonesArray* v0Assoc() { return instance()->strangeArrays[smuV0Assoc]; }
  /// returns pointer to the xi list
  static TClonesArray* xis() { return instance()->strangeArrays[smuXi]; }
  /// returns pointer to the mc xi list
  static TClonesArray* xisMc() { return instance()->strangeArrays[smuXiMc]; }
  /// returns pointer to the xi association list
  static TClonesArray* xiAssoc() { return instance()->strangeArrays[smuXiAssoc]; }
  /// returns pointer to the kink list
  static TClonesArray* kinks() { return instance()->strangeArrays[smuKink]; }
  /// returns pointer to the mc kink list
  static TClonesArray* kinksMc() { return instance()->strangeArrays[smuKinkMc]; }
  /// returns pointer to the kink association list
  static TClonesArray* kinkAssoc() { return instance()->strangeArrays[smuKinkAssoc]; }
  /// returns pointer to the list of strangeCuts
  static TClonesArray* strangeCuts() { return instance()->strangeArrays[smuCut]; }
  /// returns pointer to the i-th v0
  static StV0MuDst* v0s(Int_t i) { return (StV0MuDst*)instance()->strangeArrays[smuV0]->UncheckedAt(i); }
  static StV0Mc* v0sMc(Int_t i) { return (StV0Mc*)instance()->strangeArrays[smuV0Mc]->UncheckedAt(i); }
  static StStrangeAssoc* v0Assoc(Int_t i) { return (StStrangeAssoc*)instance()->strangeArrays[smuV0Assoc]->UncheckedAt(i); }
  /// returns pointer to the i-th xi
  static StXiMuDst* xis(Int_t i) { return (StXiMuDst*)(void*)instance()->strangeArrays[smuXi]->UncheckedAt(i); }
  static StXiMc* xisMc(Int_t i) { return (StXiMc*)instance()->strangeArrays[smuXiMc]->UncheckedAt(i); }
  static StStrangeAssoc* xiAssoc(Int_t i) { return (StStrangeAssoc*)instance()->strangeArrays[smuXiAssoc]->UncheckedAt(i); }
  /// returns pointer to the i-th kink
  static StKinkMuDst* kinks(Int_t i) { return (StKinkMuDst*)instance()->strangeArrays[smuKink]->UncheckedAt(i); }
  static StKinkMc* kinksMc(Int_t i) { return (StKinkMc*)instance()->strangeArrays[smuKinkMc]->UncheckedAt(i); }
  static StStrangeAssoc* kinkAssoc(Int_t i) { return (StStrangeAssoc*)instance()->strangeArrays[smuKinkAssoc]->UncheckedAt(i); }
  /// returns pointer to the i-th stranneCut (of type TCut)
  static TCut* strangeCuts(Int_t i) { return (TCut*)instance()->strangeArrays[smuCut]->UncheckedAt(i); }
#endif
  /// returns pointer to current StMuEmcCollection
  static StMuEmcCollection* muEmcCollection() { if (instance()->mMuEmcCollectionArray) return (StMuEmcCollection*) instance()->mMuEmcCollectionArray->UncheckedAt(0); else return instance()->mMuEmcCollection; }
   /// returns pointer to current StMuFmsCollection
  static StMuFmsCollection* muFmsCollection() { return instance()->mMuFmsCollection; }
  /// returns pointer to current StMuPmdCollection
  static StMuPmdCollection* pmdCollection() { if (instance()->mMuPmdCollectionArray)  return (StMuPmdCollection*) instance()->mMuPmdCollectionArray->UncheckedAt(0); else return instance()->mMuPmdCollection; }
  /// returns pointer to current StEmcCollection
  static StEmcCollection* emcCollection() {  return instance()->mEmcCollection; }
  /// returns pointer to current StFmsCollection
  static StFmsCollection* fmsCollection() {  return instance()->mFmsCollection; }

  /// returns pointer to the i-th muTofHit
  static StMuTofHit* tofHit(Int_t i) { return (StMuTofHit*)instance()->tofArrays[muTofHit]->UncheckedAt(i); }
  /// returns pointer to the i-th tofData
  static StTofData* tofData(Int_t i) { return (StTofData*)instance()->tofArrays[muTofData]->UncheckedAt(i); }
  // run 5 - dongx
  /// returns pointer to the i-th tofRawData
  static StTofRawData* tofRawData(Int_t i) { return (StTofRawData*)instance()->tofArrays[muTofRawData]->UncheckedAt(i); }
  /// returns pointer to the i-th muBTofHit
  static StMuBTofHit* btofHit(Int_t i) { return (StMuBTofHit*)instance()->btofArrays[muBTofHit]->UncheckedAt(i); }
  /// returns pointer to the i-th btofRawHit - dongx
  static StBTofRawHit* btofRawHit(Int_t i) { return (StBTofRawHit*)instance()->btofArrays[muBTofRawHit]->UncheckedAt(i); }
  /// returns pointer to the btofHeader - dongx
  static StBTofHeader* btofHeader() { return (StBTofHeader*)instance()->btofArrays[muBTofHeader]->UncheckedAt(0); }

  static StMuEpdHit* epdHit(int i) { return (StMuEpdHit*)instance()->epdArrays[muEpdHit]->UncheckedAt(i); }  // MALisa

  static StMuMtdHit* mtdHit(int i) { return (StMuMtdHit*)instance()->mtdArrays[muMTDHit]->UncheckedAt(i); }
    static StMuMtdRawHit* mtdRawHit(int i) { return (StMuMtdRawHit*)instance()->mtdArrays[muMTDRawHit]->UncheckedAt(i); }
    static StMuMtdHeader* mtdHeader() { return (StMuMtdHeader*)instance()->mtdArrays[muMTDHeader]->UncheckedAt(0); } 
    
    
  /// returns pointer to eztHeader 
  static  EztEventHeader* eztHeader() { return (EztEventHeader*)instance()->eztArrays[muEztHead]->UncheckedAt(0); }

//    static StMuBTofHit* btofHit(Int_t i) { return (StMuBTofHit*)instance()->btofArrays[muBTofHit]->UncheckedAt(i); }

    
  /// returns pointer to eztTrig 
  static  EztTrigBlob* eztTrig() 
        { return (EztTrigBlob*)instance()->eztArrays[muEztTrig]->UncheckedAt(0); }

  /// returns pointer to eztFpd 
  static  EztFpdBlob* eztFpd() 
        { return (EztFpdBlob*)instance()->eztArrays[muEztFpd]->UncheckedAt(0); }

  /// returns pointer to ETOW 
  static  EztEmcRawData* eztETow() 
        { return (EztEmcRawData*)instance()->eztArrays[muEztETow]->UncheckedAt(0); }
  /// returns pointer to eztESmd +pre/post
  static  EztEmcRawData* eztESmd() 
        { return (EztEmcRawData*)instance()->eztArrays[muEztESmd]->UncheckedAt(0); }

  static UInt_t numberOfPrimaryVertices()  { return instance()->arrays[muPrimaryVertex]->GetEntriesFast(); }
  static UInt_t numberOfPrimaryTracks()  { return instance()->mCurrPrimaryTracks ? instance()->mCurrPrimaryTracks->GetEntriesFast() : 0; }
  static UInt_t numberOfGlobalTracks()   { return instance()->arrays[muGlobal]->GetEntriesFast(); }
  static UInt_t numberOfOtherTracks()    { return instance()->arrays[muOther]->GetEntriesFast(); }
  static UInt_t numberOfL3Tracks()       { return instance()->arrays[muL3]->GetEntriesFast(); }
  static UInt_t numberOfRichSpectras()   { return instance()->arrays[muRich]->GetEntriesFast(); }
  static UInt_t numberOfDetectorStates() { return instance()->arrays[muState]->GetEntriesFast(); }
  static UInt_t numberOfL3AlgoAccepts()  { return instance()->arrays[muAccept]->GetEntriesFast(); }
  static UInt_t numberOfL3AlgoRejects()  { return instance()->arrays[muReject]->GetEntriesFast(); }
  static UInt_t numberOfCovGlobTracks()  { return instance()->arrays[muCovGlobTrack]->GetEntriesFast(); }
  static UInt_t numberOfCovPrimTracks()  { return instance()->arrays[muCovPrimTrack]->GetEntriesFast(); }
  static UInt_t numberOfKFTracks()       { return instance()->arrays[muKFTracks]->GetEntriesFast(); }
  static UInt_t numberOfKFVertices()     { return instance()->arrays[muKFVertices]->GetEntriesFast(); }
  static UInt_t numberOfMcVertices()     { return instance()->mcVertices()->GetEntriesFast(); }
  static UInt_t numberOfMcTracks()     { return instance()->mcTracks()->GetEntriesFast(); }
#ifndef __NO_STRANGE_MUDST__
  static UInt_t numberOfV0s()            { return instance()->strangeArrays[smuV0]->GetEntriesFast(); }
  static UInt_t numberOfV0sMc()          { return instance()->strangeArrays[smuV0Mc]->GetEntriesFast(); }
  static UInt_t numberOfV0Assoc()        { return instance()->strangeArrays[smuV0Assoc]->GetEntriesFast(); }
  static UInt_t numberOfXis()            { return instance()->strangeArrays[smuXi]->GetEntriesFast(); }
  static UInt_t numberOfXisMc()          { return instance()->strangeArrays[smuXiMc]->GetEntriesFast(); }
  static UInt_t numberOfXiAssoc()        { return instance()->strangeArrays[smuXiAssoc]->GetEntriesFast(); }  
  static UInt_t numberOfKinks()          { return instance()->strangeArrays[smuKink]->GetEntriesFast(); }
  static UInt_t numberOfKinksMc()        { return instance()->strangeArrays[smuKinkMc]->GetEntriesFast(); } 
  static UInt_t numberOfKinkAssoc()      { return instance()->strangeArrays[smuKinkAssoc]->GetEntriesFast(); }
  static UInt_t numberOfStrangeCuts()    { return instance()->strangeArrays[smuCut]->GetEntriesFast(); }
#endif
  // tofr
  static UInt_t numberOfTofHit()        { return instance()->tofArrays[muTofHit]->GetEntriesFast(); }
  static UInt_t numberOfTofData()       { return instance()->tofArrays[muTofData]->GetEntriesFast(); }
  // run 5 - dongx
  static UInt_t numberOfTofRawData()    { return instance()->tofArrays[muTofRawData]->GetEntriesFast(); }
  // dongx
  static UInt_t numberOfBTofHit()       { return instance()->btofArrays[muBTofHit]->GetEntriesFast(); }
  static UInt_t numberOfBTofRawHit()    { return instance()->btofArrays[muBTofRawHit]->GetEntriesFast(); }

  static unsigned int numberOfEpdHit()       { return instance()->epdArrays[muEpdHit]->GetEntriesFast(); }

  static unsigned int numberOfMTDHit()       { return instance()->mtdArrays[muMTDHit]->GetEntriesFast(); }
  static unsigned int numberOfBMTDRawHit()    { return instance()->mtdArrays[muMTDRawHit]->GetEntriesFast(); }
    
  static UInt_t GetNPrimaryVertex()    { return instance()->numberOfPrimaryVertices(); }  
  static UInt_t GetNPrimaryTrack()    { return instance()->numberOfPrimaryTracks(); }  
  static UInt_t GetNGlobalTrack()     { return instance()->numberOfGlobalTracks(); }   
  static UInt_t GetNOtherTrack()      { return instance()->numberOfOtherTracks(); }    
  static UInt_t GetNL3Track()         { return instance()->numberOfL3Tracks(); }       
  static UInt_t GetNRichSpectra()     { return instance()->numberOfRichSpectras(); }   
  static UInt_t GetNDetectorState()   { return instance()->numberOfDetectorStates(); } 
  static UInt_t GetNL3AlgoAccept()    { return instance()->numberOfL3AlgoAccepts(); }  
  static UInt_t GetNL3AlgoReject()    { return instance()->numberOfL3AlgoRejects(); }  
#ifndef __NO_STRANGE_MUDST__
  static UInt_t GetNV0()              { return instance()->numberOfV0s(); }            
  static UInt_t GetNV0Mc()            { return instance()->numberOfV0sMc(); }            
  static UInt_t GetNV0Assoc()         { return instance()->numberOfV0Assoc(); }            
  static UInt_t GetNXi()              { return instance()->numberOfXis(); }            
  static UInt_t GetNXiMc()            { return instance()->numberOfXisMc(); }            
  static UInt_t GetNXiAssoc()         { return instance()->numberOfXiAssoc(); }            
  static UInt_t GetNKink()            { return instance()->numberOfKinks(); }
  static UInt_t GetNKinkMc()          { return instance()->numberOfKinksMc(); }            
  static UInt_t GetNKinkAssoc()       { return instance()->numberOfKinkAssoc(); }            
  static UInt_t GetNStrangeCut()      { return instance()->numberOfStrangeCuts(); }    
#endif
  static UInt_t GetNTofHit()          { return instance()->numberOfTofHit(); }
  static UInt_t GetNTofData()         { return instance()->numberOfTofData(); }
  // run 5 - dongx
  static UInt_t GetNTofRawData()      { return instance()->numberOfTofRawData(); }
  // dongx
  static UInt_t GetNBTofHit()         { return instance()->numberOfBTofHit(); }
  static UInt_t GetNBTofRawHit()      { return instance()->numberOfBTofRawHit(); }

  static unsigned int GetNEpdHit()         { return instance()->numberOfEpdHit(); }

  static unsigned int GetNMTDHit()         { return instance()->numberOfMTDHit(); }
  static unsigned int GetNMTDRawHit()      { return instance()->numberOfBMTDRawHit(); }
    
  virtual void Print(Option_t *option = "") const; ///< Print basic event info
  static void printPrimaryTracks();
  static void printGlobalTracks() ;
  static void printVertices() ;
  void printKFVertices(); 
  void printKFTracks(); 
  void printMcVertices();
  void printMcTracks();
  void PrintMcVx(UInt_t idVx = 1);
  virtual Bool_t IsGoodTrigger() const;
  friend class StMuDstMaker;
  friend class StMuIOMaker;

  // Maps
#if !defined(__CINT__) && !defined(__CLING__)
  virtual Bool_t Accept(const StMuTrack *gTrack = 0);
  virtual Bool_t Accept(const StMuPrimaryVertex *RcVx = 0);
  virtual Bool_t Accept(const StMuMcTrack *McTrack = 0);
  virtual Bool_t Accept(const StMuMcVertex *McVx = 0);
#endif
  map<Int_t,Int_t>                            &IdGlTk2Indx();
  map<Int_t,Int_t>                            &IdPrTk2Indx();
  map<Int_t,Int_t>                            &IdPrVx2Indx();
  map<Int_t,Int_t>                            &IdKFTk2Indx();
  map<Int_t,Int_t>                            &IdKFVx2Indx();
  
  multimap<StMuMcVertex *,StMuMcTrack *>      &McVx2McTkR(); // 
  map<StMuMcVertex *,StMuMcTrack *>           &McVx2McParentTk(); 
  map<Int_t,StMuMcTrack *>                    &Id2McTk(); // 
  map<Int_t,StMuMcVertex *>                   &Id2McVx(); // All Mc Vx, StMuMcVertex *McVx = Id2McVx[Id]();
  map<Int_t,StMuMcVertex *>                   &Id2McVxR();// Reconstructable, i.e. contains > 1 Reconstructable Mc Tracks
  map<Int_t,StMuPrimaryVertex*>               &Id2RcVx();
  map<Int_t,Int_t>                            &IndxRcTk2Id();
  map<Int_t,Int_t>                            &IndxKFTk2Id();
  multimap<StMuPrimaryVertex*, StMuTrack *>   &RcVx2RcTk();
  map<StMuPrimaryVertex*,StMuMcVertex *>      &RcVx2McVx();
  multimap<StMuMcVertex *,StMuPrimaryVertex*> &McVx2RcVx();
  vector<StMuPrimaryVertex *>                 &RcVxs();  // All accepted RcVx
  vector<StMuPrimaryVertex *>                 &RecoVx();  //  1 to 1 Mc to Rc match
  vector<StMuPrimaryVertex *>                 &CloneVx(); //  1 to many (>1) Mc to Rc match
  vector<StMuPrimaryVertex *>                 &GhostVx(); //  no Mc match
  vector<StMuMcVertex *>                      &LostVx();  //  no Rc match
  map<Int_t,KFParticle*>                      &IdVx2KFVx(); // 
  map<KFParticle*,StMuPrimaryVertex*>         &KFVx2RcVx();
  multimap<StMuPrimaryVertex*,KFParticle*>    &RcVx2KFVx();
  map<KFParticle*,StMuMcVertex *>             &KFVx2McVx(); 
  multimap<StMuMcVertex*,KFParticle*>         &McVx2KFVx(); 
  multimap<Int_t,StMuTrack *>                 &IdMc2RcTk(); // Reconstucted Track to IdTruth
  map<Int_t,Int_t>                            &IdGlobalId2IdPrimaryTrack(); // map global to primary track Ids from vertex with idTruth == 1
  map<Int_t,Int_t>                            &IdGlobal2IdPrimaryTrack();   // map global Id+1 to primary track Id+1 from vertex with idTruth == 1
  multimap<Int_t,Int_t>                       &IdMc2IdRcTracks(); // map between global and Mc tracks from primary Mc vertex
  multimap<Int_t,Int_t>                       &IdMc2IdRcVertices(); // map between indexes Mc and Rc Vertices
  multimap<StMuMcTrack*,StMuTrack*>           &McTrack2GlobalTrack(); // McTrack => MuTrack global
  multimap<StMuMcTrack*,StMuTrack*>           &McTrack2PrimaryTrack();// McTrack => MuTrack primary
  multimap<StMuMcTrack*,KFParticle*>          &McTrack2KFParticle();  // McTrack => KFParticle fitted to vertex
  static Int_t                                MinNoTpcMcHits; // minimum no. of TPC hits in order to consider the MC track reconstractable
  static Int_t                                MinNoTpcRcHits; // minimum no. of TPC hits in order to consider the RC track as good
 private:
  multimap<StMuMcVertex *,StMuMcTrack *>      mMcVx2McTkRMap; 
  map<StMuMcVertex *,StMuMcTrack *>           mMcVx2McParentTkMap; 
  map<Int_t,StMuMcTrack *>                    mId2McTkMap; // 
  map<Int_t,StMuMcVertex *>                   mId2McVxMap; // All Mc Vx, StMuMcVertex *McVx = Id2McVx[Id];
  map<Int_t,StMuMcVertex *>                   mId2McVxRMap;// Reconstructable, i.e. contains > 1 Reconstructable Mc Tracks
  map<Int_t,StMuPrimaryVertex*>               mId2RcVxMap;
  map<Int_t,Int_t>                            mIndxRcTk2IdMap;
  map<Int_t,Int_t>                            mIndxKFTk2IdMap;
  multimap<StMuPrimaryVertex*, StMuTrack *>   mRcVx2RcTkMap;
  map<StMuPrimaryVertex*,StMuMcVertex *>      mRcVx2McVxMap;
  multimap<StMuMcVertex *,StMuPrimaryVertex*> mMcVx2RcVxMap;
  map<Int_t,KFParticle*>                      mIdVx2KFVxMap; // 
  map<KFParticle*,StMuPrimaryVertex*>         mKFVx2RcVxMap;
  multimap<StMuPrimaryVertex*,KFParticle*>    mRcVx2KFVxMap;
  map<KFParticle*,StMuMcVertex *>             mKFVx2McVxMap; 
  multimap<StMuMcVertex*,KFParticle*>         mMcVx2KFVxMap; 
  multimap<Int_t,StMuTrack *>                 mIdMc2RcTkMap; // Reconstucted Track to IdTruth
  map<Int_t,Int_t>                            mIdGlobalId2IdPrimaryTrackMap; // Primary track Id to Global Track Id
  map<Int_t,Int_t>                            mIdGlobal2IdPrimaryTrackMap; // Primary track Id to Global Track Id
  multimap<Int_t,Int_t>                       mIdMc2IdRcTracksMap; // Primary track Id to Global Track Id
  multimap<Int_t,Int_t>                       mIdMc2IdRcVerticesMap; // 
  multimap<StMuMcTrack*,StMuTrack *>          mMcTrack2GlobalTrackMap; 
  multimap<StMuMcTrack*,StMuTrack *>          mMcTrack2PrimaryTrackMap; 
  multimap<StMuMcTrack*,KFParticle *>         mMcTrack2KFParticleMap; 
  vector<StMuPrimaryVertex *>                 mRcVxsVec;  // All accepted RcVx
  vector<StMuPrimaryVertex *>                 mRecoVxVec;  //  1 to 1 Mc to Rc match
  vector<StMuPrimaryVertex *>                 mCloneVxVec; //  1 to many (>1) Mc to Rc match
  vector<StMuPrimaryVertex *>                 mGhostVxVec; //  no Mc match
  vector<StMuMcVertex *>                      mLostVxVec;  //  no Rc match
  map<Int_t,Int_t>                            mIdGlTk2IndxMap;
  map<Int_t,Int_t>                            mIdPrTk2IndxMap;
  map<Int_t,Int_t>                            mIdPrVx2IndxMap;
  map<Int_t,Int_t>                            mIdKFTk2IndxMap;
  map<Int_t,Int_t>                            mIdKFVx2IndxMap;
 public:
  Bool_t selectVertex();
  static void setVtxMode(const PicoVtxMode vtxMode) { instance()->mVtxMode = vtxMode;}
  static void SetMaxTrackDca(Double_t cut = 50);
  static void SetMaxVertexTransError(Double_t cut = 0.0050);
  static void SetVxXYrange(Double_t xmin = -0.3, Double_t xmax = 0., Double_t ymin = -0.27, Double_t ymax = -0.13);
  static void SetVxZrange(Double_t zmin = -70, Double_t zmax = 70.);
  static void SetVxRmax(Double_t rmax = 2);
  static void SetTpcVpdVzDiffCut(Float_t cut = 3) { instance()->mTpcVpdVzDiffCut = cut;}
  
  // Increment this by 1 every time the class structure is changed
  ClassDef(StMuDst,4)
};

#endif

/***************************************************************************
 *
 * $Log: StMuDst.h,v $
 * Revision 1.54  2018/02/27 04:11:57  jdb
 * Added epdArrays
 *
 * Revision 1.53  2017/01/19 23:03:27  smirnovd
 * StMuDst: Make methods static as logic suggests
 *
 * Revision 1.52  2017/01/19 23:03:13  smirnovd
 * Promise to not modify original StMuTrack when converting to StTrack
 *
 * Revision 1.51  2014/05/16 15:06:45  jdb
 * chaned StMuDst{.h,.cxx} to add setMtdArray function
 *
 * Revision 1.50  2013/12/04 19:56:32  jdb
 * Added StMuMtdPidTraits.{cxx, h} added Mtd items to StMuMtdHit.h, StMuDst.{cxx,h}, StMuDstMaker.cxx, StMuTrack.{cxx,h}
 *
 * Revision 1.49  2013/07/23 11:02:59  jeromel
 * Undo changes (KF and other)
 *
 * Revision 1.46  2013/04/08 18:07:55  fisyak
 * Add branches for KFParticles, fix problem with zero cov. matrix for primary tracks
 *
 * Revision 1.45  2012/11/26 23:14:32  fisyak
 * Replace GetEntries() by GetEntriesFast(), fix print outs
 *
 * Revision 1.44  2012/11/15 22:26:13  sangalin
 * Added the FGT. Fixed bugs in array offsets for the MTD.
 *
 * Revision 1.43  2012/09/28 22:38:05  tone421
 * Changed array stucture of MTD upon request of the TOF group. MTD arrays now on top level, rather than within __NARRAYS__
 *
 * Revision 1.42  2011/05/04 19:51:32  tone421
 * Added MTD infomation
 *
 * Revision 1.41  2011/04/08 01:25:50  fisyak
 * Add branches for MC track and vertex information, add IdTruth to  tracks and vertices, reserve a possiblity to remove Strange MuDst
 *
 * Revision 1.40  2010/05/26 04:25:50  tone421
 * Added StTriggerData arrays in muevent and fixed an issue with PMD arrays being read....
 *
 * Revision 1.39  2010/03/08 19:06:51  tone421
 * Two things. Global tracks how are filled with an index to primary at birth. Added StMuDst::fixTrackIndicesG(), which is used for matching the primary track indices to global tracks. Previously, this was quite slow -  see this post:
 *
 * http://www.star.bnl.gov/HyperNews-star/protected/get/starsoft/8092/1/1/1.html
 *
 * for more details.
 *
 * Revision 1.38  2010/02/01 23:15:27  fine
 * replace non-static method
 *
 * Revision 1.37  2010/02/01 22:54:34  fine
 * replace non-static method
 *
 * Revision 1.36  2010/01/25 03:57:39  tone421
 * Added FMS and Roman pot arrays
 *
 * Revision 1.35  2009/08/28 14:55:55  tone421
 * Changed tofArrays to btofArrays returned objects in numberOfBTofHit() and numberOfBTofRawHit()
 *
 * Revision 1.34  2009/02/20 16:37:44  tone421
 * *** empty log message ***
 *
 * Revision 1.32  2008/03/19 14:51:03  fisyak
 * Add two clone arrays for global and primary track covariance matrices, remove mSigmaDcaD and mSigmaDcaZ
 *
 * Revision 1.31  2007/09/18 02:29:58  mvl
 * Added basic printing functionality. For convenience and to assist data consistency checks
 *
 * Revision 1.30  2005/08/22 17:29:12  mvl
 * Made setVertexId static, changed globalTracks() to return
 * TObjArray* (for similarity to primaryTracks.h)
 * and added primaryVertex() to return current vertex
 *
 * Revision 1.29  2005/08/19 19:46:05  mvl
 * Further updates for multiple vertices. The main changes are:
 * 1) StMudst::primaryTracks() now returns a list (TObjArray*) of tracks
 *    belonging to the 'current' primary vertex. The index number of the
 *    'current' vertex can be set using StMuDst::setCurrentVertex().
 *    This also affects StMuDst::primaryTracks(int i) and
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
 * Revision 1.28  2005/07/15 21:45:08  mvl
 * Added support for multiple primary vertices (StMuPrimaryVertex). Track Dcas are now calculated with repect to the first vertex in the list (highest rank), but another vertex number can be specified. Tarcks also store the index of the vertex they belong to (StMuTrack::vertexIndex())
 *
 * Revision 1.27  2005/07/06 21:40:18  fisyak
 * use template version of StPhysicalHelixD
 *
 * Revision 1.26  2005/04/12 21:56:29  mvl
 * Changes by Xin Dong for year-5 TOF data format: extra TClonesArray and routines to fill it from StEvent (StTofRawData).
 *
 * Revision 1.25  2004/11/29 15:53:22  mvl
 * Additions by Jan for Fpd ezTree
 *
 * Revision 1.24  2004/10/29 20:18:18  jeromel
 * Proto arg name must not be repeated
 *
 * Revision 1.23  2004/10/28 00:11:33  mvl
 * Added stuff to support ezTree mode of MuDstMaker.
 * This is a special mode for fast-online processing of fast-detector data.
 *
 * Revision 1.22  2004/10/21 02:56:35  mvl
 * Added pointer to StEmcColleciton for Emc clustering etc.
 * Also made some technical changes for backward compatibility mode with
 * StMuIOMaker (pointers to TClonesArray for StMuEmcCollection)
 *
 * Revision 1.21  2004/10/19 01:45:26  mvl
 * Changes to split Emc and Pmd collections. Minor change to track copying logic
 *
 * Revision 1.20  2004/08/25 04:05:56  mvl
 * Added getters for StStrangeAssocs
 *
 * Revision 1.19  2004/07/27 02:35:23  mvl
 * Added access methods for Strangeness Monte-Carlo arrays
 *
 * Revision 1.18  2004/05/02 04:10:13  perev
 * private => protected
 *
 * Revision 1.17  2004/04/20 18:41:11  perev
 * Change arrays to pointer to StMuDstMaker::arrays StMuDst.h
 *
 * Revision 1.16  2004/04/09 22:04:52  subhasis
 * after tof createevent fix by Xin
 *
 * Revision 1.15  2004/04/09 03:36:14  jeromel
 * Removed TOF support entirely for now as we need a working version ... Will
 * revisit later.
 *
 * Revision 1.14  2004/04/02 03:24:53  jeromel
 * Changes implements PMD and TOF.  TOF is clearly incomplete.
 *
 * Revision 1.13  2003/10/08 21:17:15  laue
 * StMuEmcUtil updates from Alex Suaide
 * StMuDst and StMuDstMaker fixes to take the double inheritance of the
 * StKinkMuDsts into account. A void* had to be introduced when casting
 * TObject* to StKinkMuDst*.
 *
 * Revision 1.12  2003/09/10 22:33:41  perev
 * Grid for MuDst corrections
 *
 * Revision 1.11  2003/04/15 18:48:34  laue
 * Minor changes to be able to filter MuDst.root files and an example
 * how to do this. The StMuDstFilterMaker is just an example, it has to be
 * customized (spoilers, chrome weels, etc.) by the user.
 *
 * Revision 1.10  2003/01/23 21:59:50  laue
 * Modification to compile on Solaris.
 *
 * Revision 1.9  2003/01/09 18:59:45  laue
 * initial check in of new EMC classes and the changes required
 *
 * Revision 1.8  2002/08/23 17:30:18  laue
 * additional member functions added (Helen Caines' request)
 *
 * Revision 1.7  2002/08/20 19:55:49  laue
 * Doxygen comments added
 *
 * Revision 1.6  2002/05/20 17:23:31  laue
 * StStrangeCuts added
 *
 * Revision 1.5  2002/04/01 22:42:30  laue
 * improved chain filter options
 *
 * Revision 1.4  2002/03/20 16:04:11  laue
 * minor changes, mostly added access functions
 *
 * Revision 1.3  2002/03/14 04:12:44  laue
 * bug fix: StMuL3EventSummary.cxx
 * update: StMuDst.h StMuDst.cxx
 *
 * Revision 1.2  2002/03/08 20:04:31  laue
 * change from two trees to 1 tree per file
 *
 * Revision 1.1  2002/03/08 17:04:17  laue
 * initial revision
 *
 *
 **************************************************************************/
