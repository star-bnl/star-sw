/***************************************************************************
 *
 * $Id: StMuDst.h,v 1.56 2019/02/21 14:00:02 jdb Exp $
 * Author: Frank Laue, BNL, laue@bnl.gov
 *
 ***************************************************************************/

#ifndef StMuDst_h
#define StMuDst_h

#include "TObject.h"
#include "TClonesArray.h"

class StMuDstMaker;
class StMuEvent;
class StMuPrimaryVertex;
class StMuTrack;
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
class StETofCollection;   // fseck
class StMuETofCollection; // fseck
class StMuETofHeader;     // fseck
class StMuETofDigi;       // fseck
class StMuETofHit;        // fseck
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

#include "StPhysicalHelixD.hh"

#include "TObject.h"
#include "StMuArrays.h"
#include "StMuException.hh"



#define ARRAY(NAME)  static TClonesArray* (NAME)##s() { return tca_##NAME##s;}
#define OBJECT(TYPE,FUNC) static TYPE FUNC##(unsigned int i=0) { if (FUNC##s() && (i<(unsigned int)FUNC##s()->GetEntriesFast()) ) return (##TYPE##)FUNC##s()->UncheckedAt(i); return 0;}

#define DO(TYPE,NAME) ARRAY(NAME)    OBJECT(TYPE,NAME)


/** 
    @class StMuDst
    Top class of the 'dataformat'. This class exists only in memory and is not 
    written/read to/from disk.
    However, this class is used to hold the pointers to all the TClonesArrays that have 
    been read from disk.
    The class is used to navigate within a 'physics' event (to access tracks, 
    detector info, etc).  
    
*/
class StMuDst : public TObject {
public:
  /// constructor
  StMuDst(); 
  /// set the pointers to the TClonesArrays
  static void set(StMuDstMaker* maker);
  /// set the pointers to the TClonesArrays
  /// dongx
    static void set(TClonesArray** /* Arrays */, 
#ifndef __NO_STRANGE_MUDST__
		    TClonesArray** /* StrangeArrays */, 
#endif
		    TClonesArray** mc_ptca=0, 
		    TClonesArray** emc_ptca=0, 
		    TClonesArray** fms_ptca=0, 
		    TClonesArray** pmd_ptca=0, 
		    TClonesArray** tof_ptca=0, 
		    TClonesArray** btof_ptca=0,
        TClonesArray** etof_col=0,  // jdb
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
  static void setEmcCollection(StEmcCollection *emc_coll) { mEmcCollection=emc_coll; }
  
  static void setFmsCollection(StFmsCollection *fms_coll) { mFmsCollection=fms_coll; }

  /// resets the pointers to the TClonesArrays to 0
  static void unset();
  /// checks and if necessary corrects the indecies of elements pointing to each other (e.g., a primary track's index to the corresponding global track)
  static void fixTrackIndices(TClonesArray* primary, TClonesArray* global);
  /// checks and if necessary corrects the indecies of elements pointing to each other (e.g., a primary track's index to the corresponding global track)
  void fixTrackIndices();
  //fills gloabl track's mIndex2Global with the index to the respective primary track
  static void fixTrackIndicesG(int mult=1);
  /// creates a StEvent from the StMuDst (this) and returns a pointer to it. (This function is not yet finished)  
  StEvent* createStEvent();
  /// helper function to create a StTrackGeometry
  static StTrackGeometry* trackGeometry(int q, StPhysicalHelixD* h);
  /// creates a StTrack from an StMuTrack and return pointer to it
  static StTrack* createStTrack(const StMuTrack*);
  /// dongx
  static void fixTofTrackIndices(TClonesArray* btofHit, TClonesArray* primary, TClonesArray* global);
  static void fixETofTrackIndices(TClonesArray* btofHit, TClonesArray* primary, TClonesArray* global);
  static void fixMtdTrackIndices(TClonesArray* mtdHit, TClonesArray* primary, TClonesArray* global);
  ///
  void fixTofTrackIndices();
  void fixETofTrackIndices();
  void fixMtdTrackIndices();

  void setMtdArray(StMtdCollection *mtd_coll); 
  
  // fseck
  void setETofArray( const StETofCollection* etof_coll );
  void addETofHit( const StMuETofHit* hit );



 protected:
  /// array of TClonesArrays
  static TClonesArray** arrays;
#ifndef __NO_STRANGE_MUDST__
  /// array of TClonesArrays for the stuff inherited from the StStrangeMuDst
  static TClonesArray** strangeArrays;
#endif
  static TClonesArray** mcArrays;
  /// array of TClonesArrays for the stuff inherited from the Emc
  static TClonesArray** emcArrays;
  /// array of TClonesArrays for the stuff inherited from the Fms
  static TClonesArray** fmsArrays;
  /// array of TClonesArrays for the stuff inherited from the Pmd 
  static TClonesArray** pmdArrays;
  /// array of TClonesArrays for the stuff inherited from the TOF
  static TClonesArray** tofArrays;
  /// array of TClonesArrays for the stuff inherited from the BTOF // dongx
  static TClonesArray** btofArrays;  
  /// array of TClonesArrays for ETof
  static TClonesArray** etofArrays;
  /// array of TClonesArrays for Epd
  static TClonesArray** epdArrays;
  /// array of TClonesArrays for the stuff inherited from the Mtd
  static TClonesArray** mtdArrays;  
  /// array of TClonesArrays for the stuff inherited from the Fgt
  static TClonesArray** fgtArrays;
  // pointer to array with MuEmcCollection (for backward compatible mode)
  static TClonesArray *mMuEmcCollectionArray;
  /// pointer to EmcCollection (manages the EmcArrays)
  static StMuEmcCollection *mMuEmcCollection;
  // pointer to array with MuPmdCollection (for backward compatible mode)
  static TClonesArray *mMuPmdCollectionArray;
  /// pointer to FmsCollection (manages the FmsArrays)
  static StMuFmsCollection *mMuFmsCollection; 
  /// pointer to PmdCollection (manages the PmdArrays)
  static StMuPmdCollection *mMuPmdCollection;
  /// pointer to EmcCollecion (for Emc clusterfinding etc)
  static StEmcCollection *mEmcCollection;
  /// pointer to FmsCollecion (for Fms clusterfinding etc)
  static StFmsCollection *mFmsCollection;

  /// array of TClonesArrays for the stuff inherited from the EZT (ezTree)
  static TClonesArray** eztArrays;

  /// Index number of current primary vertex
  static Int_t     mCurrVertexId;
  /// Temporary array to collect tracks from currect primary vertex
  static TObjArray *mCurrPrimaryTracks;
  /// Helper function to collect tracks for the current prim vertex
  static void collectVertexTracks();
  
public:
  /// Set the index number of the current primary vertex (used by both primaryTracks() functions and for StMuEvent::refMult())
  static void setVertexIndex(Int_t vtx_id);
  /// Get the index number of the current primary vertex 
  static Int_t currentVertexIndex() {return mCurrVertexId; }
  /// returns pointer to the n-th TClonesArray 
  static TClonesArray* array(int type) { return arrays[type]; }
#ifndef __NO_STRANGE_MUDST__
  /// returns pointer to the n-th TClonesArray from the strangeness arrays
  static TClonesArray* strangeArray(int type) { return strangeArrays[type]; }
#endif
  static TClonesArray* mcArray(int type) { return mcArrays[type]; }
  /// returns pointer to the n-th TClonesArray from the emc arrays
  static TClonesArray* emcArray(int type) { return emcArrays[type]; }
   /// returns pointer to the n-th TClonesArray from the fms arrays
  static TClonesArray* fmsArray(int type) { return fmsArrays[type]; }
    /// returns pointer to the n-th TClonesArray from the pmd arrays
  static TClonesArray* pmdArray(int type) { return pmdArrays[type]; }
  /// returns pointer to the n-th TClonesArray from the tof arrays
  static TClonesArray* tofArray(int type) { return tofArrays[type]; }
  /// returns pointer to the n-th TClonesArray from the btof arrays // dongx
  static TClonesArray* btofArray(int type) { return btofArrays[type]; }
  /// returns pointer to the n-th TClonesArray from the etof arrays // FS
  static TClonesArray* etofArray(int type) { return etofArrays[type]; }
  /// returns pointer to the n-th TClonesArray from the mtd arrays
  static TClonesArray* mtdArray(int type) { return mtdArrays[type]; }
  /// returns pointer to the n-th TClonesArray from the fgt arrays
  static TClonesArray* fgtArray(int type) { return fgtArrays[type]; }
  /// returns pointer to the n-th TClonesArray from the ezt arrays
  static TClonesArray* eztArray(int type) { return eztArrays[type]; }
  /// returns pointer to the EpdHitCollection
  static TClonesArray* epdHits() { return epdArrays[muEpdHit]; }  // MALisa
  /// returns pointer to the primary vertex list
  static TClonesArray* primaryVertices() { return arrays[muPrimaryVertex]; }
  /// returns pointer to a list of tracks belonging to the selected primary vertex
  static TObjArray* primaryTracks() { return mCurrPrimaryTracks; } 
  /// returns pointer to the global tracks list
  static TObjArray* globalTracks() { return arrays[muGlobal]; }
  /// returns pointer to the other tracks list (all tracks that are not flagged as primary of global)
  static TClonesArray* otherTracks() { return arrays[muOther]; }
  /// returns pointer to the l3Tracks list
  static TClonesArray* l3Tracks() { return arrays[muL3]; }
  /// returns pointer to the list of rich spectra
  static TClonesArray* richSpectra() { return arrays[muRich]; }
  /// returns pointer to the list of detector states
  static TClonesArray* detectorStates() { return arrays[muState]; }
  /// returns pointer to list of accepted l3 algorithms 
  static TClonesArray* l3AlgoAccept() { return arrays[muAccept]; }
  /// returns pointer to list rejected l3 algorithms 
  static TClonesArray* l3AlgoReject() { return arrays[muReject]; }
  static TClonesArray* covGlobTrack() {return arrays[muCovGlobTrack];}
  static TClonesArray* covPrimTrack() {return arrays[muCovPrimTrack];}

  /// returns pointer to current StMuEvent (class holding the event wise information, e.g. event number, run number)
  static StMuEvent* event() { return (StMuEvent*)arrays[muEvent]->UncheckedAt(0); }
  /// return pointer to current primary vertex
  static StMuPrimaryVertex* primaryVertex() { return (StMuPrimaryVertex*)arrays[muPrimaryVertex]->UncheckedAt(mCurrVertexId); }
  /// return pointer to i-th primary vertex
  static StMuPrimaryVertex* primaryVertex(int i) { return (StMuPrimaryVertex*)arrays[muPrimaryVertex]->UncheckedAt(i); }
  /// return pointer to i-th primary track 
  static StMuTrack* primaryTracks(int i) { return (StMuTrack*)mCurrPrimaryTracks->UncheckedAt(i); }
  /// return pointer to i-th global track 
  static StMuTrack* globalTracks(int i) { return (StMuTrack*)arrays[muGlobal]->UncheckedAt(i); }
  /// return pointer to i-th other track  (track that is not flagged as primary of global)
  static StMuTrack* otherTracks(int i) { return (StMuTrack*)arrays[muOther]->UncheckedAt(i); }
  /// return pointer to i-th l3 track
  static StMuTrack* l3Tracks(int i) { return (StMuTrack*)arrays[muL3]->UncheckedAt(i); }
  /// returns pointer to i-th StRichSpectra
  static StRichSpectra* richSpectra(int i) { return (StRichSpectra*)arrays[muRich]->UncheckedAt(i); }
  /// returns pointer to i-th StDetectorState
  static StDetectorState* detectorStates(int i) { return (StDetectorState*)arrays[muState]->UncheckedAt(i); }
  /// returns pointer to i-th accepted StL3AlgorithmInfo
  static StL3AlgorithmInfo* l3AlgoAccept(int i) { return (StL3AlgorithmInfo*)arrays[muAccept]->UncheckedAt(i); }
  /// returns pointer to i-th rejected StL3AlgorithmInfo
  static StL3AlgorithmInfo* l3AlgoReject(int i) { return (StL3AlgorithmInfo*)arrays[muReject]->UncheckedAt(i); }
  //returns pp2pp infomation
  static StMuRpsCollection* RpsCollection() { return (StMuRpsCollection*)arrays[mupp2pp]->UncheckedAt(0); }
  static StMuMtdCollection* MtdCollection() { return (StMuMtdCollection*)arrays[muMtd]->UncheckedAt(0); }

	static StDcaGeometry* covGlobTracks(int i) { return (StDcaGeometry*)arrays[muCovGlobTrack]->UncheckedAt(i); }
  static StMuPrimaryTrackCovariance* covPrimTracks(int i) { return (StMuPrimaryTrackCovariance*)arrays[muCovPrimTrack]->UncheckedAt(i); }
 
#ifndef __NO_STRANGE_MUDST__
  /// returns pointer to current StStrangeEvMuDst (class holding the event wise information, e.g. event number, run number)
  static StStrangeEvMuDst* strangeEvent() { return (StStrangeEvMuDst*)strangeArrays[smuEv]->UncheckedAt(0); }
  /// returns pointer to MC version of current StStrangeEvMuDst
  static StStrangeEvMuDst* strangeEventMc() { return (StStrangeEvMuDst*)strangeArrays[smuEvMc]->UncheckedAt(0); }
  /// returns pointer to the v0 list
  static TClonesArray* v0s() { return strangeArrays[smuV0]; }
  /// returns pointer to the mc v0 list
  static TClonesArray* v0sMc() { return strangeArrays[smuV0Mc]; }
  /// returns pointer to the v0 association list
  static TClonesArray* v0Assoc() { return strangeArrays[smuV0Assoc]; }
  /// returns pointer to the xi list
  static TClonesArray* xis() { return strangeArrays[smuXi]; }
  /// returns pointer to the mc xi list
  static TClonesArray* xisMc() { return strangeArrays[smuXiMc]; }
  /// returns pointer to the xi association list
  static TClonesArray* xiAssoc() { return strangeArrays[smuXiAssoc]; }
  /// returns pointer to the kink list
  static TClonesArray* kinks() { return strangeArrays[smuKink]; }
  /// returns pointer to the mc kink list
  static TClonesArray* kinksMc() { return strangeArrays[smuKinkMc]; }
  /// returns pointer to the kink association list
  static TClonesArray* kinkAssoc() { return strangeArrays[smuKinkAssoc]; }
  /// returns pointer to the list of strangeCuts
  static TClonesArray* strangeCuts() { return strangeArrays[smuCut]; }
  /// returns pointer to the i-th v0
  static StV0MuDst* v0s(int i) { return (StV0MuDst*)strangeArrays[smuV0]->UncheckedAt(i); }
  static StV0Mc* v0sMc(int i) { return (StV0Mc*)strangeArrays[smuV0Mc]->UncheckedAt(i); }
  static StStrangeAssoc* v0Assoc(int i) { return (StStrangeAssoc*)strangeArrays[smuV0Assoc]->UncheckedAt(i); }
  /// returns pointer to the i-th xi
  static StXiMuDst* xis(int i) { return (StXiMuDst*)(void*)strangeArrays[smuXi]->UncheckedAt(i); }
  static StXiMc* xisMc(int i) { return (StXiMc*)strangeArrays[smuXiMc]->UncheckedAt(i); }
  static StStrangeAssoc* xiAssoc(int i) { return (StStrangeAssoc*)strangeArrays[smuXiAssoc]->UncheckedAt(i); }
  /// returns pointer to the i-th kink
  static StKinkMuDst* kinks(int i) { return (StKinkMuDst*)(void*)strangeArrays[smuKink]->UncheckedAt(i); }
  static StKinkMc* kinksMc(int i) { return (StKinkMc*)strangeArrays[smuKinkMc]->UncheckedAt(i); }
  static StStrangeAssoc* kinkAssoc(int i) { return (StStrangeAssoc*)strangeArrays[smuKinkAssoc]->UncheckedAt(i); }
  /// returns pointer to the i-th stranneCut (of type TCut)
  static TCut* strangeCuts(int i) { return (TCut*)strangeArrays[smuCut]->UncheckedAt(i); }
#endif
  /// returns pointer to current StMuEmcCollection
  static StMuEmcCollection* muEmcCollection() { if (mMuEmcCollectionArray) return (StMuEmcCollection*) mMuEmcCollectionArray->UncheckedAt(0); else return mMuEmcCollection; }
   /// returns pointer to current StMuFmsCollection
  static StMuFmsCollection* muFmsCollection() { return mMuFmsCollection; }
  /// returns pointer to current StMuPmdCollection
  static StMuPmdCollection* pmdCollection() { if (mMuPmdCollectionArray)  return (StMuPmdCollection*) mMuPmdCollectionArray->UncheckedAt(0); else return mMuPmdCollection; }
  /// returns pointer to current StEmcCollection
  static StEmcCollection* emcCollection() {  return mEmcCollection; }
  /// returns pointer to current StFmsCollection
  static StFmsCollection* fmsCollection() {  return mFmsCollection; }

  /// returns pointer to the i-th muTofHit
  static StMuTofHit* tofHit(int i) { return (StMuTofHit*)tofArrays[muTofHit]->UncheckedAt(i); }
  /// returns pointer to the i-th tofData
  static StTofData* tofData(int i) { return (StTofData*)tofArrays[muTofData]->UncheckedAt(i); }
  // run 5 - dongx
  /// returns pointer to the i-th tofRawData
  static StTofRawData* tofRawData(int i) { return (StTofRawData*)tofArrays[muTofRawData]->UncheckedAt(i); }
  /// returns pointer to the i-th muBTofHit
  static StMuBTofHit* btofHit(int i) { return (StMuBTofHit*)btofArrays[muBTofHit]->UncheckedAt(i); }
  /// returns pointer to the i-th btofRawHit - dongx
  static StBTofRawHit* btofRawHit(int i) { return (StBTofRawHit*)btofArrays[muBTofRawHit]->UncheckedAt(i); }
  /// returns pointer to the btofHeader - dongx
  static StBTofHeader* btofHeader() { return (StBTofHeader*)btofArrays[muBTofHeader]->UncheckedAt(0); }

  // fseck ---
  /// returns pointer to the i-th StMuEtofDigi
  static StMuETofDigi* etofDigi(int i) { return (StMuETofDigi*)etofArrays[muETofDigi]->UncheckedAt(i); }
  /// returns pointer to the i-th StMuETofHit
  static StMuETofHit* etofHit(int i) { return (StMuETofHit*)etofArrays[muETofHit]->UncheckedAt(i); }
  /// returns pointer to the StMuETofHeader
  static StMuETofHeader* etofHeader() { return (StMuETofHeader*)etofArrays[muETofHeader]->UncheckedAt(0); }
  // -- ---

  static StMuEpdHit* epdHit(int i) { return (StMuEpdHit*)epdArrays[muEpdHit]->UncheckedAt(i); }  // MALisa

  static StMuMtdHit* mtdHit(int i) { return (StMuMtdHit*)mtdArrays[muMTDHit]->UncheckedAt(i); }
    static StMuMtdRawHit* mtdRawHit(int i) { return (StMuMtdRawHit*)mtdArrays[muMTDRawHit]->UncheckedAt(i); }
    static StMuMtdHeader* mtdHeader() { return (StMuMtdHeader*)mtdArrays[muMTDHeader]->UncheckedAt(0); } 
    
    
  /// returns pointer to eztHeader 
  static  EztEventHeader* eztHeader() { return (EztEventHeader*)eztArrays[muEztHead]->UncheckedAt(0); }

//    static StMuBTofHit* btofHit(int i) { return (StMuBTofHit*)btofArrays[muBTofHit]->UncheckedAt(i); }

    
  /// returns pointer to eztTrig 
  static  EztTrigBlob* eztTrig() 
        { return (EztTrigBlob*)eztArrays[muEztTrig]->UncheckedAt(0); }

  /// returns pointer to eztFpd 
  static  EztFpdBlob* eztFpd() 
        { return (EztFpdBlob*)eztArrays[muEztFpd]->UncheckedAt(0); }

  /// returns pointer to ETOW 
  static  EztEmcRawData* eztETow() 
        { return (EztEmcRawData*)eztArrays[muEztETow]->UncheckedAt(0); }
  /// returns pointer to eztESmd +pre/post
  static  EztEmcRawData* eztESmd() 
        { return (EztEmcRawData*)eztArrays[muEztESmd]->UncheckedAt(0); }

  static unsigned int numberOfPrimaryVertices()  { return arrays[muPrimaryVertex]->GetEntriesFast(); }
  static unsigned int numberOfPrimaryTracks()  { return mCurrPrimaryTracks->GetEntriesFast(); }
  static unsigned int numberOfGlobalTracks()   { return arrays[muGlobal]->GetEntriesFast(); }
  static unsigned int numberOfOtherTracks()    { return arrays[muOther]->GetEntriesFast(); }
  static unsigned int numberOfL3Tracks()       { return arrays[muL3]->GetEntriesFast(); }
  static unsigned int numberOfRichSpectras()   { return arrays[muRich]->GetEntriesFast(); }
  static unsigned int numberOfDetectorStates() { return arrays[muState]->GetEntriesFast(); }
  static unsigned int numberOfL3AlgoAccepts()  { return arrays[muAccept]->GetEntriesFast(); }
  static unsigned int numberOfL3AlgoRejects()  { return arrays[muReject]->GetEntriesFast(); }
  static unsigned int numberOfCovGlobTracks()  { return arrays[muCovGlobTrack]->GetEntriesFast(); }
  static unsigned int numberOfCovPrimTracks()  { return arrays[muCovPrimTrack]->GetEntriesFast(); }
#ifndef __NO_STRANGE_MUDST__
  static unsigned int numberOfV0s()            { return strangeArrays[smuV0]->GetEntriesFast(); }
  static unsigned int numberOfV0sMc()          { return strangeArrays[smuV0Mc]->GetEntriesFast(); }
  static unsigned int numberOfV0Assoc()        { return strangeArrays[smuV0Assoc]->GetEntriesFast(); }
  static unsigned int numberOfXis()            { return strangeArrays[smuXi]->GetEntriesFast(); }
  static unsigned int numberOfXisMc()          { return strangeArrays[smuXiMc]->GetEntriesFast(); }
  static unsigned int numberOfXiAssoc()        { return strangeArrays[smuXiAssoc]->GetEntriesFast(); }  
  static unsigned int numberOfKinks()          { return strangeArrays[smuKink]->GetEntriesFast(); }
  static unsigned int numberOfKinksMc()        { return strangeArrays[smuKinkMc]->GetEntriesFast(); } 
  static unsigned int numberOfKinkAssoc()      { return strangeArrays[smuKinkAssoc]->GetEntriesFast(); }
  static unsigned int numberOfStrangeCuts()    { return strangeArrays[smuCut]->GetEntriesFast(); }
#endif
  // tofr
  static unsigned int numberOfTofHit()        { return tofArrays[muTofHit]->GetEntriesFast(); }
  static unsigned int numberOfTofData()       { return tofArrays[muTofData]->GetEntriesFast(); }
  // run 5 - dongx
  static unsigned int numberOfTofRawData()    { return tofArrays[muTofRawData]->GetEntriesFast(); }
  // dongx
  static unsigned int numberOfBTofHit()       { return btofArrays[muBTofHit]->GetEntriesFast(); }
  static unsigned int numberOfBTofRawHit()    { return btofArrays[muBTofRawHit]->GetEntriesFast(); }

  // fseck
  static unsigned int numberOfETofDigi()      { return etofArrays[muETofDigi]->GetEntriesFast(); }
  static unsigned int numberOfETofHit()       { return etofArrays[muETofHit]->GetEntriesFast(); }

  static unsigned int numberOfEpdHit()       { return epdArrays[muEpdHit]->GetEntriesFast(); }

  static unsigned int numberOfMTDHit()       { return mtdArrays[muMTDHit]->GetEntriesFast(); }
  static unsigned int numberOfBMTDRawHit()    { return mtdArrays[muMTDRawHit]->GetEntriesFast(); }
    
  static unsigned int GetNPrimaryVertex()    { return numberOfPrimaryVertices(); }  
  static unsigned int GetNPrimaryTrack()    { return numberOfPrimaryTracks(); }  
  static unsigned int GetNGlobalTrack()     { return numberOfGlobalTracks(); }   
  static unsigned int GetNOtherTrack()      { return numberOfOtherTracks(); }    
  static unsigned int GetNL3Track()         { return numberOfL3Tracks(); }       
  static unsigned int GetNRichSpectra()     { return numberOfRichSpectras(); }   
  static unsigned int GetNDetectorState()   { return numberOfDetectorStates(); } 
  static unsigned int GetNL3AlgoAccept()    { return numberOfL3AlgoAccepts(); }  
  static unsigned int GetNL3AlgoReject()    { return numberOfL3AlgoRejects(); }  
#ifndef __NO_STRANGE_MUDST__
  static unsigned int GetNV0()              { return numberOfV0s(); }            
  static unsigned int GetNV0Mc()            { return numberOfV0sMc(); }            
  static unsigned int GetNV0Assoc()         { return numberOfV0Assoc(); }            
  static unsigned int GetNXi()              { return numberOfXis(); }            
  static unsigned int GetNXiMc()            { return numberOfXisMc(); }            
  static unsigned int GetNXiAssoc()         { return numberOfXiAssoc(); }            
  static unsigned int GetNKink()            { return numberOfKinks(); }
  static unsigned int GetNKinkMc()          { return numberOfKinksMc(); }            
  static unsigned int GetNKinkAssoc()       { return numberOfKinkAssoc(); }            
  static unsigned int GetNStrangeCut()      { return numberOfStrangeCuts(); }    
#endif
  static unsigned int GetNTofHit()          { return numberOfTofHit(); }
  static unsigned int GetNTofData()         { return numberOfTofData(); }
  // run 5 - dongx
  static unsigned int GetNTofRawData()      { return numberOfTofRawData(); }
  // dongx
  static unsigned int GetNBTofHit()         { return numberOfBTofHit(); }
  static unsigned int GetNBTofRawHit()      { return numberOfBTofRawHit(); }
  // fseck
  static unsigned int GetNETofDigi()        { return numberOfETofDigi(); }
  static unsigned int GetNETofHit()         { return numberOfETofHit(); }

  static unsigned int GetNEpdHit()         { return numberOfEpdHit(); }

  static unsigned int GetNMTDHit()         { return numberOfMTDHit(); }
  static unsigned int GetNMTDRawHit()      { return numberOfBMTDRawHit(); }
    
  virtual void Print(Option_t *option = "") const; ///< Print basic event info
  static void printPrimaryTracks();
  static void printGlobalTracks() ;
  static void printVertices() ;

  friend class StMuDstMaker;
  friend class StMuIOMaker;

  // Increment this by 1 every time the class structure is changed
  ClassDef(StMuDst,5)
};

#endif

/***************************************************************************
 *
 * $Log: StMuDst.h,v $
 * Revision 1.56  2019/02/21 14:00:02  jdb
 * Bumped the ClassDef versions in MuDst where eTOF was added. I also added the etofTypes to the LinkDef file
 *
 * Revision 1.55  2019/02/21 13:32:54  jdb
 * Inclusion of ETOF MuDst code. This code adds support for the full set of ETOF data which includes EtofDigi, EtofHit, EtofHeader. The code essentially copies similar structures from StEvent and additionally rebuilds the maps between Digis and Hits. Accessor methods are added based on the pattern from BTOF to provide access to data at various levels. The code for accessing the PID traits provided by ETOF is also provided
 *
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
 * Revision 1.47  2013/04/10 19:28:35  jeromel
 * Step back to 04/04 version (van aware) - previous changes may be recoverred
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
