/***************************************************************************
 *
 * $Id: StMuDst.h,v 1.56 2019/02/21 14:00:02 jdb Exp $
 * Author: Frank Laue, BNL, laue@bnl.gov
 *
 ***************************************************************************/

#ifndef StMuDst_h
#define StMuDst_h
#define __TFG_MUDST__FORMAT_EXT__
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
enum PicoVtxMode {NotSet=0, Default=1, Vpd=2, VpdOrDefault=3, Mtd=4, FXT=5};

class StMuDst : public TObject {
public:
  // constructor
  StMuDst(); 
  // set the pointers to the TClonesArrays
  void set(StMuDstMaker* maker);
  // set the pointers to the TClonesArrays
  // dongx
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
  // set pointer to current StEmcCollection
  static void setEmcCollection(StEmcCollection *emc_coll) { instance()->mEmcCollection=emc_coll; }
  
  static void setFmsCollection(StFmsCollection *fms_coll) { instance()->mFmsCollection=fms_coll; }
  void  ResetMaps();
  // resets the pointers to the TClonesArrays to 0
  void unset();
  // checks and if necessary corrects the indecies of elements pointing to each other (e.g., a primary track's index to the corresponding global track)
  static void fixTrackIndices(TClonesArray* primary, TClonesArray* global);
  // checks and if necessary corrects the indecies of elements pointing to each other (e.g., a primary track's index to the corresponding global track)
  static void fixTrackIndices();
  //fills gloabl track's mIndex2Global with the index to the respective primary track
  static void fixTrackIndicesG(Int_t mult=1);
  // creates a StEvent from the StMuDst (this) and returns a pointer to it. (This function is not yet finished)  
  StEvent* createStEvent();
  // helper function to create a StTrackGeometry
  static StTrackGeometry* trackGeometry(int q, StPhysicalHelixD* h);
  // creates a StTrack from an StMuTrack and return pointer to it
  static StTrack* createStTrack(const StMuTrack*);
  // dongx
  static void fixTofTrackIndices(TClonesArray* btofHit, TClonesArray* primary, TClonesArray* global);
  static void fixETofTrackIndices(TClonesArray* btofHit, TClonesArray* primary, TClonesArray* global);
  static void fixMtdTrackIndices(TClonesArray* mtdHit, TClonesArray* primary, TClonesArray* global);
  //
  void fixTofTrackIndices();
  void fixETofTrackIndices();
  void fixMtdTrackIndices();

  void setMtdArray(StMtdCollection *mtd_coll); 
  static StMuDst *instance() {return fgMuDst;}
  void SetInstance() {fgMuDst = this;}
  static PicoVtxMode vtxMode()  {return mVtxMode;}
  static Float_t     TpcVpdVzDiffCut() {return mTpcVpdVzDiffCut;}
  static Double_t    erMax()    {return fgerMax;}	
  static Double_t    dca3Dmax() {return fgdca3Dmax;} 	
  static Double_t    VxXmin()   {return fgVxXmin;}	
  static Double_t    VxXmax()   {return fgVxXmax;}	
  static Double_t    VxYmin()   {return fgVxYmin;}	
  static Double_t    VxYmax()   {return fgVxYmax;}	
  static Double_t    VxZmin()   {return fgVxZmin;}	
  static Double_t    VxZmax()   {return fgVxZmax;}  	
  static Double_t    VxRmax()   {return fgVxRmax;}    
  static PicoVtxMode VtxMode()  {return mVtxMode;}   
  
  // fseck
  void setETofArray( const StETofCollection* etof_coll );
  void addETofHit( const StMuETofHit* hit );



 protected:
  static Double_t  fgerMax;
  static Double_t  fgdca3Dmax; 
  static Double_t  fgVxXmin;
  static Double_t  fgVxXmax;
  static Double_t  fgVxYmin;
  static Double_t  fgVxYmax;
  static Double_t  fgVxZmin;
  static Double_t  fgVxZmax;
  static Double_t  fgVxRmax;
  static PicoVtxMode mVtxMode;
  static Float_t   mTpcVpdVzDiffCut;

  static StMuDst *fgMuDst; //!
  // array of TClonesArrays
  TClonesArray** arrays;
#ifndef __NO_STRANGE_MUDST__
  // array of TClonesArrays for the stuff inherited from the StStrangeMuDst
  TClonesArray** strangeArrays;
#endif
  TClonesArray** mcArrays;
  // array of TClonesArrays for the stuff inherited from the Emc
  TClonesArray** emcArrays;
  // array of TClonesArrays for the stuff inherited from the Fms
  TClonesArray** fmsArrays;
  // array of TClonesArrays for the stuff inherited from the Pmd 
  TClonesArray** pmdArrays;
  // array of TClonesArrays for the stuff inherited from the TOF
  TClonesArray** tofArrays;
  // array of TClonesArrays for the stuff inherited from the BTOF // dongx
  TClonesArray** btofArrays;  
  /// array of TClonesArrays for ETof
  TClonesArray** etofArrays;
  // array of TClonesArrays for Epd
  TClonesArray** epdArrays;
  // array of TClonesArrays for the stuff inherited from the Mtd
  TClonesArray** mtdArrays;  
  // array of TClonesArrays for the stuff inherited from the Fgt
  TClonesArray** fgtArrays;
  // pointer to array with MuEmcCollection (for backward compatible mode)
  TClonesArray *mMuEmcCollectionArray;
  // pointer to EmcCollection (manages the EmcArrays)
  StMuEmcCollection *mMuEmcCollection;
  // pointer to array with MuPmdCollection (for backward compatible mode)
  TClonesArray *mMuPmdCollectionArray;
  // pointer to FmsCollection (manages the FmsArrays)
  StMuFmsCollection *mMuFmsCollection; 
  // pointer to PmdCollection (manages the PmdArrays)
  StMuPmdCollection *mMuPmdCollection;
  // pointer to EmcCollecion (for Emc clusterfinding etc)
  StEmcCollection *mEmcCollection;
  // pointer to FmsCollecion (for Fms clusterfinding etc)
  StFmsCollection *mFmsCollection;

  // array of TClonesArrays for the stuff inherited from the EZT (ezTree)
  TClonesArray** eztArrays;

  // Index number of current primary vertex
  Int_t     mCurrVertexId;
  // Temporary array to collect tracks from currect primary vertex
  TObjArray *mCurrPrimaryTracks;
  // Helper function to collect tracks for the current prim vertex
  void collectVertexTracks();
  
public:
  // Set the index number of the current primary vertex (used by both primaryTracks() functions and for StMuEvent::refMult())
  static void setVertexIndex(Int_t vtx_id);
  // Get the index number of the current primary vertex 
  static Int_t currentVertexIndex() ;
  // returns pointer to the n-th TClonesArray 
  static TClonesArray* array(Int_t type) ;
#ifndef __NO_STRANGE_MUDST__
  // returns pointer to the n-th TClonesArray from the strangeness arrays
  static TClonesArray* strangeArray(Int_t type) ;
#endif
  static TClonesArray* mcArray(Int_t type) ;
  static TClonesArray* mcVertices()      ;
  static TClonesArray* mcTracks()        ;
  // returns pointer to the n-th TClonesArray from the emc arrays
  static TClonesArray* emcArray(Int_t type) ;
  // returns pointer to the n-th TClonesArray from the fms arrays
  static TClonesArray* fmsArray(Int_t type) ;
  // returns pointer to the n-th TClonesArray from the pmd arrays
  static TClonesArray* pmdArray(Int_t type) ;
  // returns pointer to the n-th TClonesArray from the tof arrays
  static TClonesArray* tofArray(Int_t type) ;
  // returns pointer to the n-th TClonesArray from the btof arrays // dongx
  static TClonesArray* btofArray(Int_t type) ;
  /// returns pointer to the n-th TClonesArray from the etof arrays // FS
    static TClonesArray* etofArray(int type) ;
  // returns pointer to the n-th TClonesArray from the mtd arrays
  static TClonesArray* mtdArray(Int_t type) ;
  // returns pointer to the n-th TClonesArray from the fgt arrays
  static TClonesArray* fgtArray(Int_t type) ;
  // returns pointer to the n-th TClonesArray from the ezt arrays
  static TClonesArray* eztArray(int type) ;
  // returns pointer to the EpdHitCollection
  static TClonesArray* epdHits() ;
  // returns pointer to the primary vertex list
  static TClonesArray* primaryVertices() ;
  static TClonesArray* allPrimaryTracks() ;
  // returns pointer to a list of tracks belonging to the selected primary vertex
  static TObjArray* primaryTracks() ;
  // returns pointer to the global tracks list
  static TObjArray* globalTracks() ;
  // returns pointer to the other tracks list (all tracks that are not flagged as primary of global)
  static TClonesArray* otherTracks() ;
  // returns pointer to the l3Tracks list
  static TClonesArray* l3Tracks() ;
  // returns pointer to the list of rich spectra
  static TClonesArray* richSpectra() ;
  // returns pointer to the list of detector states
  static TClonesArray* detectorStates() ;
  // returns pointer to list of accepted l3 algorithms 
  static TClonesArray* l3AlgoAccept() ;
  // returns pointer to list rejected l3 algorithms 
  static TClonesArray* l3AlgoReject() ;
  static TClonesArray* covGlobTrack() ;
  static TClonesArray* covPrimTrack() ;
  static TClonesArray* KFTracks() ;
  static TClonesArray* KFVertices() ;

  // returns pointer to current StMuEvent (class holding the event wise information, e.g. event number, run number)
  static StMuEvent* event() ;
  static Int_t      eventId();
  // return pointer to current primary vertex
  static StMuPrimaryVertex* primaryVertex() ;
  // return pointer to i-th primary vertex
  static StMuPrimaryVertex* primaryVertex(Int_t i) ;
  // return pointer to i-th primary track 
  static StMuTrack* primaryTracks(Int_t i) ;
  // return pointer to i-th global track 
  static StMuTrack* globalTracks(Int_t i) ;
  // return pointer to i-th other track  (track that is not flagged as primary of global)
  static StMuTrack* otherTracks(Int_t i) ;
  // return pointer to i-th l3 track
  static StMuTrack* l3Tracks(Int_t i) ;
  // returns pointer to i-th StRichSpectra
  static StRichSpectra* richSpectra(Int_t i) ;
  // returns pointer to i-th StDetectorState
  static StDetectorState* detectorStates(Int_t i) ;
  // returns pointer to i-th accepted StL3AlgorithmInfo
  static StL3AlgorithmInfo* l3AlgoAccept(Int_t i) ;
  // returns pointer to i-th rejected StL3AlgorithmInfo
  static StL3AlgorithmInfo* l3AlgoReject(Int_t i) ;
  //returns pp2pp infomation
  static StMuRpsCollection* RpsCollection() ;
  static StMuMtdCollection* MtdCollection() ;

  static StDcaGeometry* covGlobTracks(Int_t i) ;
  static StMuPrimaryTrackCovariance* covPrimTracks(Int_t i) ;
  static KFParticle*                 KFtrack(Int_t i)  ;
  static KFVertex*                   KFvertex(Int_t i) ;
  static StMuMcTrack*                MCtrack(Int_t i)  ;
  static StMuMcVertex*               MCvertex(Int_t i) ;
 
#ifndef __NO_STRANGE_MUDST__
  // returns pointer to current StStrangeEvMuDst (class holding the event wise information, e.g. event number, run number)
  static StStrangeEvMuDst* strangeEvent() ;
  // returns pointer to MC version of current StStrangeEvMuDst
  static StStrangeEvMuDst* strangeEventMc() ;
  // returns pointer to the v0 list
  static TClonesArray* v0s() ;
  // returns pointer to the mc v0 list
  static TClonesArray* v0sMc() ;
  // returns pointer to the v0 association list
  static TClonesArray* v0Assoc() ;
  // returns pointer to the xi list
  static TClonesArray* xis() ;
  // returns pointer to the mc xi list
  static TClonesArray* xisMc() ;
  // returns pointer to the xi association list
  static TClonesArray* xiAssoc() ;
  // returns pointer to the kink list
  static TClonesArray* kinks() ;
  // returns pointer to the mc kink list
  static TClonesArray* kinksMc() ;
  // returns pointer to the kink association list
  static TClonesArray* kinkAssoc() ;
  // returns pointer to the list of strangeCuts
  static TClonesArray* strangeCuts() ;
  // returns pointer to the i-th v0
  static StV0MuDst* v0s(Int_t i) ;
  static StV0Mc* v0sMc(Int_t i) ;
  static StStrangeAssoc* v0Assoc(Int_t i) ;
  // returns pointer to the i-th xi
  static StXiMuDst* xis(Int_t i) ;
  static StXiMc* xisMc(Int_t i) ;
  static StStrangeAssoc* xiAssoc(Int_t i) ;
  // returns pointer to the i-th kink
  static StKinkMuDst* kinks(Int_t i) ;
  static StKinkMc* kinksMc(Int_t i) ;
  static StStrangeAssoc* kinkAssoc(Int_t i) ;
  // returns pointer to the i-th stranneCut (of type TCut)
  static TCut* strangeCuts(Int_t i) ;
#endif
  // returns pointer to current StMuEmcCollection
  static StMuEmcCollection* muEmcCollection() ;
   // returns pointer to current StMuFmsCollection
  static StMuFmsCollection* muFmsCollection() ;
  // returns pointer to current StMuPmdCollection
  static StMuPmdCollection* pmdCollection() ;
  // returns pointer to current StEmcCollection
  static StEmcCollection* emcCollection() ;
  // returns pointer to current StFmsCollection
  static StFmsCollection* fmsCollection() ;

  // returns pointer to the i-th muTofHit
  static StMuTofHit* tofHit(Int_t i) ;
  // returns pointer to the i-th tofData
  static StTofData* tofData(Int_t i) ;
  // run 5 - dongx
  // returns pointer to the i-th tofRawData
  static StTofRawData* tofRawData(Int_t i) ;
  // returns pointer to the i-th muBTofHit
  static StMuBTofHit* btofHit(Int_t i) ;
  // returns pointer to the i-th btofRawHit - dongx
  static StBTofRawHit* btofRawHit(Int_t i) ;
  // returns pointer to the btofHeader - dongx
  static StBTofHeader* btofHeader() ;

  // fseck ---
  /// returns pointer to the i-th StMuEtofDigi
    static StMuETofDigi* etofDigi(Int_t  i);
  /// returns pointer to the i-th StMuETofHit
      static StMuETofHit* etofHit(Int_t  i);
  /// returns pointer to the StMuETofHeader
	static StMuETofHeader* etofHeader();

  static StMuEpdHit* epdHit(Int_t  i) ;

  static StMuMtdHit* mtdHit(Int_t  i) ;
  static StMuMtdRawHit* mtdRawHit(Int_t  i) ;
  static StMuMtdHeader* mtdHeader() ;
    
    
  // returns pointer to eztHeader 
  static  EztEventHeader* eztHeader() ;

//    static StMuBTofHit* btofHit(Int_t i) ;

    
  // returns pointer to eztTrig 
  static  EztTrigBlob* eztTrig() 
    ;

  // returns pointer to eztFpd 
  static  EztFpdBlob* eztFpd() 
    ;

  // returns pointer to ETOW 
  static  EztEmcRawData* eztETow() 
    ;
  // returns pointer to eztESmd +pre/post
  static  EztEmcRawData* eztESmd() 
    ;

  static UInt_t numberOfPrimaryVertices()  ;
  static UInt_t numberOfPrimaryTracks()  ;
  static UInt_t numberOfGlobalTracks()   ;
  static UInt_t numberOfOtherTracks()    ;
  static UInt_t numberOfL3Tracks()       ;
  static UInt_t numberOfRichSpectras()   ;
  static UInt_t numberOfDetectorStates() ;
  static UInt_t numberOfL3AlgoAccepts()  ;
  static UInt_t numberOfL3AlgoRejects()  ;
  static UInt_t numberOfCovGlobTracks()  ;
  static UInt_t numberOfCovPrimTracks()  ;
  static UInt_t numberOfKFTracks()       ;
  static UInt_t numberOfKFVertices()     ;
  static UInt_t numberOfMcVertices()     ;
  static UInt_t numberOfMcTracks()     ;
#ifndef __NO_STRANGE_MUDST__
  static UInt_t numberOfV0s()            ;
  static UInt_t numberOfV0sMc()          ;
  static UInt_t numberOfV0Assoc()        ;
  static UInt_t numberOfXis()            ;
  static UInt_t numberOfXisMc()          ;
  static UInt_t numberOfXiAssoc()        ;
  static UInt_t numberOfKinks()          ;
  static UInt_t numberOfKinksMc()        ;
  static UInt_t numberOfKinkAssoc()      ;
  static UInt_t numberOfStrangeCuts()    ;
#endif
  // tofr
  static UInt_t numberOfTofHit()        ;
  static UInt_t numberOfTofData()       ;
  // run 5 - dongx
  static UInt_t numberOfTofRawData()    ;
  // dongx
  static UInt_t numberOfBTofHit()       ;
  static UInt_t numberOfBTofRawHit()    ;

  // fseck
  static unsigned int numberOfETofDigi();
  static unsigned int numberOfETofHit();

  static unsigned int numberOfEpdHit();

  static unsigned int numberOfMTDHit()       ;
  static unsigned int numberOfBMTDRawHit()    ;
    
  static UInt_t GetNPrimaryVertex()    ;
  static UInt_t GetNPrimaryTrack()    ;
  static UInt_t GetNGlobalTrack()     ;
  static UInt_t GetNOtherTrack()      ;
  static UInt_t GetNL3Track()         ;
  static UInt_t GetNRichSpectra()     ;
  static UInt_t GetNDetectorState()   ;
  static UInt_t GetNL3AlgoAccept()    ;
  static UInt_t GetNL3AlgoReject()    ;
#ifndef __NO_STRANGE_MUDST__
  static UInt_t GetNV0()              ;
  static UInt_t GetNV0Mc()            ;
  static UInt_t GetNV0Assoc()         ;
  static UInt_t GetNXi()              ;
  static UInt_t GetNXiMc()            ;
  static UInt_t GetNXiAssoc()         ;
  static UInt_t GetNKink()            ;
  static UInt_t GetNKinkMc()          ;
  static UInt_t GetNKinkAssoc()       ;
  static UInt_t GetNStrangeCut()      ;
#endif
  static UInt_t GetNTofHit()          ;
  static UInt_t GetNTofData()         ;
  // run 5 - dongx
  static UInt_t GetNTofRawData()      ;
  // dongx
  static UInt_t GetNBTofHit()         ;
  static UInt_t GetNBTofRawHit()      ;
  // fseck
  static unsigned int GetNETofDigi();
  static unsigned int GetNETofHit();

  static unsigned int GetNEpdHit()         ;

  static unsigned int GetNMTDHit()         ;
  static unsigned int GetNMTDRawHit()      ;
    
  virtual void Print(Option_t *option = "") const; //< Print basic event info
  static void printPrimaryTracks();
  static void printAllPrimaryTracks();
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
