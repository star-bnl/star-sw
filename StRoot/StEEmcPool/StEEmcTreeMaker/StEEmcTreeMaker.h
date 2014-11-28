/*
 * Created by S. Gliske, May 2012
 *
 * Description: General utility class for reading and writing
 * EEmcTrees.  The "tree" is actually several trees, divided into
 * "parts."  The parts (may) include the following:
 *
 * - Part 1
 *   - Event header information
 *   - Spin information
 *   - Location of the primary vertex
 *   - An EEmcEnergy_t container, holding the calibrated but unprocessed
          energy response of the EEMC
 * - Part 2
 *   - TClonesArray of EEmcClusters
 *   - TClonesArray of EEmcHits
 * - Part 3
 *   - TClonesArray of EM particle candidates, i.e. particles detected
 *        by the EEMC
 *   - TClonesArray of "two particle" candidates, i.e. a candidate parent
 *        particle which decays (decayed) into two EM particles.
 * - Part 4 (unused in any current analysis--not fully debugged)
 *   - Set of high towers and trigger patches for the event
 *
 *
 */

#ifndef StEEmcTreeMaker_H_
#define StEEmcTreeMaker_H_

#include <set>
#include <Rtypes.h>
#include <TVector3.h>
#include "StMaker.h"

// need this in the .h for linking purposes
#include "StRoot/StTriggerUtilities/StTriggerSimuMaker.h"

class StSpinInfoMaker_t;
class StSpinInfo_t;
class StEEmcEnergyMaker_t;
class EEmcEnergy_t;
class StEEmcHitMaker_t;

class StSimpleCluster_t;
class EEmcSmdCluster_t;
class StEEmcHit_t;
class EEmcHit_t;
class TrigSet;

class StEEmcTreeMaker_t : public StMaker {
 public:
   /// constructor
   StEEmcTreeMaker_t( const Char_t *myName );

   /// deconstructor
   virtual ~StEEmcTreeMaker_t();

   /// Initialize
   Int_t Init();

   /// Build an event
   Int_t Make();

   /// Clear for next event
   void Clear(Option_t *opts="");

   /// Write everything to file
   Int_t Finish();

   // to keep track of trees being read in or written out
   enum iostatus_t { READ, WRITE, IGNORE };

   // to keep track of the type/stage of the tree
   enum treeTypeEnum_t { PART_1, PART_2, PART_3, PART_4, NUM_TREE_PARTS };

   /// modifiers
   void setTreeStatus( treeTypeEnum_t type, iostatus_t iostatus, const Char_t* fileName );
   void setMaxNumEvents( Int_t maxNum );
   void setStartingEvent( Int_t num ){ mNumEvents = num; };
   void setNumTowerThres( UInt_t num );   // for placing a cut on the number of "high" towers

   void setSpinInfoMkr( StSpinInfoMaker_t* spinInfoMkr );
   void setEEmcEnergyMkr( StEEmcEnergyMaker_t* eMkr );
   void setEEmcHitMkr( StEEmcHitMaker_t* hMkr );
   void setEEmcTreeReader( StEEmcTreeMaker_t *treeRdr );

   void doSpinInfoIO( Bool_t doIt = 1 );
   void doEvtHddrIO( Bool_t doIt = 1 );
   void doMakePairs( Bool_t doIt = 1 );

   void setHTTPthres( Double_t ht, Double_t tp ); // both ht and tp used for Part1, just ht used for Part 4

   /// accessors
   StEvtHddr*         getEvtHddr();
   StSpinInfo_t*      getSpinInfo();
   EEmcEnergy_t*      getEEmcEnergy();
   TVector3*          getVertex();
   UInt_t             getBbcOnlineTimeDiff();

   TClonesArray*      getHitArray();

   const TrigSet*     getHardwareTriggerSet();
   const TrigSet*     getSoftwareTriggerSet();

   Int_t              getNumPart1EventsWritten() const;
   Int_t              getNumEEmcClusters() const;
   Int_t              getNumEEmcHits() const;
   Int_t              getNumParticleCandidates1() const;
   Int_t              getNumParticleCandidates2() const;

   TIter              getEEmcClusterIter();
   TIter              getEEmcHitIter();
   TIter              getEEmcParticleCandidateIter1();
   TIter              getEEmcParticleCandidateIter2();

   /// TODO: write copy constructor and equals operator.  Should not
   /// ever be used anyhow, but for completeness should eventually
   /// write them.

 protected:
   /// filenames
   iostatus_t mIOStat[ NUM_TREE_PARTS ];
   std::string mFilename[ NUM_TREE_PARTS ];

   /// TFiles/TTrees for writing
   TFile *mFile[ NUM_TREE_PARTS ];
   TTree *mTree[ NUM_TREE_PARTS ];

   /// TChains for reading
   TChain *mChain[ NUM_TREE_PARTS ];

   /// number of events processed / written outt
   Int_t mNumEvents, mNumPart1EventsWritten;

   /// max number of events
   Int_t mMaxNumEvents;

   /// whether to save various things
   Bool_t mSpinInfoIO;
   Bool_t mEventHddrIO;

   /// whether to make candidate pairs in addition to single hit
   /// particle candidates
   Bool_t mDoMakePairs;

   /// thresholds for keeping the event
   UInt_t mNumTowers;
   Double_t mHTthres, mTPthres;

   /// The data

   /// the following pointers are not owned by the class
   StEvtHddr    *mEventHddr;
   StSpinInfo_t *mSpinInfo;
   EEmcEnergy_t *mEEmcEnergy;

   /// BBC time difference
   UInt_t mBbcOnlineTimeDiff;

   // vertex rank
   Float_t mVertexRank;

   /// the following pointers are owned by the class
   TVector3 *mVertex;
   TrigSet  *mHrdTrigSet, *mSftTrigSet;  // hardware (MuDst) and software (simulated) triggers
   TClonesArray *mClusArr;
   TClonesArray *mHitArr;
   TClonesArray *mParticleArr1;
   TClonesArray *mParticleArr2;
   TArrayF *mET0ht, *mET0tp;

   // makers
   StSpinInfoMaker_t* mSpinInfoMkr;
   StEEmcEnergyMaker_t* mEnMkr;
   StEEmcHitMaker_t* mHitMkr;
   StEEmcTreeMaker_t* mTreeRdr;  // to read the mEEmcEnergy, when this one is ignoring it.

   // extra functions
   Int_t openForRead( treeTypeEnum_t type );
   Int_t openForWrite( treeTypeEnum_t type, const Char_t *name );
   Int_t fillPart1();
   Int_t fillPart2();
   Int_t fillPart3();
   Int_t fillPart4();

   static void copySimpleClusterToSmdCluster( const EEmcEnergy_t& eemcEnergy, Short_t sector, Bool_t inLayerV, const StSimpleCluster_t& other, EEmcSmdCluster_t& clus );
   static void copyStEEmcHitToEEmcHit( const EEmcEnergy_t& eemcEnergy, Int_t uClusIdx, Int_t vClusIdx, const StEEmcHit_t& other, EEmcHit_t& hit );

   const Float_t *mEta;

 private:
   // for ROOT
   ClassDef( StEEmcTreeMaker_t, 1 );

};


// inline accessors

inline StEvtHddr*         StEEmcTreeMaker_t::getEvtHddr(){ return mEventHddr; };
inline StSpinInfo_t*      StEEmcTreeMaker_t::getSpinInfo(){ return mSpinInfo; };
inline EEmcEnergy_t*      StEEmcTreeMaker_t::getEEmcEnergy(){ return mEEmcEnergy; };
inline TVector3*          StEEmcTreeMaker_t::getVertex(){ return mVertex; };
inline UInt_t             StEEmcTreeMaker_t::getBbcOnlineTimeDiff(){ return mBbcOnlineTimeDiff; };

inline TClonesArray*      StEEmcTreeMaker_t::getHitArray(){ return mHitArr; };

inline Int_t              StEEmcTreeMaker_t::getNumEEmcClusters() const { return ( mClusArr ? mClusArr->GetEntriesFast() : 0 ); };
inline Int_t              StEEmcTreeMaker_t::getNumEEmcHits() const { return ( mHitArr ? mHitArr->GetEntriesFast() : 0 ); };
inline Int_t              StEEmcTreeMaker_t::getNumParticleCandidates1() const { return ( mParticleArr1 ? mParticleArr1->GetEntriesFast() : 0 ); };
inline Int_t              StEEmcTreeMaker_t::getNumParticleCandidates2() const { return ( mParticleArr2 ? mParticleArr2->GetEntriesFast() : 0 ); };

inline TIter              StEEmcTreeMaker_t::getEEmcClusterIter(){ return TIter( mClusArr ); };
inline TIter              StEEmcTreeMaker_t::getEEmcHitIter(){ return TIter( mHitArr ); };
inline TIter              StEEmcTreeMaker_t::getEEmcParticleCandidateIter1(){ return TIter( mParticleArr1 ); };
inline TIter              StEEmcTreeMaker_t::getEEmcParticleCandidateIter2(){ return TIter( mParticleArr2 ); };

inline void StEEmcTreeMaker_t::setHTTPthres( Double_t ht, Double_t tp ){ mHTthres = ht; mTPthres = tp; };

inline Int_t StEEmcTreeMaker_t::getNumPart1EventsWritten() const { return mNumPart1EventsWritten; };

inline void StEEmcTreeMaker_t::doMakePairs( Bool_t doIt ){ mDoMakePairs = doIt; };

inline void StEEmcTreeMaker_t::setNumTowerThres( UInt_t num ){ mNumTowers = num; };

#endif

/*
 * $Id: StEEmcTreeMaker.h,v 1.2 2013/02/21 21:28:50 sgliske Exp $
 * $Log: StEEmcTreeMaker.h,v $
 * Revision 1.2  2013/02/21 21:28:50  sgliske
 * added vertex rank
 *
 * Revision 1.1  2012/11/26 19:06:10  sgliske
 * moved from offline/users/sgliske/StRoot/StEEmcPool/StEEmcTreeMaker to StRoot/StEEmcPool/StEEmcTreeMaker
 *
 * 
 */
