/*
 * Created by S. Gliske, May 2012
 *
 * Description: General utility class for reading and writing
 * McEEmcTrees.  The tree contains the following branches:
 *
 * - TClonesArray of EEmcEnergy_t holding deposition of energy for each particle hitting the EEMC
 * - TClonesArray of McParticle_t for each particle hitting the EEMC
 * - TClonesArray of McParticle_t for each parent and ancestor of the particles hitting the EEMC
 * - TClonesArray of TVector3 being the stat and stop vertices for all the above particles
 *
 */

#ifndef StMcEEmcTreeMaker_H_
#define StMcEEmcTreeMaker_H_

#include <set>
#include <map>

#include <Rtypes.h>
#include <TVector3.h>

#include "StMaker.h"

class McParticle_t;
class StMcTrack;
class StMcVertex;

class StMcEEmcTreeMaker_t : public StMaker {
 public:
   /// constructor
   StMcEEmcTreeMaker_t( const Char_t *myName );

   /// deconstructor
   virtual ~StMcEEmcTreeMaker_t();

   /// Initialize
   Int_t Init();

   /// Build an event
   Int_t Make();

   /// Clear for next event
   void Clear(Option_t *opts="");

   /// Write everything to file
   Int_t Finish();

   // to keep track of trees being read in or written out
   enum iostatus_t { READ, WRITE, UNSET };

   /// modifiers
   void setTreeStatus( iostatus_t iostatus, const Char_t* fileName );
   void setMaxNumEvents( Int_t maxNum );
   void setEnergyThreshold( Float_t val );  // particles leaving less energy than this value in the ETOW are ignored.

   void addTrigger( Int_t trig );
   void removeTrigger( Int_t trig );

   /// accessors
   Int_t              getNumIncidentParticles() const;
   Int_t              getNumAncestorParticles() const;
   Int_t              getNumVertices() const;

   TIter              getIncidentParticleIter();
   TIter              getAncestorParticleIter();
   McParticle_t*      getAncestor( Int_t idx );
   TVector3*          getVertex( Int_t idx);

   /// TODO: write copy constructor and equals operator.  Should not
   /// ever be used anyhow, but for completeness should eventually
   /// write them.

 protected:
   /// filenames
   iostatus_t mIOStat;
   std::string mFilename;

   /// TFiles/TTrees for writing
   TFile *mFile;
   TTree *mTree;

   /// TChains for reading
   TChain *mChain;

   /// number of events processed / written outt
   Int_t mNumEvents;

   /// max number of events
   Int_t mMaxNumEvents;

   // Amount of energy in tower required in order to not ignore the
   // particle having hit
   Float_t mTowEnergyThres;

   // list of valid triggers (used when writing)
   std::set< Int_t > mTriggerSet;

   /// The data

   TClonesArray *mEEmcEnergyArr;
   TClonesArray *mAncestorParticleArr;
   TClonesArray *mIncidentParticleArr;
   TClonesArray *mVertexArr;
   Double_t mBjX1, mBjX2;

   // maps for indexing
   std::map< const StMcTrack*, Int_t  > mAncestorMap;
   std::map< const StMcVertex*, Int_t > mVertexMap;
   std::map< const StMcTrack*, Int_t  >::iterator mAncestorMapIter;
   std::map< const StMcVertex*, Int_t >::iterator mVertexMapIter;

   // extra functions
   Int_t fill();
   Int_t getAncestorIdx( const StMcTrack* );
   Int_t getVertexIdx( const StMcVertex* );

 private:
   // for ROOT
   ClassDef( StMcEEmcTreeMaker_t, 1 );

};

// inline functions

inline Int_t StMcEEmcTreeMaker_t::getNumIncidentParticles()     const { return mIncidentParticleArr->GetEntriesFast(); };
inline Int_t StMcEEmcTreeMaker_t::getNumAncestorParticles()     const { return mAncestorParticleArr->GetEntriesFast(); };
inline Int_t StMcEEmcTreeMaker_t::getNumVertices()              const { return mVertexArr          ->GetEntriesFast(); };

inline TIter         StMcEEmcTreeMaker_t::getIncidentParticleIter(){ return TIter( mIncidentParticleArr ); };
inline TIter         StMcEEmcTreeMaker_t::getAncestorParticleIter(){ return TIter( mAncestorParticleArr ); };

inline McParticle_t* StMcEEmcTreeMaker_t::getAncestor( Int_t idx ) {
   return (McParticle_t*)( idx < mAncestorParticleArr->GetEntriesFast() ? (*mAncestorParticleArr)[idx] : 0 );
};

inline TVector3* StMcEEmcTreeMaker_t::getVertex( Int_t idx ) {
   return (TVector3*)( idx < mVertexArr->GetEntriesFast() ? (*mVertexArr)[idx] : 0 );
};

inline void StMcEEmcTreeMaker_t::addTrigger( Int_t trig ){ mTriggerSet.insert( trig ); };
inline void StMcEEmcTreeMaker_t::removeTrigger( Int_t trig ){ mTriggerSet.erase( trig ); };

#endif

/*
 * $Id: StMcEEmcTreeMaker.h,v 1.2 2013/03/19 18:49:08 sgliske Exp $
 * $Log: StMcEEmcTreeMaker.h,v $
 * Revision 1.2  2013/03/19 18:49:08  sgliske
 * added Bjorken x1 and x2
 *
 * Revision 1.1  2012/11/26 19:06:11  sgliske
 * moved from offline/users/sgliske/StRoot/StEEmcPool/StEEmcTreeMaker to StRoot/StEEmcPool/StEEmcTreeMaker
 *
 * 
 */
