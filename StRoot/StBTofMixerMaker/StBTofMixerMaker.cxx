/***************************************************************************
 *
 * $Id: StBTofMixerMaker.cxx,v 1.3 2018/06/21 03:38:46 jdb Exp $
 *
 * Author: Nickolas Luttrell (Rice University), November 2016
 ***************************************************************************
 *
 * Description: StBTofMixerMaker.cpp - This maker accepts the BTof Collection
 * and checks for duplicate hits (hits in the same cell of the BTof). Upon
 * finding a duplicate, it takes the hit with the lowest time of flight. The
 * original BTof collection is replaced with the new, clean collection. This
 * can be used for embedding, as both simulation and data hits are parsed.
 *
 ***************************************************************************
 *
 * $Log: StBTofMixerMaker.cxx,v $
 * Revision 1.3  2018/06/21 03:38:46  jdb
 * Fixed the MixerMakers technique for updating the BTofCollection-previously was not storing MC hits in some embedding cases
 *
 * Revision 1.2  2017/10/20 17:50:33  smirnovd
 * Squashed commit of the following:
 *
 *     StBTof: Remove outdated ClassImp macro
 *
 *     Prefer explicit namespace for std:: names in header files
 *
 *     Removed unnecessary specification of default std::allocator
 *
 * Frank signed-off
 *
 * Revision 1.1  2017/03/02 18:40:57  jeromel
 * First version of BTofMixer jdb / nl
 *
 *
 ***************************************************************************/

#include <Stiostream.h>
#include "StBTofMixerMaker.h"
#include "StEventTypes.h"
#include "StEvent/StBTofCollection.h"

#include "StBTofSimMaker/StBTofSimMaker.h"


//_____________________________________________________________________________
StBTofMixerMaker::StBTofMixerMaker(const char *name):StMaker(name)
{
    //! set default values
    mEvent = 0;
    mEventCollection = 0;
    mBTofSimCollection = 0;
    mNewCollection = 0;
}

StBTofMixerMaker::~StBTofMixerMaker()
{
    //! Destructor
}

//_____________________________________________________________________________
int StBTofMixerMaker::Init()
{

    return StMaker::Init();
}


//_____________________________________________________________________________
int StBTofMixerMaker::InitRun(int runnumber)
{
    return kStOK;
}

//_____________________________________________________________________________
int StBTofMixerMaker::FinishRun(int runnumber)
{
    return kStOk;
}


//_____________________________________________________________________________
int StBTofMixerMaker::Finish()
{
    return kStOK;
}

//_____________________________________________________________________________
int StBTofMixerMaker::Make()
{
    int keyId = 0;    //!< Unique identifier for a given cell. Scaling factors insure that no digits can be overwritten.

    mEvent = (StEvent*)GetInputDS("StEvent");   //! Load StEvent
    if (!mEvent) {
        LOG_ERROR << "No StEvent! Bailing out ..." << endm;
    }
    
    StBTofSimMaker *btofSim = (StBTofSimMaker *)GetMaker("TofSim");  //! Load the StBTofSimMaker
    
    if (btofSim) {
        mBTofSimCollection = btofSim->GetBTofCollection();  //! Load StBTofSimMaker collection
        mIsEmbedding = btofSim->getEmbeddingMode();
    }
    
    if (!mIsEmbedding) {
        LOG_WARN << "Embedding flag is false! StBTofMixerMaker should only be run if the embedding mode is active in StBTofSimMaker!" << endm;
        return kStOK;
    }

    mEventCollection = mEvent->btofCollection();    //! Should always be a btofcollection in StEvent when MixerMaker runs
    if(!mEventCollection) {
        LOG_ERROR << "No BTofCollection! Bailing out ..." << endm;
        return kStFatal;
    }
    LOG_DEBUG << "The original size of the collection was " << mEventCollection->tofHits().size() << endm;
    LOG_DEBUG << "There are " << mBTofSimCollection->tofHits().size() << " hits in the BTofSimMakers Collection" << endm;
    
    std::vector<StBTofHit*> sortedEventHitsVec(23447,0);    //! Size of vector determined by number of channels in BTof AND Vpd
    
    //! Store hits into a sorted vector. Sorting minimizes the computation needed when looking for duplicates
    for (int j=0; j < (int)mEventCollection->tofHits().size(); j++) {
        keyId = mEventCollection->tofHits()[j]->ID()*mEventCollection->tofHits()[j]->kNCell + mEventCollection->tofHits()[j]->cell()-1;
        sortedEventHitsVec[keyId] = mEventCollection->tofHits()[j];
    }
    
    findDuplicates(sortedEventHitsVec, mBTofSimCollection);
    mNewCollection = new StBTofCollection();
    LOG_DEBUG << "size of sortedEventHitsVec=" << sortedEventHitsVec.size() << endm;
    for (int k=0; k < (int)sortedEventHitsVec.size(); k++) {
        if (sortedEventHitsVec[k]) {
            StBTofHit aBTofHit = *sortedEventHitsVec[k];
            mNewCollection->addHit(new StBTofHit(aBTofHit));
        }
    }
    
    // mEventCollection = mNewCollection;  //! Link StEvent to the new mixer collection.
    mNewCollection->setHeader( new StBTofHeader( *(mEventCollection->tofHeader()) ) );
    LOG_DEBUG << "mEventCollection=" << mEventCollection << endm;
    LOG_DEBUG << "mEvent->btofCollection()=" << mEvent->btofCollection() << endm;
    mEvent->setBTofCollection( mNewCollection );
    LOG_DEBUG << "mEvent->btofCollection()=" << mEvent->btofCollection() << endm;

    //! Fill StBTofHeader --
    LOG_DEBUG << "... Modified StBTofCollection Stored in StEvent! " << endm;

    LOG_DEBUG << "The size of the collection is now " << mEvent->btofCollection()->tofHits().size() << endm;

    return kStOK;
}


//_____________________________________________________________________________
// Takes BTofCollections from StEvent and one of the SimMakers, finds duplicate hits and chooses earlier time.
// This function performs the task of embedding
void StBTofMixerMaker::findDuplicates (std::vector<StBTofHit*> &eventHits, StBTofCollection *simHits) {
    for (int i=0; i<(int)simHits->tofHits().size(); i++) {
        if (simHits->tofHits()[i]) {
            int simKeyId = simHits->tofHits()[i]->ID()*simHits->tofHits()[i]->kNCell + simHits->tofHits()[i]->cell()-1;
            if (eventHits[simKeyId] != 0) {
                // perform check to see which Leading edge time occurs sooner
                if ( eventHits[i]->leadingEdgeTime() > simHits->tofHits()[i]->leadingEdgeTime() ) {
                    eventHits[simKeyId] = simHits->tofHits()[i];    // Replace slow hit with faster (simulation) hit
                }
            }
            else {
                eventHits[simKeyId] = simHits->tofHits()[i];    // Embed simulation hit
            }
        }
    }
}


// End StBTofMixerMaker.cpp
