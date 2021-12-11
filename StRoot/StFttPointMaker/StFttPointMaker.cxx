/***************************************************************************
 *
 * StFttPointMaker.cxx
 *
 * Author: jdb 2021
 ***************************************************************************
 *
 * Description: StFttPointMaker - class to fill the StFttPoint in StEvent
 * 
 *
 ***************************************************************************/
#include <vector>
#include <map>
#include <array>
#include <algorithm>


#include "StEvent.h"
#include "StEnumerations.h"

#include "StFttPointMaker.h"


#include "StEvent/StFttRawHit.h"
#include "StEvent/StFttCluster.h"
#include "StEvent/StFttPoint.h"
#include "StEvent/StEvent.h"
#include "StEvent/StFttCollection.h"

#include "StFttDbMaker/StFttDb.h"


//_____________________________________________________________
StFttPointMaker::StFttPointMaker( const char* name )
: StMaker( name ),
  mEvent( 0 ),          /// pointer to StEvent
  mDebug( false ),       /// print out of all full messages for debugging
  mUseTestData( false ),
  mFttDb( nullptr )
{
    LOG_DEBUG << "StFttPointMaker::ctor"  << endm;
}

//_____________________________________________________________
StFttPointMaker::~StFttPointMaker()
{  /* no op */ }

//_____________________________________________________________
Int_t
StFttPointMaker::Init()
{
    return kStOk;
}

//_____________________________________________________________
Int_t
StFttPointMaker::InitRun( Int_t runnumber )
{ 
    return kStOk;
}

//_____________________________________________________________
Int_t
StFttPointMaker::FinishRun( Int_t runnumber )
{ 
    return kStOk;
}

//_____________________________________________________________
Int_t
StFttPointMaker::Finish()
{ 
    return kStOk;
}


//_____________________________________________________________
Int_t
StFttPointMaker::Make()
{ 
    LOG_DEBUG << "StFttPointMaker::Make()" << endm;

    mEvent = (StEvent*)GetInputDS("StEvent");
    if(mEvent) {
        LOG_DEBUG<<"Found StEvent"<<endm;
    } else {
        return kStOk;
    }
    mFttCollection=mEvent->fttCollection();
    if(!mFttCollection) {
        return kStOk;
    }

    mFttDb = static_cast<StFttDb*>(GetDataSet("fttDb"));

    assert( mFttDb );

    if ( mUseTestData )
        InjectTestData();

    // next we will need them in even more detail
    // per strip group, but start here
    std::map< UChar_t, std::vector<StFttCluster *> > clustersPerRob[4];

    for ( StFttCluster* clu : mFttCollection->clusters() ) {
        UChar_t rob = clu->quadrant() + ( clu->plane() * StFttDb::nQuadPerPlane );
        clustersPerRob[ clu->orientation() ] [ rob ].push_back( clu );
        // group clusters by quadrant, hor, vert, hdiag, vdiag
    } // loop on hit

    return kStOk;
}

void StFttPointMaker::InjectTestData(){
    mFttCollection->rawHits().clear();

    // TODO: inject clean strip hits to test cluster finder
    // StFttRawHit *hit = new StFttRawHit( sec, rdo, feb, vm, vmm[0].ch, vmm[0].adc, vmm[0].bcid, vmm[0].tb );
    // hit->setMapping( plane, quadrant, row, strip ) 
}
