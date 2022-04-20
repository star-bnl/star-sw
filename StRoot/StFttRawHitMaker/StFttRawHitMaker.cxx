/***************************************************************************
 *
 * StFttRawHitMaker.cxx
 *
 * Author: jdb 2021
 ***************************************************************************
 *
 * Description: StFttRawHitMaker - class to fill the StEvent from DAQ reader
 *
 ***************************************************************************/
#include <vector>
#include <map>
#include <array>
#include <algorithm>

#include "StRTSBaseMaker.h"
#include "StDAQMaker/StDAQReader.h"
#include "StRtsTable.h"

#include "StEvent.h"

#include "StFttRawHitMaker.h"


#include "StEvent/StFttRawHit.h"
#include "StEvent/StEvent.h"
#include "StEvent/StFttCollection.h"

#include "StMuDSTMaker/COMMON/StMuTypes.hh"
#include "StMuDSTMaker/COMMON/StMuFttUtil.h"


//_____________________________________________________________
StFttRawHitMaker::StFttRawHitMaker( const char* name )
: StRTSBaseMaker( "stgc", name ),
  mEvent( 0 ),          /// pointer to StEvent
  mFttCollection( 0 ),  // StFttCollection
  mRunYear( 0 ),        /// year in which the data was taken (switch at 1st Oct)
  mDebug( false ),      /// print out of all full messages for debugging
  mReadMuDst(0)         /// read from MuDst->StEvent
{ /* no op */ }


//_____________________________________________________________
Int_t
StFttRawHitMaker::Init()
{
    return kStOk;
}

//_____________________________________________________________
Int_t
StFttRawHitMaker::InitRun( Int_t runnumber )
{ 
    mRunYear = ( runnumber + 727000 ) / 1000000 + 1999;
    return kStOk;
}

//_____________________________________________________________
Int_t
StFttRawHitMaker::FinishRun( Int_t runnumber )
{ 
    return kStOk;
}

//_____________________________________________________________
Int_t
StFttRawHitMaker::Finish()
{ 
    return kStOk;
}


//_____________________________________________________________
Int_t
StFttRawHitMaker::Make()
{ 
    LOG_DEBUG << "StFttRawHitMaker::Make()" << endm;

    mEvent = (StEvent*)GetInputDS("StEvent");
    if(mEvent) {
        LOG_DEBUG<<"Found StEvent"<<endm;
    } else {
        mEvent = new StEvent();
        AddData(mEvent);
        LOG_DEBUG <<"Added StEvent"<<endm;
    }

	if ( mReadMuDst > 0 ) 
			return readMuDst();

    mFttCollection=mEvent->fttCollection();
    if(!mFttCollection) {
        mFttCollection = new StFttCollection();
        mEvent->setFttCollection(mFttCollection);
        LOG_DEBUG <<"Added StFttCollection"<<endm;
    } else {
        mFttCollection = mEvent->fttCollection();
        LOG_DEBUG <<"Found StFttCollection"<<endm;
    }

    StRtsTable* daqdta = nullptr;

    // Loop on all available daq dta vmm blocks
    while ( (daqdta = GetNext( "vmm" )) ) {

        if ( daqdta == nullptr ) {
            LOG_WARN << "StFttRawHitMaker::Make() - NO STGC DATA found in event" << endm;
            return kStOk;
        }

        // do unpacking of the raw data
        int inputSizeBytes = daqdta->GetSize();

        int rdo = daqdta->Rdo();
        int sec = daqdta->Sector();

        if( mDebug ) {
            LOG_INFO << "InputSize (bytes): " << inputSizeBytes << endm;
            LOG_INFO << "Sector: " << daqdta->Sector() << endm;
            LOG_INFO << "Pad: " << daqdta->Pad() << endm;
            LOG_INFO << "Row: " << daqdta->Row() << endm;
            LOG_INFO << "Rdo: " << daqdta->Rdo() << endm;
            LOG_INFO << "InputSize (bytes): " << inputSizeBytes << endm;
            LOG_INFO << "ROWS: " << daqdta->GetNRows() << endm;
        }

        for (auto it = daqdta->begin(); it != daqdta->end(); ++it) {
            
            struct stgc_vmm_t *vmm = (stgc_vmm_t *)  ( *it );
            u_char feb = vmm[0].feb_vmm >> 2 ;  // feb [0..5]
            u_char vm = vmm[0].feb_vmm & 3 ;    // VMM [0..3]

            // create the StFttRawHit object
            StFttRawHit *hit = new StFttRawHit( sec, rdo, feb, vm, vmm[0].ch, vmm[0].adc, vmm[0].bcid, vmm[0].tb, vmm[0].bcid_delta );
            // add it to the collection in StEvent
            mFttCollection->addRawHit( hit );
            if ( mDebug ){
                PrintTheVMM( vmm );
            }
        } // loop it
    } // while daqdta

    return kStOk;
}

int StFttRawHitMaker::readMuDst() {
    StMuDst* mudst = (StMuDst*)GetInputDS("MuDst");
    if(!mudst){LOG_ERROR<<"StFttRawHitMaker::readMuDst() found no MuDst"<<endm; return kStErr;}
    StMuFttCollection* mufttColl= mudst->muFttCollection();
    if(!mufttColl){LOG_ERROR<<"StFttRawHitMaker::readMuDst found no MuFttCollection"<<endm; return kStErr;}
    StMuFttUtil util;    
    mFttCollection = util.getFtt(mufttColl);
    mEvent->setFttCollection(mFttCollection);
}
