#include <vector>
#include <map>
#include <array>
#include <algorithm>    // std::is_sorted

#include "StDAQMaker/StDAQReader.h"
#include "StDAQMaker/StDAQMaker.h"
#include "StRtsTable.h"

#include "StEvent.h"

#include "StStgcHitMaker.h"
#include "StEvent/StStgcCollection.h"
#include "StEvent/StFtsStgcHit.h"

// #include "DAQ_READER/daq_dta.h"


//_____________________________________________________________
StStgcHitMaker::StStgcHitMaker( const char* name )
: StRTSBaseMaker( "stgc", name ),
  mEvent( 0 ),          /// pointer to StEvent
  mRunYear( 0 ),        /// year in which the data was taken (switch at 1st Oct)
  mDebug( false )       /// print out of all full messages for debugging
{
    LOG_DEBUG << "StStgcHitMaker::ctor"  << endm;
}

//_____________________________________________________________
StStgcHitMaker::~StStgcHitMaker()
{  /* no op */

}

//_____________________________________________________________
Int_t
StStgcHitMaker::Init()
{
    LOG_INFO << "StStgcHitMaker::Init" << endm;

    // ntuple = new TNtuple("ntuple","data from ascii file","SEC:RDO:ALTRO:CH:ADC:TB");


	if ( write_stgc_tree ){

		fTree = new TFile("ntuple.root", "RECREATE");
		printf( "fTree=%p", fTree );
		fTree->cd();
		treeStgc = new TTree( "stgc", "stgc raw data" );

		treeStgc->Branch( "rdo", &rdo, "rdo/s" );
		treeStgc->Branch( "sec", &sec, "sec/s" );
		treeStgc->Branch( "altro", &altro, "altro/s" );
		treeStgc->Branch( "ch", &ch, "ch/s" );
		treeStgc->Branch( "n", &n, "n/s" );

		treeStgc->Branch( "adc", adc, "adc[n]/s" );
		treeStgc->Branch( "tb", tb, "tb[n]/s" );
	}

    return kStOk;
}

//_____________________________________________________________
Int_t
StStgcHitMaker::InitRun( Int_t runnumber )
{ 
    mRunYear = ( runnumber + 727000 ) / 1000000 + 1999;

    LOG_INFO << "runnumber: " << runnumber << "  --> year: " << mRunYear << endm;

    return kStOk;
}

//_____________________________________________________________
Int_t
StStgcHitMaker::FinishRun( Int_t runnumber )
{ 
    return kStOk;
}

//_____________________________________________________________
Int_t
StStgcHitMaker::Finish()
{ 
    LOG_INFO << "StStgcHitMaker::Finish" << endm;

    if ( fTree && write_stgc_tree ) {
        fTree->cd();
        fTree->Write();
    }
    return kStOk;
}


//_____________________________________________________________
/*!
 * This method is to obtain the ETofCollection from StEvent.
 * If StEvent is in the chain, retrieve it;
 * if no StEvent in the chain, a new StEvent is created.
 */
StStgcCollection*
StStgcHitMaker::GetStgcCollection()
{
    /// Get StEvent if any at once
    StStgcCollection *stgcCollection = 0;
    mEvent = dynamic_cast<StEvent *>( GetInputDS( "StEvent" ) );

    if ( mEvent ) {
        stgcCollection = mEvent->stgcCollection();

        /// Need to create the eTof collection
        if ( !stgcCollection )  {
            ///  Save the eTof collection to StEvent
            LOG_INFO << "StStgcHitMaker::GetStgcCollection - making new StStgcCollection and giving it to StEvent" << endm;
            stgcCollection = new StStgcCollection();
            mEvent->setStgcCollection( stgcCollection );
        }
        else {
            LOG_INFO << "StStgcHitMaker::GetStgcCollection - StEvent already has a StStgcCollection - not making a new one" << endm;
        }
    }
    else {
        LOG_WARN << "No StEvent found by StStgcHitMaker::GetStgcCollection" << endm;
    }

    return stgcCollection;
}


//_____________________________________________________________
Int_t
StStgcHitMaker::Make()
{ 
    LOG_INFO << "StStgcHitMaker::Make()" << endm;

    StStgcCollection* stgcCollection = GetStgcCollection();

    bool do_log_out = false;

    // not looping over RDOs since we only have one this time.
    rdo = 1;
    n = 0;
    sec = 0;
    altro = 0;
    ch = 0;
    for ( int i = 0; i < 100; i++ ){
        adc[i] = 0;
        tb[i] = 0;
    }

    StRtsTable* dd = GetNext( "altro" );

    while ( dd ){
        if ( nullptr == dd ) {
            return kStOk;
        }

        sec = dd->Sector();
        altro = dd->Row();
        ch = dd->Pad();

        if ( do_log_out ) printf("STGC ALTRO: sec %02d, RDO %d: ALTRO %3d:%d\n",dd->Sector(),rdo,dd->Row(),dd->Pad()) ;
        int iInputSizeBytes = dd->GetSize();

        if ( do_log_out ) printf( "ALTRO size bytes: %d\n", iInputSizeBytes );
        if ( do_log_out ) printf( "ALTRO nRows: %ld\n", dd->GetNRows() );

        n = dd->GetNRows();
        unsigned short * data = ( unsigned short* ) ( *dd->begin() );
        size_t index = 0;
        for ( int i = 0; i < (dd->GetNRows())*2; i+=2 ){
            if ( do_log_out ) printf( "tb = %d, adc = %d \n", data[i+1], data[i]  );
            adc[index] = data[i];
            tb[index] = data[i+1];
            index++;
        }

        if ( stgcCollection ){
            stgcCollection->addHit( 0,
                new StFtsStgcHit( rdo, sec, altro, ch, index, adc, tb )
            );    
        }
        

		if (write_stgc_tree && treeStgc)
			treeStgc->Fill();

        dd = GetNext( "altro" );
        if ( do_log_out ) cout << "GetNext(ALTRO) = " << dd << endl;
    }

    return kStOk;
}


//_____________________________________________________________
/*!
 * process events of data taken in 2019+
 */
void
StStgcHitMaker::processEvent( )
{

    // check eTOF information in StEvent
    checkEvent();
}


//_____________________________________________________________
/*!
 * check collection in StEvent (create if necessary)
 */
void
StStgcHitMaker::checkEvent() {

}
//_____________________________________________________________

