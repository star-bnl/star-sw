#include "StFttHitCalibMaker.h"

// #include "StFttRawHitMaker/StFttRawHitMaker.h"

#include "StEvent/StFttRawHit.h"
#include "StEvent/StEvent.h"
#include "StEvent/StFttCollection.h"

#include "StEvent/StFttCluster.h"

#include "StFttDbMaker/StFttDb.h"

#include "TFile.h"
#include "TCanvas.h"

#include <set>
//_____________________________________________________________                                                       
StFttHitCalibMaker::StFttHitCalibMaker(const char *name):
StMaker("fttHitCalib",name), mHelper(nullptr)
{                                            
    LOG_DEBUG << "StFttHitCalibMaker::ctor"  << endm;
    mHelper = new HitCalibHelper();
}
//_____________________________________________________________                                                       
StFttHitCalibMaker::~StFttHitCalibMaker()
{
  if(mHelper) delete mHelper;
  mHelper = nullptr;
}

//_____________________________________________________________                                                       
Int_t StFttHitCalibMaker::Init()
{
    LOG_INFO << "StFttHitCalibMaker::Init" << endm;
    return kStOk;
}
//_____________________________________________________________                                                       
Int_t StFttHitCalibMaker::InitRun(Int_t runnumber)
{ 
    mHelper->clear();
    return kStOk;
}

//_____________________________________________________________                                                       
Int_t StFttHitCalibMaker::FinishRun(Int_t runnumber)
{ 
    return kStOk;
}

//-------------------------------------------------------------                                                       
Int_t StFttHitCalibMaker::Finish()
{ 
    LOG_INFO << "StFttHitCalibMaker::Finish()" << endm;

    if (this->mCalibMode == StFttHitCalibMaker::CalibMode::Calibration) {
        LOG_INFO << "Writing StFttHitCalib parameters to plaintext: " << endm;
        WriteCalibrationToPlainText();
    }

    return kStOk;
}


void StFttHitCalibMaker::WriteCalibrationToPlainText() {

    ofstream outf( "fttRawHitTime.dat" );
    for ( int uuid = 0; uuid <= 400; uuid++ ){
        Short_t anchor = mHelper->anchor( uuid );
        auto hist = mHelper->histFor( uuid );
        size_t counts = hist.size(); 
        size_t samples = mHelper->samples( uuid );
        outf << TString::Format( "%d\t%d\t%lu\t%lu", uuid, (int) anchor, counts, samples ) << endl;
    }
    outf.close();

}

//_____________________________________________________________                                                       
Int_t StFttHitCalibMaker::Make()
{ 
    LOG_INFO << "StFttHitCalibMaker::Make()" << endm;

    mEvent = (StEvent*)GetInputDS("StEvent");
    if(mEvent) {
        LOG_DEBUG<<"Found StEvent"<<endm;
    } else {
        return kStOk;
    }
    mFttCollection=mEvent->fttCollection();
    if(!mFttCollection) {
        return kStOk;
    } else {
        LOG_DEBUG <<"Found StFttCollection"<<endm;
    }

    mFttDb = static_cast<StFttDb*>(GetDataSet("fttDb"));


    for ( auto rawHit : mFttCollection->rawHits() ) {

        UShort_t fob = (UShort_t)mFttDb->fob( rawHit );
        UShort_t uuid = rawHit->vmm() + ( StFttDb::nVMMPerFob * fob );

        mHelper->fill( uuid, rawHit->dbcid() );

        if ( mHelper->ready( uuid ) ){
            rawHit->setTime( mHelper->time( uuid, rawHit->dbcid()) );
        } else {
            rawHit->setTime( -4097 );
        }
    }

    return kStOk;
}