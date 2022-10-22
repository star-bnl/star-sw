#include "StEvent/StFstHitCollection.h"
#include "StEvent/StFstHit.h"
#include "StEvent/StFstEvtCollection.h"
#include "StEvent/StFstRawHit.h"

#include "StMuDSTMaker/COMMON/StMuFstHit.h"
#include "StMuDSTMaker/COMMON/StMuFstRawHit.h"
#include "StMuDSTMaker/COMMON/StMuFstUtil.h"
#include "StMuDSTMaker/COMMON/StMuFstCollection.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StEvent/StEvent.h"
#include "St_base/StMessMgr.h"
#include "StEvent/StEventTypes.h"
#include "StEvent/StTriggerData.h"
#include "StEvent/StContainers.h"

#include <algorithm>  // For std::find
#include <iterator>  // For std::distance

#include "TCollection.h"  // For TIter
#include "TRefArray.h"
#include "TVector3.h"

ClassImp(StMuFstUtil)

StMuFstUtil::StMuFstUtil()
{
}
StMuFstUtil::~StMuFstUtil()
{
}

StMuFstCollection* StMuFstUtil::getMuFst(StFstHitCollection *fstcol, StFstEvtCollection *fstevtcol)
{
    LOG_DEBUG << "StMuFstUtil::getMuFst" << endm;
    if(!fstcol) return NULL;
    StMuFstCollection* muFst=new StMuFstCollection();
    fillMuFst(muFst,fstcol,fstevtcol);
    return muFst;
} // getMuFst

StFstHitCollection* StMuFstUtil::getFst(StMuFstCollection* muFst)
{
    if(!muFst) return NULL;

    StFstHitCollection *fst=new StFstHitCollection();
    fillFst(fst,muFst);
    return fst;
} //getFst

void StMuFstUtil::fillMuFst(StMuFstCollection *muFst,StFstHitCollection *fstcol,StFstEvtCollection *fstevtcol)
{
    LOG_INFO << "fillMuFst" << endm;
    if(!fstcol) return;
    if(!muFst) return;

    fillMuFstHits(muFst, fstcol, fstevtcol);

} // fillMuFst

void StMuFstUtil::fillFst(StFstHitCollection* fstcol,StMuFstCollection* muFst)
{
    if(!muFst) return;
    if(!fstcol) return;
    fillFstHits(fstcol, muFst);
} // fillFst

void StMuFstUtil::fillMuFstHits(StMuFstCollection* muFst,
        StFstHitCollection* fstcol,
        StFstEvtCollection *fstevtcol)
{
    LOG_INFO << "fillMuFstHits" << endm;
    //fill FST Raw Hits
    if(fstevtcol) {
        LOG_DEBUG << "fillMuFstHits (raw hits)" << endm;
        if(fstevtcol->numberOfRawHits() > 0) {
            const StSPtrVecFstRawHit &vecRawHit = fstevtcol->rawHits();
            for(unsigned int i=0; i<fstevtcol->numberOfRawHits(); i++)
            {
                StMuFstRawHit* muFstRawHit = muFst->addRawHit();
                muFstRawHit->set( vecRawHit[i] );
            }  // for i hit
        } // if has FST Raw hit

    } // if has FST Raw hit collection

    //fill FST hits
    for(int wedgeIdx=0; wedgeIdx<kFstNumWedges; wedgeIdx++ )
    {
        StFstWedgeHitCollection* wedgeHitCollection = fstcol->wedge(wedgeIdx);
        for(int sensorIdx=0; sensorIdx<kFstNumSensorsPerWedge; sensorIdx++)
        {
            StFstSensorHitCollection* sensorHitCollection = wedgeHitCollection->sensor(sensorIdx);

            const StSPtrVecFstHit &vecHit = sensorHitCollection->hits();
            for(unsigned int i=0; i<sensorHitCollection->numberOfHits(); i++)
            {
                StMuFstHit* muFstHit = muFst->addHit();
                muFstHit->set( vecHit[i] );
            }  // for i hit
        }  // for sensorIdx
    }  // for wedgeIdx

} // fillMuFstHits

void StMuFstUtil::fillFstHits(StFstHitCollection* fstcol,
        StMuFstCollection* muFst)
{
    // Using TIter to iterate is safe in the case of hits being NULL
    TIter next(muFst->getHitArray());
    StMuFstHit* muHit(NULL);
    while ((muHit = static_cast<StMuFstHit*>(next())))
    {
        StFstHit *newHit = new StFstHit(muHit->getDisk(), muHit->getWedge(), muHit->getSensor(), muHit->getApv(), muHit->getCharge(), muHit->getChargeErr(), muHit->getMaxTimeBin(), muHit->getMeanRStrip(), muHit->getMeanPhiStrip(), muHit->getNRawHits(), muHit->getNRawHitsR(), muHit->getNRawHitsPhi());
        newHit->setId(muHit->getId());
        newHit->setIdTruth(muHit->getIdTruth());

        newHit->setLocalPosition(muHit->localPosition(0), muHit->localPosition(1), muHit->localPosition(2)); //set local position on sensor

        fstcol->addHit(newHit);
    }  // while
} // fillFstHits
