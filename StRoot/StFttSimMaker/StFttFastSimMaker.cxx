
#include "StFttFastSimMaker.h"

#include "StEvent/StEvent.h"
#include "St_base/StMessMgr.h"

#include "StThreeVectorF.hh"

#include "tables/St_g2t_fts_hit_Table.h"
#include "StEvent/StFttCollection.h"
#include "StEvent/StFttPoint.h"

#include "StarGenerator/UTIL/StarRandom.h"


StFttFastSimMaker::StFttFastSimMaker(const Char_t *name)
    : StMaker{name} {}

int StFttFastSimMaker::Init() {
    return StMaker::Init();
}

Int_t StFttFastSimMaker::Make() {
    LOG_DEBUG << "StFttFastSimMaker::Make" << endm;

    // Get the existing StEvent, or add one if it doesn't exist.
    StEvent *event = static_cast<StEvent *>(GetDataSet("StEvent"));
    if (!event) {
        event = new StEvent;
        AddData(event);
        LOG_DEBUG << "Creating StEvent" << endm;
    }

    if ( event->fttCollection() == nullptr ){
        LOG_INFO << "Creating FttCollection" << endm;
        StFttCollection *fttcollection = new StFttCollection();
        event->setFttCollection(fttcollection);
    }

    St_g2t_fts_hit *g2t_stg_hits = (St_g2t_fts_hit *)GetDataSet("geant/g2t_stg_hit");
    // size_t numFwdHitsPrior = mFwdHitsFtt.size();
    if (!g2t_stg_hits){
        LOG_WARN << "geant/g2t_stg_hit is empty" << endm;
        return kStOk;
    }

    const double sigXY = 0.01;
    int nstg = g2t_stg_hits->GetNRows();
    LOG_DEBUG << "This event has " << nstg << " stg hits in geant/g2t_stg_hit " << endm;
    for (int i = 0; i < nstg; i++) {
        g2t_fts_hit_st *git = (g2t_fts_hit_st *)g2t_stg_hits->At(i);
        if (0 == git)
            continue; // geant hit
        int track_id = git->track_p;
        int volume_id = git->volume_id;
        int plane_id = (volume_id - 1) / 100;           // from 1 - 16. four chambers per station

        // only use the hits on the front modules
        if ( volume_id % 2 ==0 )
            continue;

        float x = git->x[0] + gRandom->Gaus(0, sigXY); // 100 micron blur according to approx sTGC reso
        float y = git->x[1] + gRandom->Gaus(0, sigXY); // 100 micron blur according to approx sTGC reso
        float z = git->x[2];
        
        StFttPoint *point = new StFttPoint();
        point->setPlane(plane_id);
        point->setQuadrant(0); // TODO this could be improved, but it is not used in the current implementation
        StThreeVectorD xyz;
        xyz.set(x, y, z);
        point->setXYZ( xyz );
        point->setIdTruth( track_id );
        event->fttCollection()->addPoint(point);
    } // loop on hits
    return kStOk;
}
