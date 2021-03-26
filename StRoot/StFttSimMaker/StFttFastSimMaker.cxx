
#include "StFttFastSimMaker.h"

#include "StEvent/StEvent.h"
#include "St_base/StMessMgr.h"

#include "StEvent/StRnDHit.h"
#include "StEvent/StRnDHitCollection.h"
#include "StThreeVectorF.hh"

#include "TCanvas.h"
#include "TCernLib.h"
#include "TH2F.h"
#include "TLine.h"
#include "TString.h"
#include "TVector3.h"
#include "tables/St_g2t_fts_hit_Table.h"
#include "tables/St_g2t_track_Table.h"
#include <array>

#include "StarGenerator/UTIL/StarRandom.h"

namespace FttGlobal {
    const bool verbose = false;
}

StFttFastSimMaker::StFttFastSimMaker(const Char_t *name)
    : StMaker{name},
      hGlobalYX(0),
      hOctantYX(0),
      hOctantWireYX(0),
      hOctantStripYX(0),
      hWireDeltasX(0),
      hWireDeltasY(0),
      hStripDeltasX(0),
      hStripDeltasY(0),
      hWirePullsX(0),
      hWirePullsY(0),
      hStripPullsX(0),
      hStripPullsY(0),
      hPointsPullsX(0),
      hPointsPullsY(0) {}

int StFttFastSimMaker::Init() {
    iEvent = 0;

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

    if (0 == event->rndHitCollection()) {
        event->setRnDHitCollection(new StRnDHitCollection());
        LOG_DEBUG << "Creating StRnDHitCollection for FTS" << endm;
    }

    FillThinGapChambers(event);
    iEvent++;

    return kStOk;
}

/**
 * Maps a global hit to a local coordinate system for a given quadrant
 * The quadrants are numbered clockwise as:
 * 0 1
 * 3 2
 * Does NOT support rotations. Maybe added later if we need to
 * The coordinate system is x positive to the right and y positive up (top coords are greater than bottom coords)
 */
void StFttFastSimMaker::GlobalToLocal(float x, float y, int disk, int &quad, float &localX, float &localY) {
    // quad RECT
    float qr = -1;
    float ql = -1;
    float qb = -1;
    float qt = -1;

    QuadBottomLeft(disk, 0, qb, ql);
    qr = ql + STGC_QUAD_WIDTH;
    qt = qb + STGC_QUAD_HEIGHT;
    if (x >= ql && x < qr && y >= qb && y < qt) {
        quad = 0;
        localX = x - ql;
        localY = y - qb;
    }

    QuadBottomLeft(disk, 1, qb, ql);
    qr = ql + STGC_QUAD_WIDTH;
    qt = qb + STGC_QUAD_HEIGHT;
    if (x >= ql && x < qr && y >= qb && y < qt) {
        quad = 1;
        localX = x - ql;
        localY = y - qb;
    }
    QuadBottomLeft(disk, 2, qb, ql);
    qr = ql + STGC_QUAD_WIDTH;
    qt = qb + STGC_QUAD_HEIGHT;
    if (x >= ql && x < qr && y >= qb && y < qt) {
        quad = 2;
        localX = x - ql;
        localY = y - qb;
    }
    QuadBottomLeft(disk, 3, qb, ql);
    qr = ql + STGC_QUAD_WIDTH;
    qt = qb + STGC_QUAD_HEIGHT;
    if (x >= ql && x < qr && y >= qb && y < qt) {
        quad = 3;
        localX = x - ql;
        localY = y - qb;
    }

    return;
}

float StFttFastSimMaker::DiskOffset(int disk) {
    assert(disk >= 9 && disk <= 12);
    if (disk == 9)
        return 10;
    if (disk == 10)
        return 11;
    if (disk == 11)
        return 12;
    if (disk == 12)
        return 13;
    return 10;
}

float StFttFastSimMaker::DiskRotation(int disk) {
    assert(disk >= 9 && disk <= 12);
    // these are
    if (disk == 9)
        return this->sTGC_disk9_theta;
    if (disk == 10)
        return this->sTGC_disk10_theta;
    if (disk == 11)
        return this->sTGC_disk11_theta;
    if (disk == 12)
        return this->sTGC_disk12_theta;
    return 0;
}

void StFttFastSimMaker::QuadBottomLeft(int disk, int quad, float &bottom, float &left) {
    float hbp = DiskOffset(disk);
    if ( FttGlobal::verbose )   {LOG_INFO << "disk: " << disk << ", offset = " << hbp << endm;}

    // quad 0 RECT
    float q0l = hbp - STGC_QUAD_WIDTH;
    float q0b = hbp;

    float q1l = hbp;
    float q1b = -hbp;

    float q2l = -hbp;
    float q2b = -hbp - STGC_QUAD_HEIGHT;

    float q3l = -hbp - STGC_QUAD_WIDTH;
    float q3b = hbp - STGC_QUAD_HEIGHT;

    if (0 == quad) {
        bottom = q0b;
        left = q0l;
    }
    if (1 == quad) {
        bottom = q1b;
        left = q1l;
    }
    if (2 == quad) {
        bottom = q2b;
        left = q2l;
    }
    if (3 == quad) {
        bottom = q3b;
        left = q3l;
    }
    return;
}

/**
 * Map a local coordinate back to global coords
 */
void StFttFastSimMaker::LocalToGlobal(float localX, float localY, int disk, int quad, float &globalX, float &globalY) {
    // quad RECT
    float qb = -1;
    float ql = -1;
    QuadBottomLeft(disk, quad, qb, ql);

    globalX = localX + ql;
    globalY = localY + qb;
    return;
}

/**
 * Checks if a vertical (face=0) and horizontal (face=1) wires are overlapping
 * used for determining if ghost hits can be created from the overlapped wires
 * 
 */
bool StFttFastSimMaker::Overlaps(StRnDHit *hitA, StRnDHit *hitB) {
    // require that they are in the same disk!
    if (hitA->layer() != hitB->layer())
        return false;
    int disk = hitA->layer();
    // require that they are in the same quadrant of the detector
    if (hitA->wafer() != hitB->wafer())
        return false;
    int quad = hitA->wafer();

    float x1 = hitA->double2();
    float y1 = hitA->double3();
    float x2 = hitB->double2();
    float y2 = hitB->double3();

    float b = -1, l = -1;
    QuadBottomLeft(disk, quad, b, l);

    float lx1 = x1 - l;
    float ly1 = y1 - b;
    float lx2 = x2 - l;
    float ly2 = y2 - b;

    int chunkx1 = lx1 / STGC_WIRE_LENGTH;
    int chunky1 = ly1 / STGC_WIRE_LENGTH;

    int chunkx2 = lx2 / STGC_WIRE_LENGTH;
    int chunky2 = ly2 / STGC_WIRE_LENGTH;

    if (chunkx1 != chunkx2)
        return false;
    if (chunky1 != chunky2)
        return false;
    return true;
}

void StFttFastSimMaker::FillThinGapChambers(StEvent *event) {
    // Read the g2t table
    St_g2t_fts_hit *hitTable = static_cast<St_g2t_fts_hit *>(GetDataSet("g2t_stg_hit"));
    if (!hitTable) {
        LOG_INFO << "g2t_stg_hit table is empty" << endm;
        return;
    } // if !hitTable

    StRnDHitCollection *ftscollection = event->rndHitCollection();

    std::vector<StRnDHit *> hits;

    // Prepare to loop over all hits
    const int nhits = hitTable->GetNRows();
    const g2t_fts_hit_st *hit = hitTable->GetTable();

    StarRandom &rand = StarRandom::Instance();

    float dx = STGC_SIGMA_X;
    float dy = STGC_SIGMA_Y;
    float dz = STGC_SIGMA_Z;

    int nSTGCHits = 0;
    sTGCNRealPoints = 0;
    sTGCNGhostPoints = 0;

    for (int i = 0; i < nhits; i++) {
        hit = (g2t_fts_hit_st *)hitTable->At(i);
        if (0 == hit)
            continue;

        float xhit = hit->x[0];
        float yhit = hit->x[1];
        float zhit = hit->x[2];
        int volume_id = hit->volume_id;
        int disk = (volume_id - 1) / 4 + 9 ; // add 7 to differentiat from FST - dedicated collection will not need 

        LOG_DEBUG << "sTGC hit: volume_id = " << volume_id << " disk = " << disk << endm;
        if (disk < 9)
            continue;

        float theta = DiskRotation(disk);

        float x_blurred = xhit + rand.gauss( STGC_SIGMA_X);
        float y_blurred = yhit + rand.gauss( STGC_SIGMA_Y);

        float x_rot = -999, y_rot = -999;
        this->rot(-theta, x_blurred, y_blurred, x_rot, y_rot);

        int quad = -1;
        float localX = -999, localY = -999;
        GlobalToLocal(x_rot, y_rot, disk, quad, localX, localY);

        // not in the active region
        if (quad < 0 || quad > 3)
            continue;
        nSTGCHits++;

        StRnDHit *ahit = new StRnDHit();

        ahit->setPosition({x_blurred, y_blurred, zhit});
        ahit->setPositionError({dx, dy, 0.1});

        ahit->setDouble0(xhit);
        ahit->setDouble1(yhit);
        ahit->setDouble2(x_rot);
        ahit->setDouble3(y_rot);

        ahit->setLayer(disk); // disk mapped to layer 
        ahit->setLadder(2);   // indicates a point
        ahit->setWafer(quad); // quadrant number

        ahit->setIdTruth(hit->track_p, 0);
        ahit->setDetectorId(kFtsId); // TODO: use dedicated ID for Ftt when StEvent is updated

        float Ematrix[] = {
            dx * dx, 0.f, 0.f,
            0.f, dy * dy, 0.f,
            0.f, 0, 0.f, dz * dz};

        ahit->setErrorMatrix(Ematrix);
        hits.push_back(ahit);

        if (!STGC_MAKE_GHOST_HITS) {
            // Make this "REAL" hit.
            if (FttGlobal::verbose){
                ahit->Print();
            }
            ftscollection->addHit(ahit);
            sTGCNRealPoints++;
        }
    }

    if (STGC_MAKE_GHOST_HITS) {
        for (auto &hit0 : hits) { // first loop on hits
            float hit0_x = hit0->double2();
            float hit0_y = hit0->double3();
            int disk0 = hit0->layer();
            int quad0 = hit0->wafer();
            float theta = DiskRotation(disk0);

            for (auto &hit1 : hits) { // second loop on hits
                float hit1_x = hit1->double2();
                float hit1_y = hit1->double3();
                int disk1 = hit1->layer();
                int quad1 = hit1->wafer();

                if (disk0 != disk1)
                    continue;
                if (quad0 != quad1)
                    continue;

                // check on overlapping segments
                if (false == Overlaps(hit0, hit1))
                    continue;

                float x = hit0_x;
                float y = hit1_y;

                int qaTruth = 0;
                int idTruth = 0;
                if (hit1_x == hit0_x && hit1_y == hit0_y) {
                    sTGCNRealPoints++;
                    qaTruth = 1;
                    idTruth = hit0->idTruth();
                } else {
                    sTGCNGhostPoints++;
                    qaTruth = 0;
                }

                float rx = -999, ry = -999;
                this->rot(theta, x, y, rx, ry);
                // the trick here is that rotations (in 2D) will commute
                // so the earlier -theta rotation and this +theta
                // rotation cancel for real hits
                // but not so for ghost hits

                StRnDHit *ahit = new StRnDHit();

                ahit->setPosition({rx, ry, hit0->position().z()});
                ahit->setPositionError({dx, dy, 0.1});

                ahit->setDouble0(hit0_x);
                ahit->setDouble1(hit0_y);
                ahit->setDouble2(hit1_x);
                ahit->setDouble3(hit1_y);

                ahit->setLayer(disk0); // disk mapped to layer
                ahit->setLadder(2);    // indicates a point
                ahit->setWafer(quad0); // quadrant number

                ahit->setIdTruth(idTruth, qaTruth);
                ahit->setDetectorId(kFtsId); // TODO: use dedicated ID for Ftt when StEvent is updated

                float Ematrix[] = {
                    dx * dx, 0.f, 0.f,
                    0.f, dy * dy, 0.f,
                    0.f, 0, 0.f, dz * dz};
                ahit->setErrorMatrix(Ematrix);
                ftscollection->addHit(ahit);

            } // loop hit1
        } // loop hit0


        // in this case the hits used in the original array were not saved, but copied so we need to delete them

        for ( size_t i = 0; i < hits.size(); i++ ){
            delete hits[i];
            hits[i] = nullptr;
        }
    } // make Ghost Hits

    if (FttGlobal::verbose) {
        LOG_INFO << "nHits (all FTS) = " << nhits << endm;
    }
    if (FttGlobal::verbose) {
        LOG_INFO << "nSTGC = " << nSTGCHits << endm;
    }
    if (FttGlobal::verbose) {
        LOG_INFO << "nReal = " << sTGCNRealPoints << endm;
    }
    if (FttGlobal::verbose) {
        LOG_INFO << "nGhost = " << sTGCNGhostPoints << endm;
    }

} // fillThinGap
