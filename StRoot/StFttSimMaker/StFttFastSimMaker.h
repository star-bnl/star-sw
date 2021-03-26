
#ifndef ST_FTT_FAST_SIM_MAKER_H
#define ST_FTT_FAST_SIM_MAKER_H

class g2t_emc_hit_st;
class StFtsHit;
class StEvent;

#include "StChain/StMaker.h"
#include <vector>

#include "TFile.h"
#include "TH1F.h"
#include "TH2F.h"
#include "TNtuple.h"

class StRnDHit;

class StFttFastSimMaker : public StMaker {
  public:
    explicit StFttFastSimMaker(const Char_t *name = "fttSim");
    virtual ~StFttFastSimMaker() {}
    Int_t Make();
    int Init();
    int Finish() {
        return kStOk;
    }
    virtual const char *GetCVS() const;

    void SetDiskRotation(int disk, float degrees) {

        const float deg_to_radians = 0.017453292f; // = 3.1415926 / 180.0;
        if (9 == disk)
            sTGC_disk9_theta = degrees * deg_to_radians;
        else if (10 == disk)
            sTGC_disk10_theta = degrees * deg_to_radians;
        else if (11 == disk)
            sTGC_disk11_theta = degrees * deg_to_radians;
        else if (12 == disk)
            sTGC_disk12_theta = degrees * deg_to_radians;
        return;
    }

  private:
    void FillThinGapChambers(StEvent *event);

    int iEvent;

    TH2F *hGlobalYX;
    TH2F *hOctantYX;

    TH2F *hOctantWireYX;
    TH2F *hOctantStripYX;

    TH2F *hWireDeltasX;
    TH2F *hWireDeltasY;
    TH2F *hStripDeltasX;
    TH2F *hStripDeltasY;

    TH2F *hWirePullsX;
    TH2F *hWirePullsY;
    TH2F *hStripPullsX;
    TH2F *hStripPullsY;

    TH2F *hPointsPullsX;
    TH2F *hPointsPullsY;

    //table to keep pointer to hit for each disc, r & phi strips

    // convert x, y to quandrant and local X, Y
    // quadrants are
    // 0 1
    // 3 2
    void GlobalToLocal(float x, float y, int disk, int &quad, float &localX, float &localY);
    void LocalToGlobal(float localX, float localY, int disk, int quad, float &globalX, float &globalY);
    bool Overlaps(StRnDHit *hitA, StRnDHit *hitB);
    void QuadBottomLeft(int disk, int quad, float &bottom, float &left);
    float DiskOffset(int disk);
    float DiskRotation(int disk);

    void rot(float theta, float x, float y, float &xp, float &yp) {
        xp = x * cos(theta) - y * sin(theta);
        yp = x * sin(theta) + y * cos(theta);
    }

    const double STGC_BEAM_CUT_OUT = 6.0;   // cm
    const double STGC_QUAD_WIDTH = 60.0;    // cm
    const double STGC_QUAD_HEIGHT = 60.0;   // cm
    const double STGC_WIRE_WIDTH = 0.32;    // cm
    const double STGC_SIGMA_X = 0.01;       // 100 microns
    const double STGC_SIGMA_Y = 0.01;       // 100 microns
    const double STGC_SIGMA_Z = 0.001;      // 10 microns
    const double STGC_WIRE_LENGTH = 15.0;   // cm
    const bool STGC_MAKE_GHOST_HITS = true; //should be moved to run-time opt

    float sTGC_disk9_theta = 0.0f;
    float sTGC_disk10_theta = 0.0f;
    float sTGC_disk11_theta = 0.0f;
    float sTGC_disk12_theta = 0.0f;

    int sTGCNRealPoints = 0;
    int sTGCNGhostPoints = 0;

    ClassDef(StFttFastSimMaker, 0)
};

inline const char *StFttFastSimMaker::GetCVS() const {
    static const char cvs[] = "Tag $Name:  $ $Id: StFttFastSimMaker.h,v 1.1 2021/03/26 14:11:40 jdb Exp $ built " __DATE__ " " __TIME__;
    return cvs;
}

#endif
