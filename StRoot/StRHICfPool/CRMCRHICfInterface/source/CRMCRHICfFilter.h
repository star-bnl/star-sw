#ifndef _CRMCRHICfFilter_h_
#define _CRMCRHICfFilter_h_

#include <stdexcept>

#include "CRMCRHICfOption.h"
#include "TH2Poly.h"

class CRMCRHICfFilter 
{
    enum RHICfTower
    {
        SmallTower = 0,
        LargeTower = 1
    };

    public:
        CRMCRHICfFilter(int runType=CRMCRHICfOption::RunType::ALL);
        ~CRMCRHICfFilter();

        void Init();

        bool IsInterestedParticle(int pid);
        int GetRHICfGeoHit(double posX, double posY, double posZ, double px, double py, double pz, double e);

    private:
        double GetRHICfDetectorBoundary(int towerIdx, int xyIdx, int boundaryIdx);

        Int_t fRHICfRunType;
        TH2Poly* fRHICfPoly; // only west

};


#endif
