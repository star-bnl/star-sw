#include "CRMCRHICfFilter.h"

CRMCRHICfFilter::CRMCRHICfFilter(int runType)
: fRHICfRunType(runType), fRHICfPoly(nullptr)
{
}

CRMCRHICfFilter::~CRMCRHICfFilter()
{
    delete fRHICfPoly;
    fRHICfPoly = nullptr;
}

void CRMCRHICfFilter::Init()
{
    double distTowersCenter = 47.4; // [mm], Distance between the center of small and large tower
    double towerPosYByRun = 0.; // [mm], RHICf detector y-axis shift position by run types with respect to global coordinate
    if(fRHICfRunType == CRMCRHICfOption::RunType::TL){towerPosYByRun = -47.4;}
    if(fRHICfRunType == CRMCRHICfOption::RunType::TS){towerPosYByRun = 0.;}
    if(fRHICfRunType == CRMCRHICfOption::RunType::TOP){towerPosYByRun = 21.6;}

    fRHICfPoly = new TH2Poly();
    fRHICfPoly -> SetName("RHICfDetector");
    fRHICfPoly -> SetStats(0);

    double x[4], y[4];
    for(int tower=0; tower<2; tower++){
        double towerGlobalPosY = towerPosYByRun;
        if(tower == RHICfTower::LargeTower){towerGlobalPosY += distTowersCenter;}

        for(int boundary=0; boundary<4; boundary++){
            x[boundary] = GetRHICfDetectorBoundary(tower, 0, boundary);
            y[boundary] = towerGlobalPosY + GetRHICfDetectorBoundary(tower, 1, boundary);
        }
        fRHICfPoly -> AddBin(4, x, y);
    }
    if(!fRHICfPoly){throw std::runtime_error("RHICf geometry is not initialized");}
}

bool CRMCRHICfFilter::IsInterestedParticle(int pid)
{
    // only listed for final state particles
    int pdg = abs(pid);
    switch(pdg)
    {
        case 2112: return true; // n
        case 130 : return true; // K0_L
        case 22  : return true; // gamma
        
        default  : return false; // other charged and uninterested particles
    }
    return false;
}

int CRMCRHICfFilter::GetRHICfGeoHit(double posX, double posY, double posZ, double px, double py, double pz, double e)
{
    if(fRHICfRunType == CRMCRHICfOption::RunType::ALL){
        return 1;
    }
    double momMag = sqrt(px*px + py*py + pz*pz);
    double unitVecX = px/momMag;
    double unitVecY = py/momMag;
    double unitVecZ = pz/momMag;

    double detPosZ = 17800.; // [mm]
    double z = detPosZ - posZ;
    if(z < 0.){return -1;} // create z-position cut

    double x = z * (unitVecX/unitVecZ) + posX;
    double y = z * (unitVecY/unitVecZ) + posY;

    int type = fRHICfPoly -> FindBin(x, y);
    if(type < 1 || type > 2){return -1;} // RHICf geometrical hit cut

    return type;
}

double CRMCRHICfFilter::GetRHICfDetectorBoundary(int towerIdx, int xyIdx, int boundaryIdx)
{
    const double SQRT2 = sqrt(2);
    double detectorSize = 0.;
    if(towerIdx == RHICfTower::SmallTower){detectorSize = 20.;} // [mm]
    else if(towerIdx == RHICfTower::LargeTower){detectorSize = 40.;} // [mm]
    else{return -999.;}

    if(boundaryIdx < 0 || boundaryIdx > 3){return -999.;}
    bool isEvenNumber = (boundaryIdx%2 == 0)? true : false;

    double valueSign = (boundaryIdx < 2)? 1. : -1.;
    double detectorBoundaryPoint = valueSign * SQRT2 * detectorSize/2.;

    if(xyIdx == 0){ // x-axis
        if(isEvenNumber){ return detectorBoundaryPoint; }// even number
        else{ return 0.; }// odd number
    }
    else if(xyIdx == 1){ // y-axis
        if(isEvenNumber){ return 0.; } // even number
        else{ return detectorBoundaryPoint; }// odd number
    }

    return -999.;
}
