#include "RHICfFilter.h"

#include "StarGenerator/EVENT/StarGenEvent.h"
#include "StarGenerator/EVENT/StarGenPPEvent.h"
#include "StarGenerator/EVENT/StarGenParticle.h"

RHICfFilter::RHICfFilter( const char* name ) 
: StarFilterMaker(name), 
  mRHICfRunType(RHICfRunType::NON),
  mHitMultiplicity(1),
  mRHICfPoly(nullptr)
{
}

RHICfFilter::~RHICfFilter() 
{
  delete mRHICfPoly;
  mRHICfPoly = nullptr;
}

void RHICfFilter::SetRHICfRunType(int type)
{
  if (RHICfRunType::TL <= type && type <= RHICfRunType::TOP) 
    {
      mRHICfRunType = type;
    }
}

void RHICfFilter::SetHitMultiplicity(int hit)
{
  // Allowed range for hit multiplicity is [0,4]. Values outside this range
  // are reset to 0. This preserves existing behavior but may indicate a
  // configuration issue at the call site.
  mHitMultiplicity = hit;
  if (mHitMultiplicity < 0 || mHitMultiplicity > 4) {
    LOG_WARN << "RHICfFilter::SetHitMultiplicity received out-of-range value "
             << hit << ", resetting to 0." << endm;
    mHitMultiplicity = 0;
  }
}

Int_t RHICfFilter::Init()
{
  if (!InitRHICfGeometry()){return kStErr;}
  return kStOk;
}

Int_t RHICfFilter::Filter( StarGenEvent *_event ) 
{
  // Get a reference to the current event
  StarGenEvent& event = (_event)? *_event : *mEvent;
  StarGenParticle* part;

  int hitTrackNum = 0;
  // Loop over tracks to find particles of interest
  int npart = event.GetNumberOfParticles();
  for ( int ipart=1; ipart<npart; ++ipart ){ // skip header
    part = event[ipart];

    if (abs(part->GetId()) < 10){continue;}
    if (part -> GetStatus() != StarGenParticle::kFinal){continue;}

    int pid = part -> GetId();
    double posX = part -> GetVx();
    double posY = part -> GetVy();
    double posZ = part -> GetVz();
    double px = part -> GetPx();
    double py = part -> GetPy();
    double pz = part -> GetPz();
    double e = part -> GetEnergy();

    // Particle selections
    pid = abs(pid);
    if ( 11 < pid && pid < 19 ){continue;} // lepton cut (except electron)
    if ( e  < 1. ){continue;} // energy cut 1 GeV
    if ( pz <= 0. ){continue;} // opposite side cut

    bool isInterestedParticle = IsInterestedParticle(pid);
    
    // cut the final state charged particle generated Z-position before DX magnet
    if ( !isInterestedParticle && posZ < 1500. ){continue;}

    int hit = GetRHICfGeoHit(posX, posY, posZ, px, py, pz, e);
    if (hit < 0){continue;}

    hitTrackNum++;
  }
  if(mHitMultiplicity <= hitTrackNum){
    LOG_INFO << "RHICf particles detected. "<< hitTrackNum << endm;
    return StarGenEvent::kAccept;
  }
  return StarGenEvent::kReject;
}

int RHICfFilter::InitRHICfGeometry()
{
  if(mRHICfRunType == RHICfRunType::NON){
    LOG_WARN << "RHICfFilter::InitRHICfGeometry() warning!!! RHICf run type is not set!!!" << endm;
    return kStErr;
  }

  double distTowersCenter = 4.74; // [cm], Distance between the center of small and large tower
  double towerPosYByRun = 0.; // [cm], RHICf detector y-axis shift position by run types with respect to global coordinate
  if(mRHICfRunType == RHICfRunType::TL){towerPosYByRun = -4.74;}
  if(mRHICfRunType == RHICfRunType::TS){towerPosYByRun = 0.;}
  if(mRHICfRunType == RHICfRunType::TOP){towerPosYByRun = 2.16;}

  mRHICfPoly = new TH2Poly();
  mRHICfPoly -> SetName("RHICfPoly");
  mRHICfPoly -> SetStats(0);

  double x[4], y[4];
  for(int tower=0; tower<2; tower++){
    double towerGlobalPosY = towerPosYByRun;
    if(tower == RHICfTower::LargeTower){towerGlobalPosY += distTowersCenter;}

    for(int boundary=0; boundary<4; boundary++){
      x[boundary] = GetRHICfDetectorBoundary(tower, 0, boundary);
      y[boundary] = towerGlobalPosY + GetRHICfDetectorBoundary(tower, 1, boundary);
    }
    mRHICfPoly -> AddBin(4, x, y);
  }

  return kStOk;
}

int RHICfFilter::GetRHICfGeoHit(double posX, double posY, double posZ, double px, double py, double pz, double e)
{
  double momMag = sqrt(px*px + py*py + pz*pz);    assert(momMag>0.0);
  double unitVecX = px/momMag;
  double unitVecY = py/momMag;
  double unitVecZ = pz/momMag;                   

  double detPosZ = 1780.; // [cm]
  double z = detPosZ - posZ;
  if(z < 0.){return -1;} // create z-position cut

  double x = z * (unitVecX/unitVecZ) + posX;
  double y = z * (unitVecY/unitVecZ) + posY;

  int type = mRHICfPoly -> FindBin(x, y);
  if(type < 1 || type > 2){return -1;} // RHICf geometrical hit cut

  return type;
} 

bool RHICfFilter::IsInterestedParticle(int pid)
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

double RHICfFilter::GetRHICfDetectorBoundary(int towerIdx, int xyIdx, int boundaryIdx)
{
  const double SQRT2 = sqrt(2);
  double detectorSize = 0.;
  if(towerIdx == RHICfTower::SmallTower){detectorSize = 2.;} // [cm]
  else if(towerIdx == RHICfTower::LargeTower){detectorSize = 4.;} // [cm]
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