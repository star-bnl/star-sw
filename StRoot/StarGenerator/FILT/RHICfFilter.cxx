#include "RHICfFilter.h"

#include "StarGenerator/EVENT/StarGenEvent.h"
#include "StarGenerator/EVENT/StarGenPPEvent.h"
#include "StarGenerator/EVENT/StarGenParticle.h"

RHICfFilter::RHICfFilter( const char* name ) 
: StarFilterMaker(name), 
  mRHICfRunType(-1),
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
  if (0 <= type && type <= 2) 
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
    if(part -> GetStatus() != StarGenParticle::kFinal){continue;}

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
    if( 11 < pid && pid < 19 ){continue;} // lepton cut (except electron)
    if ( e  < 1. ){continue;} // energy cut 1 GeV
    if ( pz <= 0. ){continue;} // opposite side cut

    bool isInterestedParticle = IsInterestedParticle(pid);
    
    // cut the final state charged particle generated Z-position before DX magnet
    if( !isInterestedParticle && posZ < 1500. ){continue;}

    int hit = GetRHICfGeoHit(posX, posY, posZ, px, py, pz, e);
    if(hit < 0){continue;}

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
  double tsDetSize = 2.; // [cm]
  double tlDetSize = 4.; // [cm]
  double detBoundCut = 0.0; // [cm]
  double distTStoTL = 4.74; // [cm]

  double detBeamCenter = 0.; // [cm]

  if(mRHICfRunType < 0 || mRHICfRunType > 2){
    LOG_WARN << "RHICfFilter::InitRHICfGeometry() warning!!! RHICf run type is not set!!!" << endm;
    return 0;
  }
  if(mRHICfRunType == 0){detBeamCenter = -4.74;} // TL
  if(mRHICfRunType == 1){detBeamCenter = 0.;} // TS
  if(mRHICfRunType == 2){detBeamCenter = 2.16;} // TOP

  double towerBoundary[2][4][2]; // [TS, TL][bound square][x, y]
  double towerCenterPos[2]; // [TS, TL] y pos

  towerBoundary[0][0][0] = sqrt(2)*((tsDetSize - detBoundCut*2.)/2.); 
  towerBoundary[0][0][1] = 0.;
  towerBoundary[0][1][0] = 0.; 
  towerBoundary[0][1][1] = sqrt(2)*((tsDetSize - detBoundCut*2.)/2.); 
  towerBoundary[0][2][0] = -1.*sqrt(2)*((tsDetSize - detBoundCut*2.)/2.); 
  towerBoundary[0][2][1] = 0.; 
  towerBoundary[0][3][0] = 0.; 
  towerBoundary[0][3][1] = -1.*sqrt(2)*((tsDetSize - detBoundCut*2.)/2.); 

  towerBoundary[1][0][0] = sqrt(2)*((tlDetSize - detBoundCut*2.)/2.);
  towerBoundary[1][0][1] = 0.;
  towerBoundary[1][1][0] = 0.;
  towerBoundary[1][1][1] = sqrt(2)*((tlDetSize - detBoundCut*2.)/2.);
  towerBoundary[1][2][0] = -1.*sqrt(2)*((tlDetSize - detBoundCut*2.)/2.);
  towerBoundary[1][2][1] = 0.;
  towerBoundary[1][3][0] = 0.;
  towerBoundary[1][3][1] = -1.*sqrt(2)*((tlDetSize - detBoundCut*2.)/2.);
  
  towerCenterPos[0] = detBeamCenter;
  towerCenterPos[1] = distTStoTL + detBeamCenter;

  mRHICfPoly = new TH2Poly();
  mRHICfPoly -> SetName("RHICfPoly");
  mRHICfPoly -> SetStats(0);

  double x[4];
  double y[4];

  for(int t=0; t<2; t++){
    for(int i=0; i<4; i++){
      double xPos = towerBoundary[t][i][0];
      double yPos = towerCenterPos[t] + towerBoundary[t][i][1];
      x[i] = xPos;
      y[i] = yPos;
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
