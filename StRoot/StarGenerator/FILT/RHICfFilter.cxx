#include "RHICfFilter.h"

#include "StarGenerator/EVENT/StarGenEvent.h"
#include "StarGenerator/EVENT/StarGenPPEvent.h"
#include "StarGenerator/EVENT/StarGenParticle.h"

RHICfFilter::RHICfFilter( const char* name ) 
: StarFilterMaker(name), mRHICfRunType(-1), mHitMultiplicity(1)
{
  mRHICfPoly = 0; 
  memset(mRHICfTowerBoundary, 0, sizeof(mRHICfTowerBoundary));
  memset(mRHICfTowerCenterPos, 0, sizeof(mRHICfTowerCenterPos));
}

RHICfFilter::~RHICfFilter()
{
  if(mRHICfPoly){
    delete mRHICfPoly;
    mRHICfPoly = 0;
  }
}

void RHICfFilter::SetRHICfRunType(int type)
{
  if(0 <= type && type <= 2){
    mRHICfRunType = type;
  }
}

void RHICfFilter::SetHitMultiplicity(int hit)
{
  mHitMultiplicity = hit;
  if(mHitMultiplicity < 0 || mHitMultiplicity > 4){
    mHitMultiplicity = 0;
  }
}

int RHICfFilter::Init()
{
  if(!InitRHICfGeometry()){return kStErr;}
  return kStOk;
}

int RHICfFilter::Filter( StarGenEvent *_event ) 
{
  // Get a reference to the current event
  StarGenEvent& event = (_event)? *_event : *mEvent;
  StarGenParticle *part;

  int hitTrackNum = 0;
  // Loop over tracks to find particles of interest
  int npart = event.GetNumberOfParticles();
  for ( int ipart=1; ipart<npart; ipart++ ){
    part = event[ipart];

    if (TMath::Abs(part->GetId()) < 10){continue;}
    if(part -> GetStatus() != StarGenParticle::kFinal){continue;}

    int pid = part -> GetId();
    double posX = part -> GetVx();
    double posY = part -> GetVy();
    double posZ = part -> GetVz();
    double px = part -> GetPx();
    double py = part -> GetPy();
    double pz = part -> GetPz();
    double e = part -> GetEnergy();

    // neutrino particle cut
    if(11 < abs(pid) && abs(pid) < 19 ){continue;}

    bool isNeutral = IsNeutralParticle(pid);
    
    // cut the final state charged particle generated Z-position before end of DX magnet
    if(!isNeutral && posZ < 1500. && pz > 0){continue;}

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
    cout << "RHICfFilter::InitRHICfGeometry() warning!!! RHICf run type is not setted!!!" << endl;
    return 0;
  }
  if(mRHICfRunType == 0){detBeamCenter = -4.74;} // TL
  if(mRHICfRunType == 1){detBeamCenter = 0.;} // TS
  if(mRHICfRunType == 2){detBeamCenter = 2.16;} // TOP

  mRHICfTowerBoundary[0][0][0] = sqrt(2)*((tsDetSize - detBoundCut*2.)/2.); 
  mRHICfTowerBoundary[0][0][1] = 0.;
  mRHICfTowerBoundary[0][1][0] = 0.; 
  mRHICfTowerBoundary[0][1][1] = sqrt(2)*((tsDetSize - detBoundCut*2.)/2.); 
  mRHICfTowerBoundary[0][2][0] = -1.*sqrt(2)*((tsDetSize - detBoundCut*2.)/2.); 
  mRHICfTowerBoundary[0][2][1] = 0.; 
  mRHICfTowerBoundary[0][3][0] = 0.; 
  mRHICfTowerBoundary[0][3][1] = -1.*sqrt(2)*((tsDetSize - detBoundCut*2.)/2.); 

  mRHICfTowerBoundary[1][0][0] = sqrt(2)*((tlDetSize - detBoundCut*2.)/2.);
  mRHICfTowerBoundary[1][0][1] = 0.;
  mRHICfTowerBoundary[1][1][0] = 0.;
  mRHICfTowerBoundary[1][1][1] = sqrt(2)*((tlDetSize - detBoundCut*2.)/2.);
  mRHICfTowerBoundary[1][2][0] = -1.*sqrt(2)*((tlDetSize - detBoundCut*2.)/2.);
  mRHICfTowerBoundary[1][2][1] = 0.;
  mRHICfTowerBoundary[1][3][0] = 0.;
  mRHICfTowerBoundary[1][3][1] = -1.*sqrt(2)*((tlDetSize - detBoundCut*2.)/2.);
  
  mRHICfTowerCenterPos[0] = detBeamCenter;
  mRHICfTowerCenterPos[1] = distTStoTL + detBeamCenter;

  mRHICfPoly = new TH2Poly();
  mRHICfPoly -> SetName("RHICfPoly");
  mRHICfPoly -> SetStats(0);

  double x[4];
  double y[4];

  for(int t=0; t<2; t++){
    for(int i=0; i<4; i++){
      double xPos = mRHICfTowerBoundary[t][i][0];
      double yPos = mRHICfTowerCenterPos[t] + mRHICfTowerBoundary[t][i][1];
      x[i] = xPos;
      y[i] = yPos;
    }
    mRHICfPoly -> AddBin(4, x, y);
  }

  if(!mRHICfPoly){return 0;}
  return 1;
}

int RHICfFilter::GetRHICfGeoHit(double posX, double posY, double posZ, double px, double py, double pz, double e)
{
  if(e < 1.){return -1;} // energy cut 1 GeV

  double momMag = sqrt(px*px + py*py + pz*pz);
  double unitVecX = px/momMag;
  double unitVecY = py/momMag;
  double unitVecZ = pz/momMag;

  if(unitVecZ < 0){return -1;} // opposite side cut

  double z = mRHICfDetZ - posZ;
  if(z < 0.){return -1;} // create z-position cut

  double x = z * (unitVecX/unitVecZ) + posX;
  double y = z * (unitVecY/unitVecZ) + posY;

  int type = mRHICfPoly -> FindBin(x, y);
  if(type < 1 || type > 2){return -1;} // RHICf geometrical hit cut

  return type;
} 

bool RHICfFilter::IsNeutralParticle(int pid)
{
  // only listed for final state particles
  int pdg = abs(pid);
  switch(pdg)
  {
    case 2212: return false; // p
    case 11  : return false; // e
    case 321 : return false; // charged K
    case 211 : return false; // charged pi
    case 2112: return true; // n
    case 130 : return true; // K0_L
    case 22  : return true; // gamma

    default  : return false;
  }
  return false;
}
