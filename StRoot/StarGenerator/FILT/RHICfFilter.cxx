#include "RHICfFilter.h"

#include "StarGenerator/EVENT/StarGenEvent.h"
#include "StarGenerator/EVENT/StarGenPPEvent.h"
#include "StarGenerator/EVENT/StarGenParticle.h"

RHICfFilter::RHICfFilter( const char* name ) 
: StarFilterMaker(name), mRHICfRunType(-1), mIsOnlyPi0(0), mIsOnlyNeutron(0), mIsPi0Event(0), mIsNeuEvent(0), mEnergyCut(20.)
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

void RHICfFilter::SetEnergyCut(double val){mEnergyCut = val;}
void RHICfFilter::SetOnlyPi0(){mIsOnlyPi0 = true;}
void RHICfFilter::SetOnlyNeutron(){mIsOnlyNeutron = true;}

int RHICfFilter::Init()
{
  if(!InitRHICfGeometry()){return kStErr;}
  if(mIsOnlyPi0 && mIsOnlyNeutron){return kStErr;}
  return kStOk;
}

int RHICfFilter::Filter( StarGenEvent *_event ) 
{
  // Get a reference to the current event
  StarGenEvent& event = (_event)? *_event : *mEvent;

  ClearEvent();
  StarGenParticle *part;

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

    int hit = GetRHICfGeoHit(posX, posY, posZ, px, py, pz, e);
    if(hit < 0){continue;}

    if(pid == 22){ // for RHICf gamma
      mRHICfGammaIdx.push_back(ipart);
    }
    if(pid == 2112){ // for RHICf neutron
      mIsNeuEvent = true;
    }
  }

  // Find a RHICf pi0 events
  int RHICfgammaNum = mRHICfGammaIdx.size();
  for(int i=0; i<RHICfgammaNum; i++){
    int gamma1Idx = mRHICfGammaIdx[i];
    part = event[gamma1Idx];

    int gamma1MotherIdx = part -> GetFirstMother();

    part = event[gamma1MotherIdx];
    int gamma1MotherPID = part -> GetId();
    int gamma1MotherNumber = part -> GetIndex();

    if(gamma1MotherPID != 111){continue;}

    for(int j=i+1; j<RHICfgammaNum; j++){
      int gamma2Idx = mRHICfGammaIdx[j];
      part = event[gamma2Idx];

      int gamma2MotherIdx = part -> GetFirstMother();

      part = event[gamma2MotherIdx];
      int gamma2MotherPID = part -> GetId();
      int gamma2MotherNumber = part -> GetIndex();

      if(gamma2MotherPID != 111){continue;}

      if(gamma1MotherNumber == gamma2MotherNumber){
        mIsPi0Event = true;
      }
    }
  }

  if(!mIsOnlyPi0 && !mIsOnlyNeutron){
    if(mIsPi0Event || mIsNeuEvent){
      return StarGenEvent::kAccept;
    }
  }
  else if(mIsOnlyPi0 && mIsPi0Event){
    return StarGenEvent::kAccept;
  }
  else if(mIsOnlyNeutron && mIsNeuEvent){
    return StarGenEvent::kAccept;
  }
  return StarGenEvent::kReject;
}

int RHICfFilter::InitRHICfGeometry()
{
  double tsDetSize = 2.; // [cm]
  double tlDetSize = 4.; // [cm]
  double detBoundCut = 0.2; // [cm]
  double distTStoTL = 4.74; // [cm]

  double detBeamCenter = 0.; // [cm]

  if(mRHICfRunType < 0 || mRHICfRunType > 2){
    cout << "RHICfFilter::InitRHICfGeometry() warning!!! RHICf run type is not setted!!!" << endl;
    return 0;
  }
  if(mRHICfRunType == 0){detBeamCenter = 0.;} // TS
  if(mRHICfRunType == 1){detBeamCenter = -4.74;} // TL
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
  if(e < mEnergyCut){return -1;} // energy cut

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

void RHICfFilter::ClearEvent()
{
  mRHICfGammaIdx.clear();
  mIsPi0Event = false;
  mIsNeuEvent = false;
}