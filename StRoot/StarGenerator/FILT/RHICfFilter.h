#ifndef __RHICfFilter_h__
#define __RHICfFilter_h__

#include "TH2Poly.h"
/*!
  \class RHICfFilter
  \brief RHICf pi0 and Neutron events filter as RHICf Run type
 */

#include "StarFilterMaker.h"

class RHICfFilter : public StarFilterMaker
{
public:
  RHICfFilter( const char* name = "rhicfFilter" );
  virtual ~RHICfFilter();

  void SetRHICfRunType(int type); // [0=TS, 1=TL, 2=TOP]
  void SetHitMultiplicity(int hit); // default == 1

  Int_t Init() override;
  Int_t Filter( StarGenEvent *event = 0 ) override;

private:
  int InitRHICfGeometry();
  int GetRHICfGeoHit(double posX, double posY, double posZ, double px, double py, double pz, double e);

  bool IsNeutralParticle(int pid); 

  int mRHICfRunType; // [0=TL, 1=TS, 2=TOP]
  int mHitMultiplicity;

  TH2Poly* mRHICfPoly; // only west
  double mRHICfTowerBoundary[2][4][2]; // [TS, TL][bound square][x, y]
  double mRHICfTowerCenterPos[2]; // [TS, TL] y pos

  double mRHICfDetZ;// = 1780.; // [cm]

  ClassDef(RHICfFilter,0);
};

#endif
