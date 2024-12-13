#ifndef __RHICfFilter_h__
#define __RHICfFilter_h__

#include <vector>
#include "TH2Poly.h"

/*!
  \class RHICfFilter
  \brief RHICf pi0 and Neutron events filter as RHICf Run type
 */

#include "StarFilterMaker.h"

class RHICfFilter : public StarFilterMaker
{
public:
  RHICfFilter( const char* name = "rhicffilt" );
  virtual ~RHICfFilter();

  void SetRHICfRunType(int type); // [0=TS, 1=TL, 2=TOP]
  void SetEnergyCut(double val);
  void SetOnlyPi0();
  void SetOnlyNeutron();

  int Init();
  int Filter( StarGenEvent *event = 0 );

private:
  int InitRHICfGeometry();
  int GetRHICfGeoHit(double posX, double posY, double posZ, double px, double py, double pz, double e);
  void ClearEvent(); // clear the temporary array in events

  int mRHICfRunType; // [0=TS, 1=TL, 2=TOP]
  bool mIsOnlyPi0;
  bool mIsOnlyNeutron;

  TH2Poly* mRHICfPoly; // only west
  double mRHICfTowerBoundary[2][4][2]; // [TS, TL][bound square][x, y]
  double mRHICfTowerCenterPos[2]; // [TS, TL] y pos

  vector<int> mRHICfGammaIdx;
  bool mIsPi0Event;
  bool mIsNeuEvent;

  double mEnergyCut; // [GeV]
  double mRHICfDetZ = 1780.; // [cm]

  ClassDef(RHICfFilter,0);
};

#endif