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
  ~RHICfFilter();

  void SetRHICfRunType(int type); 
  void SetHitMultiplicity(int hit); // default == 1

  virtual Int_t Init();
  virtual Int_t Filter( StarGenEvent *event = 0 );
  
private:
  int InitRHICfGeometry();
  int GetRHICfGeoHit(double posX, double posY, double posZ, double px, double py, double pz, double e);

  bool IsInterestedParticle(int pid); 

  int mRHICfRunType;
  int mHitMultiplicity;

  TH2Poly* mRHICfPoly; // only west

  enum RHICfRunType
  {
    TL = 0,
    TS = 1,
    TOP = 2,
    NON = -1
  };

  ClassDef(RHICfFilter,0);
};

#endif
