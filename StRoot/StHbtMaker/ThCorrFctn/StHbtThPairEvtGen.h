/***************************************************************************
 *
 * $Id: 
 *
 * Author: Laurent Conin, Fabrice Retiere, Subatech, France
 ***************************************************************************
 *
 * Description : ThPair wich return :
 *               RealMomentum : HiddenInfo Momentum
 *               EmPoint : HiddenInfo EmPoint
 *               Pid :  HiddenInfo Pid
 *               MeasMomentum : Particle Momentum
 *
 ***************************************************************************
 *
 * $Log: 
 *
 ***************************************************************************/

#ifndef StHbtThPairEvtGen_hh
#define StHbtThPairEvtGen_hh

#include "StHbtMaker/Infrastructure/StHbtPair.hh"
#include "StHbtMaker/Base/StHbtThPair.hh"


class StHbtThPairEvtGen : public StHbtThPair{
public:
  StHbtThPairEvtGen();
  ~StHbtThPairEvtGen() {};
  virtual void Set(const StHbtPair* aPair);

#ifdef __ROOT__
  ClassDef(StHbtThPairEvtGen,1)
#endif

};

#endif
