/***************************************************************************
 *
 * $Id: 
 *
 * Author: Laurent Conin, Fabrice Retiere, Subatech, France
 ***************************************************************************
 *
 * Description : used by default by ThCorrFctn, so that ThCorrFctn can
 *               be used as a normal CorrFctn (i.e pluged in Analysis)
 *
 ***************************************************************************
 *
 * $Log: 
 *
 ***************************************************************************/

#ifndef StHbtThPairDummy_hh
#define StHbtThPairDummy_hh


#include "StHbtMaker/Infrastructure/StHbtPair.hh"
#include "StHbtMaker/Base/StHbtThPair.hh"

class StHbtThPairDummy : public StHbtThPair{
public:
  StHbtThPairDummy();
  virtual void Set(const StHbtPair* aPair);
  
  void UpdateWeight();
  virtual void setVariables(const StHbtPair*);
};

#endif
