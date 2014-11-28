/***************************************************************************
 *
 * $Id: 
 *
 * Author: Laurent Conin, Fabrice Retiere, Subatech, France
 ***************************************************************************
 *
 * Description : pure abstract theoretical correlation function
 *
 ***************************************************************************
 *
 * $Log: 
 *
 ***************************************************************************/
#ifndef StHbtThCorrFctn_hh
#define StHbtThCorrFctn_hh

#include "StHbtMaker/Base/StHbtCorrFctn.hh"
#include "StHbtMaker/Infrastructure/StHbtNamed.hh"
#include "StHbtMaker/Infrastructure/StHbtPair.hh"
#include "StHbtMaker/Base/StHbtThPair.hh"
#include "StHbtMaker/ThCorrFctn/StHbtThPairDummy.h"

class StHbtThCorrFctn : public virtual StHbtCorrFctn ,public virtual StHbtNamed{

public:
// --- Constructor
  StHbtThCorrFctn() :StHbtCorrFctn(),StHbtNamed() {};
  StHbtThCorrFctn(const char* aName) :StHbtCorrFctn(),StHbtNamed(aName) {};
  StHbtThCorrFctn(const StHbtThCorrFctn& aCf ) :StHbtCorrFctn(aCf),StHbtNamed(aCf) {};
  virtual ~StHbtThCorrFctn() {};


  void AddRealPair(const StHbtPair*);
  void AddMixedPair(const StHbtPair*);

  virtual void AddNum(StHbtThPair*)=0;
  virtual void AddDen(StHbtThPair*)=0;

  virtual StHbtThCorrFctn* ThClone() const =0;


protected:
  StHbtThPairDummy mDefThPair;//!

};

inline void StHbtThCorrFctn::AddRealPair(const StHbtPair* aPair) {
  mDefThPair.Set(aPair);
  AddNum(&mDefThPair);
}
inline void StHbtThCorrFctn::AddMixedPair(const StHbtPair* aPair) {
  mDefThPair.Set(aPair);
  AddDen(&mDefThPair);
}

#endif
