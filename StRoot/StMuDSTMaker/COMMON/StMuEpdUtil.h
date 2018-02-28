/****************************************************************
 *
 * Author: Mike Lisa
 *
 *****************************************************************
 *
 * Description:
 * Convert StEpdHitCollection in StEvent to 
 * StMuEpditCollection in MuDst
 *
 *****************************************************************
 *
 *
 ****************************************************************/
#ifndef StMuEpdUtil_h
#define StMuEpdUtil_h
#include "TObject.h"

class StMuEpdHitCollection;
class StEvent;
class StEpdCollection;

class StMuEpdUtil : public TObject
{
 protected:
 
 public:
  StMuEpdUtil();
  ~StMuEpdUtil();
  StMuEpdHitCollection* getMuEpdHit(StEpdCollection *);
  void fillMuEpdHit(StMuEpdHitCollection*, StEpdCollection*);
  
  ClassDef(StMuEpdUtil,1)
};
    
#endif
