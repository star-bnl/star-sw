#ifndef StMuTofUtil_h
#define StMuTofUtil_h
#include "TObject.h"

class StMuTofHitCollection;
class StEvent;
class StTofCollection;

class StMuTofUtil : public TObject
{
 protected:
 
 public:
  StMuTofUtil();
  ~StMuTofUtil();
  StMuTofHitCollection* getMuTofHit(StTofCollection *);
  void fillMuTofHit(StMuTofHitCollection*, StTofCollection*);
  
  ClassDef(StMuTofUtil,1)
};
    
#endif


