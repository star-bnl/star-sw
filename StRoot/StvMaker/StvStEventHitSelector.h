//StvStEventHitSelector.h

#ifndef StvStEventHitSelector_HH
#define StvStEventHitSelector_HH


#include "TNamed.h"
class StEvent;

class StvStEventHitSelector : public TNamed 
{
 public:
 enum  {kMarked = 1};  
 public:
    StvStEventHitSelector(const char* name = "HitSelector");
    int Edit(const StEvent *evt);
    void SetPtMin(double pt) { mPtMin = pt;}
private:
double mPtMin;

ClassDef(StvStEventHitSelector,0)
};

#endif
