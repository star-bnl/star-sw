/***************************************************************************
 
 **************************************************************************/
#ifndef StTrackForPool_hh
#define StTrackForPool_hh
#include "StThreeVectorD.hh"
#include <TObject.h> 

class StTrackForPool : public TObject {
public:
 StThreeVectorD mMomentum;
 long mEventNumber;

 void setTrackForPool(StThreeVectorD momentum, long event); 
 
protected:
private:
};


#endif
