/***************************************************************************
 
 **************************************************************************/
#ifndef StTrackForPool_hh
#define StTrackForPool_hh
#include "StThreeVector.hh"
#include <TObject.h> 

class StTrackForPool : public TObject {
public:
 StThreeVector<double> mMomentum;
 long mEventNumber;

 void setTrackForPool(StThreeVector<double> momentum, long event); 
 
protected:
private:
};


#endif
