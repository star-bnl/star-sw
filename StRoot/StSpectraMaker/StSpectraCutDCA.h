#ifndef StSpectraCutDCA_hh
#define StSpectraCutDCA_hh
#include "StEventTypes.h"
#include "StSpectraCut.h"

class StSpectraCutDCA : public StSpectraCut {

 private:

 protected:

 public:

  StSpectraCutDCA(double lowLimit, double highLimit);

  ~StSpectraCutDCA();
 
  bool satisfiesCut(StV0Vertex *v0,StEvent* event)
       {return StSpectraCut::satisfiesCut(v0,event);}
  bool satisfiesCut(StTrack* track,StEvent* event);

};

#endif
