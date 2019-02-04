#ifndef StTpc3DMeasurement_h
#define StTpc3DMeasurement_h
#include "TVectorD.h"
#include "GenFit/SpacepointMeasurement.h"
class StTpcHit;
class StiHitErrorCalculator;
class StTpc3DMeasurement : public genfit::SpacepointMeasurement {
 public:
 StTpc3DMeasurement(const StTpcHit *tpcHit, genfit::TrackPoint* trackPoint);
  virtual ~StTpc3DMeasurement() {}

  virtual genfit::AbsMeasurement* clone() const {return new StTpc3DMeasurement(*this);}
 protected:
 public:
  ClassDef(StTpc3DMeasurement,1)
};
#endif // StTpc3DMeasurement_h
