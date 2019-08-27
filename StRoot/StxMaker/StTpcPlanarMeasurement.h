#ifndef StTpcPlanarMeasurement_h
#define StTpcPlanarMeasurement_h
#include "TVectorD.h"
#include "StPlanarMeasurement.h"
#include "StDetectorDbMaker/StiHitErrorCalculator.h"
#include "TGeoMatrix.h"
class StTpcHit;
#if 0
class genfit::StateOnPlane;
#endif
class StTpcPlanarMeasurement : public StPlanarMeasurement {
 public:
  StTpcPlanarMeasurement(int nDim = 2);
  StTpcPlanarMeasurement(const TVectorD& rawHitCoords, const TMatrixDSym& rawHitCov, int detId, int hitId, genfit::TrackPoint* trackPoint);
  StTpcPlanarMeasurement(const StTpcHit *hit, genfit::TrackPoint* trackPoint);
  virtual ~StTpcPlanarMeasurement() {}
  virtual genfit::AbsMeasurement* clone() const {return new StTpcPlanarMeasurement(*this);}
#if 0
  virtual const TVectorD& getRawHitCoords(genfit::StateOnPlane *state=0) const;
  virtual const TMatrixDSym& getRawHitCov(genfit::StateOnPlane *state=0) const;
#endif
  virtual       TVectorD& getRawHitCoords(const genfit::StateOnPlane *state=0);
  virtual       TMatrixDSym& getRawHitCov(const genfit::StateOnPlane *state=0);
  static        TGeoHMatrix* Plane(Int_t sector, Int_t row) {return  SectorRowMatrices[sector-1][row-1];}
  static        void SetPlane(TGeoHMatrix* aPlane, Int_t sector, Int_t row) { SectorRowMatrices[sector-1][row-1] = aPlane;}
 protected:
  static TGeoHMatrix *SectorRowMatrices[24][72];
  
 public:
  ClassDef(StTpcPlanarMeasurement,1)
};
#endif // StTpcPlanarMeasurement_h
