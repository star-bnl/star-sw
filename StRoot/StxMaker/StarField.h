#ifndef genfit_StarField_h
#define genfit_StarField_h
#include "GenFit/AbsBField.h"
#include "StarMagField/StarMagField.h"
namespace genfit{
class StarField  : public AbsBField {
public:
  StarField(){}
  virtual ~StarField(){;}
  TVector3 get(const TVector3& position) const {Double_t X[3], B[3]; position.GetXYZ(X); StarMagField::Instance()->BField(X,B); return TVector3(B);}
  void get(const double& posX, const double& posY, const double& posZ, double& Bx, double& By, double& Bz) const {TVector3 B(this->get(TVector3(posX, posY, posZ))); Bx = B.X(); By = B.Y(); Bz = B.Z(); }
};
} /* End of namespace genfit */
#endif // genfit_StarField_h
