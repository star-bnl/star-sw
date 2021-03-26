#ifndef genfit_STARField_h
#define genfit_STARField_h

#include "TVector3.h"
#include "StarMagField/StarMagField.h"
#include "GenFit/AbsBField.h"

//_______________________________________________________________________________________
// Adaptor for STAR magnetic field loaded via StarMagField Maker
class StarFieldAdaptor : public genfit::AbsBField {
  public:
    StarFieldAdaptor() {};
    virtual TVector3 get(const TVector3 &position) const {
        double x[] = {position[0], position[1], position[2]};
        double B[] = {0, 0, 0};

        if (StarMagField::Instance())
            StarMagField::Instance()->Field(x, B);

        return TVector3(B);
    };
    virtual void get(const double &_x, const double &_y, const double &_z, double &Bx, double &By, double &Bz) const {
        double x[] = {_x, _y, _z};
        double B[] = {0, 0, 0};

        if (StarMagField::Instance())
            StarMagField::Instance()->Field(x, B);

        Bx = B[0];
        By = B[1];
        Bz = B[2];
        return;
    };
};


#endif
