#include "TMatrixT.h"
//class TMatrixT<double>;

class Tps{
 protected:

 public:
  Tps(int nMeasurements = 200);
  void Fit(int nMeasurements, double* xMeasurement, double* yMeasurement, double* zMeasurement, double lambda = 0);
  double Ur(double r2);
  double Z(double x, double y);

  TMatrixT<double> *X;
  TMatrixT<double> *Y;
  TMatrixT<double> *W;
  TMatrixT<double> *A;
};
