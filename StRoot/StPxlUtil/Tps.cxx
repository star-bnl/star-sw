#include "Tps.h"
#include "TMatrixT.h"
#include <math.h>

Tps::Tps(int nMeasurements)
{
  X = new TMatrixT<double>(nMeasurements, 1);
  Y = new TMatrixT<double>(nMeasurements, 1);
  W = new TMatrixT<double>(nMeasurements, 1);
  A = new TMatrixT<double>(3, 1);
}

void Tps::Fit(int nMeasurements, double* xMeasurement, double* yMeasurement, double* zMeasurement, double lambda)
{
  TMatrixT<double> P(nMeasurements, 3);
  TMatrixT<double> One(nMeasurements, 1); 

  One += 1;
  X->Use(nMeasurements, 1, xMeasurement); 
  Y->Use(nMeasurements, 1, yMeasurement); 
  P.SetSub(0,0, One);
  P.SetSub(0,1, *X);
  P.SetSub(0,2, *Y);
  //  P.Print();
  
  TMatrixT<double> K(nMeasurements, nMeasurements);
  for(int i=0; i<nMeasurements; i++)
    for(int j=0; j<nMeasurements; j++)
      {
	double r2 = pow((*X)[i][0]-(*X)[j][0], 2) + pow((*Y)[i][0]-(*Y)[j][0], 2);
	K[i][j] = Ur(r2);
      }
  //  K.Print();

  TMatrixT<double> I(nMeasurements, nMeasurements);
  I.UnitMatrix();
  K += lambda*I;
  
  TMatrixT<double> L(nMeasurements+3, nMeasurements+3);
  L.SetSub(0,0, K);
  L.SetSub(0,nMeasurements, P);
  L.SetSub(nMeasurements,0, P.Transpose(P));
  //  L.Print();

  TMatrixT<double> V(nMeasurements, 1, zMeasurement);
  TMatrixT<double> Y(nMeasurements+3, 1);
  Y.SetSub(0,0, V);
  //  Y.Print();

  TMatrixT<double> WA(Y);
  L.Invert();
  //  L.Print();
  WA.Mult(L, Y);

  W->ResizeTo(nMeasurements, 1);
  *W = WA.GetSub(0, nMeasurements-1, 0, 0);
  *A = WA.GetSub(nMeasurements, nMeasurements+2, 0, 0);
  
  //  A->Print();
  //  W->Print();
}

double Tps::Ur(double r2)
{
  if(r2==0) return 0;
  return r2*log(r2);
}

double Tps::Z(double x, double y)
{
  double z = 0;
  z = (*A)[0][0]+(*A)[1][0]*x+(*A)[2][0]*y;
  for(int i=0; i<W->GetNrows(); i++)
    {
      double r2 = pow(x-(*X)[i][0], 2) + pow(y-(*Y)[i][0], 2);
      z += (*W)[i][0]*Ur(r2);
    }
  return z;
}


