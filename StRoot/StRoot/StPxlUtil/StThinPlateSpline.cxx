/*!
 * \class StThinPlateSpline
 * \author Qiu Hao, March 2013
 * \Initial Revision.
 */
/***************************************************************************
 *
 * $Id: StThinPlateSpline.cxx,v 1.2 2014/01/28 19:29:47 qiuh Exp $
 *
 * Author: Qiu Hao, March 2013
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StThinPlateSpline.cxx,v $
 * Revision 1.2  2014/01/28 19:29:47  qiuh
 * *** empty log message ***
 *
 *
 **************************************************************************/

#include "StThinPlateSpline.h"
#include "TMatrixT.h"
#include <math.h>

StThinPlateSpline::StThinPlateSpline(Int_t nMeasurements)
{
   mX = new TMatrixT<double>(nMeasurements, 1);
   mY = new TMatrixT<double>(nMeasurements, 1);
   mW = new TMatrixT<double>(nMeasurements, 1);
   mA = new TMatrixT<double>(3, 1);
}

StThinPlateSpline::StThinPlateSpline(Int_t nMeasurements, Double_t *X, Double_t *Y, Double_t *W, Double_t *A)
{
   mX = new TMatrixT<double>(nMeasurements, 1, X);
   mY = new TMatrixT<double>(nMeasurements, 1, Y);
   mW = new TMatrixT<double>(nMeasurements, 1, W);
   mA = new TMatrixT<double>(3, 1, A);
}

StThinPlateSpline::StThinPlateSpline(Int_t nMeasurements, Float_t *X, Float_t *Y, Float_t *W, Float_t *A)
{
   mX = new TMatrixT<double>(nMeasurements, 1);
   mY = new TMatrixT<double>(nMeasurements, 1);
   mW = new TMatrixT<double>(nMeasurements, 1);
   mA = new TMatrixT<double>(3, 1);
   for (int j = 0; j < 3; j++)
      (*mA)[j][0] = A[j];
   for (int j = 0; j < nMeasurements; j++)
      (*mX)[j][0] = X[j];
   for (int j = 0; j < nMeasurements; j++)
      (*mY)[j][0] = Y[j];
   for (int j = 0; j < nMeasurements; j++)
      (*mW)[j][0] = W[j];
}

StThinPlateSpline::~StThinPlateSpline()
{
   delete mA;
   delete mX;
   delete mY;
   delete mW;
}

void StThinPlateSpline::fit(Int_t nMeasurements, Double_t *xMeasurement, Double_t *yMeasurement, Double_t *zMeasurement, Double_t lambda)
{
   TMatrixT<double> P(nMeasurements, 3);
   TMatrixT<double> One(nMeasurements, 1);

   One += 1;
   mX->Use(nMeasurements, 1, xMeasurement);
   mY->Use(nMeasurements, 1, yMeasurement);
   P.SetSub(0, 0, One);
   P.SetSub(0, 1, *mX);
   P.SetSub(0, 2, *mY);
   //  P.Print();

   TMatrixT<double> K(nMeasurements, nMeasurements);
   for (int i = 0; i < nMeasurements; i++)
      for (int j = 0; j < nMeasurements; j++) {
         double r2 = pow((*mX)[i][0] - (*mX)[j][0], 2) + pow((*mY)[i][0] - (*mY)[j][0], 2);
         K[i][j] = ur(r2);
      }
   //  K.Print();

   TMatrixT<double> I(nMeasurements, nMeasurements);
   I.UnitMatrix();
   K += lambda * I;

   TMatrixT<double> L(nMeasurements + 3, nMeasurements + 3);
   L.SetSub(0, 0, K);
   L.SetSub(0, nMeasurements, P);
   L.SetSub(nMeasurements, 0, P.Transpose(P));
   //  L.Print();

   TMatrixT<double> V(nMeasurements, 1, zMeasurement);
   TMatrixT<double> Y(nMeasurements + 3, 1);
   Y.SetSub(0, 0, V);
   //  Y.Print();

   TMatrixT<double> WA(Y);
   L.Invert();
   //  L.Print();
   WA.Mult(L, Y);

   mW->ResizeTo(nMeasurements, 1);
   *mW = WA.GetSub(0, nMeasurements - 1, 0, 0);
   *mA = WA.GetSub(nMeasurements, nMeasurements + 2, 0, 0);

   //  mA->Print();
   //  mW->Print();
}

Double_t StThinPlateSpline::ur(Double_t r2) const
{
   if (r2 == 0) return 0;
   return r2 * log(r2);
}

Double_t StThinPlateSpline::z(Double_t x, Double_t y) const
{
   double z_ = 0;
   z_ = (*mA)[0][0] + (*mA)[1][0] * x + (*mA)[2][0] * y;
   for (int i = 0; i < mW->GetNrows(); i++) {
      double r2 = pow(x - (*mX)[i][0], 2) + pow(y - (*mY)[i][0], 2);
      z_ += (*mW)[i][0] * ur(r2);
   }
   return z_;
}

