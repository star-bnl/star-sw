/*!
 * \class StThinPlateSpline
 * \author Qiu Hao, March 2013
 * \Initial Revision.
 */
/***************************************************************************
 *
 * $Id: StThinPlateSpline.h,v 1.3 2014/08/06 11:43:35 jeromel Exp $
 *
 * Author: Qiu Hao, March 2013
 ***************************************************************************
 *
 * Description:
 * Thin plate spline function to describe the surface profile of a pxl sensor
 * More information at
 * https://www.star.bnl.gov/protected/heavy/qiuh/HFT/software/PXL_software.pdf
 *
 ***************************************************************************
 *
 * $Log: StThinPlateSpline.h,v $
 * Revision 1.3  2014/08/06 11:43:35  jeromel
 * Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
 *
 * Revision 1.2  2014/01/28 19:29:47  qiuh
 * *** empty log message ***
 *
 *
 **************************************************************************/
#ifndef StThinPlateSpline_hh
#define StThinPlateSpline_hh

#include "TMatrixT.h"

class StThinPlateSpline
{
public:
   StThinPlateSpline(Int_t nMeasurements = 200);
   StThinPlateSpline(Int_t nMeasurements, Double_t *X, Double_t *Y, Double_t *W, Double_t *A);
   StThinPlateSpline(Int_t nMeasurements, Float_t *X, Float_t *Y, Float_t *W, Float_t *A);
   ~StThinPlateSpline();
   //! fit measurements on a profile with tps to get mX, mY, mW, mA matrix
   void fit(Int_t nMeasurements, Double_t *xMeasurement, Double_t *yMeasurement, Double_t *zMeasurement, Double_t lambda = 0);
   Double_t ur(Double_t r2) const;
   Double_t z(Double_t x, Double_t y) const; ///< calculate z on the profile at (x,y)

   virtual const char *GetCVS() const {
      static const char cvs[] = "Tag $Name:  $ $Id: StThinPlateSpline.h,v 1.3 2014/08/06 11:43:35 jeromel Exp $ built " __DATE__ " " __TIME__ ;
      return cvs;
   }

protected:
   TMatrixT<double> *mX;
   TMatrixT<double> *mY;
   TMatrixT<double> *mW;
   TMatrixT<double> *mA;

};

#endif
