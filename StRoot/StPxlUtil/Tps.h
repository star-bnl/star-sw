/*!
 * \class Tps
 * \author Qiu Hao, March 2013
 * \Initial Revision.
 */
/***************************************************************************
 *
 * $Id: Tps.h,v 1.4 2014/01/23 01:05:06 qiuh Exp $
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
 * $Log: Tps.h,v $
 * Revision 1.4  2014/01/23 01:05:06  qiuh
 * *** empty log message ***
 *
 *
 **************************************************************************/
#ifndef STAR_Tps
#define STAR_Tps

#include "TMatrixT.h"

class Tps{
public:
    Tps(Int_t nMeasurements = 200);
    Tps(Int_t nMeasurements, Double_t* X, Double_t* Y, Double_t* W, Double_t* A);
    Tps(Int_t nMeasurements, Float_t* X, Float_t* Y, Float_t* W, Float_t* A);
    ~Tps();
    /// fit measurements on a profile with tps to get mX, mY, mW, mA matrix
    void fit(Int_t nMeasurements, Double_t* xMeasurement, Double_t* yMeasurement, Double_t* zMeasurement, Double_t lambda = 0);
    Double_t ur(Double_t r2);
    Double_t z(Double_t x, Double_t y); ///< calculate z on the profile at (x,y)

    virtual const char *GetCVS() const {
        static const char cvs[]="Tag $Name:  $ $Id: Tps.h,v 1.4 2014/01/23 01:05:06 qiuh Exp $ built "__DATE__" "__TIME__ ;
        return cvs;
    }

protected:    
    TMatrixT<double> *mX;
    TMatrixT<double> *mY;
    TMatrixT<double> *mW;
    TMatrixT<double> *mA;

};

#endif
