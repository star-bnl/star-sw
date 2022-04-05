/***************************************************************************
 *
 * $Id: StDedxPidTraits.cxx,v 2.17 2015/12/24 00:14:44 fisyak Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StDedxPidTraits.cxx,v $
 * Revision 2.17  2015/12/24 00:14:44  fisyak
 * Add GMT and SST Id and new dE/dx method
 *
 * Revision 2.16  2012/04/29 22:51:18  fisyak
 * Add field for Log2(<dX>)
 *
 * Revision 2.15  2010/08/31 19:51:56  fisyak
 * Clean up
 *
 * Revision 2.14  2009/11/19 17:08:34  fisyak
 * remove St_dst_dedx_Table
 *
 * Revision 2.13  2004/10/11 22:58:35  ullrich
 * Changed order of initialization in constructor.
 *
 * Revision 2.12  2004/07/15 16:36:23  ullrich
 * Removed all clone() declerations and definitions. Use StObject::clone() only.
 *
 * Revision 2.11  2004/03/01 17:44:37  fisyak
 * Add Print method
 *
 * Revision 2.10  2003/04/30 18:05:55  fisyak
 * Add P03ia flag, which fixes P03ia MuDst
 *
 * Revision 2.9  2001/04/05 04:00:47  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.8  2001/03/24 03:34:40  perev
 * clone() -> clone() const
 *
 * Revision 2.7  2000/12/18 17:25:13  fisyak
 * Add track length used in dE/dx calculations
 *
 * Revision 2.6  2000/01/05 16:04:11  ullrich
 * Changed method name sigma() to errorOnMean().
 *
 * Revision 2.5  1999/11/29 17:07:24  ullrich
 * Moved method() from StTrackPidTraits to StDedxPidTraits.cxx
 *
 * Revision 2.4  1999/11/23 15:56:23  ullrich
 * Added clone() const method. Was pure virtual.
 *
 * Revision 2.3  1999/11/16 14:11:38  ullrich
 * Changed variance to sigma.
 *
 * Revision 2.2  1999/10/28 22:25:01  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.1  1999/10/13 19:44:31  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StDedxPidTraits.h"
#include "StBichsel/Bichsel.h"
#include "TMath.h"
#include "Stiostream.h"
ClassImp(StDedxPidTraits)

static const char rcsid[] = "$Id: StDedxPidTraits.cxx,v 2.17 2015/12/24 00:14:44 fisyak Exp $";

Float_t StDedxPidTraits::mean() const { 
#ifdef P03ia
  static const Int_t N70 = 6;
  static const Double_t T70[N70] = {
    -1.55822e+01,
    2.15621e+01,
    -1.24423e+01,
    3.68329e+00,
    -5.50226e-01,
    3.27863e-02
  };
  Float_t dEdx = mDedx;
  if (method() == kTruncatedMeanId) {
    Double_t x = TMath::Log(length());
    Double_t Corr = T70[N70-1]; for (int i = N70-2; i >= 0; i--) Corr = Corr*x + T70[i];
    dEdx *= TMath::Exp(-Corr);
  }
  return dEdx;
#else
  return mDedx; 
#endif
}

float
StDedxPidTraits::errorOnMean() const { 
#ifndef P03ia
  return mSigma; 
#else
  static const Int_t N70 = 6;
  static const Double_t T70[N70] = {
    1.06856e+01,
    -1.52626e+01,
    8.62097e+00,
    -2.37276e+00,
    3.17727e-01,
    -1.65739e-02,
  };
  Float_t Sigma;
  if (method() == kTruncatedMeanId) {
    Double_t x = TMath::Log(length());
    Double_t Corr = T70[N70-1]; for (int i = N70-2; i >= 0; i--) Corr = Corr*x + T70[i];
    Sigma = Corr;
    return Sigma;
  }
  else return mSigma; 
#endif
}

short
StDedxPidTraits::encodedMethod() const { return mMethod; }

StDedxMethod
StDedxPidTraits::method() const
{
    switch (mMethod) {
    case kTruncatedMeanId:
        return kTruncatedMeanId;
        break;
    case kEnsembleTruncatedMeanId:
        return kEnsembleTruncatedMeanId;
        break;
    case kLikelihoodFitId:
        return kLikelihoodFitId;
        break;
    case kWeightedTruncatedMeanId:
        return kWeightedTruncatedMeanId;
        break;
    case kOtherMethodId:
        return kOtherMethodId;
        break;
    case kOtherMethodId2:
        return kOtherMethodId2;
        break;
    default:
        return kUndefinedMethodId;
        break;
    }
}
//________________________________________________________________________________
void StDedxPidTraits::Print(Option_t *opt) const {
  cout << "StDedxPidTraits : \t method" << method()
       << "\t encodedMethod :  " << encodedMethod()
       << "\t numberOfPoints : " << numberOfPoints() 
       << "\t length : "         << length()         
       << "\t mean : "           << mean()           
       << "\t errorOnMean : "    << errorOnMean()  
       << "\t Log2<dX> : "       << log2dX() << endl;
       
 }

