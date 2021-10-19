/****************************************************************************************************
 * $Id: StGlauberUtilities.h,v 1.2 2012/04/25 04:42:37 hmasui Exp $
 * $Log: StGlauberUtilities.h,v $
 * Revision 1.2  2012/04/25 04:42:37  hmasui
 * Moved several functions from StFastGlauberMcMaker. Added namespace GlauberUtilities
 *
 *
****************************************************************************************************/

#ifndef __StGlauberUtilities_h__
#define __StGlauberUtilities_h__

class TF1 ;
class TRandom ;
class TRandom3 ;
#include "Rtypes.h"

//____________________________________________________________________________________________________
// Class StGlauberUtilities: Random number, and functions
class StGlauberUtilities {
  public:
    static StGlauberUtilities* instance();
    virtual ~StGlauberUtilities(); /// Default destructor

    /// Get impact parameter
    Double_t GetImpactParameter() const ;

    /// Get maximum impact parameter
    Double_t GetMaximumImpactParameter() const ;

    /// Get theta (polar angle)
    Double_t GetTheta() const ;

    /// Get phi (azimuthal angle)
    Double_t GetPhi() const ;

    /// Get uniform distribution in 0 < x < 1
    Double_t GetUniform() const ;
    Double_t GetUniform2() const ;

    /// Set debug level
    void SetDebug(const UInt_t debug) ;

  private:
    static StGlauberUtilities* mInstance ; /// singleton
    StGlauberUtilities(); /// Default constructor

    static const UShort_t mDebugLevel ; /// Debug level for StGlauberUtilities
    TF1* mImpactParameter ; /// impact parameter

   // TRandom* mRandom ; /// Random numbers
    TRandom3* mRandom ; /// Random numbers
    UInt_t mDebug ; /// Debug level

    ClassDef(StGlauberUtilities, 0)
};

namespace GlauberUtilities {
  // Woods-saxon density profile
  Double_t WoodsSaxon(Double_t* x, Double_t* par) ;

  // Woods-saxon density profile (2D for deformed nuclei)
  Double_t WoodsSaxon2D(Double_t* x, Double_t* par) ;

  // Step function
  Double_t StepFunction(Double_t* x, Double_t* par) ;

  // Gaussian function
  Double_t Gaussian(Double_t* x, Double_t* par) ;
}


#endif

