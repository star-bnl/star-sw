
#ifndef __StGlauberConstUtilities_h__
#define __StGlauberConstUtilities_h__

#include "Rtypes.h"

//____________________________________________________________________________________________________
// namespace StGlauberConstUtilities: Utilities for constant variables in MC Glauber
namespace StGlauberConstUtilities {
  /// Impact parameter
  const UInt_t GetImpactParameterBin()   ; // Impact parameter bin
  const Double_t GetImpactParameterMax() ; // Maximum impact parameter

  /// Npart
  const UInt_t GetNpartBin()   ; // Npart bin
  const Double_t GetNpartMax() ; // Maximum Npart

  /// Ncoll
  const UInt_t GetNcollBin()   ; // Ncoll bin
  const Double_t GetNcollMax() ; // Maximum Ncoll

  /// Multiplicity
  const UInt_t GetMultiplicityBin()   ; // Multiplicity bin
  const Double_t GetMultiplicityMax() ; // Maximum multiplicity

  /// Centrality
  const UInt_t GetCentralityBin()   ; // centrality bin
  const Double_t GetCentralityMin(const UInt_t icent) ; // Minimum centrality
  const Double_t GetCentralityMax(const UInt_t icent) ; // Maximum centrality

  const Bool_t IsCentralityOk(const UInt_t icent, const Double_t centrality) ; // Centrality check
};

#endif

