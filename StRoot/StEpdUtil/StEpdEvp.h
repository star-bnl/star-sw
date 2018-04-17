#ifndef _StEpdEvp_H
#define _StEpdEvp_H
/****************************************************
 *  \author Mike Lisa
 *  \date 16 April 2018
 *  
 *  \description:
 *   The most basic event-plane finder using the EPD.
 *     It shows how to use the StEpdGeom class, and I
 *     expect it will serve as an example for better and
 *     more sophisticated algorithms.
 *   I plan to make improvements to this and implement corrections
 *     (shifing, recentering, etc.) but my intention at least
 *     for now is to keep this pretty basic.
 *   Its first use is for Gene's QA code.
 ******************************************************/

class TClonesArray;
class StEpdGeom;
#include "TMath.h"   // has definition of kTRUE and kFALSE for instance

class StEpdEvp{
 public: 
  StEpdEvp();
  ~StEpdEvp();

  /// Event plane angles from East and West sides
  /// \param order       order of event plane. 1="directed flow plane", 2="elliptic flow plane" etc
  /// \param col         pointer to TClonesArray of StEpdHits.  Get this from the StMuDst
  double PsiEast(int order, TClonesArray* col);  // "order" starts at 1, duh.  (unlike e.g. mBBC ...)
  double PsiWest(int order, TClonesArray* col);  // so like use order=2 for the 2nd-order event plane.

  /// If kRawData==kTRUE, then adc/nMipQtX is used to calculate nMIP for the weighting, where X=B or C
  ///   Note that kRawData is set to kFALSE when StEpdEvp is constructed (i.e. assume calibrated data in muDst)
  /// \param raw       if true, then use adc information.  Otherwise use StEpdHit::nMip();
  void UseRawData(bool raw=kTRUE){kRawData=raw;}

  /// When calculating Q-vectors, we use weight of nMip, but it has been found empirically that
  /// "capping" nMip at some value (here called max) actually improves resolution
  /// Since max=3 has been found to be a good value, mMax=3 is set at construction of StEpdEvp
  /// \param max       max nMip value to use in Q-vector weighting
  void SetMaxMipWeight(double max){mMax=max;}

  /// When using adc values (kRawData==kTRUE), nMip is calculated as adc/mipPeak.  The QT32B ADCs
  ///   were used for Tiles 10-31.  This set to 115 when StEpdEvp is constructed
  /// \param mipPeak    position (in ADC units) of MIP peak for the STAR QT32B ADCs
  void SetMipPeakQt32B(double mipPeak){mNmipQtB=mipPeak;}

  /// When using adc values (kRawData==kTRUE), nMip is calculated as adc/mipPeak.  The QT32C ADCs
  ///   were used for Tiles 1-9.  This set to 160 when StEpdEvp is constructed
  /// \param mipPeak    position (in ADC units) of MIP peak for the STAR QT32C ADCs
  void SetMipPeakQt32C(double mipPeak){mNmipQtC=mipPeak;}

 private:

  double Psi(int order, int ew, TClonesArray* col);   /// generic private method for code reuse for East and West

  StEpdGeom* mEpdGeom;     /// EPD geometry class. Independent of any STAR infrastructure

  bool   kRawData;         /// if set true, then ADC value is used, rather than nMIP
  double mMax;             /// "cap" parameter to suppress effects of Landau fluctuations.  Typical value: 3
  double mNmipQtB;         /// ADC value of MIP peak on rings 6-16 (read out thru QT32Bs).  Used if kRawData=true
  double mNmipQtC;         /// ADC value of MIP peak on rings 1-5  (read out thru QT32Cs).  Used if kRawData=true
  double mnMipThreshold;   /// low-signal threshold, to cut out noise basically.
};

#endif
