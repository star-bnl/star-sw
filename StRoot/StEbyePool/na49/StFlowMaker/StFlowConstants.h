//////////////////////////////////////////////////////////////////////
//
// $Id: StFlowConstants.h,v 1.1 2001/02/23 00:50:26 posk Exp $
//
// Authors: Art Poskanzer, LBNL, and Alexander Wetzler, IKF, Dec 2000
//
//////////////////////////////////////////////////////////////////////
//
// Description: constants for the flow makers
//
//////////////////////////////////////////////////////////////////////
//
// $Log: StFlowConstants.h,v $
// Revision 1.1  2001/02/23 00:50:26  posk
// NA49 version of STAR software.
//
// Revision 1.3  2000/05/11 20:00:30  posk
//
//////////////////////////////////////////////////////////////////////

#ifndef StFlowConstants_h
#define StFlowConstants_h
#include "Rtypes.h"

class Flow{

 public:

  enum { nHars         =   6, 
	 nSels         =   2,
	 nSubs         =   2,
	 nPhiBins      = 120,
         nSinCosPtBins =  20,
         nSinCosYBins  =  50,
	 n_qBins       =  50 };

  typedef Double_t PhiWgt_t[nSels][nHars][nPhiBins];
  typedef Double_t MeanCos_t[nHars][nSinCosYBins][nSinCosPtBins];
  typedef Double_t MeanSin_t[nHars][nSinCosYBins][nSinCosPtBins];

  static const Float_t sinCosYMin;
  static const Float_t sinCosYMax;
  static const Float_t sinCosPtMax;
  static const Float_t yCM;
  static const Float_t qMax;

  ClassDef(Flow,1)               // macro for rootcint
};

#endif

