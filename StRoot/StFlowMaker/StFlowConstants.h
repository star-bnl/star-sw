//////////////////////////////////////////////////////////////////////
//
// $Id: StFlowConstants.h,v 1.2 2000/03/28 23:21:01 posk Exp $
//
// Author: Art Poskanzer and Raimond Snellings 
//////////////////////////////////////////////////////////////////////
//
// Description: constants for the flow makers
//////////////////////////////////////////////////////////////////////
//
// $Log: StFlowConstants.h,v $
// Revision 1.2  2000/03/28 23:21:01  posk
// Allow multiple instances of the AnalysisMaker.
//
// Revision 1.1  2000/03/02 23:02:36  posk
// Changed extensions from .hh and .cc to .h and .cxx .
//
// Revision 1.2  1999/12/16 18:05:21  posk
// Fixed Linux compatability again.
//
// Revision 1.1  1999/12/15 22:01:20  posk
// Added StFlowConstants.hh
//
//
//////////////////////////////////////////////////////////////////////

#ifndef StFlowConstants_h
#define StFlowConstants_h
#include "Rtypes.h"

class Flow{

 public:

  enum { nHars    =   6, 
	 nSels    =   2,
	 nSubs    =   2,
	 nPhiBins = 120 };

  typedef Double_t PhiWgt_t[nSels][nHars][nPhiBins];

  ClassDef(Flow,1)               // macro for rootcint
};

#endif

