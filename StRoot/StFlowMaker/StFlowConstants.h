//////////////////////////////////////////////////////////////////////
//
// $Id: StFlowConstants.h,v 1.13 2002/03/12 02:33:18 posk Exp $
//
// Author: Art Poskanzer and Raimond Snellings 
//          FTPC added by Markus Oldenburg, MPI, Dec 2000
//          Cumulants added by Aihong Tang, KSU, Nov 2001
//
//////////////////////////////////////////////////////////////////////
//
// Description: constants for the flow makers
//.
//////////////////////////////////////////////////////////////////////

#ifndef StFlowConstants_h
#define StFlowConstants_h
#include "Rtypes.h"

class Flow{

 public:

  enum {
    nHars        =   3, 
    nSels        =   2,
    nSubs        =   2,
    nPhiBins     = 120,
    nPhiBinsFtpc = 360,
    nEtaBins     = 90,
    //nEtaBins     = 30,
    nPtBins      = 40,
    nPtBinsPart  = 40,
    nCumulIntegOrders =   3, 
    nCumulInteg_qMax  =   8,
    nCumulDiffOrders  =   2,
    nCumulDiff_qMax   =   8
  };

  typedef Double_t PhiWgt_t[nSels][nHars][nPhiBins];
  typedef Double_t PhiWgtFtpc_t[nSels][nHars][nPhiBinsFtpc];

  static Float_t etaMin;
  static Float_t etaMax;
  static Float_t ptMin;
  static Float_t ptMax;
  static Float_t ptMaxPart;

  ClassDef(Flow,1)               // macro for rootcint
};

#endif

//////////////////////////////////////////////////////////////////////
//
// $Log: StFlowConstants.h,v $
// Revision 1.13  2002/03/12 02:33:18  posk
// Now makes pico files in SL02c.
//
// Revision 1.12  2002/01/31 01:04:41  posk
// *** empty log message ***
//
// Revision 1.11  2001/12/18 19:21:59  posk
// "proton" and "antiproton" changed to "pr+" and "pr-".
// Compiles on Solaris.
//
// Revision 1.10  2001/12/11 21:33:41  posk
// Went from one to four sets of histograms for making the event plane isotropic.
// StFlowEvent::PhiWeight() has changed arguments and return value.
// The ptWgt saturates above 2 GeV/c.
//
// Revision 1.9  2001/11/10 01:08:06  posk
// Moved some constants into StFlowConstants.
//
// Revision 1.8  2001/11/09 21:10:26  posk
// Switched from CERNLIB to TMath. Little q is now normalized.
//
// Revision 1.7  2001/11/02 04:49:52  aihong
// add func. for cumulant maker
//
// Revision 1.6  2001/05/22 20:17:09  posk
// Now can do pseudorapidity subevents.
//
// Revision 1.5  2000/12/12 20:22:04  posk
// Put log comments at end of files.
// Deleted persistent StFlowEvent (old micro DST).
//
// Revision 1.4  2000/12/08 17:03:38  oldi
// Phi weights for both FTPCs included.
//
// Revision 1.1  2000/03/02 23:02:36  posk
// Changed extensions from .hh and .cc to .h and .cxx .
//
// Revision 1.1  1999/12/15 22:01:20  posk
// Added StFlowConstants.hh
//
//////////////////////////////////////////////////////////////////////
