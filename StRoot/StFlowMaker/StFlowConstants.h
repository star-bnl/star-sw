//////////////////////////////////////////////////////////////////////
//
// $Id: StFlowConstants.h,v 1.28 2010/03/08 16:52:49 posk Exp $
//
// Author: Art Poskanzer and Raimond Snellings 
//          FTPC added by Markus Oldenburg, MPI, Dec 2000
//          Cumulants added by Aihong Tang, KSU, Nov 2001
//
//////////////////////////////////////////////////////////////////////
//
// Description: constants for the flow makers
//
//////////////////////////////////////////////////////////////////////

#ifndef StFlowConstants_h
#define StFlowConstants_h
#include "Rtypes.h"

class Flow{

 public:

  enum {
    nHars             = 4,   // 4
    nSels             = 2,
    nSubs             = 2,
    nTheta            = 5,   // 5 LYZ
    nTheta1           = 5,   // 5 LYZ
    nRBins            = 150, // LYZ
    nPhiBins          = 120,
    nPhiBinsFtpc      = 120,
    nEtaBins          = 90,  // 90
    nEtaBinsTpcOnly   = 30,
    nPtBins           = 40,
    nPtBinsPart       = 60,
    nCumulIntegOrders = 3,   // Cumu
    nCumulInteg_qMax  = 8,   // Cumu
    nCumulDiffOrders  = 2,   // Cumu
    nCumulDiff_qMax   = 8,   // Cumu
    nCumulMixHar_pMax = 8,   // for directed flow. Eq.(29) in Borghini v1 paper
    nCumulMixHar_qMax = 4,   // for directed flow
    nCents            = 9,
    zdcsmd_nPsiBins   = 64,  // ZDCSMD
    TERMS             = 10,  // DirCumu correlation
    TYPES             = 2,   // differential or integrated
    PHASES            = 2,   // cos or sin
    SPECIES           = 1,   // number of different particles
    PTBINS            = 62,  // DirCumu
    MAXMULT           = 3000 // DirCumu maximum multiplicity of an event
  };

  typedef Double_t PhiWgt_t[nSels][2][nPhiBins]; // only odd and even harmonics
  typedef Double_t PhiWgtFtpc_t[nSels][2][nPhiBinsFtpc];
  typedef Double_t ZDCSMD_PsiWgt_t[64];  
  typedef Double_t ReCent_t[nSels][nHars][4];   // 3 TPCs for LYZ, 4 for ana
  typedef Double_t ReCentering_shifts_t[2][9][3][2][3];//cos or sin ; cent bin ; charge/charge combo ; B field sign ; TPC side/TPC side combo 
  //typedef Double_t ReCentering_shifts_t[2][9][3][2];
  typedef Double_t PhiWgtFtpc_2D_t[6][6][120];

  static Float_t etaMin;
  static Float_t etaMax;
  static Float_t etaMinTpcOnly;
  static Float_t etaMaxTpcOnly;
  static Float_t etaSymZSlopeTpc;
  static Float_t etaSymZSlopeFtpc;
  static Float_t rMax; // LYZ
  static Float_t j01;  // LYZ
  static Float_t epsV1;// LYZ
  static Float_t ptMin;
  static Float_t ptMax;
  static Float_t ptMaxPart;

  static Int_t   cent200Full[nCents];
  static Int_t   cent200Half[nCents];
  static Int_t   cent200Year4Full[nCents];
  static Int_t   cent200Year4Half[nCents];
  static Int_t   cent200Year7[nCents];
  static Int_t   cent130[nCents];
  static Int_t   cent62[nCents];
  static Int_t   cent22[nCents];

  static Double_t zdcsmd_wx0,zdcsmd_ex0,zdcsmd_wy0,zdcsmd_ey0;
  static Double_t zdcsmdPedstal[2][2][8];
  static Double_t zdcsmdGainFac[2][2][8];

  ClassDef(Flow,1)               // macro for rootcint
};

#endif

//////////////////////////////////////////////////////////////////////
//
// $Log: StFlowConstants.h,v $
// Revision 1.28  2010/03/08 16:52:49  posk
// Added StFlowDirectCumulantMaker written by Dhevan Gangadharan.
//
// Revision 1.27  2009/11/24 19:23:00  posk
// Added reCenter option to remove acceptance correlations instead of phiWgt.
//
// Revision 1.26  2009/08/04 23:00:26  posk
// Reads year 7 MuDsts.
//
// Revision 1.25  2007/02/06 18:57:47  posk
// In Lee Yang Zeros method, introduced recentering of Q vector.
// Reactivated eta symmetry cut.
//
// Revision 1.24  2006/07/06 16:55:59  posk
// Calculation of v1 for selection=2 is done with mixed harmonics.
//
// Revision 1.23  2006/02/22 19:22:02  posk
// Additions needed for the StFlowLeeYangZerosMaker
//
// Revision 1.22  2005/02/08 20:57:36  psoren
// trigger and centrality selections were updated for all runs after run 4 to be compatible with trigger collections. Added TriggersFound() and GetFlowTriggerBitMap() functions.
//
// Revision 1.21  2004/12/17 22:33:05  aihong
// add in full Psi weight for ZDC SMD and fix a few bugs, done by Gang
//
// Revision 1.20  2004/12/17 15:50:08  aihong
// check in v1{3} code
//
// Revision 1.19  2004/12/07 23:08:10  posk
// Only odd and even phiWgt hists. If the old phiWgt file contains more than
// two harmonics, only the first two are read. Now writes only the first two.
//
// Revision 1.18  2004/05/05 21:13:44  aihong
// Gang's code for ZDC-SMD added
//
// Revision 1.17  2004/02/03 22:36:36  posk
// Initialzed mPtBinsPart.
//
// Revision 1.16  2003/05/02 21:09:40  posk
// Reduced the number of harmonics from 3 to 2.
//
// Revision 1.15  2003/01/10 16:41:51  oldi
// Several changes to comply with FTPC tracks:
// - Switch to include/exclude FTPC tracks introduced.
//   The same switch changes the range of the eta histograms.
// - Eta symmetry plots for FTPC tracks added and separated from TPC plots.
// - PhiWgts and related histograms for FTPC tracks split in FarEast, East,
//   West, FarWest (depending on vertex.z()).
// - Psi_Diff plots for 2 different selections and the first 2 harmonics added.
// - Cut to exclude mu-events with no primary vertex introduced.
//   (This is possible for UPC events and FTPC tracks.)
// - Global DCA cut for FTPC tracks added.
// - Global DCA cuts for event plane selection separated for TPC and FTPC tracks.
// - Charge cut for FTPC tracks added.
//
// Revision 1.14  2002/05/23 18:54:09  posk
// Moved centrality cuts into StFlowConstants
//
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
