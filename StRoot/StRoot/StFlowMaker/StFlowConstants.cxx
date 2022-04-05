////////////////////////////////////////////////////////////////////////////
//
// $Id: StFlowConstants.cxx,v 1.21 2009/11/24 19:22:58 posk Exp $
//
// Author: Art Poskanzer and Raimond Snellings, LBNL, Oct 1999
//
////////////////////////////////////////////////////////////////////////////
//
// Description: constants for the flow makers
//
////////////////////////////////////////////////////////////////////////////
#include "StFlowConstants.h"
#include "TMath.h"
ClassImp(Flow)
  
  Float_t Flow::etaMin = -4.5;
  Float_t Flow::etaMax =  4.5;
  Float_t Flow::etaMinTpcOnly = -1.5;
  Float_t Flow::etaMaxTpcOnly =  1.5;
  Float_t Flow::etaSymZSlopeTpc = 0.0043; // run 4
  Float_t Flow::etaSymZSlopeFtpc = -0.0067; // run 4

  Float_t Flow::ptMin     = 0.;
  Float_t Flow::ptMax     = 2.;
  Float_t Flow::ptMaxPart = 6.; // 6.

  // LeeYangZeros
  Float_t Flow::rMax = 1.5;
  Float_t Flow::j01 = 2.405;
  Float_t Flow::epsV1 = 0.5;

  // centralities
  Int_t Flow::cent200Full[nCents]      = {14,30,56,94,146,217,312,431,510};
  Int_t Flow::cent200Half[nCents]      = {14,32,59,98,149,216,302,409,474};
  Int_t Flow::cent200Year4Full[nCents] = {14,31,57,96,150,222,319,441,520};
  Int_t Flow::cent200Year4Half[nCents] = {14,30,56,94,146,217,312,431,510};
  Int_t Flow::cent200Year7[nCents]     = {10,21,39,69,114,178,269,399,485};
  Int_t Flow::cent130[nCents]          = {20,100,180,270,360,460,560,660,870};
  Int_t Flow::cent62[nCents]	       = {9,20,38,65,102,154,222,313,373};
  Int_t Flow::cent22[nCents]           = {7,14,28,48,77,117,170,237,281}; // 20 GeV paper

  //ZDC_SMD Beam Center(run5040129)
  Double_t Flow::zdcsmd_wx0 = 4.39604;
  Double_t Flow::zdcsmd_ex0 = 4.72466;
  Double_t Flow::zdcsmd_wy0 = 5.19968;
  Double_t Flow::zdcsmd_ey0 = 5.53629;

  //ZDCSMD pedstals(run5040090)
  Double_t Flow::zdcsmdPedstal[2][2][8] = {
    { { 1.901,5.354,3.606,2.044,3.171,4.042,2.731,3.248} ,
      { 4.921,2.884,2.627,4.579,2.778,4.018,2.905,4.623} } ,
    { { 4.925,5.818,6.513,0.005,8.328,4.025,4.019,8.833} ,
      { 8.952,8.684,6.962,7.353,7.928,7.385,9.972,7.996} }
  };
  //ZDCSMD gain factor
  Double_t Flow::zdcsmdGainFac[2][2][8] = {
    { { 0.989, 1.148, 1.491, 1.183, 1.261, 1.22, 1.579, 1.} ,
      { 1.,1.169,1.12,1.292,0.947,1.085,1.463, 1.179} } ,
    { { 0.774499,0.840393,0.989262,0.87179,0.916639,0.927873,1.168586,1.} ,
      { 0.940065,0.978662,1.040051,1.068336,0.97021,1.133341,1.25025,1.058449} }
  };

//////////////////////////////////////////////////////////////////////
//
// $Log: StFlowConstants.cxx,v $
// Revision 1.21  2009/11/24 19:22:58  posk
// Added reCenter option to remove acceptance correlations instead of phiWgt.
//
// Revision 1.20  2009/08/04 23:00:25  posk
// Reads year 7 MuDsts.
//
// Revision 1.19  2007/02/06 18:57:45  posk
// In Lee Yang Zeros method, introduced recentering of Q vector.
// Reactivated eta symmetry cut.
//
// Revision 1.18  2006/07/06 16:55:58  posk
// Calculation of v1 for selection=2 is done with mixed harmonics.
//
// Revision 1.17  2006/02/22 19:22:01  posk
// Additions needed for the StFlowLeeYangZerosMaker
//
// Revision 1.16  2005/08/26 21:37:23  oldi
// Year 4 centrlity bins corrected (see http://www.star.bnl.gov/protected/common/common2004/trigger2004/200gev/200gevFaq.html) for details).
//
// Revision 1.15  2005/03/03 17:21:58  posk
// Initialized pFlowEvent in the constructors.
//
// Revision 1.14  2005/02/08 20:57:35  psoren
// trigger and centrality selections were updated for all runs after run 4 to be compatible with trigger collections. Added TriggersFound() and GetFlowTriggerBitMap() functions.
//
// Revision 1.13  2004/12/17 22:33:07  aihong
// add in full Psi weight for ZDC SMD and fix a few bugs, done by Gang
//
// Revision 1.12  2004/12/09 23:43:33  posk
// Minor changes in code formatting.
//
// Revision 1.11  2004/05/05 21:13:44  aihong
// Gang's code for ZDC-SMD added
//
// Revision 1.10  2003/02/28 16:49:33  posk
// Entered 20 GeV centrality cuts from Cebra.
//
// Revision 1.9  2003/01/10 16:41:50  oldi
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
// Revision 1.8  2002/06/05 16:30:24  posk
// Updated the full field 200 GeV centrality cuts.
//
// Revision 1.7  2002/05/23 18:54:08  posk
// Moved centrality cuts into StFlowConstants
//
// Revision 1.6  2002/03/12 02:33:17  posk
// Now makes pico files in SL02c.
//
// Revision 1.5  2001/12/18 19:21:53  posk
// "proton" and "antiproton" changed to "pr+" and "pr-".
// Compiles on Solaris.
//
// Revision 1.4  2001/12/11 21:33:40  posk
// Went from one to four sets of histograms for making the event plane isotropic.
// StFlowEvent::PhiWeight() has changed arguments and return value.
// The ptWgt saturates above 2 GeV/c.
//
// Revision 1.3  2001/11/13 22:43:46  posk
// Documentation updated.
//
// Revision 1.2  2001/11/10 01:08:03  posk
// Moved some constants into StFlowConstants.
//
//////////////////////////////////////////////////////////////////////
