////////////////////////////////////////////////////////////////////////////
//
// $Id: StFlowConstants.cxx,v 1.11 2004/05/05 21:13:44 aihong Exp $
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

  Float_t Flow::ptMin     = 0.;
  Float_t Flow::ptMax     = 2.;
  Float_t Flow::ptMaxPart = 6.;

  Int_t Flow::cent200Full[nCents] = {14,30,56,94,146,217,312,431,510};
  Int_t Flow::cent200Half[nCents] = {14,32,59,98,149,216,302,409,474};
  Int_t Flow::cent130[nCents]     = {20,100,180,270,360,460,560,660,870};
  Int_t Flow::cent62[nCents]	  = {9,20,38,65,102,154,222,313,373};
  Int_t Flow::cent22[nCents]      = {8,16,30,51,80,120,173,240,284}; // Cebra

  //ZDC_SMD Beam Center
  Float_t Flow::zdcsmd_wx0 = 4.386;
  Float_t Flow::zdcsmd_ex0 = 4.687;
  Float_t Flow::zdcsmd_wy0 = 5.421;
  Float_t Flow::zdcsmd_ey0 = 5.760;

  //ZDCSMD pedstals
  Float_t Flow::zdcsmdPedstal[2][2][8] ={
    { { 1.747, 5.3035, 3.5205, 2.0001, 3.0735, 4.011, 2.668, 2.9575} ,
      { 4.8705,2.886,2.518,4.505,2.638,3.994,2.8175, 4.4705} } ,
    { {4.854,5.725,6.4095,0.0005,8.167,3.9785,4.001,9.0665} ,
      {8.963,8.7425,6.9935,7.361,7.9315,7.2025,9.9905,8.0065} }
  };
  //ZDCSMD gain factor
  Float_t Flow::zdcsmdGainFac[2][2][8] ={
    { { 0.989, 1.148, 1.491, 1.183, 1.261, 1.22, 1.579, 1.} ,
      { 1.,1.169,1.12,1.292,0.947,1.085,1.463, 1.179} } ,
    { {0.774499,0.840393,0.989262,0.87179,0.916639,0.927873,1.168586,1.} ,
      {0.940065,0.978662,1.040051,1.068336,0.97021,1.133341,1.25025,1.058449} }
  };
//////////////////////////////////////////////////////////////////////
//
// $Log: StFlowConstants.cxx,v $
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
