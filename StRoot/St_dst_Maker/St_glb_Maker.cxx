// $Id: St_glb_Maker.cxx,v 1.58 1999/07/01 17:27:42 fisyak Exp $
// $Log: St_glb_Maker.cxx,v $
// Revision 1.58  1999/07/01 17:27:42  fisyak
// New global chain from  Wensheng Deng
//
// Revision 1.57  1999/06/25 22:48:38  caines
// Added ability to perform ev0 evaluation using eval flag
//
// Revision 1.56  1999/06/24 16:42:27  fisyak
// Preserve all space point
//
// Revision 1.55  1999/05/01 00:57:04  fisyak
// Change Clear function to defualt
//
// Revision 1.54  1999/04/21 18:32:58  genevb
// Vertex table declaration was reverted to old version when new maker schema was introduced
//
// Revision 1.53  1999/04/14 23:09:42  fisyak
// Add primtrk to dataset
//
// Revision 1.52  1999/04/01 23:31:42  fisyak
// Add summary information and set return kStWarn after evr fails
//
// Revision 1.51  1999/03/20 22:34:07  perev
// maker new schema
//
// Revision 1.50  1999/03/11 03:12:18  perev
// new schema
//
// Revision 1.49  1999/03/06 03:01:12  genevb
// Change dst_vertex table size as needed
//
// Revision 1.48  1999/03/04 03:08:14  fisyak
// take out PhysicalConstants
//
// Revision 1.47  1999/03/04 03:01:27  fisyak
// fix typo
//
// Revision 1.46  1999/03/04 01:19:21  fisyak
// Put release tag to run_summary table
//
// Revision 1.45  1999/03/04 00:06:22  caines
// Fixed so that tte_eval is made if no tracks - for pp events
//
// Revision 1.44  1999/03/03 17:13:43  caines
// Reduced v0,xi table memory allocations
//
// Revision 1.43  1999/03/03 04:52:55  fisyak
// Add protection for no vertex fit was made
//
// Revision 1.42  1999/02/28 15:48:48  caines
// CHanged no of xi_vertex booked
//
// Revision 1.41  1999/02/27 23:10:32  caines
// Fixed impact calc
//
// Revision 1.40  1999/02/26 19:49:27  caines
// Lots of memory fixes
//
// Revision 1.39  1999/02/23 17:10:47  caines
//  xi_aux table size reduced
//
// Revision 1.38  1999/02/23 16:50:16  fine
// wrong calling seq for ev0_eval2 module
//
// Revision 1.37  1999/02/23 03:13:52  fisyak
// Take out fake tof
//
// Revision 1.35  1999/02/22 21:27:20  kathy
// moved hist from St_glb_Maker to St_QA_Maker and had to rename some etc
//
// Revision 1.34  1999/02/20 18:49:16  fisyak
// Add event/run information
//
// Revision 1.33  1999/02/20 00:24:51  kathy
// fixed some of the histograms
//
// Revision 1.32  1999/02/19 15:29:25  caines
// Corrected so egr now runs for year1
//
// Revision 1.31  1999/02/18 18:40:57  caines
// Altered the creation of svm tables
//
// Revision 1.30  1999/02/18 16:43:10  caines
// Added in est the 4th layer tracking
//
// Revision 1.29  1999/02/17 23:58:11  caines
// changed ev0 cuts
//
// Revision 1.28  1999/02/17 20:50:57  fisyak
// reduce no. of reconstructed tracks/verteces from 100K to 20K
//
// Revision 1.27  1999/02/16 21:34:01  caines
//  Added exi back in
//
// Revision 1.26  1999/02/16 03:03:46  fisyak
// Split Make and Histograms
//
// Revision 1.25  1999/02/14 18:38:22  caines
//  Fixed ev0 bugs in hist
//
// Revision 1.24  1999/02/13 20:22:31  caines
// Added exi and temp dir for when svt not there
//
// Revision 1.23  1999/02/12 22:27:35  ogilvie
// added in spectra/pid QA histograms
//
// Revision 1.22  1999/02/12 19:23:46  didenko
// updated v0 finding code from Helen
//
// Revision 1.21  1999/02/11 02:53:37  fisyak
// Janet update to FTPC dst table
//
// Revision 1.20  1999/02/05 17:58:18  fisyak
// Spiros correction to evr
//
// Revision 1.19  1999/01/28 17:09:59  fisyak
// Add ftpc to software monitor
//
// Revision 1.18  1999/01/20 23:58:03  fisyak
// Tree 2 GetTree
//
// Revision 1.17  1999/01/02 19:08:17  fisyak
// Add ctf
//
// Revision 1.16  1998/12/21 19:41:50  fisyak
// Move dst 2 glb
//
// Revision 1.15  1998/12/21 19:26:08  fisyak
// Make ROOT include non system
//
// Revision 1.14  1998/12/17 14:37:19  fisyak
// Fix tp_param
//
// Revision 1.13  1998/12/16 22:22:40  fisyak
// New global from Spiros
//
// Revision 1.12  1998/12/12 02:37:53  fisyak
// fix evr
//
// Revision 1.11  1998/12/01 02:02:38  fisyak
// fix run_summary_param
//
// Revision 1.10  1998/11/25 21:58:24  fisyak
// Cleanup
//
// Revision 1.9  1998/11/12 23:38:36  fisyak
// Account new g2t
//
// Revision 1.8  1998/11/01 16:42:27  fisyak
// dst analysis
//
// Revision 1.7  1998/10/31 00:26:12  fisyak
// Makers take care about branches
//
// Revision 1.6  1998/10/06 18:00:34  perev
// cleanup
//
// Revision 1.5  1998/09/23 20:22:54  fisyak
// Prerelease SL98h
//
// Revision 1.4  1998/09/15 20:55:20  fisyak
// Split St_DataSet -> St_DataSet + St_DataSetIter
//
// Revision 1.3  1998/09/08 22:43:10  fisyak
// Modify St_glb_Maker to account new calling sequence
//
// Revision 1.2  1998/08/26 12:15:08  fisyak
// Remove asu & dsl libraries
//
// Revision 1.1  1998/08/18 14:06:06  fisyak
// Add to bfc dst
//
// Revision 1.3  1998/08/10 02:32:07  fisyak
// Clean up
//
// Revision 1.2  1998/07/20 15:08:15  fisyak
// Add tcl and tpt
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_glb_Maker class for Makers ( est + evr + egr + ev0 + ev0_eval)           //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include <iostream.h>
#include <stdlib.h>
#include <string.h>
//#include "PhysicalConstants.h"
#include "TMath.h"
#include "St_glb_Maker.h"

ClassImp(St_glb_Maker)

St_glb_Maker::St_glb_Maker(const char *name):StMaker(name)
{
  StMaker *saveMK = cd();
  matchMaker = new StMatchMaker();
  primaryMaker = new StPrimaryMaker();
  v0Maker = new StV0Maker();
  xiMaker = new StXiMaker();
  kinkMaker = new StKinkMaker();
  saveMK->cd();
}

St_glb_Maker::~St_glb_Maker()
{
  if(matchMaker)   delete matchMaker;
  if(primaryMaker)   delete primaryMaker;
  if(v0Maker)   delete v0Maker;
  if(xiMaker)   delete xiMaker;
  if(kinkMaker)   delete kinkMaker;
}

Int_t  St_glb_Maker::GetDebug() const
{
  return matchMaker->GetDebug()
      && primaryMaker->GetDebug()
      && v0Maker->GetDebug()
      && xiMaker->GetDebug()
      && kinkMaker->GetDebug();
}

void  St_glb_Maker::SetDebug(Int_t l=1)
{ 
  matchMaker->SetDebug(l);
  primaryMaker->SetDebug(l);
  v0Maker->SetDebug(l);
  xiMaker->SetDebug(l);
  kinkMaker->SetDebug(l);
}
