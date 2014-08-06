///////////////////////////////////////////////////////////////////////////////
//
//  $Id: StFlowMaker.h,v 1.56 2014/08/06 11:43:15 jeromel Exp $
//
// Author List: 
//  Raimond Snellings, Art Poskanzer, and Sergei Voloshin 6/99
//          FTPC added by Markus Oldenburg, MPI, Dec 2000
//          MuDst enabled by Kirill Filimonov, LBNL, Jun 2002
//
///////////////////////////////////////////////////////////////////////////////
//
// Description: 
//  Maker to fill StFlowEvent from StEvent, picoevent, or muevent
//
///////////////////////////////////////////////////////////////////////////////

#ifndef StFlowMaker_H
#define StFlowMaker_H
#include <Stiostream.h>
#include <stdlib.h>
#include "StMaker.h"
#include "TString.h"
#include "TTree.h"
#include "StFlowConstants.h"
#include "StThreeVectorF.hh"
#include "StPhysicalHelixD.hh"
class StRunInfo;
class StEvent;
class StTrack;
class StParticleDefinition;
class StFlowEvent;
class StFlowPicoTrack;
class StFlowPicoEvent;
class StMuDst;
class StMuEvent;
class StFlowSelection;
class StIOMaker;
class StFileI;
class TChain;
class TClonesArray;
class StHbtEvent; // Randy added these 2
class StHbtTrack;

class StFlowMaker : public StMaker {

public:

                StFlowMaker(const Char_t* name="Flow");
		StFlowMaker(const Char_t* name,
			      const StFlowSelection& pFlowSelect);
  virtual       ~StFlowMaker();

  Int_t         Init();
  Int_t         InitRun(int runumber);
  Int_t         Make();
  Int_t         Finish();
  StFlowEvent*  FlowEventPointer() const;
  void          PicoEventWrite(Bool_t flag=kFALSE);
  void          PicoEventRead(Bool_t flag=kFALSE);
  void          MuEventRead(Bool_t flag=kFALSE);
  void          SetPicoEventDir(const Char_t* name="./");
  void          SetPicoEventFileName(StFileI* fileList);
  void          SetMuEventDir(const Char_t* name="./");
  void          SetMuEventFileName(StFileI* fileList);
  void          SetReCentCalc(Bool_t flag=kTRUE);
  void          SetPhiWgtCalc(Bool_t flag=kTRUE);
  Bool_t        ReCentCalc();
  Bool_t        PhiWgtCalc();
  void          FillFlowEvent(StHbtEvent* hbtEvent); //rcwells added this

  StFlowSelection* FlowSelection();

  virtual const char *GetCVS() const { static const char cvs[]=
    "Tag $Name:  $ $Id: StFlowMaker.h,v 1.56 2014/08/06 11:43:15 jeromel Exp $ built " __DATE__ " " __TIME__ ;
    return cvs; }
  
protected:

  Flow::PhiWgt_t       mPhiWgt;                   //! To make event plane isotropic
  Flow::PhiWgt_t       mPhiWgtFarEast;            //! To make event plane isotropic
  Flow::PhiWgt_t       mPhiWgtEast;               //! To make event plane isotropic
  Flow::PhiWgt_t       mPhiWgtWest;               //! To make event plane isotropic
  Flow::PhiWgt_t       mPhiWgtFarWest;            //! To make event plane isotropic
  Flow::PhiWgtFtpc_t   mPhiWgtFtpcFarEast;        //! To make event plane isotropic
  Flow::PhiWgtFtpc_t   mPhiWgtFtpcEast;           //! To make event plane isotropic
  Flow::PhiWgtFtpc_t   mPhiWgtFtpcWest;           //! To make event plane isotropic
  Flow::PhiWgtFtpc_t   mPhiWgtFtpcFarWest;        //! To make event plane isotropic
  Flow::ZDCSMD_PsiWgt_t  mZDCSMD_PsiWgtWest;	  //! ZDCSMD west Psi
  Flow::ZDCSMD_PsiWgt_t  mZDCSMD_PsiWgtEast;      //! ZDCSMD east Psi
  Flow::ZDCSMD_PsiWgt_t  mZDCSMD_PsiWgtFull;      //! ZDCSMD full Psi
  Double_t mZDCSMDCenterEx, mZDCSMDCenterEy;      //! ZDCSMD Beam Center
  Double_t mZDCSMDCenterWx, mZDCSMDCenterWy;      //! ZDCSMD Beam Center
  Double_t mZDCSMDPed[2][2][8];                   //! ZDCSMD pedestal
  Flow::ReCent_t       mReCentX;                  //! Recentering parameters
  Flow::ReCent_t       mReCentY;                  //! Recentering parameters

private:

  TString          mEventFileName;            //! IO Maker file name
  TString          mEventFileNameOld;         //! IO Maker Old file name
  Char_t           mPicoEventDir[64];         // Pico-DST directory name
  StFileI*         pPicoFileList;             //! Pico File List
  Char_t           mMuEventDir[64];           // Mu-DST directory name
  StFileI*         pMuFileList;               //! Mu-DST File List
  Bool_t           mPicoEventWrite;           // switch for pico-DST
  Bool_t           mPicoEventRead;            // switch for pico-DST
  Bool_t           mMuEventRead;              // switch for Mu-DST
  Bool_t           mReCentCalc;               // switch for recentering
  Bool_t           mPhiWgtCalc;               // switch for phi weighting
  UInt_t           mEventCounter;             // number of Bytes in pico event
  Bool_t           mFirstLastPhiWgt;          // use z of first-last for phi weights
  Int_t            mRunID;                    // last run ID
  Int_t            ReadPhiWgtFile();          // get the weight file
  Int_t            ReadZDCSMDFile();	      // get the ZDCSMD constants
  Int_t            ReadReCentFile();          // get the recenting parameter file
  Int_t            InitPicoEventWrite();      // open pico-DST
  Int_t            InitPicoEventRead();       // open pico-DST
  Int_t            InitMuEventRead();         // open Mu-DST
  Int_t            InitEventRead();           // open StEvent
  void             FillFlowEvent();           // fill the flow event
  void             FillPicoEvent();           // fill pico-DST
  Bool_t           FillFromPicoDST(StFlowPicoEvent* pPicoEvent);
  Bool_t           FillFromPicoVersion7DST(StFlowPicoEvent* pPicoEvent);
  Bool_t           FillFromMuDST();
  void             CloseEventRead();          // close StEvent
  void             PrintSubeventMults();      // for testing
  StFlowSelection* pFlowSelect;               //! selection object
  StEvent*         pEvent;                    //! pointer to DST data
  StFlowEvent*     pFlowEvent;                //! pointer flow event
  StFlowPicoEvent* pPicoEvent;                // pointer to pico-DST Event
  StIOMaker*       pIOMaker;                  //! pointer to the IOMaker
  TTree*           pFlowTree;                 // pointer to pico-DST Tree
  TFile*           pPicoDST;                  //! pointer to pico-DST File
  TChain*          pPicoChain;                //! pointer to chain of pico files
  //TTree*           pMuFlowTree;               // pointer to mu-DST Tree
  TFile*           pMuDST;                    //! pointer to mu-DST File
  TChain*          pMuChain;                  //! pointer to chain of mu-DST files
  StMuDst*         pMu;                       //! pointer to Mu-DST class
  StMuEvent*       pMuEvent;                  //! pointer to Mu-DST Event
  //TClonesArray*    pMuEvents;                 //! pointer to Mu-DST Event array (not used)
  TObjArray*       pMuTracks;                 //! Mu-DST Primary Tracks
  TObjArray*       pMuGlobalTracks;           //! Mu-DST Global Tracks

  Float_t          CalcDcaSigned(const StThreeVectorF pos, 
				 const StTrack* track);
  Float_t          CalcDcaSigned(const StThreeVectorF vertex, 
				 const StPhysicalHelixD helix); 

  ClassDef(StFlowMaker,0)                    // macro for rootcint
};

inline StFlowEvent* StFlowMaker::FlowEventPointer() const { return pFlowEvent; }

inline void StFlowMaker::PicoEventWrite(Bool_t flag) {
  mPicoEventWrite=flag;
  if (flag) mPicoEventRead=kFALSE; }

inline void StFlowMaker::PicoEventRead(Bool_t flag) {
  mPicoEventRead=flag;
  if (flag) mPicoEventWrite=kFALSE; }

inline void StFlowMaker::MuEventRead(Bool_t flag) {
  mMuEventRead=flag; }

inline void StFlowMaker::SetPicoEventDir(const Char_t* name) {
  strncpy(mPicoEventDir, name, 63); mPicoEventDir[63] = '\0'; }

inline void StFlowMaker::SetPicoEventFileName(StFileI* fileList) {
  pPicoFileList = fileList; }

inline void StFlowMaker::SetMuEventDir(const Char_t* name) {
  strncpy(mMuEventDir, name, 63); mMuEventDir[63] = '\0'; }

inline void StFlowMaker::SetMuEventFileName(StFileI* fileList) {
  pMuFileList = fileList; }

inline StFlowSelection* StFlowMaker::FlowSelection() {
  return pFlowSelect; }

inline void StFlowMaker::SetReCentCalc(Bool_t flag) {
  mReCentCalc=flag; }

inline Bool_t StFlowMaker::ReCentCalc() {
  return mReCentCalc; }

inline void StFlowMaker::SetPhiWgtCalc(Bool_t flag) {
  mPhiWgtCalc=flag; }

inline Bool_t StFlowMaker::PhiWgtCalc() {
  return mPhiWgtCalc; }


#endif

///////////////////////////////////////////////////////////////////////////////
//
//  $Log: StFlowMaker.h,v $
//  Revision 1.56  2014/08/06 11:43:15  jeromel
//  Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
//
//  Revision 1.55  2010/03/05 16:49:44  posk
//  Compatable with ROOT 5.22
//
//  Revision 1.54  2009/11/24 19:23:06  posk
//  Added reCenter option to remove acceptance correlations instead of phiWgt.
//
//  Revision 1.53  2009/08/04 23:00:31  posk
//  Reads year 7 MuDsts.
//
//  Revision 1.52  2009/07/28 16:11:55  posk
//  Reinstalled hbt stuff.
//
//  Revision 1.51  2009/07/24 20:23:35  posk
//  Clean up: Removed John Wu's Grid Collector, reading any data before year4, and calculating event plane for hbt Maker. Kept only the most recent pico DST read.
//
//  Revision 1.50  2007/02/06 18:58:00  posk
//  In Lee Yang Zeros method, introduced recentering of Q vector.
//  Reactivated eta symmetry cut.
//
//  Revision 1.49  2006/02/22 19:25:39  posk
//  Changes needed for the MuDst
//  Stopped using eventSummary()
//
//  Revision 1.48  2005/08/23 20:29:43  oldi
//  Latest fix to comply with MuDst changes.
//
//  Revision 1.47  2005/08/19 19:49:13  oldi
//  Change to be in compliance with recent changes in the MuDsts.
//
//  Revision 1.46  2005/07/06 19:39:26  fisyak
//  use templated version of StThreeVectorF and StPhysicalHelixD
//
//  Revision 1.45  2005/02/10 17:39:42  posk
//  Now also works with the Grid Collector.
//
//  Revision 1.44  2004/12/22 15:15:18  aihong
//  Read run-by-run beam shifts and SMD pedestal. Done by Gang
//
//  Revision 1.42  2004/12/09 23:43:37  posk
//  Minor changes in code formatting.
//
//  Revision 1.41  2004/12/07 17:04:48  posk
//  Eliminated the very old mOnePhiWgt, which used one phiWgt histogram for flttening
//  instead of four.
//
//  Revision 1.40  2004/05/31 20:09:38  oldi
//  PicoDst format changed (Version 7) to hold ZDC SMD information.
//  Trigger cut modified to comply with TriggerCollections.
//  Centrality definition for 62 GeV data introduced.
//  Minor bug fixes.
//
//  Revision 1.39  2004/05/05 21:13:44  aihong
//  Gang's code for ZDC-SMD added
//
//  Revision 1.38  2003/12/12 02:33:06  oldi
//  Read from PicoDST version 4 enabled again (some simulations are in this format).
//
//  Revision 1.37  2003/09/10 19:47:15  perev
//  ansi corrs
//
//  Revision 1.36  2003/09/02 17:58:12  perev
//  gcc 3.2 updates + WarnOff
//
//  Revision 1.35  2003/05/06 20:38:05  posk
//  Removed all but last two versions of pico file read.
//
//  Revision 1.34  2003/01/10 16:42:31  oldi
//  Several changes to comply with FTPC tracks:
//  - Switch to include/exclude FTPC tracks introduced.
//    The same switch changes the range of the eta histograms.
//  - Eta symmetry plots for FTPC tracks added and separated from TPC plots.
//  - PhiWgts and related histograms for FTPC tracks split in FarEast, East,
//    West, FarWest (depending on vertex.z()).
//  - Psi_Diff plots for 2 different selections and the first 2 harmonics added.
//  - Cut to exclude mu-events with no primary vertex introduced.
//    (This is possible for UPC events and FTPC tracks.)
//  - Global DCA cut for FTPC tracks added.
//  - Global DCA cuts for event plane selection separated for TPC and FTPC tracks.
//  - Charge cut for FTPC tracks added.
//
//  Revision 1.33  2003/01/08 19:26:50  posk
//  PhiWgt hists sorted on sign of z of first and last points.
//  Version 6 of pico file.
//
//  Revision 1.32  2002/06/10 22:51:02  posk
//  pt and eta weighting now default.
//  DcaGlobalPart default now 0 to 1 cm.
//  Event cut order changed.
//
//  Revision 1.31  2002/06/07 22:18:42  kirill
//  Introduced MuDst reader
//
//  Revision 1.30  2002/03/12 02:33:34  posk
//  Now makes pico files in SL02c.
//
//  Revision 1.29  2002/02/01 23:06:53  snelling
//  Added entries for header information in flowPico (not everthing is available yet)
//
//  Revision 1.28  2001/12/18 19:22:29  posk
//  "proton" and "antiproton" changed to "pr+" and "pr-".
//  Compiles on Solaris.
//
//  Revision 1.27  2001/12/11 21:34:06  posk
//  Went from one to four sets of histograms for making the event plane isotropic.
//  StFlowEvent::PhiWeight() has changed arguments and return value.
//  The ptWgt saturates above 2 GeV/c.
//
//  Revision 1.26  2001/07/27 20:33:45  snelling
//  switched from StRun to StEvtHddr.
//
//  Revision 1.25  2001/07/27 01:26:30  snelling
//  Added and changed variables for picoEvent. Changed trackCut class to StTrack
//
//  Revision 1.24  2001/07/24 22:29:26  snelling
//  First attempt to get a standard root pico file again, added variables
//
//  Revision 1.23  2001/06/04 18:57:06  rcwells
//  Adding filling from HbtEvents
//
//  Revision 1.22  2001/05/22 20:17:46  posk
//  Now can do pseudorapidity subevents.
//
//  Revision 1.21  2000/12/12 20:22:05  posk
//  Put log comments at end of files.
//  Deleted persistent StFlowEvent (old micro DST).
//
//  Revision 1.20  2000/12/08 17:03:39  oldi
//  Phi weights for both FTPCs included.
//
//  Revision 1.19  2000/10/12 22:46:38  snelling
//  Added support for the new pDST's and the probability pid method
//
//  Revision 1.18  2000/09/11 17:24:09  snelling
//  Put picoreader for different versions in seperate methods
//
//  Revision 1.17  2000/08/31 18:58:24  posk
//  For picoDST, added version number, runID, and multEta for centrality.
//  Added centrality cut when reading picoDST.
//  Added pt and eta selections for particles corr. wrt event plane.
//
//  Revision 1.16  2000/08/26 21:37:02  snelling
//  Removed flownanoevent, Added multiple input for pico, fixed IO bug
//
//  Revision 1.15  2000/08/25 19:55:16  snelling
//  Changed naming pico files (1 pico per dst)
//
//  Revision 1.14  2000/07/12 17:54:38  posk
//  Added chi2 and dca cuts. Multiplied EtaSym by ::sqrt(mult).
//  Apply cuts when reading picoevent file.
//
//  Revision 1.13  2000/06/30 14:48:34  posk
//  Using MessageMgr, changed Eta Symmetry cut.
//
//  Revision 1.12  2000/06/20 16:34:26  snelling
//  fixed cout/streamer problem for mPhiWgt under Solaris
//
//  Revision 1.11  2000/06/01 18:26:37  posk
//  Increased precision of Track integer data members.
//
//  Revision 1.10  2000/05/26 21:29:29  posk
//  Protected Track data members from overflow.
//
//  Revision 1.9  2000/05/23 20:09:46  voloshin
//  added StFlowPicoEvent, persistent FlowEvent as plain root TTree
//
//  Revision 1.7  2000/05/16 20:59:32  posk
//  Voloshin's flownanoevent.root added.
//
//  Revision 1.6  2000/05/12 22:42:04  snelling
//  Additions for persistency and minor fix
//
//  Revision 1.4  2000/03/28 23:21:03  posk
//  Allow multiple instances of the AnalysisMaker.
//
//  Revision 1.3  2000/03/21 00:22:02  posk
//  Added GetCVS and some print commands.
//
//  Revision 1.1  2000/03/02 23:02:54  posk
//  Changed extensions from .hh and .cc to .h and .cxx .
//
//  Revision 1.10  2000/02/29 22:00:55  posk
//  Made SetPhiWeight inline, changed ImpactPar to Dca, etc.
//
//  Revision 1.9  2000/02/18 22:49:57  posk
//  Added PID and centrality.
//
//  Revision 1.5  1999/12/15 22:01:28  posk
//  Added StFlowConstants.hh
//
//  Revision 1.4  1999/11/30 18:52:54  snelling
//  First modification for the new StEvent
//
//  Revision 1.3  1999/11/24 18:17:15  posk
//  Put the methods which act on the data in with the data in StFlowEvent.
//
//  Revision 1.1  1999/11/04 19:02:14  snelling
//  First check in of StFlowMaker. It contains the common code from
//  StFlowTagMaker and StFlowAnalysisMaker.
//
///////////////////////////////////////////////////////////////////////////////
