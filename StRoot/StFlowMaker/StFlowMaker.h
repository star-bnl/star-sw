///////////////////////////////////////////////////////////////////////////////
//
//  $Id: StFlowMaker.h,v 1.12 2000/06/20 16:34:26 snelling Exp $
//
// Author List: 
//  Raimond Snellings, Art Poskanzer, and Sergei Voloshin 6/99
//
///////////////////////////////////////////////////////////////////////////////
//
// Description: 
//  Maker to fill StFlowEvent from StEvent
//
///////////////////////////////////////////////////////////////////////////////
//
//  $Log: StFlowMaker.h,v $
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
//  Revision 1.8  2000/05/20 00:55:17  posk
//  Condensed flownanoevent.root somewhat.
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
//  Revision 1.2  2000/03/07 17:50:59  snelling
//  Added Nano DST
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
//  Revision 1.7  1999/12/21 01:11:01  posk
//  Added more quantities to StFlowEvent.
//
//  Revision 1.6  1999/12/16 18:05:24  posk
//  Fixed Linux compatability again.
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
//  Revision 1.2  1999/11/11 23:08:58  posk
//  Rearrangement of files.
//
//  Revision 1.1  1999/11/04 19:02:14  snelling
//  First check in of StFlowMaker. It contains the common code from
//  StFlowTagMaker and StFlowAnalysisMaker.
//
//
///////////////////////////////////////////////////////////////////////////////

#ifndef StFlowMaker_H
#define StFlowMaker_H
#include <iostream.h>
#include <stdlib.h>
#include "StMaker.h"
#include "TString.h"
#include "TTree.h"
#include "StFlowConstants.h"
class StEvent;
class StPrimaryTrack;
class StParticleDefinition;
class StFlowEvent;
class StFlowNanoEvent;
class StFlowPicoTrack;
class StFlowPicoEvent;
class StFlowSelection;

class StFlowMaker : public StMaker {

public:

                StFlowMaker(const Char_t* name="Flow");
		StFlowMaker(const Char_t* name,
			      const StFlowSelection& pFlowSelect);
  virtual       ~StFlowMaker();

  Int_t         Init();
  void          PrintInfo();
  Int_t         Make();
  Int_t         Finish();
  StFlowEvent*  FlowEventPointer() const;
  void          NanoEventWrite(Bool_t flag=kFALSE);
  void          NanoEventRead(Bool_t flag=kFALSE);
  void          PicoEventWrite(Bool_t flag=kFALSE);
  void          PicoEventRead(Bool_t flag=kFALSE);
  void          FlowEventWrite(Bool_t flag=kFALSE);
  void          FlowEventRead(Bool_t flag=kFALSE);
  void          SetNanoEventFileName(const Char_t* name="flownanoevent.root");
  void          SetPicoEventFileName(const Char_t* name="flowpicoevent.root");
  virtual const char *GetCVS() const { static const char cvs[]=
    "Tag $Name:  $ $Id: StFlowMaker.h,v 1.12 2000/06/20 16:34:26 snelling Exp $ built "__DATE__" "__TIME__ ;
    return cvs; }
  
protected:

  Flow::PhiWgt_t   mPhiWgt;                   //! To make event plane isotropic

private:
  Char_t           mNanoEventFileName[64];    // nano-DST file name
  Bool_t           mNanoEventWrite;           // switch for nano-DST
  Bool_t           mNanoEventRead;            // switch for nano-DST
  Char_t           mPicoEventFileName[64];    // nano-DST file name
  Bool_t           mPicoEventWrite;           // switch for nano-DST
  Bool_t           mPicoEventRead;            // switch for nano-DST
  Bool_t           mFlowEventWrite;           // switch for StFlowEvent
  Bool_t           mFlowEventRead;            // switch for StFlowEvent
  UInt_t           mNanoEventCounter;         // number of Bytes in nano event
  UInt_t           mPicoEventCounter;         // number of Bytes in nano event
  StFlowSelection* pFlowSelect;               //! selection object
  Int_t            ReadPhiWgtFile();          // get the weight file
  Int_t            InitNanoEventWrite();      // open nano-DST
  Int_t            InitNanoEventRead();       // open nano-DST
  Int_t            InitPicoEventWrite();      // open nano-DST
  Int_t            InitPicoEventRead();       // open nano-DST
  Int_t            InitEventRead();           // open StEvent
  Int_t            InitFlowEventWrite();      // open StFlowEvent
  Int_t            InitFlowEventRead();       // open StFlowEvent
  void             FillFlowEvent();           // fill the flow event
  void             FillNanoEvent();           // fill nano-DST
  Bool_t           FillFromNanoDST(const StFlowNanoEvent* pNanoEvent);
  void             FillPicoEvent();           // fill nano-DST
  Bool_t           FillFromPicoDST(const StFlowPicoEvent* pPicoEvent);
  void             WriteFlowEvent();          // write StFlowEvent
  void             CloseEventRead();          // close StEvent
  void             PrintSubeventMults();      // for testing
  StEvent*         pEvent;                    //! pointer to DST data
  StFlowEvent*     pFlowEvent;                // pointer to micro-DST data
  StFlowNanoEvent* pNanoEvent;                // pointer to nano-DST Event
  StFlowPicoEvent* pPicoEvent;                // pointer to nano-DST Event
  TTree*           pFlowTree;                 // pointer to nano-DST Tree
  TTree*           pFlowMicroTree;            // pointer to the micro DST Tree
  TFile*           pNanoDST;                  //! pointer to nano-DST File
  TFile*           pPicoDST;                  //! pointer to nano-DST File
  TFile*           pFlowDST;                  //! pointer to micro-DST File

  ClassDef(StFlowMaker, 1)                    // macro for rootcint
};

inline StFlowEvent* StFlowMaker::FlowEventPointer() const { return pFlowEvent; }

inline void StFlowMaker::FlowEventWrite(Bool_t flag) 
          { mFlowEventWrite=flag; if (flag) mFlowEventRead=kFALSE; }

inline void StFlowMaker::FlowEventRead(Bool_t flag) 
          { mFlowEventRead=flag; if (flag) { mFlowEventWrite=kFALSE;
	  mNanoEventRead=kFALSE; mPicoEventRead=kFALSE; } }

inline void StFlowMaker::NanoEventWrite(Bool_t flag) 
          { mNanoEventWrite=flag; if (flag) mNanoEventRead=kFALSE; }

inline void StFlowMaker::PicoEventWrite(Bool_t flag) 
          { mPicoEventWrite=flag; if (flag) mPicoEventRead=kFALSE; }

inline void StFlowMaker::NanoEventRead(Bool_t flag) 
          { mNanoEventRead=flag; if (flag) { mNanoEventWrite=kFALSE;
	  mFlowEventRead=kFALSE; mPicoEventRead=kFALSE; } }

inline void StFlowMaker::PicoEventRead(Bool_t flag) 
          { mPicoEventRead=flag; if (flag) { mPicoEventWrite=kFALSE;
	  mFlowEventRead=kFALSE; mNanoEventRead=kFALSE; }}

inline void StFlowMaker::SetNanoEventFileName(const Char_t* name) {
  strncpy(mNanoEventFileName, name, 63); mNanoEventFileName[63] = '\0'; }

inline void StFlowMaker::SetPicoEventFileName(const Char_t* name) {
  strncpy(mPicoEventFileName, name, 63); mPicoEventFileName[63] = '\0'; }

#endif
