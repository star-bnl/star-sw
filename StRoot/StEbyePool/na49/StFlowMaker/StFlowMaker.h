///////////////////////////////////////////////////////////////////////////////
//
//  $Id: StFlowMaker.h,v 1.2 2001/05/14 23:04:35 posk Exp $
//
// Authors: Art Poskanzer, LBNL, and Alexander Wetzler, IKF, Dec 2000
//
///////////////////////////////////////////////////////////////////////////////
//
// Description: 
//  Maker to fill StFlowEvent from microDST
//
///////////////////////////////////////////////////////////////////////////////
//
//  $Log: StFlowMaker.h,v $
//  Revision 1.2  2001/05/14 23:04:35  posk
//  Can select PID for event plane particles. Protons not used for 1st har.
//  event plane.
//
//  Revision 1.19  2000/10/12 22:46:38  snelling
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
class StFlowEvent;
class StEbyeTrack;
class StEbyeEvent;
class StFlowSelection;
class StIOMaker;
class StFileI;
class TChain;

class StFlowMaker : public StMaker {

public:

                StFlowMaker(const Char_t* name="Flow");
		StFlowMaker(const Char_t* name,
			      const StFlowSelection& pFlowSelect);
  virtual       ~StFlowMaker();

  Int_t         Init();
  Int_t         InitRun(int);
  Int_t         Make();
  Int_t         Finish();
  StFlowEvent*  FlowEventPointer() const;
  void          SetMicroEventFileName(StFileI* fileList);

protected:

  Flow::PhiWgt_t   mPhiWgt;                   //! To make event plane isotropic
  Flow::MeanCos_t  mMeanCos;                  //! For recentering method
  Flow::MeanSin_t  mMeanSin;                  //! For recentering method


private:
  StFileI*         pMicroFileList;            //! Micro File List
  UInt_t           mMicroEventCounter;        // number of Bytes in micro event
  StFlowSelection* pFlowSelect;               //! selection object
  Int_t            ReadPhiWgtFile();          // get the weight file
  Int_t            ReadMeanSinCosFile();      // get the mean Sin/Cos file
  Int_t            InitMicroEventRead();      // open microDST
  Bool_t           FillFromMicroDST(StEbyeEvent* pMicroEvent);
  Bool_t           FillFromMicroVer1(StEbyeEvent* pMicroEvent);
  void             PrintSubeventMults();      // for testing
  StFlowEvent*     pFlowEvent;                //! pointer
  StEbyeEvent*     pMicroEvent;               //! pointer to micro-DST Event
  StIOMaker*       pIOMaker;                  //! pointer to the IOMaker
  TTree*           pEbyeTree;                 //! pointer to the micro DST Tree
  TFile*           pMicroDST;                 //! pointer to micro-DST File
  TChain*          pMicroChain;               //! pointer to chain of micro files
  Int_t            mRunID;                    // last run ID

  ClassDef(StFlowMaker, 1)                    // macro for rootcint
};

inline StFlowEvent* StFlowMaker::FlowEventPointer() const { return pFlowEvent; }

inline void StFlowMaker::SetMicroEventFileName(StFileI* fileList) {
  pMicroFileList = fileList; }

#endif
