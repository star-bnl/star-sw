// $Id: StEventQAMaker.h,v 1.5 2000/05/25 03:52:11 lansdell Exp $
// $Log: StEventQAMaker.h,v $
// Revision 1.5  2000/05/25 03:52:11  lansdell
// mirrored globtrk histograms for primtrk; removed ev0_eval, vertex: detector id histograms; added generator pT for TPC (|eta|<1), vertex: radial position histograms; merged vertex methods
//
// Revision 1.4  2000/02/07 19:49:06  kathy
// removed L3 trigger histograms and methods that created them - this table is no longer standard on the DST; created methods BookHistEval and MakeHistEval for geant vs reco evaluation histograms; filled geant vs reco evaluation histograms for table-based data
//
// Revision 1.3  1999/11/23 19:00:51  lansdell
// Reorganized Make() and include files (Gene)
//
//
///////////////////////////////////////////////////////////////////////////
//                                                                       //
//  StEventQAMaker class for QA Histograms using StEvent                 //
//     adapted from St_QA_Maker                                          //
//                                                                       //
///////////////////////////////////////////////////////////////////////////

#ifndef STAR_StEventQAMaker
#define STAR_StEventQAMaker

#include "StQABookHist.h"

class StEvent;

//////////////////////////////////////////////////////////////////////////

class StEventQAMaker : public StQABookHist {
 private:
  //! static Char_t m_VersionCVS = "$Id: StEventQAMaker.h,v 1.5 2000/05/25 03:52:11 lansdell Exp $";
 
  StEvent *event;       //! pointer to current event
 
//------------------------------------------------------------------------
  
 public: 

  StEventQAMaker(const char *name="EventQA", const char *title="StEvent/QA");
  virtual       ~StEventQAMaker();
  virtual Int_t  Init();
  virtual Int_t  Finish();
  virtual Int_t  Make();
  
  virtual void   MakeHistEvSum();
  virtual void   MakeHistGlob();
  virtual void   MakeHistDE();
  virtual void   MakeHistPrim();
  virtual void   MakeHistGen();
  virtual void   MakeHistPID();
  virtual void   MakeHistVertex();
  virtual void   MakeHistPoint();
  virtual void   MakeHistRich();
  virtual void   MakeHistEval();
  
//  virtual void   SetPntrToHistUtil(StHistUtil *m1);

// the following is a ROOT macro  that is needed in all ROOT code
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StEventQAMaker.h,v 1.5 2000/05/25 03:52:11 lansdell Exp $ built "__DATE__" "__TIME__ ; return cvs;}

  ClassDef(StEventQAMaker, 1)   //StAF chain virtual base class for Makers
    };
    
#endif
