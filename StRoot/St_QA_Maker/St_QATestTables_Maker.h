
//! $Id: St_QATestTables_Maker.h,v 1.1 1999/08/13 17:16:30 kathy Exp $
//! $Log: St_QATestTables_Maker.h,v $
//! Revision 1.1  1999/08/13 17:16:30  kathy
//! add new maker St_QATestTables_Maker in directory St_QA_Maker, written by Aya Ishihara, calculates and prints info about tables - used for QA
//!

#ifndef STAR_St_QATestTables_Maker
#define STAR_St_QATestTables_Maker

//////////////////////////////////////////////////////////////////////////
//!                                                                      //
//! St_QATestTables_Maker virtual base class for Maker                   //
//!                                                                      //
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif
#ifndef ROOT_TH1
#include "TH1.h"
#endif
#ifndef ROOT_TH2
#include "TH2.h"
#endif

#include "TList.h"
#include "TString.h"



class St_QATestTables_Maker : public StMaker {
 
private:
  Int_t number_events;
  
    //  For globtrk
  Int_t     nover_Pt_glob;
  Int_t     number_trk_glob;
  Int_t     number_events_glob;
  Int_t     number_good_trk_glob;
  Float_t   sum_Pt_glob;
  Float_t   sum_sq_Pt_glob;
  Float_t   rms_Pt_glob;
  Float_t   mean_Pt_glob;

   //  For primtrk
  Int_t     nover_Pt_prim;
  Int_t     number_trk_prim;
  Int_t     number_events_prim;
  Int_t     number_good_trk_prim;
  Float_t   sum_Pt_prim;
  Float_t   sum_sq_Pt_prim;
  Float_t   rms_Pt_prim;
  Float_t   mean_Pt_prim;
 
  //------------------------------------------------------------------------
  //
 
 public: 
                 St_QATestTables_Maker( const char *name="QATestTables", const char *title="event/QATestTables");
  virtual       ~St_QATestTables_Maker();
  virtual Int_t  Init();
  virtual Int_t  Finish();
  virtual Int_t  Make();
  virtual void   PrintInfo();
  virtual void   TestTablesInit();
  //  virtual void   TestTableEvent_summary(St_DataSet *,char *);
  //  virtual void   TestTables_test(St_DataSet *);
  virtual void   TestTables_testGlobtrk(St_DataSet *);
  virtual void   TestTables_testPrimtrk(St_DataSet *);
  virtual void   TestTablesFinish();
// the following is a ROOT macro  that is needed in all ROOT code
  ClassDef(St_QATestTables_Maker, 1)  //StAF chain virtual base class for Maker
    };
                                       
#endif
    












