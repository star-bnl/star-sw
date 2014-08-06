// $Id: StQAMakerBase.h,v 2.7 2002/04/23 01:59:56 genevb Exp $ 
// $Log: StQAMakerBase.h,v $
// Revision 2.7  2002/04/23 01:59:56  genevb
// Addition of BBC/FPD histos
//
// Revision 2.6  2002/01/26 03:04:07  genevb
// Fixed some problems with fcl histos
//
// Revision 2.5  2001/12/28 09:19:13  genevb
// Adjustments for pp running
//
// Revision 2.4  2001/08/29 20:45:15  genevb
// Trigger word histos
//
// Revision 2.3  2001/05/16 20:57:03  lansdell
// new histograms added for qa_shift printlist; some histogram ranges changed; StMcEvent now used in StEventQA
//
// Revision 2.2  2001/04/28 22:05:13  genevb
// Added EMC histograms
//
// Revision 2.1  2000/08/25 16:04:10  genevb
// Introduction of files
//
//
///////////////////////////////////////////////////////////////////////////
//                                                                       //
//  StQAMakerBase base class for QA Histogram Makers                     //
//                                                                       //
///////////////////////////////////////////////////////////////////////////

#ifndef STAR_StQAMakerBase
#define STAR_StQAMakerBase

#include "StMaker.h"
class StQABookHist;
class TList;
class TH1F;
class TH2F;

class StQAMakerBase : public StMaker {

// **************** Public Member Functions ***************************
 public:
  StQAMakerBase() {}
  StQAMakerBase(const char *name, const char *title, const char *type);
  virtual       ~StQAMakerBase();
  virtual Int_t  Init();
  virtual Int_t  Make();
// the following is a ROOT macro  that is needed in all ROOT code
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StQAMakerBase.h,v 2.7 2002/04/23 01:59:56 genevb Exp $ built "__DATE__" "__TIME__ ; return cvs;}


// ******************** Histogram Booking Constants ************************
 protected:

  Int_t ntrk;
  Int_t nmnpt;  
  Int_t nmneta; 
  Int_t nxyz;   

// ***************** Histogram Pointers ***************************
 public:
  // histogram for number of events without primary vertex
  TH1F     *mNullPrimVtx;         //!
  // histogram for number of events in mult classes
  TH1F     *mMultClass;           //!
  // histograms for event trigger words/bits
  TH1F     *mTrigWord;            //!
  TH1F     *mTrigBits;            //!
  // for method MakeEvSum - from software monitor
  TH2F     *m_glb_trk_chg;        //! all charge east/west (TPC) 
  TH2F     *m_glb_trk_chgF;       //! all charge east/west (FTPC) 

// **************** Members For Internal Use ***************************
 protected:
  TString QAMakerType;  // character string to prepend to each hist name/title
  Int_t   multiplicity; // multiplicity class of current event,
                        //  should be set in Make() of derived maker class
  TList* histsList;     // pointers to the histogram classes for the
  StQABookHist* hists;  //!     multiplicity-dependent histograms
  Int_t histsSet;
  Float_t multClass;
  Bool_t firstEvent;

  virtual void NewQABookHist(const char* prefix);
  virtual TH2F* MH1F(const Text_t* name, const Text_t* title,
                     Int_t nbinsx, Axis_t xlow, Axis_t xup);

  virtual void BookHist();
  virtual void BookHistGeneral();
  virtual void BookHistTrigger();
  virtual void BookHistEvSum();
  virtual void BookHistFcl();

  virtual void MakeHistEvSum() = 0;
  virtual void MakeHistGlob() = 0;
  virtual void MakeHistDE() = 0;
  virtual void MakeHistPrim() = 0;
  virtual void MakeHistPID() = 0;
  virtual void MakeHistVertex() = 0;
  virtual void MakeHistPoint() = 0;
  virtual void MakeHistRich() = 0;
  virtual void MakeHistEMC() {}
  virtual void MakeHistEval() = 0;
  virtual void MakeHistBBC() {}
  virtual void MakeHistFPD() {}

  ClassDef(StQAMakerBase,0)   //needed for all code that will be used in CINT
};
    
#endif
