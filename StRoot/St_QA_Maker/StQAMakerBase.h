// $Id: StQAMakerBase.h,v 2.2 2001/04/28 22:05:13 genevb Exp $ 
// $Log: StQAMakerBase.h,v $
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
  {static const char cvs[]="Tag $Name:  $ $Id: StQAMakerBase.h,v 2.2 2001/04/28 22:05:13 genevb Exp $ built "__DATE__" "__TIME__ ; return cvs;}


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

  // for method MakeEvSum - from table event_summary
  TH2F     *m_trk_tot_gd;         //! num of good trks over total - global
  TH2F     *m_glb_trk_tot;        //! # tracks total from globtrk
  TH2F     *m_glb_trk_tot_sm;     //! # tracks total from globtrk, small range
  TH2F     *m_glb_trk_plusminus;  //! # trks pos/neg. 
  TH2F     *m_glb_trk_plusminus_sm; //! # trks pos/neg., small range 
  TH2F     *m_glb_trk_chg;        //! all charge east/west (TPC) 
  TH2F     *m_glb_trk_prim;       //! # trks from primaries
  TH2F     *m_glb_trk_prim_sm;    //! # trks from primaries, small range
  TH2F     *m_vert_total;         //! total number of vertices
  TH2F     *m_vert_total_sm;      //! total number of vertices, small range
  TH2F     *m_mean_pt;            //! mean pt value
  TH2F     *m_mean_pt_sm;         //! mean pt value, small range
  TH2F     *m_mean_eta;           //! mean eta value 
  TH2F     *m_rms_eta;            //! rms eta value 
  TH2F     *m_prim_vrtr;          //! primary vrtx r position
  TH2F     *m_prim_vrtx0;         //! primary vrtx x position
  TH2F     *m_prim_vrtx1;         //! primary vrtx y position
  TH2F     *m_prim_vrtx2;         //! primary vrtx z position



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
  virtual void BookHistEvSum();

  virtual void MakeHistEvSum() = 0;
  virtual void MakeHistGlob() = 0;
  virtual void MakeHistDE() = 0;
  virtual void MakeHistPrim() = 0;
  virtual void MakeHistGen() = 0;
  virtual void MakeHistPID() = 0;
  virtual void MakeHistVertex() = 0;
  virtual void MakeHistPoint() = 0;
  virtual void MakeHistRich() = 0;
  virtual void MakeHistEMC() {}
  virtual void MakeHistEval() = 0;

  ClassDef(StQAMakerBase,0)   //needed for all code that will be used in CINT
};
    
#endif
