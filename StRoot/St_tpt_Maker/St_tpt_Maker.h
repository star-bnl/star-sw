// $Id: St_tpt_Maker.h,v 1.15 2000/06/15 19:06:15 aihong Exp $
// $Log: St_tpt_Maker.h,v $
// Revision 1.15  2000/06/15 19:06:15  aihong
// ensemble truncation for de/dx calculation added
//
// Revision 1.14  2000/03/07 05:14:55  sakrejda
// Jan's modifications to run on L3 clusters (I think)
//
// Revision 1.13  2000/02/23 03:41:39  sakrejda
// Histograms names in EVAL changed to TptTte* by Kathy.
// Also comments added.
//
// Revision 1.12  1999/07/15 13:58:28  perev
// cleanup
//
// Revision 1.11  1999/06/02 01:29:14  sakrejda
// functions to switch on/off tte_track added
//
// Revision 1.10  1999/05/05 18:45:42  liq
// include valuation plots of reconstraction
//
// Revision 1.9  1999/03/14 00:23:38  perev
// New makers
//
// Revision 1.8  1999/03/01 18:24:08  sakrejda
// evaluation and residuals calculation made switchable
//
// Revision 1.7  1999/02/25 20:55:33  love
// ntuple named final added
//
// Revision 1.6  1999/01/12 19:50:20  sakrejda
// QA histograms added to the tpt maker
//
// Revision 1.5  1999/01/08 23:19:42  sakrejda
// histogramming added
//
// Revision 1.4  1998/10/31 00:26:24  fisyak
// Makers take care about branches
//
// Revision 1.3  1998/10/06 18:00:51  perev
// cleanup
//
// Revision 1.2  1998/08/18 14:05:04  fisyak
// Add to bfc dst
//
// Revision 1.1  1998/07/21 00:36:47  fisyak
// tcl and tpt
//
#ifndef STAR_St_tpt_Maker
#define STAR_St_tpt_Maker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_tpt_Maker virtual base class for Maker                            //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#ifndef StMaker_H
#include "StMaker.h"
#endif

class St_tpg_pad_plane;
class St_tcl_tpc_index_type;
class St_tpt_pars;
class St_tpt_spars;
class St_tte_control;
class St_tdeparm;
class St_tpipar;
class TH1F;
class TH2F;
class TH3F;
class TNtuple;

enum { maxNofTracks = 20000}; 

class St_tpt_Maker : public StMaker {
private:
  Bool_t m_iftteTrack;
  Bool_t m_mkfinal;   	//control flag for final ntuple production
  Bool_t m_tteEvalOn; 	//switch for the evaluation
  Bool_t m_tptResOn;  	//switch for the residuals calculation
  Bool_t m_ensembleOn;  //switch for ensemble truncation
  TString m_InputDataSetName; //! 
  TString m_InputHitName; //!

//static Char_t m_VersionCVS = "$Id: St_tpt_Maker.h,v 1.15 2000/06/15 19:06:15 aihong Exp $";
  St_tpg_pad_plane      *m_tpg_pad_plane;	//! Constants that describe TPC pad plane
  St_tcl_tpc_index_type *m_type;   		//! Table of many-to-many index 
	                                        //! correlations for tpc evaluations
  St_tpt_pars           *m_tpt_pars;  		//! Parameters for the track finding
  St_tpt_spars          *m_tpt_spars; 		//! Parameters for the track finding
  St_tte_control        *m_tte_control;		//! Control switches for the evaluation 
  St_tdeparm            *m_tdeparm;   		//! Parameters for the tde dedx module
  St_tpipar             *m_tpipar;    		//! parameter file for tpi package
  void         MakeHistograms();// Histograms for tracking
  void         VertexEffResolutionInit();// Initial function  for VertexEffResolution
  void         VertexEffResolutionMakeHistograms();// Histograms for VertexEffResolution
  void         VertexEffResolutionFinish();// Calculate efficiency for this run

protected:

 TNtuple                *m_final; //! Final track-hit Ntuple

// These histograms are always booked:
//  - reconstructed track histograms from values calculated in St_tpt_Maker:

 TH1F *m_hits_on_track; //!number of hits assigned to a reconstructed track
 TH1F *m_hits_in_fit;   //!number of hits used in a fit
 TH1F *m_azimuth;       //!azimuthal angle
 TH1F *m_tan_dip;       //!tangent of the dip angle
 TH1F *m_r0;            //!radius for the first point
 TH1F *m_invp;          //!inverse momentum


// These histograms are only booked if m_tteEvalOn=kTrue (i.e. evaluation on):
// Tables used are:  mctrk, evaltrk, g2t_vertex 

//These are booked in VertexEffResolutionInit
 TH1F *m_vertex_x; //! pointer to 1d hist. of vertex x
 TH1F *m_vertex_y; //! pointer to 1d hist. of vertex y
 TH1F *m_vertex_z; //! pointer to 1d hist. of vertex z
 TH1F *m_vertex_xy; //! pointer to 1d hist. of vertex xy

 TH2F  *m_vertexX_vertexY; //! vertex positionY vs.  vertex X
 TH2F  *m_vertexX_vertexZ; //! vertex positionZ vs.  vertex X

 TH1F *m_average_ptr; //! pointer to 1dhist of average ptr 
 TH1F *m_average_ptg; //! pointer to 1dhist of average ptg 
 TH1F *m_average_rapidity; //! pointer to 1dhist of average pseudo_rapidity 
 TH1F *m_average_chisqxy; //! pointer to 1dhist of average chisqxy
 TH1F *m_average_chisqz; //! pointer to 1dhist of average chisqz

 TH2F  *m_vertexXY_eff; //! vertex position vs.  efficiency
 TH2F  *m_vertexZ_eff; //! vertex position vs.  efficiency


 TH1F *m_chisqxy; //! pointer to 1dhist of chisqxy
 TH1F *m_chisqz; //! pointer to 1dhist of  chisqz


   TH2F *m_vertexXY_average_ptr; //! vertex xy vs. average ptr 
   TH2F *m_vertexZ_average_ptr; //! vertex z vs. average ptr 
   TH2F *m_vertexXY_average_ptg; //! vertex xy vs. average ptg 
   TH2F *m_vertexZ_average_ptg; //! vertex z vs. average ptg 

   TH2F *m_vertexXY_average_rapidity; //! vertex xy vs. average rapidity 
   TH2F *m_vertexZ_average_rapidity; //! vertex z vs. average rapidity 

   TH2F *m_vertexXY_average_chisqxy; //! vertex xy vs. average chisqxy 
   TH2F *m_vertexZ_average_chisqxy; //! vertex z vs. average chisqxy 

   TH2F *m_vertexXY_average_chisqz; //! vertex xy vs. average chisqz 
   TH2F *m_vertexZ_average_chisqz; //! vertex z vs. average chisqz

   TH1F *m_rapidity_total1;  //! pointer to 1dhist of rapidity(vid=1,nfst>5) total events
   TH1F *m_rapidity_total2; //! pointer to 1dhist of rapidity(vid=1,nfst>5,nrec>0) total events
   TH1F *m_eff_total; //! pointer to efficiency  vs. rapidity total events
   TH1F *m_averge_eff;//! pointer to efficiency  for every event

   TH2F *m_ptg_rapidity; // ! Ptg vs. pseudo_rapidity 
   TH2F *m_ptg_rapidity_1; // ! Ptg vs. pseudo_rapidity, nrec1>=0 
   TH2F *m_ptg_rapidity_2; // ! Ptg vs. pseudo_rapidity, nrec1>0
   TH3F *m_ptg_rapidity_dpt; //!Ptg vs. pseudo_rapidity vs. abs(ptr-ptg)/ptg

   TH1F  *m_dpt; //! abs(ptr-ptg)/ptg
   TH2F  *m_dpt_ptg;//! abs(ptr-ptg)/ptg vs. ptg

   TH1F  *m_dp; //! abs(pr-pg)/pg
   TH2F  *m_dp_pg;//! abs(pr-pg)/pg vs. pg

   TH2F  *m_dp_pg_pion; //! abs(pr-pg)/pg vs. pg for pion
   TH2F  *m_dp_pg_proton; //! abs(pr-pg)/pg vs. pg for pion
   TH2F  *m_dp_pg_kaon; //! abs(pr-pg)/pg vs. pg for pion


//These are booked,filled & deleted in VertexEffResolutionMakeHistograms
   TH1F *m_rapidity1;  //! pointer to 1dhist of rapidity(vid=1,nfst>5) per event
   TH1F *m_rapidity2;  //! pointer to 1dhist of rapidity(vid=1,nfst>5,nrec>0) per event
   TH1F *m_eff1;       //! pointer to efficiency  vs. rapidity of 1 event


// - not used:
// TH1F *m_ptr;        //! pointer to 1d hist. of rec. pt
// TH1F *m_rapidity;   //! pointer to 1dhist of rapidity(vid=1,nfst>5,nrec>0) all tracks


//init ntuple
   TNtuple *m_vertex_final; //! ntuple for efficiency, average pt, rapidity,.....
    Int_t  mevent;

public: 
  St_tpt_Maker(const char *name="tpc_tracks");
  virtual       ~St_tpt_Maker();
  void SetInputHits(  TString , TString );
  virtual void   tteEval(Bool_t flag=kFALSE){m_tteEvalOn=flag;}
  virtual void   tteEvalOn() {tteEval(kTRUE);}                       // *MENU*
  virtual void   tteEvalOff(){tteEval();}                            // *MENU
  virtual void   tptRes(Bool_t flag=kFALSE){m_tptResOn=flag;}
  virtual void   tptResOn() {tptRes(kTRUE);}                         // *MENU*
  virtual void   tptResOff(){tptRes();}                              // *MENU
  virtual void   tteTrack(Bool_t flag=kFALSE){m_iftteTrack=flag;}
  virtual void   tteTrackOn() {tteTrack(kTRUE);}                     // *MENU*
  virtual void   tteTrackOff(){tteTrack();}                          // *MENU

  virtual Int_t  Init();
  virtual Int_t  Make();
  virtual Int_t  Finish();
  virtual void   Set_final(Bool_t m=kFALSE){m_mkfinal = m;}
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: St_tpt_Maker.h,v 1.15 2000/06/15 19:06:15 aihong Exp $ built "__DATE__" "__TIME__ ; return cvs;}

 ClassDef(St_tpt_Maker, 1)   //StAF chain virtual base class for Makers
};

#endif
