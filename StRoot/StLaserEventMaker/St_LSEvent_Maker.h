// $Id: St_LSEvent_Maker.h,v 1.1.1.1 1999/04/27 14:29:31 love Exp $
// $Log: St_LSEvent_Maker.h,v $
// Revision 1.1.1.1  1999/04/27 14:29:31  love
// First release of Laser Event
//


//
#ifndef STAR_St_LSEvent_Maker
#define STAR_St_LSEvent_Maker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_LSEvent_Maker virtual base class for Maker                            //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif
class St_tpg_pad_plane;
class St_tcl_tpc_index_type;
class St_tpt_pars;
class St_tpt_spars;
class TTree;
class LSEvent;

enum { maxNofTracks = 100}; 

class St_LSEvent_Maker : public StMaker {
private:
  Int_t m_runno;          //Run number to put on events (derive from filename)
  Int_t m_date;           //date to put in event header
  Int_t m_rowmin ; Int_t m_rowmax ;  //Range for the pixel branch.
  Bool_t m_mkstks;   	//control flag for stks tree production
  St_tpg_pad_plane      *m_tpg_pad_plane;	//! Constants that describe TPC pad plane
  St_tcl_tpc_index_type *m_type;   		//! Table of many-to-many index 
	                                        //! correlations for tpc evaluations
  St_tpt_pars           *m_tpt_pars;  		//! Parameters for the track finding
  St_tpt_spars          *m_tpt_spars; 		//! Parameters for the track finding
  void         MakeHistograms();// Histograms for tracking
protected:
 TTree                *m_stks; //! Stks track-hit event Tree
 LSEvent              *event;  //! Laser Straight Track Event structure 

public: 
  St_LSEvent_Maker(const char *name="tpc_stracks");
  virtual       ~St_LSEvent_Maker();

  virtual Int_t  Init();
  virtual Int_t  Make();
  virtual void   SetRun(Int_t run) {m_runno = run;} 
  virtual void   SetDate(Int_t date) {m_date = date;} 
  virtual void   SetRows(Int_t min, Int_t max) {m_rowmin=min; m_rowmax=max;}
  virtual void   PrintInfo();
  virtual void   Set_stks(Bool_t m=kTRUE){m_mkstks = m;}
ClassDef(St_LSEvent_Maker, 1)   //StAF chain virtual base class for Makers
};

#endif
