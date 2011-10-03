// $Id: St_tptsts_Maker.h,v 1.1.1.1 1999/05/10 13:24:15 love Exp $
// $Log: St_tptsts_Maker.h,v $
// Revision 1.1.1.1  1999/05/10 13:24:15  love
// Straight track Maker
//


//
#ifndef STAR_St_tptsts_Maker
#define STAR_St_tptsts_Maker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_tptsts_Maker virtual base class for Maker                            //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif
class St_tpg_pad_plane;
class St_tcl_tpc_index_type;
class St_tpt_pars;
class St_tpt_spars;
class TNtuple;

enum { maxNofTracks = 200}; 

class St_tptsts_Maker : public StMaker {
private:
  Bool_t m_mkstks;   	//control flag for stks ntuple production
  St_tpg_pad_plane      *m_tpg_pad_plane;	//! Constants that describe TPC pad plane
  St_tcl_tpc_index_type *m_type;   		//! Table of many-to-many index 
	                                        //! correlations for tpc evaluations
  St_tpt_pars           *m_tpt_pars;  		//! Parameters for the track finding
  St_tpt_spars          *m_tpt_spars; 		//! Parameters for the track finding
  void         MakeHistograms();// Histograms for tracking
protected:
 TNtuple                *m_stks; //! Stks track-hit Ntuple

public: 
  St_tptsts_Maker(const char *name="tpc_stracks");
  virtual       ~St_tptsts_Maker();

  virtual Int_t  Init();
  virtual Int_t  Make();
  virtual void   PrintInfo();
  virtual void   Set_stks(Bool_t m=kTRUE){m_mkstks = m;}
ClassDef(St_tptsts_Maker, 1)   //StAF chain virtual base class for Makers
};

#endif
