// $Id: StLaserEventMaker.h,v 1.1 1999/09/28 15:34:34 love Exp $
// $Log: StLaserEventMaker.h,v $
// Revision 1.1  1999/09/28 15:34:34  love
// change LSEvent to LaserEvent
//

// Revision 1.1.1.1  1999/09/28  love
// First release of Laser Event Maker using tpt tracks.
//


//
#ifndef STAR_StLaserEventMaker
#define STAR_StLaserEventMaker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StLaserEventMaker virtual base class for Maker                       //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif
class St_tpg_pad_plane;
class St_tcl_tpc_index_type;
class St_tpt_pars;
class TTree;
class StLaserEvent;

enum { maxNofTracks = 200}; 

class StLaserEventMaker : public StMaker {
private:
  Int_t m_runno;          //Run number to put on events (derive from filename)
  Int_t m_date;           //date to put in event header
  Int_t m_rowmin ; Int_t m_rowmax ;  //Range for the pixel branch.
  Bool_t m_mklaser;   	//control flag for laser tree production
  St_tpg_pad_plane      *m_tpg_pad_plane;	//! Constants that describe TPC pad plane
  St_tcl_tpc_index_type *m_type;   		//! Table of many-to-many index 
	                                        //! correlations for tpc evaluations
  St_tpt_pars           *m_tpt_pars;  		//! Parameters for the track finding

  void         MakeHistograms();// Histograms for tracking
protected:
 TTree                *m_laser; //! Laser track-hit event Tree
 StLaserEvent              *event;  //! Laser Straight Track Event structure 

public: 
  StLaserEventMaker(const char *name="tpc_stracks");
  virtual       ~StLaserEventMaker();

  virtual Int_t  Init();
  virtual Int_t  Make();
  virtual void   Clear(Option_t *option="");
  virtual void   SetRun(Int_t run) {m_runno = run;} 
  virtual void   SetDate(Int_t date) {m_date = date;} 
  virtual void   SetRows(Int_t min, Int_t max) {m_rowmin=min; m_rowmax=max;}
  virtual void   Set_laser(Bool_t m=kTRUE){m_mklaser = m;}
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StLaserEventMaker.h,v 1.1 1999/09/28 15:34:34 love Exp $ built "__DATE__" "__TIME__ ; return cvs;}

ClassDef(StLaserEventMaker, 1)   //StAF chain virtual base class for Makers
};

#endif
