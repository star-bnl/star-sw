// $Id: StLaserEventMaker.h,v 1.4 2000/04/24 14:36:34 love Exp $
// $Log: StLaserEventMaker.h,v $
// Revision 1.4  2000/04/24 14:36:34  love
// Write clock, drivel, tzero on Event Header.  truncate psi angles to 0-180 range
// Expand doca to do straight tracks and do 12 laser sectors, add z cut.
// Expand to 2000 possible track numbers.
//
// Revision 1.3  2000/02/15 21:01:26  love
// Maximum tracks upped to 1000
//
// Revision 1.2  1999/11/30 14:34:34  love
// Corrections to make CC5 compile.  DOCA routine added.
//
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

enum { maxNofTracks = 2000}; 

class StLaserEventMaker : public StMaker {
private:
  Int_t m_runno;          //Run number to put on events (derive from filename)
  Int_t m_date;           //date to put in event header
  Float_t m_tzero;        // record tzero etc. in the event header
  Float_t m_drivel;
  Float_t m_clock;
  Int_t m_rowmin ; Int_t m_rowmax ;  //Range for the pixel branch.
  Bool_t m_mklaser;   	          //control flag for laser tree production
  St_tpg_pad_plane      *m_tpg_pad_plane;  //! Constants that describe TPC
                                          //! pad plane geometry
  St_tcl_tpc_index_type *m_type;   	  //! Table of many-to-many index 
	                                  //! correlations for tpc evaluations
  St_tpt_pars           *m_tpt_pars;  	  //! Parameters for the track finding

  void         MakeHistograms();// Histograms for tracking
protected:
 TTree                *m_laser; //! Laser track-hit event Tree
 StLaserEvent              *event;  //! Laser Event object 

public: 
  StLaserEventMaker(const char *name="tpc_stracks");
  virtual       ~StLaserEventMaker();

  virtual Int_t  Init();
  virtual Int_t  Make();
  virtual void   Clear(Option_t *option="");
  virtual void  DOCA(Float_t r0,Float_t phi0,Float_t z0,
                      Float_t psi, Float_t tanl, Float_t curvature, Int_t q,
                      Int_t *sector, Float_t *xl, Float_t *yl, Float_t *zl); 
  virtual void   SetRun(Int_t run) {m_runno = run;} 
  virtual void   SetDate(Int_t date) {m_date = date;} 
  virtual void   SetRows(Int_t min, Int_t max) {m_rowmin=min; m_rowmax=max;}
  virtual void   Set_laser(Bool_t m=kTRUE){m_mklaser = m;}

  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StLaserEventMaker.h,v 1.4 2000/04/24 14:36:34 love Exp $ built "__DATE__" "__TIME__ ; return cvs;}

ClassDef(StLaserEventMaker, 1)   //StAF chain virtual base class for Makers
};

#endif
