// $Id: StLaserEventMaker.h,v 1.12 2001/12/23 20:08:04 pfachini Exp $
// $Log: StLaserEventMaker.h,v $
// Revision 1.12  2001/12/23 20:08:04  pfachini
// *** empty log message ***
//
// Revision 1.7  2001/11/28 17:54:21  love
// Default ExB flags to FALSE
//
// Revision 1.6  2001/07/17 17:18:45  love
// phi variable added to lasertrack def
//
// Revision 1.5  2001/03/26 18:27:00  love
// Added many features.  Calculates DOCA for laser tracks to mirror positions.  POCA
//  for non laser events to x,y = 0,0.
//
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
#include "StDbUtilities/StMagUtilities.h"
class St_tpg_pad_plane;
class St_tcl_tpc_index_type;
class St_tpt_pars;
class St_tpcDriftVelocity;

class TTree;
class StLaserEvent;

enum { maxNofTracks = 8000}; 

class StLaserEventMaker : public StMaker {
private:
  TH1* fzLaser;
  TH1* fzlWestHigh;
  TH1* fzlWestLow;
  TH1* fzlEastHigh;
  TH1* fzlEastLow;
  TH1* driftVelocityRec;
  TH1* numberTracks;
  Bool_t   mHistOut;
  Int_t m_runno;          //Run number to put on events (derive from filename)
  Int_t m_date;           //date to put in event header
  Int_t m_time;           //time to put in event header
  Float_t m_tzero;        // record tzero etc. in the event header
  Float_t m_drivel;
  Float_t m_clock;
  Float_t m_trigger;//additional time added to tZero for trigger delay
  Int_t m_rowmin ; Int_t m_rowmax ;  //Range for the pixel branch.
  Bool_t m_mklaser;   	          //control flag for laser tree production
  Bool_t m_undoExB;        // control flag for applying ExB hit corrections
  Bool_t m_undoDistort; // flag for other TPC distortion corrections
  Bool_t m_lasers;       // control flag so DOCA knows how many track sources.
  St_tpg_pad_plane      *m_tpg_pad_plane;  //! Constants that describe TPC
                                          //! pad plane geometry
  St_tcl_tpc_index_type *m_type;   	  //! Table of many-to-many index 
	                                  //! correlations for tpc evaluations
  St_tpt_pars           *m_tpt_pars;  	  //! Parameters for the track finding
  StMagUtilities            *m_mag;                 //!JT's ExB code
  int    date;
  int    time;

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
  virtual void  POCA(Float_t r0,Float_t phi0,Float_t z0,
                      Float_t psi, Float_t tanl, Float_t curvature, Int_t q,
                      Float_t *xl, Float_t *yl, Float_t *zl, Float_t *phi); 
  virtual void   SetRun(Int_t run) {m_runno = run;} 
  virtual void   SetDate(Int_t date) {m_date = date;} 
  virtual void   SetTime(Int_t time) {m_time = time;} 
  virtual void   SetRows(Int_t min, Int_t max) {m_rowmin=min; m_rowmax=max;}
  virtual void   Set_laser(Bool_t m=kTRUE){m_mklaser = m;}
  virtual void   Set_lasers(Bool_t m=kTRUE){m_lasers = m;}
  virtual void   Set_UndoExB(Bool_t m=kFALSE){m_undoExB = m;}
  virtual void   Set_UndoDistort(Bool_t m=kFALSE){m_undoDistort = m;}
  virtual void   UndoExB(Float_t *x,Float_t *y,Float_t *z);

  virtual void PrintInfo();
  virtual Int_t Finish();
  St_tpcDriftVelocity* driftTable();
  void WriteTableToFile();    //Write drift velocity table
  void WriteHistFile();       // Write out .root file with results
  void HistFileByDefault();   // Write out file on Finish

  double fzlAverageEastHigh();
  double fzlAverageEastLow();
  double fzlAverageWestHigh();
  double fzlAverageWestLow();
  double driftVelocityReco;

  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StLaserEventMaker.h,v 1.12 2001/12/23 20:08:04 pfachini Exp $ built "__DATE__" "__TIME__ ; return cvs;}

ClassDef(StLaserEventMaker, 1)   //StAF chain virtual base class for Makers
};

#endif
