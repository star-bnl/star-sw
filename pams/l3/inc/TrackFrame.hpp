//
// TTrackFrame
//
//   This class supports a general tracker development environment.
//   The purpose of this class is to free the tracking developer from
//   doing all this nasty root stuff. He only concentrates on writing
//   the trackers, the rest is common to all trackers.
//
//   Functionality:
//	Init()	   		- initialize the tracking environment
//	SetTracker(VTracker&)	- set the tracking algorithm
//	SetEvent(int)		- position on event (automatic is first event)
//	NextEvent()	        - get event and set to next event
//	Interactive()		- switch to interactive mode
//	Done()			- free memory, etc...		
//
//   Example:
//	...
//	TTrackFrame 	theFrame;
//	TMyTracker	theTracker("setup_1.ini");
//
//	theFrame.Init();
//	theFrame.SetTracker(theTracker);
//	theFrame.OpenAscii("sect13out.tex");
//      theFrame.SetEvent();
//	do
//	{
//        theFrame.ProcessEvent();
//	  theFrame.Interactive();
//	} while (theFrame.NextEvent);
//	theFrame.Done();

#ifndef _included_trackframe_
#define _included_trackframe_

// some includes
// tracklab classes
#include "TTrack.hpp"
#include "TrackBase.hpp"
// #include <fstream.h>
#include "Common.h"				// include common definitions
#include "types.h"				// include common types
#include "PAM.h"
#include "l3t_tpc_para.h"
#include "tcl_tphit.h"
#include "tpt_track.h"

class TTrackFrame 
{
public:
// stupid constructor
  TTrackFrame();
// stupid destructor
  ~TTrackFrame();
// Initialize the frame
  void Init();
// initalize from struct
  void Initialize(L3T_TPC_PARA_ST* para);
// Set the tracking algorithm
  void SetTracker(VTracker* theTracker);
// (re)process current event
  void ProcessEvent(void);
// free memory and clean up
  void Done(void);
// fill event
  BOOL FillEvent();
  BOOL TTrackFrame::FillEvent(  
			      TABLE_HEAD_ST     *tphit_h,      
			      TCL_TPHIT_ST      *table ,
			      int FirstSector,
			      int NumberOfSectors);
//
  int FillResultTable( TABLE_HEAD_ST *hit_h,   TCL_TPHIT_ST *tphit, 
                       TABLE_HEAD_ST *track_h, TPT_TRACK_ST *tracks);
//
// Request current track.
// This method passes the last built track from the tracker
  VTrack *GetCurrentTrack();
//
// enumeration for input file methods
//
  enum {imNone, imDSPACK, imStar, imStarXDF};
// List of unused (free) points
  THitList* fFreePoints;
// List of produced tracks
  TTrackList *fTracks;
// lists of unused points (if you want to divide the calculation into sectors)
  THitList* fFreePointsSector[24];
// same for tracks
  TTrackList* fTracksSector[24];
// number of sectors to track
  int FFirstSector;
  int FLastSector;
protected:
  // protected members; for extending the functionality
  // the tracker
  VTracker *fTracker;
};

// inline functions

// Request current track.
// This method passes the last built track from the tracker
inline VTrack *TTrackFrame::GetCurrentTrack()
{
  return fTracker->GetCurrentTrack();
}

#endif
