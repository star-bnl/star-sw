#ifndef _included_trackbase_
#define _included_trackbase_
// VTracker
//
//   This virtual base class supports an interface to all trackers.
//   Trackers derived from this class should implement all virtual
//   methods.
//   The virtual methods are all implemented but with little or no
//   functionality.
//
//   Functionality:
//      Init		    	    - Initialize structures
//      The following Create methods are implemented virtually to enable
//      inherited classes to define their own lists and structures without the
//      need to rewrite much code. However, the lists and structures must be
//      inherited from TObjArray, TBaseTrack and TPoint.
//      NOTE: The tracking class is NOT the owner of the various lists and
//            structures. They must be managed by the application.
//      CreatePointList         - Creates a new spacepointlist
//      CreatePoint             - Creates a new spacepoint    
//	CreateTrackList         - Creates a new tracklist
//      CreateTrack             - Creates a new track
//	BuildAllTracks          - Find all tracks and fill the tracklist
//      BuildOneTrack           - Find one track and fill a track
//      ReadIniFile             - Read the ini-file
//	    WriteIniFile        - Write the ini-file
//	    ShowParameterDialog	- Show the parameter dialog
//
//   Example:
//	    ...
//	    TObjArray  *TrackList;
//
//      TrackList = theTracker.CreateTrackList();
//      theTracker.Init();
//	    ...
//      theTracker.ReadIniFile("mytracking.ini");
//	    ...
//      theTracker.BuildAllTracks(FreeList, TrackList);

// some defines

// some includes
#include "THit.hpp"				// include THit
#include "TTrack.hpp"			// include virtual Track-interface
#include "Common.h"
#include "types.h"

// class declaration

class VTracker
{
public:
// constructor
   VTracker();
// Destructor must be virtual in virtual base classes 
   virtual ~VTracker(); 
// Initialize the tracker
   virtual void Init(void);
// Do the tracking once. This function needs the free-point-list and
// an empty track as Input.
// It removes used points from this list, if fRemovedUsedPoints is true.
// A returned value of FALSE means "no track found", TRUE means "track found".
   virtual BOOL  BuildOneTrack(THitList* freepoints, TTrack *track);
// Do the tracking repeatedly until no more tracks are found. This function
// needs the free-point-list and a track-list as input. It creates
// new tracks and new point-lists if needed and puts them into the
// tracklist.
   virtual void BuildAllTracks( THitList* freepoints, TTrackList* tracklist);
// request currently built track
   virtual TTrack* GetCurrentTrack();
// Create a new instance of a point-list.
   virtual THitList* CreatePointList(void);
// Creates a new instance of a point.
   virtual THit* CreatePoint(void);
// Creates a new instance of a track-list.
   virtual TTrackList* CreateTrackList(void);
// Creates a new instance of a track.
   virtual TTrack* CreateTrack(void);
// Display and handle a tracking-parameter dialog, which allows to
// load tracking parameters from disk, change them and save them
// on disk.
   virtual void ShowParameterDialog(void) ;

private:
	// pointer to currently generated track
   TTrack* fCurrentTrack;
  
};

// inline functions

// Request currently built track
inline TTrack * VTracker::GetCurrentTrack()
{
	return fCurrentTrack;
}

// Create a new instance of a point-list.
inline THitList* VTracker::CreatePointList()
{
	THitList* temp;

	temp = new THitList();
	return temp;
}

// Creates a new instance of a hit.
inline THit* VTracker::CreatePoint(void)
{
	return new THit();
}

// Creates a new instance of a track-list.
inline TTrackList* VTracker::CreateTrackList()
{
	TTrackList* temp;

	temp = new TTrackList();
	return temp;
}

// Creates a new instance of a track.
inline TTrack* VTracker::CreateTrack(void)
{
	return new TTrack();
}



#endif
