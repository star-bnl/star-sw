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
//	BuildAllTracks		- Find all tracks and fill the tracklist
//      BuildOneTrack           - Find one track and fill a track
//      ReadIniFile	        - Read the ini-file
//	WriteIniFile            - Write the ini-file
//	ShowParameterDialog     - Show the parameter dialog
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

// some includes
#include "TrackBase.hpp"

// constructor
VTracker::VTracker() 
{

}

// Destructor must be virtual in virtual base classes 
VTracker::~VTracker()
{

}

// Initialize the tracker
void VTracker::Init()
{

}

// Do the tracking once. This function needs the free-point-list and
// an empty track as Input.
// It removes used points from this list, if fRemovedUsedPoints is true.
// A returned value of kFALSE means "no track found", kTRUE means "track found".
BOOL  VTracker::BuildOneTrack(THitList* freepoints, TTrack *track)
{
  // This virtual method is intended to be user defined
//  Error << ">> BuildOneTrack: This virtual method is undefined" << endl;
  // no track was built
  fCurrentTrack = 0;
  // return "no track found"
  return FALSE;
}

// Do the tracking repeatedly until no more tracks are found. This function
// needs the free-point-list and a track-list as input. It creates
// new tracks and new point-lists if needed and puts them into the
// tracklist.
void VTracker::BuildAllTracks( THitList* freepoints, TTrackList* tracklist)
{
// create pointer to temporary track object
  TTrack *temptrack;

// create initial temporary track object
  temptrack = CreateTrack();
// track generation loop
  while (BuildOneTrack(freepoints, temptrack))
  {
// add track object to track-list
    tracklist->append(temptrack);
// create new temporary track object
    temptrack = CreateTrack();
  }
  // the last temporary track object is empty, delete it
  delete temptrack;
}
//
// Display and handle a tracking-parameter dialog, which allows to
// load tracking parameters from disk, change them and save them
// on disk.
//
void VTracker::ShowParameterDialog()
{
  // this method is intended to be user defined
//  Error << ">> ShowParameterDialog: This virtual method is undefined" << endl; 
}
