#ifndef TTRACKER_INC
#define TTRACKER_INC
// TTracker
//
//	This class is inherited from the VTracker base-class.
//	It implements pablo's fast tracker FFT
//	The virtual methods from VTracker are fully implemented.
//
//	Functionality:
//	Virtual interface:
//		Init		    		- Initialize structures
//		The following Create methods are implemented virtually to enable
//		inherited classes to define their own lists and structures without the
//		need to rewrite much code. However, the lists and structures must be
//		inherited from TObjArray, TBaseTrack and TPoint.
//		NOTE: The tracking class is NOT the owner of the various lists and
//				structures. They must be managed by the application.
//		CreatePointList			- Creates a new spacepointlist
//		CreatePoint				- Creates a new spacepoint    
//		CreateTrackList			- Creates a new tracklist
//		CreateTrack				- Creates a new track
//		BuildAllTracks			- Find all tracks and fill the tracklist
//		BuildOneTrack			- Find one track and fill a track
//		ReadIniFile				- Read the ini-file
//		WriteIniFile			- Write the ini-file
//		ShowParameterDialog		- Show the parameter dialog
//	Other methods:
//		(adopted from pablo's tracker)
//		SetDefaults				- Sets tracking defaults
//		SetPointers				- Fill lists (volumes, hits)
//		Reset					- Initial reset (allocates memory?...)
//		EventReset				- Reset for every event
//		SetConformalCoordinate	- Calculates conformal coordinates (for primary vertex)
//		GetPrimaryTracks		- Builds all primary tracks
//		GetSecondaryTracks		- Builds all secondary tracks
//		Segment					- Searches a start-tracksegment
//		Follow					- Expand tracksegment to full length (one hit each call)

//
//	Example:
//		...
//		TObjArray  *TrackList;
//
//		TrackList = theTracker.CreateTrackList();
//		theTracker.Init();
//		...
//		theTracker.ReadIniFile("mytracking.ini");
//		...
//		theTracker.BuildAllTracks(FreeList, TrackList);



// some defines
inline double sqr(double val) {return(val*val);};
// some includes

#include "TTrack.hpp"			// include TTrack
#include "TrackBase.hpp"		// include VTracker
#include "TVolume.hpp"			// include TVolume
#include "THit.hpp"			// include THit
#include "Common.h"			// include common definitions
#include "TMerger.hpp"          	// include TMerger
// class declaration

class TTrackerFTF : public VTracker
{
private:
// private data members
// volume - collects hits in discrete (padrow, eta, phi) slices
   TVolume* FVolume;
   float FPrimaryVertexX;		// position of primary vertex in x
   float FPrimaryVertexY;		// position of primary vertex in y
   float FPrimaryVertexR;		// position of primary vertex in r
   float FPrimaryVertexPhi;	// position of primary vertex in phi
   static BOOL FMergePrimaries;		// merge primaries?
// statics
   static BOOL FSFitSz;		// fit in sz-plane?
   static BOOL FSPhiClosed;		// closed phi-space?
   static double FSDPhiLimit;	// limit of difference in phi-space
   static double FSDEtaLimit;	// limit of difference in eta-space
   static double FSChi2Cut;	// cut in chi-square for new hit on track
   static double FSGoodChi2;	// limit for a good chi2 for new hits on track
   static double FSGoodDistance;// acceptable distance hit/track
   static double FSChi2TrackCut;	// chi-square cut for new track
   static int FSMinimumHitsPerSegment; // minimum number of hits for a track-starting segment
   static int FSMinimumHitsPerTrack;	// minimum number of hits for a valid track
   static int FSMaxSearchPadrowsTrack;	// maximum number of missings hits on track
   static int FSMaxSearchPadrowsSegment;	// maximum number of missings hits on track in starting segment
// number of eta- and phi-slices
   static WORD FSNumberOfEtaSlices;
   static WORD FSNumberOfPhiSlices;
// minimum and maximum of phi- and eta-slices
   static double FSPhiMin, FSPhiMax;
   static double FSEtaMin, FSEtaMax;
   static int    FNPrimaryLoops ;
   static int    FNSecondaryLoops;
   static int    FInnerMostRow;
   static int    FOuterMostRow;
public:
// public types
   enum TTrackType {ttPrimaries, ttSecondaries, ttBooster}; 
// public methods
// constructors
// stupid constructor
   TTrackerFTF() : VTracker() { FVolume = 0;};
// destructor
   virtual ~TTrackerFTF() { if (FVolume != 0) delete FVolume; };
// public methods
// set tracking defaults
   void SetDefaults();
// initialize once (pablo's reset)
   void Reset();
// initialize before each event
   void ResetEvent();
//
// pablo's setpointers is substituted by AddHitToVolume.
// conformal-coordinates and cylinder coordinates are calculated in TTrackFrame,
// filling the volume with hits is done at this point.
//   
   inline void AddHitToVolume(THit* NewValue) { FVolume->AddHit(NewValue); };

// get all tracks (primaries or secondaries)
// Parameters:
//	tracklist       - list of tracks (filled by this method)
//	tracktype       - determines, which kind of track are to be found (primaries, secondaries,  ?)
//
   void GetTracks( TTrackList* tracklist, TTrackType tracktype);
//
// calculates de/dx information for all tracks
// Parameters:
//	tracklist		- list of tracks (for which de/dx is to be calculated)
//
  void dEdX(TTrackList* tracklist);
// merge close tracks
// Parameters:
//	tracklist		- list of tracks
//	NewValue		- track that should be merged to another track
// Return
//	TRUE			- track was merged and isn't valid any longer
//	FALSE			- track couldn't be merged, you should add it to the tracklist
	BOOL CombineTracks(TTrackList* tracklist, TTrack* NewValue);
// segmenter; builds a starting track-segment for the track-follower
// Paramters:
//	currenttrack		- track under construction. to this track, segmenter-hits should be added
//	maxsearchpadrow		- maximum number of padrows that will be searched one hit
//	secondaries		- find secondaries
	THit* Segment(TTrack* currenttrack, int maxsearchpadrows, BOOL secondaries);
// track-follower; extends a track. the track was started using Segment...
// Parameters:
//	maxsearchpadrow		- maximum number of padrows that will be searched one hit
//	secondaries		- find secondaries
	THit* Follow(TTrack* currenttrack, int maxsearchpadrows, BOOL secondaries);
// merge primaries?
	static void SetMergePrimaries(BOOL NewValue) {FMergePrimaries = NewValue; };

// setconformalcoordinates, resetfit, fillprimary, fillsecondary are methods of THit
// deletecandidate(-track) is a method of VTrack
//
// Inherited virtual methods (from VTracker)
//
// Initialize the tracker
   virtual void Init(void)
   {
      FVolume = 0;
      FPrimaryVertexX = 0.0;
      FPrimaryVertexY = 0.0;
      FPrimaryVertexR = 0.0;
      FPrimaryVertexPhi = 0.0;
   };
// Do the tracking once. This function needs the free-point-list and
// an empty track as Input.
// It removes used points from this list, if fRemovedUsedPoints is true.
// A returned value of FALSE means "no track found", TRUE means "track found".
// virtual BOOL  BuildOneTrack(THitList* freepoints, TTrack *track);
// Do the tracking repeatedly until no more tracks are found. This function
// needs the free-point-list and a track-list as input. It creates
// new tracks and new point-lists if needed and puts them into the
// tracklist.
   virtual void BuildAllTracks( THitList* freepoints, TTrackList* tracklist);
// create an instance of TVolume and assign it to FVolume
   void CreateVolume() 
   { 
      FVolume = new TVolume(); 
// set some statics
      FVolume->SetStatics(FSNumberOfEtaSlices, FSNumberOfPhiSlices, 
                          FSPhiMin, FSPhiMax, FSEtaMin, FSEtaMax);
   };
// free an instance of TVolume in FVolume
   void DestroyVolume() { delete FVolume; };
// request currently built track
// virtual TTrack* GetCurrentTrack();
// Create a new instance of a point-list.
// virtual THitList* CreatePointList(void);
// Creates a new instance of a point.
// virtual THit* CreatePoint(void);
// Creates a new instance of a track-list.
// virtual TTrackList* CreateTrackList(void);
// Creates a new instance of a track.
// virtual TTrack* CreateTrack(void);
// Read an ini-file containing all tracking parameters from disk

// static setters and getters
   static BOOL GetSFitSz() { return FSFitSz; };								// fit in sz-plane?
   static BOOL GetSPhiClosed() { return FSPhiClosed; };	// closed phi-space?
   static double GetSDPhiLimit() { return FSDPhiLimit; };	// limit of difference in phi-space
   static double GetSDEtaLimit() { return FSDEtaLimit; };	// limit of difference in eta-space
   static double GetSChi2Cut() { return FSChi2Cut; };	// cut in chi-square for new hit on track
   static double GetSGoodChi2() { return FSGoodChi2; };	// limit for a good chi2 for new hits on track
   static double GetSGoodDistance() { return FSGoodDistance; };	// acceptable distance hit/track
   static double GetSChi2TrackCut() { return FSChi2TrackCut; }; // chi-square cut for new track
// minimum number of hits for a track-starting segment
   static int GetSMinimumHitsPerSegment() { return FSMinimumHitsPerSegment; }; 
// min. # hits for a valid track
   static int GetSMinimumHitsPerTrack() { return FSMinimumHitsPerTrack; };
// maximum number of missings hits on track
   static int GetSMaxSearchPadrowsTrack() { return FSMaxSearchPadrowsTrack; };	
// maximum number of missings hits on track in starting segment
   static int GetSMaxSearchPadrowsSegment() { return FSMaxSearchPadrowsSegment; };

   static void SetSFitSz(BOOL NewValue) { FSFitSz = NewValue; };	// fit in sz-plane?
   static void SetSPhiClosed(BOOL NewValue) { FSPhiClosed = NewValue; };// closed phi-space?
   static void SetSDPhiLimit(double NewValue) 
      { FSDPhiLimit = NewValue; };// limit of difference in phi-space
   static void SetSDEtaLimit(double NewValue) 
      { FSDEtaLimit = NewValue; };// limit of difference in eta-space
   static void SetSChi2Cut(double NewValue) 
      { FSChi2Cut = NewValue; };	// cut in chi-square for new hit on track
   static void SetSGoodChi2(double NewValue) 
      { FSGoodChi2 = NewValue; };    // limit for a good chi2 for new hits on track
   static void SetSGoodDistance(double NewValue) 
      { FSGoodDistance = NewValue; };// acceptable distance hit/track
   static void SetSChi2TrackCut(double NewValue) 
      { FSChi2TrackCut = NewValue; };// chi-square cut for new track
   static void SetSMinimumHitsPerSegment(int NewValue) 
      { FSMinimumHitsPerSegment = NewValue; };	// minimum number of hits for a track-starting segment
   static void SetSMinimumHitsPerTrack(int NewValue) 
      { FSMinimumHitsPerTrack = NewValue; };	// minimum number of hits for a valid track
   static void SetSMaxSearchPadrowsTrack(int NewValue) 
      { FSMaxSearchPadrowsTrack = NewValue; };// maximum number of missings hits on track
   static void SetSMaxSearchPadrowsSegment(int NewValue) 
      { FSMaxSearchPadrowsSegment = NewValue; };// maximum number of missings hits on track
//
// set statics
//
   static void SetStatics(WORD NumberOfEtaSlices, WORD NumberOfPhiSlices, 
                          double phimin, double phimax, double etamin, double etamax)
   {
// eta-slices
      FSNumberOfEtaSlices = NumberOfEtaSlices;
// phi-slices
      FSNumberOfPhiSlices = NumberOfPhiSlices;
// mins and maxs
      FSPhiMin = phimin;
      FSPhiMax = phimax;
      FSEtaMin = etamin;
      FSEtaMax = etamax;
   };
};
//
// merge close tracks
// Parameters:
//	tracklist	- list of tracks
//	NewValue	- track that should be merged to another track
// Return
//	TRUE		- track was merged and isn't valid any longer
//	FALSE		- track couldn't be merged, you should add it to the tracklist

inline BOOL TTrackerFTF::CombineTracks(TTrackList* tracklist, TTrack* NewValue)
{
/*
	// scan all tracks and calculate minimum distance in (pt phi tanl)-space
   double pt, tanl, psi;
   double mindist = 1e6;
   double dist;
   int firstpadrow, lastpadrow;
   slist_item it;
   slist_item itmin;

	pt = NewValue->GetPt();
	tanl = NewValue->GetTanL();
	psi = NewValue->GetPsi();
	firstpadrow = NewValue->GetHits()->tail()->GetPadrow();
	lastpadrow = NewValue->GetHits()->head()->GetPadrow();

	// scan all tracks
	it = tracklist->first();
	while (it != NULL)
	{
		dist = abs((*tracklist)[it].GetPt()-pt) + abs((*tracklist)[it].GetPsi()-psi) +
			abs((*tracklist)[it].GetTanL()-tanl);
		if ((dist < mindist) && 
			((firstpadrow > (*tracklist)[it].GetHits()->head()->GetPadrow()) ||
			(lastpadrow < (*tracklist)[it].GetHits()->tail()->GetPadrow())))
		{
			mindist = dist;
			itmin = it;
		}
		it = tracklist->succ(it);
	}
	if (mindist < FSMinDistMerge)
	{
		// distance is low enough; merge track
		(*tracklist)[itmin].AddSegment(*NewValue, FPrimaryVertexX, FPrimaryVertexY, FPrimaryVertexR, FPrimaryVertexPhi, FSFitSz);
		(*tracklist)[itmin].FMerged = TRUE;
		return TRUE;
	}
	else
		return FALSE;
*/
	return FALSE;
}
#endif
