#ifndef TTRACK_INC
#define TTRACK_INC
// TTrack-includefile
//
//	This class contains additional methods to calculate parameters and add hits.
//	It is based on VTrack and uses its common interface and members.
//
//	New members:
//		Fit		- Fit parameters (static, common to all tracks)
//		FSBField	- magnetic field (static, common to all tracks)
//
//	New Methods:
//		AddHit             	- add new hit to track (only pointer to hit), 
//				          update fit and mark hit as used.
//		AddSegment		- add track-segment to track, update fit, etc.
//		FinishPrimaryTrack	- finish primary track; calculates track-parameters 
//                                        from intermediate parameters.
//		FinishSecondaryTrack	- finish secondary track; calculates track-parameters 
//                                        from intermediate parameters.
//		ResetFitParameters	- resets the fit-parameters


// some includes
#include "TTrackFit.hpp"		// include TTrackFit
#include "VTrack.hpp"			// include virtual track-interface
#include "Common.h"			// include common definitions
#include "types.h"			// include common types
#ifdef LEDA
#include "_memory.hpp"	// include the LEDA memory manager (that's the most efficient mm i know)
#endif
//
// class declaration
//
class TTrack : public VTrack
{
private:
// static private members
// static members are common to all tracks
// fit parameters in each track are needed for merging!
//
   TTrackFit Fit;			// fit-parameters are needed only once for all tracks and are 
					// recycled for each track. saves memory; no!
   static double FSBField;		// B-field should be fixed for one event (default: 0.5 (ref: pablo))
public:
   void CalculateDeDx();
//
// helpers for merging
//
   BOOL todelete;	// was merged away, should be deleted from list
   BOOL FMerged;	// is a merged track
//
// public members
//
   float FSTrack;	// tracklength
//
// constructors
// general constructor
//
   TTrack() {FSTrack = 0.0; FMerged = FALSE; todelete = FALSE;};
// special constructors
// destructor
// virtual ~TTrack() {	// does nothing };
// static methods
// get fit-parameters
   TTrackFit* GetFit() {return &Fit;};
// set magnetic field (common to all tracks)
   static void SetBField(double NewValue) {FSBField = NewValue;};
//
// other public methods
// Get tracklength
   double GetS() {return FSTrack;};
// Set tracklength
   void SetS(float NewValue) { FSTrack = NewValue; };
// add new hit to track (only pointer to hit), update fit and mark hit as used
// Parameters:
//		NewValue			- hit to add
//		requiredhitsforfit	- number of hits on track needed to make a fit
//		dofitsz				- fit in sz-plane
   void AddHit(THit* NewValue, int requiredhitsforfit, BOOL dofitsz);
// add track-segment to track
// Parameters:
//	NewValue	- Track(segment) to add
//	vertexx		- x-coordinate of (primary) vertex
//	vertexy		- y-coordinate of (primary) vertex
//	vertexr		- cylinder-coordinate r of (primary) vertex
//	vertexphi	- cylinder-coordinate phi of (primary) vertex
//	dofitsz		- TRUE: make special fit
   void AddSegment(TTrack& NewValue, float vertexx, float vertexy, float vertexr, 
                   float vertexphi, BOOL dofitsz);
// finish primary track; calculates track-parameters from intermediate parameters (was fill_primary)
// Parameters:
//	vertexx		- x-coordinate of (primary) vertex
//	vertexy		- y-coordinate of (primary) vertex
//	vertexr		- cylinder-coordinate r of (primary) vertex
//	vertexphi	- cylinder-coordinate phi of (primary) vertex
//	dofitsz		- TRUE: make special fit
void FinishPrimaryTrack(float vertexx, float vertexy, float vertexr, 
    float vertexphi, BOOL dofitsz);
// finish secondary track; calculates track-parameters from intermediate parameters (was fill_secondary)
// Parameters:
//	DoFitSz		- TRUE: make special fit
   void FinishSecondaryTrack(BOOL DoFitSz);
// reset fit parameters in Fit member
   void ResetFitParameters()
   {
      Fit.Reset();
   };
//
// get hit-list
//
   THitList* GetHits() {return &FHits; };
//
#ifdef LEDA
//
// needed for LEDA
//	friend istream& operator>>(istream& I, TTrack& s) {return I;};
//	friend ostream& operator<<(ostream& O, const TTrack& s) {return O;};
   LEDA_MEMORY(TTrack);
#endif
};


#endif
