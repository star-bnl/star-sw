#ifndef TMERGER_INC
#define TMERGER_INC
// Include file / class declaration
// Track merger
// Description:
//	The merger combines tracks which are close in (Pt, Phi, TanL) space and not overlaping.
//	It first builds a 2d-array of Psi and TanL slices and then checks for each track in a slice
//	if there is a combinable partner-track in the same slice or in the 8 nearby slices.
//	if a combinable track is found, the resulting chi2 is checked. if the check
//	was positive, both tracks are combined.
// Author:
//	Dirk Schmischke, IKF
// History:
//	6-14-97:	implementation

// defines

// includes
#include "types.h"
#include "TTrack.hpp"
// types
// singly linked list of pointers to tracks
typedef slist<TTrack*> TTrackPtrList;

// class declarations

class TMerger
{
private:
   TTrackPtrList** F2DArray;	// psi, tanl
//
// build 3d-array of slices
   void BuildSlices(TTrackList* tracks);
// merge tracks
   void Merge(BOOL psiclosed);
// clean tracklist from deleted tracks
   void Clean(TTrackList* tracks);
public:
   static int NumberOfPsiSlices;
   static int NumberOfTanLSlices;

   static float MinSlicePsi;
   static float MaxSlicePsi;
   static float MinSliceTanL;
   static float MaxSliceTanL;
   static float FSMinDistMerge;

   static float FPrimaryVertexX;
   static float FPrimaryVertexY;
   static float FPrimaryVertexR;
   static float FPrimaryVertexPhi;
   static BOOL FSFitSz;
// constructor
// receives input-tracklist, builds slices, merges tracks and outputs new tracklist via input
   TMerger(TTrackList* input, BOOL psiclosed);
//
// destructor
// deletes internal lists
   ~TMerger()
   {
// delete lists in 2d-array
      for(int y=0; y < NumberOfPsiSlices * NumberOfTanLSlices; y++)
         if (F2DArray[y] != NULL) delete F2DArray[y];
// delete 2d-array
      free(F2DArray);
   };
};



#endif
