// Source file / class definition
// Track merger
// Description:
//	The merger combines tracks which are close in (Pt, Psi, TanL) space and not overlaping.
//	It first builds a 2d-array of Psi TanL slices and then checks for each track in a slice
//	if there is a combinable partner-track in the same slice or in the 8 nearby slices.
//	if a combinable track is found, the resulting chi2 is checked. if the check
//	was positive, both tracks are combined.
// Author:
//	Dirk Schmischke, IKF
// History:
//	6-14-97:	implementation

// defines

// includes
#include "TMerger.hpp"
#include "types.h"
// 
// Class constructor
// receives input-tracklist, builds slices, merges tracks and outputs new tracklist via input
//
TMerger::TMerger(TTrackList* input, BOOL psiclosed)
{
//
// build dynamic array
//
   F2DArray = (TTrackPtrList**) 
               malloc(NumberOfPsiSlices * NumberOfTanLSlices * sizeof(TTrackPtrList*));
//
// reset 2d-array
//
   for(int y=0; y < NumberOfPsiSlices * NumberOfTanLSlices; y++) F2DArray[y] = NULL;
//
// build slices
//
   BuildSlices(input);
//
// merge tracks
//
   Merge(psiclosed);
//
// clean tracklist from deleted tracks
//
   Clean(input);
};
//
// build 3d-array of slices
//
void TMerger::BuildSlices(TTrackList* tracks)
{
	slist_item it;

	// scan all tracks
	it = tracks->first();
	while (it != NULL)
	{
		// reference to track
		TTrack& trackref = *((*tracks).inf(it));
		// check on length of track
		if (trackref.GetHits()->size() > 40)
		{
			it = tracks->succ(it);
			continue;
		}
		// check for primaries only
		if (trackref.GetSecondary())
		  {
		    it = tracks->succ(it);
		    continue;
		  }
		// calculate slice-coordinates
		int psislice, tanlslice;
		
		psislice = (int)((trackref.GetPsi() - MinSlicePsi) / 
			(MaxSlicePsi - MinSlicePsi) * NumberOfPsiSlices);
		if (psislice < 0)
			psislice = 0;
		if (psislice >= NumberOfPsiSlices)
			psislice = NumberOfPsiSlices-1;

		tanlslice = int((trackref.GetTanL() - MinSliceTanL) / 
			(MaxSliceTanL - MinSliceTanL) * NumberOfTanLSlices);
		if (tanlslice < 0)
			tanlslice = 0;
		if (tanlslice >= NumberOfTanLSlices)
			tanlslice = NumberOfTanLSlices-1;

		// do we already have a tracklist in this slice?
		if (F2DArray[psislice*NumberOfTanLSlices+tanlslice])
		{
			// yes add this track to the list
			F2DArray[psislice*NumberOfTanLSlices+tanlslice]->append(&trackref);
		}
		else
		{
			// no, make a new list
			F2DArray[psislice*NumberOfTanLSlices+tanlslice] = new TTrackPtrList;
			// and then add this track to the list
			F2DArray[psislice*NumberOfTanLSlices+tanlslice]->append(&trackref);
		}
		// next track
		it = tracks->succ(it);
	}
}

// merge tracks
void TMerger::Merge(BOOL psiclosed)
{
#define N_LOOP  9
   int loop_psi[N_LOOP] = { 0, 0, 0, 1, 1, 1,-1,-1,-1 };	// neighbors in psi and tanl
   int loop_tanl[N_LOOP] = { 0,-1, 1, 0,-1, 1, 0,-1, 1 };
   int psislice = 0;	// for closed psi spaces
   int tanlslice = 0;	// for closed psi spaces
   TTrack* trackptr = NULL;
   TTrack* temptrackptr = NULL;
   TTrack* foundtrackptr = NULL;
   double pt, eta, psi;
   double mindist = 1e6;
   double dist;
   int firstpadrow, lastpadrow;

// scan through slices
   for(int psicount=0; psicount < NumberOfPsiSlices; psicount++)
   {		
      for(int tanlcount=0; tanlcount < NumberOfTanLSlices; tanlcount++)
      {
// are there any tracks in this slice?
         if (F2DArray[psicount*NumberOfTanLSlices+tanlcount])
         {
// yes, scan this list
				forall(trackptr, *(F2DArray[psicount*NumberOfTanLSlices+tanlcount]))
				{
					// don't look at deleted tracks
					if (trackptr->todelete)
						continue;	
					mindist = 1e6;
					pt = trackptr->GetPt();
					eta = trackptr->GetEta();
					psi = trackptr->GetPsi();
					firstpadrow = trackptr->GetHits()->tail()->GetPadrow();
					lastpadrow = trackptr->GetHits()->head()->GetPadrow();
					// compare this entry with all entries in this slice...
					forall(temptrackptr, *(F2DArray[psicount*NumberOfTanLSlices+tanlcount]))
					{
						// don't compare with the same track
						if (temptrackptr == trackptr)
							continue;	// skip this
						// don't look at deleted tracks
						if (temptrackptr->todelete)
							continue;
						// don't look at different charges
						if (temptrackptr->GetCharge() != trackptr->GetCharge())
							continue;
						// dist is weighted 3 times for delta_pt, 1.5 times for delta_eta,
						// pt is fractional
						dist = 3.0 * fabs(temptrackptr->GetPt()-pt) / (max(pt, 0.01))
							+ fabs(temptrackptr->GetPsi()-psi) +
							1.5 * fabs(temptrackptr->GetEta()-eta);
						// smaller distance and possible candidate?
						if ((dist < mindist) && 
							((firstpadrow > temptrackptr->GetHits()->head()->GetPadrow()) ||
							(lastpadrow < temptrackptr->GetHits()->tail()->GetPadrow())))
						{
							mindist = dist;
							foundtrackptr = temptrackptr;
						}
					}						
					// ...and the neighboring slices
					for(int volumecount=0; volumecount < N_LOOP; volumecount++)
					{

						// get psi-slice
						psislice = psicount + loop_psi[volumecount];
						// wrap index if psi is closed
						if (psiclosed)
						{
							if (psislice < 0)
								psislice += NumberOfPsiSlices;
							else if (psislice >= NumberOfPsiSlices)
								psislice -= NumberOfPsiSlices;
						}
						else if ((psislice < 0) || (psislice >= NumberOfPsiSlices))
							continue;	// don't use this slice
						// get tanl-slice
						tanlslice = tanlcount + loop_tanl[volumecount];
						// check eta-bounds
						if ((tanlslice < 0) || (tanlslice >= NumberOfTanLSlices))
							continue;	// don't use this slice
						// are there any tracks in this slice?
						if (F2DArray[psislice*NumberOfTanLSlices+tanlslice])
						{
							forall(temptrackptr, *(F2DArray[psislice*NumberOfTanLSlices+tanlslice]))
							{
								// don't look at deleted tracks
								if (temptrackptr->todelete)
									continue;
								// don't look at different charges
								if (temptrackptr->GetCharge() != trackptr->GetCharge())
									continue;
								dist = 3.0 * fabs(temptrackptr->GetPt()-pt) / (max(pt, 0.01))
									+ fabs(temptrackptr->GetPsi()-psi) +
									1.5 * fabs(temptrackptr->GetEta()-eta);
								// smaller distance and possible candidate?
								if ((dist < mindist) && 
									((firstpadrow > temptrackptr->GetHits()->head()->GetPadrow()) ||
									(lastpadrow < temptrackptr->GetHits()->tail()->GetPadrow())))
								{
									mindist = dist;
									foundtrackptr = temptrackptr;
								}
							}
						}
					}
					// now check if the distance is small enough to merge
					if (mindist < FSMinDistMerge)
					{
						// distance is low enough; merge track
						trackptr->AddSegment(*foundtrackptr, FPrimaryVertexX, FPrimaryVertexY, FPrimaryVertexR, FPrimaryVertexPhi, FSFitSz);
						trackptr->FMerged = TRUE;
						// delete the merged-away track afterwards
						foundtrackptr->todelete = TRUE;
						foundtrackptr = NULL;
					}
				}
			}
		}
	}
}

// clean tracklist from deleted tracks
void TMerger::Clean(TTrackList* tracks)
{
	slink* el;

	// scan all tracks
	for(el = tracks->first(); el != 0; )
	{
		TTrack* track = tracks->inf(el);
		if (track->todelete)
		{
			// remove from list
			slink* next = el->succ;
			tracks->remove(el);
			el = next;
			if (el == 0)
				break;
		}
		else
			el = tracks->succ(el);
	}

}
