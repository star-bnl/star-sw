// TTracker
//
//	This class is inherited from the VTracker base-class.
//	It implements pablo's fast tracker FTF
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
//		CreatePoint			- Creates a new spacepoint    
//		CreateTrackList			- Creates a new tracklist
//		CreateTrack			- Creates a new track
//		BuildAllTracks			- Find all tracks and fill the tracklist
//		BuildOneTrack			- Find one track and fill a track
//		ReadIniFile			- Read the ini-file
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

// some includes

#include "TTracker.hpp"
#include "THit.hpp"
#include "TTrack.hpp"
#include "Common.h"
#include "types.h"
#include <stdio.h>

#undef max
#define min(a,b)        ( ( (a) < (b) ) ? (a) : (b) )
#define max(a,b)        ( ( (a) > (b) ) ? (a) : (b) )

#define N_LOOP 9 

extern "C" float rftTimer ( ) ;

//********************************************************************************
// Do the tracking once. This function needs the free-point-list and
// an empty track as Input.
// It removes used points from this list, if fRemovedUsedPoints is true.
// A returned value of FALSE means "no track found", TRUE means "track found".
// virtual BOOL  BuildOneTrack(THitList* freepoints, TTrack *track);
// Do the tracking repeatedly until no more tracks are found. This function
// needs the free-point-list and a track-list as input. It creates
// new tracks and new point-lists if needed and puts them into the
// tracklist.
//********************************************************************************
void TTrackerFTF::BuildAllTracks( THitList* freepoints, TTrackList* tracklist) {
      THit* temp;
//
// create volume
//
      CreateVolume();
//
// build volume from hitlist
//
      float timeBefore = rftTimer ( ) ;
      forall(temp, *freepoints) AddHitToVolume(temp);
      float timeAdding = rftTimer ( ) ;
//
// get primaries
//
      for ( int i = 0 ; i < FNPrimaryLoops ; i++ )
         GetTracks( tracklist, ttPrimaries);
      float timePrimaries = rftTimer ( ) ;
//
//    Merge primaries if requested
//
      if (FMergePrimaries){
         TMerger merge(tracklist, FSPhiClosed);
      }
      float timeMerging = rftTimer ( ) ;
//
// get secondaries
//
      for ( int j = 0 ; j < FNSecondaryLoops ; j++ )
         GetTracks ( tracklist, ttSecondaries);

      float timeSecondaries = rftTimer ( ) ;

      float timeTotal = timeAdding + timePrimaries + timeMerging + timeSecondaries ;
      printf ( " \n Times: adding %5.2f, Primaries %5.2f, Merging %5.2f, Secondaries %5.2f ",
                 timeAdding, timePrimaries, timeMerging, timeSecondaries ) ;
      printf ( " \n Total time = %5.2f ", timeTotal ) ;
//
// destroy volume
//
      DestroyVolume();
      FVolume = 0;
   };
//****************************************************************************************
// segmenter; builds a starting track-segment for the track-follower
// Paramters:
//	currenttrack			- track under construction. to this track, segmenter-hits should be added
//	maxsearchpadrow			- maximum number of padrows that will be searched one hit
//	secondaries				- find secondaries
//****************************************************************************************
THit* TTrackerFTF::Segment(TTrack* currenttrack, int maxsearchpadrows, BOOL secondaries)
{
   int loop_eta[N_LOOP] = { 0, 0, 0, 1, 1, 1,-1,-1,-1 }; // neighbors in eta and phi
   int loop_phi[N_LOOP] = { 0,-1, 1, 0,-1, 1, 0,-1, 1 };
   int padrowindex, etaindex, phiindex;			// loop indeices
   int PadrowStart, PadrowEnd;				// limits in padrow-direction
   double sLocal ;
   double dx, dy, d3 , dr;
   double dphi, deta;					// distance in eta and phi
   double angle, d_angle ;
   double distance = 100.0F;

   static double segment_xy_angle = 0.0;		// keep angle

   THit* lasthit;					// last hit on track
   THit* selectedhit = NULL;				// new hit on track
   THitList* volumehits;				// list of hits in current volume

#ifdef TRKDEB
   if ( theTracker->track_debug && theTracker->para->debug_level >= 2 )
   printf ( " \n **** Trying to extend segment **** " );
#endif
// Calculate limits on the volume loop
   lasthit = currenttrack->GetHits()->tail();
// calculate low- and high-limit of search-padrow
   PadrowStart = max((1), (lasthit->GetPadrow() - 1));
   PadrowEnd = max(1, PadrowStart - maxsearchpadrows);
// loop over volume neighborhood
   for(int volumeindex = 0; volumeindex <= N_LOOP; volumeindex++)
   {
// get phi-index
      phiindex = lasthit->GetPhiIndex() + loop_phi[volumeindex];
// wrap index if phi is closed
      if (FSPhiClosed)
      {
         if (phiindex < 1)
            phiindex += THit::GetNumberOfPhiSlices();
         else if (phiindex > THit::GetNumberOfPhiSlices())
            phiindex -= THit::GetNumberOfPhiSlices();
      }
      else if ((phiindex < 1) || (phiindex > THit::GetNumberOfPhiSlices()))
                   continue;	// don't use this hit
// get eta-index
         etaindex = lasthit->GetEtaIndex() + loop_eta[volumeindex];
// check eta-bounds
         if ((etaindex < 1) || (etaindex > THit::GetNumberOfEtaSlices())) continue;
//
// loop over padrows in search-range
//
         for (padrowindex = PadrowStart; padrowindex >= PadrowEnd; padrowindex--)
         {
#ifdef TRKDEB
            last_hit->nvol_tried++ ;
#endif
// get list of hits in current volume
            volumehits = (*FVolume)(padrowindex, etaindex, phiindex);
// any hits?
            if (volumehits == NULL) continue;	// no, next volume
// loop over hits in this volume (first hit will always be present)
            THit* candidate = NULL;
            forall(candidate, *volumehits) {
#ifdef TRKDEB
            debug_in_volume ( candidate_hit ) ;
#endif
// check wether hit was used before
            if ( candidate->GetTrack() == 0 ) {
// no!
// get distance between last hit on track and this candidate in phi
               dphi  = (double)fabs(lasthit->GetPhi() - candidate->GetPhi()) ; 
// check phi against bounds that make sense
               if ((dphi > FSDPhiLimit) && (dphi < 2*Pi - FSDPhiLimit)) 
                   continue ;// out of bounds, next hit
//
// Make sure we want to look at the difference in eta
//
               if ((lasthit->GetDz() < 1000.0) && (candidate->GetDz() < 1000.0)) {
                  deta  = (double)fabs(lasthit->GetEta() - candidate->GetEta()); 
// check eta against bound
                  if (deta > FSDEtaLimit) continue;	// out of bounds, next hit
               }
               else 
                  deta = 0.0F ;
// get distance in padrow-direction
               dr = (double)(lasthit->GetPadrow() - padrowindex);
// get 3-d distance
               d3 = (double)(To_deg * dr * (dphi + deta));

#ifdef TRKDEB
               if ( theTracker->track_debug && para->debug_level >= 2 ) {
                  printf ( " \n dr,dphi,deta  %7.2f %7.2f %7.2f ",dr,dphi,deta ) ;
                  printf ( " \n 3d  distance  %7.2f ", d3 );
                  printf ( " \n Min distance  %7.2f ", distance );
                  if ( d3 < distance )
                     printf ( " \n Best point, keep it !!! " );  
                  else
                  {
                     printf ( "\n Worse than previous, reject !! " );
                     candidate_hit->Plot ( theTracker->color_back );
                  }
                  theTracker->debug_ask() ;
               }
#endif
// Check whether this is the closest hit
               if ( d3 < distance )
               {
// Check segment angles
                  dx = candidate->GetX() - lasthit->GetX();
                  dy = candidate->GetY() - lasthit->GetY();
                  angle = (double)atan2(dy, dx);
                  if (angle < 0) angle += 2*Pi;
// is it the second hit on this track?
                  if (currenttrack->GetHits()->size() <= 1)
                  {
// yes, take the closest hit
                     distance = d3 ;
                     selectedhit = candidate;
                     if (d3 < FSGoodDistance) goto found;// we found our hit; shortcircuit the loops
                  }
                  else
                  {
// no
// Make sure xy angle between two last segments makes sense
// for second hit onwards
                     d_angle = (double)fabs(angle - segment_xy_angle) ;
                     if (d_angle > Pi) d_angle = 2*Pi - d_angle;
                     if (d_angle < 5.0 * FSDPhiLimit) 
                     {
// angles make sense
                        distance = d3;
                        selectedhit = candidate;
                        if (d3 < FSGoodDistance) goto found ;
                     }
                  }
                }
// End if checking whether this hit had already been used
              }
// End hit loop  
           }
// End row loop      
         }
// End volume loop inside cone      
      }
#ifdef TRKDEB
   if ( theTracker->track_debug && para->debug_level >= 2 ) {
      if ( selected_hit != 0 )
         printf ( " \n FTF_segment: Search succesful, hit %d selected ",selected_hit->id );
      else
         printf ( " \n FTF_segment: Search unsuccesful " );
      theTracker->debug_ask () ;
	}
#endif
   found: ;		// second end of shortcircuit

   if (selectedhit != NULL)
   {
//
// If sz fit update s
// Calculate track length if sz plane considered
//
      segment_xy_angle = angle;
      if (FSFitSz)
      {
         dx = selectedhit->GetX() - lasthit->GetX();
         dy = selectedhit->GetY() - lasthit->GetY();
// update tracklength
         sLocal = lasthit->GetS() + (double)sqrt(square(dx) + square(dy));
// put current tracklength into hit
         selectedhit->SetS(sLocal);
      }
// Calculate conformal coordinates for secondaries only
      if (secondaries)
      {
// vertex is first point on track
         selectedhit->SetConformalCoordinates(currenttrack->GetHits()->head()->GetX(),
         currenttrack->GetHits()->head()->GetY());
      }
#ifdef TRKDEB
/*
 *-->   Keep info about # tries
 */
      nhit_tried += last_hit->nhit_tried ;
      nvol_tried += last_hit->nvol_tried ;
#endif
   }
// result
   return selectedhit;
}
//*****************************************************************************************
// track-follower; extends a track. the track was started using Segment...
// Parameters:
//	currenttrack	- track under construction. to this track, foloower-hits should be added
//	maxsearchpadrow	- maximum number of padrows that will be searched one hit
//	secondaries	- find secondaries
//*****************************************************************************************
THit* TTrackerFTF::Follow(TTrack* currenttrack, int maxsearchpadrows, BOOL secondaries)
{
#define N_LOOP  9
   int loop_eta[N_LOOP] = { 0, 0, 0, 1, 1, 1,-1,-1,-1 }; // neighbors in eta and phi
   int loop_phi[N_LOOP] = { 0,-1, 1, 0,-1, 1, 0,-1, 1 };
   int padrowindex, etaindex, phiindex;			// loop indices
   int PadrowStart, PadrowEnd;				// limits in padrow-direction
   double slocal, dx, dy, dxy, dsz ;
   double temp;
   double lchi2, lchi2_xy, lchi2_sz ;
   double deta, dphi;
   double  chi2_min, chi2_xy_min, chi2_sz_min ;
   THit* lasthit;					// last hit on track
   THit* selectedhit = NULL;				// new hit on track
   TTrackFit* fitparameters = currenttrack->GetFit();
   THitList* volumehits;				// list of hits in current volume

#ifdef TRKDEB
	if ( theTracker->track_debug && theTracker->para->debug_level >= 2 )
		printf ( " \n ===> Going into Track extension <=== " );
#endif
//
// Calculate limits on the volume loop
//
   lasthit = currenttrack->GetHits()->tail();
//
// calculate low- and high-limit of search-padrow
//
   PadrowStart = max(1, lasthit->GetPadrow() - 1);
   PadrowEnd = max(1, PadrowStart - maxsearchpadrows);
   chi2_xy_min = chi2_sz_min = 0.F ;
   lchi2_sz    = 0.F ;
   chi2_min    = FSChi2Cut;	// set chi-square-cut for good hit on track
//
// loop over padrows in search-range
//
   for (padrowindex = PadrowStart; padrowindex >= PadrowEnd; padrowindex--)
   {
// loop over volume neighborhood
      for(int volumeindex = 0; volumeindex < N_LOOP; volumeindex++)
      {
// get phi-index
         phiindex = lasthit->GetPhiIndex() + loop_phi[volumeindex];
// wrap index if phi is closed
         if (FSPhiClosed)
         {
            if (phiindex < 1) 
               phiindex += THit::GetNumberOfPhiSlices();
            else if (phiindex > THit::GetNumberOfPhiSlices())
               phiindex -= THit::GetNumberOfPhiSlices();
         }
         else if ((phiindex < 1) || (phiindex > THit::GetNumberOfPhiSlices()))
            continue;     // don't use this hit
//
// get eta-index
//
         etaindex = lasthit->GetEtaIndex() + loop_eta[volumeindex];
// check eta-bounds
         if ((etaindex < 1) || (etaindex > THit::GetNumberOfEtaSlices())) continue;	

#ifdef TRKDEB
         last_hit->nvol_tried++ ; 
#endif
//
// get list of hits in current volume
//
         volumehits = (*FVolume)(padrowindex, etaindex, phiindex);
// any hits?
         if (volumehits == NULL) continue;	// no, next volume
//
// loop over hits in this volume (first hit will always be present)
//
         THit* candidate = NULL;
         forall(candidate, *volumehits)
         {
#ifdef TRKDEB
            debug_in_volume ( candidate_hit ) ;
#endif
//
// check wether hit was used before
//
            if ( candidate->GetTrack() == 0  ) {
// no!
// get distance between last hit on track and this candidate in phi
//
               dphi  = (double)fabs(lasthit->GetPhi() - candidate->GetPhi()) ; 
// check phi against bounds that make sense
               if ((dphi > FSDPhiLimit) && (dphi < 2*Pi - FSDPhiLimit)) 
                  continue ;// out of bounds, next hit
// Make sure we want to look at the difference in eta
               if ((lasthit->GetDz() < 1000.0) && (candidate->GetDz() < 1000.0))
               {
                  deta  = (double)fabs(lasthit->GetEta() - candidate->GetEta()); 
// check eta against bound
                  if (deta > FSDEtaLimit) continue;	// out of bounds, next hit
               }
               else deta = 0.0F ;
#ifdef TRKDEB
               debug_follow_candidate ( candidate_hit );
#endif
//
// Calculate conformal coordinates for secondaries only
// vertex is first point on track
//
               if (secondaries)
                  candidate->SetConformalCoordinates(currenttrack->GetHits()->head()->GetX(),
                                                      currenttrack->GetHits()->head()->GetY());
// Calculate distance in conformal space
               temp = (double)(fitparameters->a2_xy * candidate->GetConformalX() - 
                                 candidate->GetConformalY() + fitparameters->a1_xy);
               dxy  = square(temp) / (square(fitparameters->a2_xy) + 1);
// Calculate chi2 (in conformal space)
               lchi2_xy = (double)(dxy * candidate->GetDxy2());
// Now in the sz plane
               if (FSFitSz) {
//
// Get tracklength "s" and calculate distance hit-line
//
                  dx = lasthit->GetX() - candidate->GetX();
                  dy = lasthit->GetY() - candidate->GetY();
                  slocal = lasthit->GetS() + sqrt (square(dx) + square(dy));

                  temp = (double)(fitparameters->a2_sz * slocal - candidate->GetZ() + 
                                   fitparameters->a1_sz) ;
                  dsz  = square(temp) / (square(fitparameters->a2_sz) + 1 ) ;
// Calculate chi2 (in sz)
                  lchi2_sz = (double)(dsz * candidate->GetDz2());
               }
               lchi2 = (lchi2_xy + lchi2_sz);

#ifdef TRKDEB
               debug_follow_success ( dxy, dsz, lchi2_xy, lchi2_sz, chi2_min, candidate_hit ) ;
#endif

// Check whether the chi2 square (of the track) is better than previous one
               if (lchi2 < chi2_min) {
// yes, update chi-squares
                  chi2_min = (double)lchi2 ;
                  chi2_xy_min = (double)lchi2_xy;
                  chi2_sz_min = (double)lchi2_sz;
// keep this hit for the moment
                  selectedhit = candidate;

                  if (FSFitSz) selectedhit->SetS(slocal);
// if a good chi2 (for a new hit) is found let's stop here
                  if (lchi2 < FSGoodChi2) goto found;// hit found; shortcircuit loops
               }
            }
         } // End hit loop
      } // End volume
			
   } // End row loop
// Add this hit to the track candidate
   found: ;	// second end of shortcircuit
//
// Check wether the chi2 (of the track) is good enough
//
   if ( chi2_min > FSChi2Cut) selectedhit = NULL;	// no, delete this candidate!
//
// did we find something?
//
   if (selectedhit != NULL) {
//
// yes, set chi-squares
//
      currenttrack->SetChi2(currenttrack->GetChi2(0) + chi2_xy_min, 0);
      selectedhit->SetChi2Xy(chi2_xy_min);
//
// if sz-fit update track length and chi-square in sz
//
      if (FSFitSz) {
// set tracklength to distance of this hit to primary vertex
         currenttrack->SetS(selectedhit->GetS());
// update sz-chi-square for track
         currenttrack->SetChi2(currenttrack->GetChi2(1) + chi2_sz_min, 1);
// same for hit
         selectedhit->SetChi2Sz(chi2_sz_min);
      }
   }

#ifdef TRKDEB
   if ( theTracker->track_debug && para->debug_level >= 2 )
   {
      if ( selected_hit != 0 )
         printf ( " FTF_follow_2: Search succesful, hit selected %d ",selected_hit->id );
      else
      {
         printf ( " FTF_follow_2: Search unsuccesful " );
         if ( chi2_min > para->chi2_hit_cut )
            printf ( " hit chi2 %f larger than cut %f ", chi2_min, para->chi2_hit_cut ) ;
      }
      theTracker->debug_ask () ;
   }
//
//-->   Keep info about # tries
//
    if ( selected_hit != 0 ) 
    {
      nhit_tried += last_hit->nhit_tried ;
      nvol_tried += last_hit->nvol_tried ;
    }
#endif
// Return the selected hit
   return selectedhit ;
}

//***************************************************************************************************
// get all tracks (primaries or secondaries)
// Parameters:
//	tracklist   - list of tracks (filled by this method)
//	tracktype   - determines, which kind of track are to be found (primaries, secondaries,  ?)
//***************************************************************************************************
void TTrackerFTF::GetTracks( TTrackList* tracklist, TTrackType tracktype)
{
   THit*	nexthit;		               // the next hit on my track
   TTrack* currenttrack;		      // a new track
   int	MinimumHitsPerSegment;		// minimum required hits for one starting segment
   int	MinimumNumberOfHitsForFit;	// minimum required hits for fit
//
// set some parameters
//
   switch (tracktype) {
      case ttPrimaries:	// look for primaries
         MinimumNumberOfHitsForFit = 1;		// minimum required hits for a fit
         MinimumHitsPerSegment = FSMinimumHitsPerSegment;
	      break;
      case ttSecondaries:	// look for secondaries
         MinimumNumberOfHitsForFit = 2;		// minimum required hits for a fit
	      MinimumHitsPerSegment = max(FSMinimumHitsPerSegment,3) ;// we need at least 3 hits for secondaries
         break;
   }
//
// Loop over padrows
//
   for (int padrowindex = HIGH_PADROW  ; padrowindex >= FSMinimumHitsPerTrack; padrowindex--) {
//
// Loop over hits in this particular padrow
// we loop over all hits of this padrow in the Volume
//
      FVolume->SetFirstHitInPadrow(padrowindex);
//
      while ((nexthit = FVolume->GetNextHitInPadrow()) != NULL)
      {
// Check: hit was not used before?
         if (nexthit->GetUsed()) continue;	// yes, next hit
//
// One more track 
//
         nexthit->SetS(0.) ;
         currenttrack = new TTrack( );
// Set fit parameters to zero
         currenttrack->ResetFitParameters();
// Go into hit looking loop
         while (nexthit != NULL) 
         {
// add this hit to the track and update fit-parameters
            currenttrack->AddHit(nexthit, MinimumNumberOfHitsForFit, FSFitSz); 
// Check this is not last row to be considered
            if (nexthit->GetPadrow() <= LOW_PADROW) break;	// it is, exit
//
            if (currenttrack->GetHits()->size() < MinimumHitsPerSegment )
// If number of hits is low try to extend the segment without any fit
               nexthit = Segment(currenttrack, FSMaxSearchPadrowsSegment, (tracktype == ttSecondaries));
            else 
// is high use fit information
               nexthit = Follow(currenttrack, FSMaxSearchPadrowsTrack, (tracktype == ttSecondaries));
            } // End hit looking loop
//
// Now we've got a track candidate
// Let's check whether to keep this track
//
            double normalized_chi2 = 
			(currenttrack->GetChi2(0)+currenttrack->GetChi2(1))
                              /currenttrack->GetHits()->size();
    	    if ((currenttrack->GetHits()->size() >= FSMinimumHitsPerTrack) &&
				(normalized_chi2 < FSChi2TrackCut) )
            {
//
// Track is long enough and has a good total chi2 
// let's store fit parameters 
//
            if (tracktype == ttPrimaries) 
            {
// do the track-fit for primaries
               currenttrack->FinishPrimaryTrack(FPrimaryVertexX, FPrimaryVertexY,
						FPrimaryVertexR, FPrimaryVertexPhi, FSFitSz);
// flag as primary
               currenttrack->SetSecondary(0);
//
// shall we merge primary tracks?
/* done by TMerger
//
               if (FMergePrimaries) {
//
// yes, did we merge the current track?
                  if (!CombineTracks(tracklist, currenttrack))
                     tracklist->append(*currenttrack);
                  delete currenttrack;
               }
               else
*/
               {
// Add Track to Tracklist
                  tracklist->append(currenttrack);
               }
	    }
            else
            {
// do the track-fit for secondaries
               currenttrack->FinishSecondaryTrack(FSFitSz);
// flag as secondary
               currenttrack->SetSecondary(1);
// Add Track to Tracklist
               tracklist->append(currenttrack);
            }
         }
         else
         {
//
// If not long enough track delete it
// first unlock the used hits
//
            currenttrack->RemoveTrack();
// then delete this track
            delete currenttrack;
         }
      } // End loop over hits inside row
		
    } // End loop over rows
	return;
}
