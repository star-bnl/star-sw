//
// TpcTrackFrame
//
//   This class supports a general tracker development environment.
//   The purpose of this class is to free the tracking developer from
//   doing all this nasty root stuff. He only concentrates on writing
//   the trackers, the rest is common to all trackers.
//
//   Functionality:
//	Init()	   		- initialize the tracking environment
//	SetTracker(VTracker&)	- set the tracking algorithm
//	Done()			- free memory, etc...		
//
//   Example:
//	...
//	TpcTrackFrame 	theFrame;
//	TMyTracker	theTracker("setup_1.ini");
//
//	theFrame.Init();
//	theFrame.SetTracker(theTracker);
//	theFrame.OpenAscii("sect13out.tex");
//  theFrame.SetEvent();  // default starts at first event
//	do
//	{
//    theFrame.ProcessEvent();
//	  theFrame.Interactive();
//	} while (theFrame.NextEvent);
//	theFrame.Done();

// some includes
#include "TpcTrackFrame.hpp"
#include "TTracker.hpp"
#include "Common.h"
#include "types.h"

// stupid constructor
TpcTrackFrame::TpcTrackFrame() 
{
  // create the free point list (with default parameters, should be adapted)
  fFreePoints = new THitList();
  // create free point lists for sectorwise calculation
  for(int count = 0; count < 24; count++)
    {
      fFreePointsSector[count] = new THitList();
      fTracksSector[count] = 0;
    }
  // initialize properties
  fTracker = 0;
  fTracks = 0;
}

// stupid destructor
TpcTrackFrame::~TpcTrackFrame()
{
  // delete the freepoint list
  Done();
}


// Initialize the frame
void TpcTrackFrame::Init()
{
  // set default values for static members in THit, TTrack and TTrackerFTF
  // THit:
  THit::SetErrorScaleSz(3.0);
  THit::SetErrorScaleXy(5.0);
  // phimin, phimax, phislices
  THit::SetPhiParameters(-0.000001/To_deg, 360.000001/To_deg, 20);
  // etamin, etamax, etasclices
  THit::SetEtaParameters(-2.2, 2.2, 60);
  // TTrack:
  TTrack::SetBField(0.5);
  //TTracker:
  TTrackerFTF::SetSFitSz(FALSE);	// TRUE
  TTrackerFTF::SetSPhiClosed(FALSE);
  TTrackerFTF::SetSDPhiLimit(0.1);	// 0.1
  TTrackerFTF::SetSDEtaLimit(0.1);	// 0.1
  // chi2_hit_cut
  TTrackerFTF::SetSChi2Cut(150.0);	// 150
  // chi2_hit_good
  TTrackerFTF::SetSGoodChi2(50.0);
  //good_distance
  TTrackerFTF::SetSGoodDistance(2.0);
  // not a constant
//  TTrackerFTF::SetStrack(0.0);
  // chi2_track_cut
  TTrackerFTF::SetSChi2TrackCut(150.0);	// 150
  // n_hit_segm
  TTrackerFTF::SetSMinimumHitsPerSegment(3);
  // mn_hit_trk
  TTrackerFTF::SetSMinimumHitsPerTrack(5);
  // dr_track
  TTrackerFTF::SetSMaxSearchPadrowsTrack(4);
  // dr_segm
  TTrackerFTF::SetSMaxSearchPadrowsSegment(2);
}

// Initialize the frame via an parameter struct
void TpcTrackFrame::Initialize(L3T_TPC_PARA_ST* params)
{
  // set default values for static members in THit, TTrack and TTrackerFTF
  // THit:
  THit::SetErrorScaleSz(params->ErrorScaleSz);
  THit::SetErrorScaleXy(params->ErrorScaleXy);
  // phimin, phimax, phislices
  THit::SetPhiParameters(params->Phimin/To_deg, params->Phimax/To_deg, params->PhiSlices);
  // etamin, etamax, etasclices
  THit::SetEtaParameters(params->Etamin, params->Etamax, params->EtaSlices);
  // TVolume: obsolete moved into TTrackerFTF
  // etaslices, phislices, phimin, phimax, etamin, etamax
  TTrackerFTF::SetStatics((short)params->EtaSlices, (short)params->PhiSlices, 
			  params->Phimin/To_deg, params->Phimax/To_deg, 
			  params->Etamin, params->Etamax);
  // TTrack:
  TTrack::SetBField(params->BField);
  //TTracker:

  TTrackerFTF::SetSFitSz(params->SFitSz);
  TTrackerFTF::SetSPhiClosed(params->SPhiClosed);
  TTrackerFTF::SetSDPhiLimit(params->SDPhiLimit);
  TTrackerFTF::SetSDEtaLimit(params->SDEtaLimit);
  // chi2_hit_cut
  TTrackerFTF::SetSChi2Cut(params->SChi2Cut);
  // chi2_hit_good
  TTrackerFTF::SetSGoodChi2(params->SGoodChi2);
  //good_distance
  TTrackerFTF::SetSGoodDistance(params->SGoodDistance);
  // chi2_track_cut
  TTrackerFTF::SetSChi2TrackCut(params->SChi2TrackCut);
  // n_hit_segm
  TTrackerFTF::SetSMinimumHitsPerSegment(params->SMinimumHitsPerSegment);
  // mn_hit_trk
  TTrackerFTF::SetSMinimumHitsPerTrack(params->SMinimumHitsPerTrack);
  // dr_track
  TTrackerFTF::SetSMaxSearchPadrowsTrack(params->SMaxSearchPadrowsTrack);
  // dr_segm
  TTrackerFTF::SetSMaxSearchPadrowsSegment(params->SMaxSearchPadrowsSegment);
  // merge primaries?
  TTrackerFTF::SetMergePrimaries(params->MergePrimaries);
  // TMerger:
  
  TMerger::NumberOfPsiSlices = params->NumberOfPsiSlices;
  TMerger::NumberOfTanLSlices = params->NumberOfTanLSlices;
  
  TMerger::MinSlicePsi = params->MinSlicePsi;
  TMerger::MaxSlicePsi = params->MaxSlicePsi;
  TMerger::MinSliceTanL = params->MinSliceTanL;
  TMerger::MaxSliceTanL = params->MaxSliceTanL;
  // maximum distance to merge primaries
  TMerger::FSMinDistMerge = params->SDPsiMaxMerge;
}

// Set the tracking algorithm
void TpcTrackFrame::SetTracker(VTracker* thetracker)
{
  fTracker = thetracker;
}
//************************************************************************************
// (re)process current event and build all tracks
//************************************************************************************
void TpcTrackFrame::ProcessEvent()
{
  int nSectors = 24 ;
  int count;
  if (fTracks)
    {
      if (fTracks) delete fTracks;
      for(count = 0; count < nSectors ; count++)
	{
	  if (fTracksSector[count])
	    delete fTracksSector[count];
	  fTracksSector[count] = 0;
	}
      fTracks = 0;
    }
//
// Create a new track list
//
  fTracks = fTracker->CreateTrackList();
//
// create new track lists for sectorwise calculation
//
  for (count = 0; count < nSectors; count++)
      fTracksSector[count] = fTracker->CreateTrackList();
//
// Build all tracks
// check for sector calculation
//
  if ( FLastSector == 0 )
    fTracker->BuildAllTracks( fFreePoints, fTracks);
  else
    {
      for(count = FFirstSector-1; count < FLastSector ; count++)
	{
	  // debug output
#ifdef DEBUG
	  printf("build tracks for sector %d\n", count+1);
#endif
          int firstTrackId = (count+1) * 1000 ;
	  fTracker->BuildAllTracks ( fFreePointsSector[count], fTracksSector[count]);
	}
      // merge all lists
      fTracks->clear();
      // debug output
#ifdef DEBUG
      printf(" connect all tracklists\n");
#endif
      for (count = FFirstSector-1; count < FLastSector; count++)
	fTracks->conc(*fTracksSector[count]);
      // run the merger globally over the list of all tracks
      // done by TMerger object (phi is closed)
      // debug output
#ifdef DEBUG
      printf(" start the global merger\n");
#endif
      TMerger merge(fTracks, 1);
    }
}


// free event memory and clean up
void TpcTrackFrame::Done()
{
#ifdef LEDA
//  Error << ">> Cleaning environment" << endl;
// kill all the memory at once, but leave the first memory block allocated for further use
  std_memory_mgr.kill_but_first();
#endif
//
// and get a new freepoint list
//
  fFreePoints = new THitList();
// create free point lists for sectorwise calculation
  for(int count = 0; count < 24; count++)
    {
      fFreePointsSector[count] = new THitList();
      fTracksSector[count] = 0;
    }
// tracklist is deleted
  fTracks = 0;
}

BOOL TpcTrackFrame::FillEvent( TABLE_HEAD_ST     *tphit_h,      
			                 TCL_TPHIT_ST      *table,
			                 int FirstSector,
			                 int LastSector )
{

  FFirstSector = FirstSector;
  FLastSector  = LastSector ;
  // loop over all point
  THit *Point;
  int  sect ;
  for(int count = 0; count < tphit_h->nok ; count++)
  {
//
//   Check whether hit is in desired range
//
	sect = table[count].row / 100;
	if ( LastSector != 0 && sect < FirstSector && sect > LastSector ) continue ;
//
//
//   Get rid of track assignment
//
    table[count].track = 0 ;
//
// create a new point
//
    Point = new THit();
// fill in the point data
    Point->SetId(count);
    Point->SetX (table[count].x);
    Point->SetY (table[count].y);
    Point->SetZ (table[count].z);
    Point->SetPadrow(table[count].row % 100);
    Point->SetCharge(table[count].q);
    Point->SetDx(table[count].dx);	// otherwise we won't find anything
    Point->SetDy(table[count].dy);
    Point->SetDz(table[count].dz);
    Point->SetMCID(0);
    Point->SetConformalCoordinates(0.0, 0.0);
// Calculate cylinder coordinates (relativ to (0,0,0))
    Point->SetCylinderCoordinates();
// If fit in sz required -> calculate errors
    Point->SetFitSz();
    Point->SetTrack(0);
// sectorwise calculation?
    if( LastSector == 0)
// add to pointlist
      fFreePoints->append(Point);
    else
// add to sector hitlist
	  fFreePointsSector[sect-1]->append(Point);
  } // end fill points
  return 1;
}
//****************************************************************
//
//****************************************************************
int TpcTrackFrame::FillResultTable
       ( TABLE_HEAD_ST *hit_h,   TCL_TPHIT_ST *hit, 
         TABLE_HEAD_ST *track_h, TPT_TRACK_ST *tracks)
{
  TTrack*  track;
  THit*    tHit = NULL  ;       // hit  pointer 

  
  int counter = 0;
  forall(track, *fTracks) {
     tracks[counter].flag     = 1;
     tracks[counter].hitid    = 0;
     tracks[counter].id       = counter + 1 ;
     tracks[counter].dedx[0]  = 0.0;
     tracks[counter].dedx[1]  = 0.0;
     tracks[counter].ndedx    = 0;
     tracks[counter].nfit     = tracks[counter].nrec = track->GetHits()->size();
     tracks[counter].q        = (long)track->GetCharge();
     tracks[counter].chisq[0] = track->GetChi2(0);
     tracks[counter].chisq[1] = track->GetChi2(1);
     tracks[counter].invp     = 1.0F/track->GetPt();
     tracks[counter].phi0     = track->GetPhi0();
     tracks[counter].psi      = track->GetPsi();
     tracks[counter].r0       = track->GetR0();
     tracks[counter].tanl     = track->GetTanL();
     tracks[counter].z0       = track->GetZ0();
//
//     Loop over hits of this track
//     and assign them to this track
//
     int id ; 
     int nHits = 1 ;
     forall(tHit, *(track->GetHits())) {
        id = tHit->GetId() ;
        if ( id < 0 || id > hit_h->nok - 1 ) {
           printf ( "TpcTrackFrame: \n Error assigning hits to tracks " ) ; 
           continue ;
        }
        hit[id].track = 1000 * (counter + 1) + nHits ;
        nHits++ ;
     }
//
//    Increase counter and check bounds
//
     counter++;
     if(counter >= track_h->maxlen)
     {
        track_h->nok = counter;
        return 1;
     }
  }
  track_h->nok = counter;
//
//    Loop over hits to get track assignment
//

  return 0;
}
