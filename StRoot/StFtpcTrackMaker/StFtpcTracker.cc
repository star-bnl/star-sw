// $Id: StFtpcTracker.cc,v 1.4 2000/05/15 14:28:13 oldi Exp $
// $Log: StFtpcTracker.cc,v $
// Revision 1.4  2000/05/15 14:28:13  oldi
// problem of preVertex solved: if no main vertex is found (z = NaN) StFtpcTrackMaker stops with kStWarn,
// refitting procedure completed and included in StFtpcTrackMaker (commented),
// new constructor of StFtpcVertex due to refitting procedure,
// minor cosmetic changes
//
// Revision 1.3  2000/05/12 12:59:17  oldi
// removed delete operator for mSegment in StFtpcConfMapper (mSegment was deleted twice),
// add two new constructors for StFtpcTracker to be able to refit already existing tracks,
// minor cosmetics
//
// Revision 1.2  2000/05/11 15:14:53  oldi
// Changed class names *Hit.* due to already existing class StFtpcHit.cxx in StEvent
//
// Revision 1.1  2000/05/10 13:39:31  oldi
// Initial version of StFtpcTrackMaker
//

//----------Author:        Holm G. H&uuml;mmler, Markus D. Oldenburg
//----------Last Modified: 12.05.2000
//----------Copyright:     &copy MDO Production 1999

#include "StFtpcTracker.hh"
#include "StFtpcPoint.hh"
#include "StFtpcTrack.hh"

///////////////////////////////////////////////////////////////////////////////////
//                                                                               //
// StFtpcTracker class - interface class for the different Ftpc track algorithms //
//                                                                               //
// This class contains the pointers needed to do tracking in the Ftpc i.e. a     // 
// pointer to the vertex, pointers to clusters and tracks.                       //
//                                                                               //
///////////////////////////////////////////////////////////////////////////////////

ClassImp(StFtpcTracker)


StFtpcTracker::StFtpcTracker()
{
  // Default constructor.
  // Sets the pointers to 0 an cut for momnetum fit loosely.

  mVertex = 0;
  mHit = 0;
  mTrack = 0;

  mHitsCreated = (Bool_t)false;
  mVertexCreated = (Bool_t)false;

  mMaxDca = 100.;
}


StFtpcTracker::StFtpcTracker(St_fcl_fppoint *fcl_fppoint, Double_t vertexPos[3], Double_t max_Dca)
{
  // Usual used constructor.
  // Sets up the pointers and the cut value for the momentum fit.

  mHitsCreated = (Bool_t)false;
  mMaxDca = max_Dca;
  mTrack = new TClonesArray("StFtpcTrack", 0);

  Int_t n_clusters = fcl_fppoint->GetNRows();          // number of clusters
  fcl_fppoint_st *point_st = fcl_fppoint->GetTable();  // pointer to first cluster structure

  if(vertexPos == NULL) {
      mVertex = new StFtpcVertex(point_st, n_clusters);
  }
  
  else {
    mVertex = new StFtpcVertex(vertexPos);
  }

  mVertexCreated = (Bool_t)true;
}


StFtpcTracker::StFtpcTracker(StFtpcVertex *vertex, TClonesArray *hit, TClonesArray *track, Double_t dca)
{
  // Constructor to handle the case where everything is there already.

  mVertex = vertex;
  mHit = hit;
  mHitsCreated = (Bool_t) false;
  mVertexCreated = (Bool_t) false;
  mTrack = track;
  mMaxDca = dca;
}


StFtpcTracker::StFtpcTracker(StFtpcVertex *vertex, St_fcl_fppoint *fcl_fppoint, St_fpt_fptrack *fpt_fptrack, Double_t dca)
{
  // Constructor to handle the case where everything is there already but only in StAF tables.

  mVertex = vertex;
  mVertexCreated = (Bool_t)false;

  // Copy clusters into ClonesArray.
  Int_t n_clusters = fcl_fppoint->GetNRows();          // number of clusters
  fcl_fppoint_st *point_st = fcl_fppoint->GetTable();  // pointer to first cluster structure

  mHit = new TClonesArray("StFtpcPoint", n_clusters);    // create TClonesArray
  mHitsCreated = (Bool_t)true;

  TClonesArray &hit = *mHit;
  
  for (Int_t i = 0; i < n_clusters; i++) {
    new(hit[i]) StFtpcPoint(point_st++);
    ((StFtpcPoint *)mHit->At(i))->SetHitNumber(i);
  }

  // Copy tracks into ClonesArray.
  Int_t n_tracks = fpt_fptrack->GetNRows();  // number of tracks
  fpt_fptrack_st *track_st = fpt_fptrack->GetTable();  // pointer to first track structure

  mTrack = new TClonesArray("StFtpcTrack", n_tracks);    // create TClonesArray
  TClonesArray &track = *mTrack;
  
  for (Int_t i = 0; i < n_tracks; i++) {
    new(track[i]) StFtpcTrack(track_st++, mHit);
  }

  mMaxDca = dca;
}


StFtpcTracker::~StFtpcTracker()
{
  // Destructor.

  if (mTrack) {
    mTrack->Delete();
    delete mTrack;
  }
  
  if (mHitsCreated) {
    mHit->Delete();
    delete mHit;
  }

  if (mVertex && mVertexCreated) {
    delete mVertex;
  }
  
  return;
}


Int_t StFtpcTracker::FitAndWrite(St_fpt_fptrack *trackTableWrapper)
{
  // Writes tracks to STAF table.
  
  fpt_fptrack_st *trackTable= trackTableWrapper->GetTable();

  if (mTrack) {
    Int_t num_tracks = mTrack->GetEntriesFast();
    
    if(num_tracks > trackTableWrapper->GetTableSize()) {
      num_tracks = trackTableWrapper->GetTableSize();
    }

    StFtpcTrack *track;
    
    for (Int_t i=0; i<num_tracks; i++) {
      track = (StFtpcTrack *)mTrack->At(i);
      track->Fit(mVertex, mMaxDca);  
      track->Write(&(trackTable[i]));    
    }
   
    trackTableWrapper->SetNRows(num_tracks);
    cout << "Writing " << num_tracks << " tracks." << endl;

    return 0;
  }

  else {
    cout << "Tracks not written (No tracks found!)." << endl;
    return -1;
  }
}


Int_t StFtpcTracker::Write()
{
  // Writes tracks and clusters in ROOT file.
  // In the moment this makes no sense because the important information
  // about momentum of tracks and coordinates of clusters or stored in
  // StThreeVerctor<double> which does not inherit from TObject. So it
  // is not written out!

  mHit->Write();
  mTrack->Write();
  
  return 0;
}
