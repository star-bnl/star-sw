// $Id: StFtpcTracker.cc,v 1.1 2000/05/10 13:39:31 oldi Exp $
// $Log: StFtpcTracker.cc,v $
// Revision 1.1  2000/05/10 13:39:31  oldi
// Initial version of StFtpcTrackMaker
//

//----------Author:        Holm G. H&uuml;mmler, Markus D. Oldenburg
//----------Last Modified: 27.04.2000
//----------Copyright:     &copy MDO Production 1999

#include "StFtpcTracker.hh"
#include "StFtpcHit.hh"
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
  // Sets the pointers to 0 an dcut for momnetum fit loosely.

  mVertex = 0;
  mHit = 0;
  mTrack = 0;

  mMaxDca = 100.;
}


StFtpcTracker::StFtpcTracker(St_fcl_fppoint *fcl_fppoint, Double_t vertexPos[3], Double_t max_Dca)
{
  // Usual used constructor.
  // Sets up the pointers and the cut value for the momentum fit.

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
}


StFtpcTracker::~StFtpcTracker()
{
  // Destructor.

  if (mTrack) {
    mTrack->Delete();
    delete mTrack;
  }

  if (mVertex) {
    delete mVertex;
  }
  
  return;
}


Int_t StFtpcTracker::Write(St_fpt_fptrack *trackTableWrapper)
{
  // Writes tracks to STAF table.
  
  fpt_fptrack_st *trackTable= trackTableWrapper->GetTable();

  if (mTrack) {
    Int_t num_tracks = mTrack->GetEntriesFast();
    
    //if(num_tracks > trackTableWrapper->GetHeader()->maxlen)
    //  num_tracks = trackTableWrapper->GetHeader()->maxlen;

    if(num_tracks > trackTableWrapper->GetTableSize())
      num_tracks = trackTableWrapper->GetTableSize();


    for (Int_t i=0; i<num_tracks; i++) {
      ((StFtpcTrack *)(mTrack->At(i)))->Fit(mVertex, mMaxDca);    
      ((StFtpcTrack *)(mTrack->At(i)))->Write(&(trackTable[i]));    
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
