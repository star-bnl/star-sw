// $Id: StFtpcTracker.hh,v 1.5 2000/07/18 21:22:17 oldi Exp $
// $Log: StFtpcTracker.hh,v $
// Revision 1.5  2000/07/18 21:22:17  oldi
// Changes due to be able to find laser tracks.
// Cleanup: - new functions in StFtpcConfMapper, StFtpcTrack, and StFtpcPoint
//            to bundle often called functions
//          - short functions inlined
//          - formulas of StFormulary made static
//          - avoid streaming of objects of unknown size
//            (removes the bunch of CINT warnings during compile time)
//          - two or three minor bugs cured
//
// Revision 1.4  2000/07/03 12:48:14  jcs
// use (pre)Vertex id to access vertex coordinates for unconstrained fit and
// for constrained fit
//
// Revision 1.3  2000/05/15 14:28:14  oldi
// problem of preVertex solved: if no main vertex is found (z = NaN) StFtpcTrackMaker stops with kStWarn,
// refitting procedure completed and included in StFtpcTrackMaker (commented),
// new constructor of StFtpcVertex due to refitting procedure,
// minor cosmetic changes
//
// Revision 1.2  2000/05/12 12:59:18  oldi
// removed delete operator for mSegment in StFtpcConfMapper (mSegment was deleted twice),
// add two new constructors for StFtpcTracker to be able to refit already existing tracks,
// minor cosmetics
//
// Revision 1.1  2000/05/10 13:39:33  oldi
// Initial version of StFtpcTrackMaker
//

//////////////////////////////////////////////////////////////////////////////////
//                                                                              //
// StFtpcTracker class - interface class to call the different track algorithms //
//                       for the Ftpc.                                          //
//                                                                              //
//////////////////////////////////////////////////////////////////////////////////

#ifndef STAR_StFtpcTracker
#define STAR_StFtpcTracker

#include "TObject.h"
#include "StFtpcVertex.hh"
#include "TClonesArray.h"
#include "tables/St_fpt_fptrack_Table.h"
#include "tables/St_fcl_fppoint_Table.h"

class StFtpcTracker : public TObject {

protected:

            StFtpcVertex   *mVertex;        // pointer to the vertex
            TClonesArray   *mHit;           // ClonesArray of clusters
            TClonesArray   *mTrack;         // ClonesArray of tracks
                  Bool_t    mHitsCreated;   // indicator if this class created the mHit ClonesArray
                  Bool_t    mVertexCreated; // indicator if this class created the mVertex
                Double_t    mMaxDca;        // cut value for momentum fit

public:

            StFtpcTracker();  // default constructor
            StFtpcTracker(St_fcl_fppoint *fcl_fppoint, Double_t vertexPos[3] = NULL, Double_t max_Dca = 100.);            // real constructor
            StFtpcTracker(StFtpcVertex *vertex, TClonesArray *hit, TClonesArray *track, Double_t max_Dca = 100.);         // constructor if everything is already there
            StFtpcTracker(TClonesArray *hits, Double_t vertexPos[3], Double_t max_Dca = 100.);                            // constructor to handle arbitrary hits 
            StFtpcTracker(StFtpcVertex *vertex, St_fcl_fppoint *fcl_fppoint, St_fpt_fptrack *fpt_fptrack, Double_t dca);  // constructor do do refitting

  virtual  ~StFtpcTracker();  // destructor

    Int_t   FitAndWrite(St_fpt_fptrack *trackTable, Int_t id_start_vertex);   // does momentum fit and writes tracks to STAF table
    Int_t   Write();                                                          // writes tracks and clusters in ROOT file

  // getter
  StFtpcVertex  *GetVertex()            { return mVertex;                  }  // returns the vertex
         Int_t   GetNumberOfClusters()  { return mHit->GetEntriesFast();   }  // returns the number of clusters
         Int_t   GetNumberOfTracks()    { return mTrack->GetEntriesFast(); }  // returns the number of tracks
  TClonesArray  *GetClusters()          { return mHit;                     }  // returns ClonesArray of clusters
  TClonesArray  *GetTracks()            { return mTrack;                   }  // returns ClonesArray of tracks
      Double_t   GetMaxDca()      const { return mMaxDca;                  }  // returns cut value for momentum fir

  // setter
          void   SetMaxDca(Double_t f)  { mMaxDca = f;                     }  // sets cut value for momentum fit 

  ClassDef(StFtpcTracker, 1)  //Ftpc tracker interface class
};


inline Int_t StFtpcTracker::Write()
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


#endif
