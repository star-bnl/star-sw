// $Id: StFtpcTracker.hh,v 1.2 2000/05/12 12:59:18 oldi Exp $
// $Log: StFtpcTracker.hh,v $
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

            StFtpcVertex   *mVertex;      // pointer to the vertex
            TClonesArray   *mHit;         // ClonesArray of clusters
            TClonesArray   *mTrack;       // ClonesArray of tracks
                  Bool_t    mHitsCreated; // indicator if this class created the mHit ClonesArray
                Double_t    mMaxDca;      // cut value for momentum fit

public:

            StFtpcTracker();  // default constructor
            StFtpcTracker(St_fcl_fppoint *fcl_fppoint, Double_t vertexPos[3] = NULL, Double_t max_Dca = 100.);             // real constructor
            StFtpcTracker(StFtpcVertex *vertex, TClonesArray *hit, TClonesArray *track, Double_t dca);                    // constructor if everything is already there
            StFtpcTracker(StFtpcVertex *vertex, St_fcl_fppoint *fcl_fppoint, St_fpt_fptrack *fpt_fptrack, Double_t dca);  // sonstructor do do refitting

  virtual  ~StFtpcTracker();  // destructor

    Int_t   FitAndWrite(St_fpt_fptrack *trackTable);    // does momentum fit and writes tracks to STAF table
    Int_t   Write();                                    // writes tracks and clusters in ROOT file

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

#endif
