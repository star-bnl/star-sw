// $Id: StFtpcTracker.hh,v 1.14 2002/06/04 13:41:37 oldi Exp $
// $Log: StFtpcTracker.hh,v $
// Revision 1.14  2002/06/04 13:41:37  oldi
// Minor change: 'west' -> 'hemisphere' (just a naming convention)
//
// Revision 1.13  2002/04/05 16:51:13  oldi
// Cleanup of MomentumFit (StFtpcMomentumFit is now part of StFtpcTrack).
// Each Track inherits from StHelix, now.
// Therefore it is possible to calculate, now:
//  - residuals
//  - vertex estimations obtained by back extrapolations of FTPC tracks
// Chi2 was fixed.
// Many additional minor (and major) changes.
//
// Revision 1.12  2002/01/29 11:08:29  oldi
// Write() renamed to WriteCluster() resp. WriteTrack() to avoid compiler warnings.
// As a result the functions TObject::Write() are available again (directly).
//
// Revision 1.11  2001/07/12 08:35:54  oldi
// New function GetTime(char name[10]) introduced.
//
// Revision 1.10  2001/04/02 14:20:23  oldi
// Some minor changes due to Insure++ was reporting problems.
// These changes do not affect the physical output of StFtpcTrackMaker!
//
// Revision 1.9  2001/01/30 13:31:54  oldi
// New variable mTime introduced to count total time consumption.
//
// Revision 1.8  2001/01/25 15:22:34  oldi
// Review of the complete code.
// Fix of several bugs which caused memory leaks:
//  - Tracks were not allocated properly.
//  - Tracks (especially split tracks) were not deleted properly.
//  - TClonesArray seems to have a problem (it could be that I used it in a
//    wrong way). I changed all occurences to TObjArray which makes the
//    program slightly slower but much more save (in terms of memory usage).
// Speed up of HandleSplitTracks() which is now 12.5 times faster than before.
// Cleanup.
//
// Revision 1.7  2000/11/23 01:33:16  oldi
// Proper initialization of some variables to avoid Insure++ error messages.
//
// Revision 1.6  2000/11/10 18:39:09  oldi
// TBenchmark object 'mBech' moved from StFtpcConfMapper to here. This implied changes in the constructors.
// New function CalcEnergyLoss(FDE_FDEPAR_ST *fdepar) which replaces the pams/fde modul.
// New function FitAnddEdxAndWrite() introduced which replaces CalcEnergyLoss() and FitAndWrite().
//
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
#include "TBenchmark.h"
#include "StFtpcVertex.hh"
#include "TObjArray.h"
#include "tables/St_fpt_fptrack_Table.h"
#include "tables/St_fcl_fppoint_Table.h"
#include "tables/St_fde_fdepar_Table.h"

class StFtpcTracker : public TObject {

protected:

              TBenchmark   *mBench;         // benchmark object (just for run-time measurements)
                 Float_t    mTime;          // total time consumption
      
            StFtpcVertex   *mVertex;        // vertex used for tracking
            StFtpcVertex   *mVertexEast;    // vertex estimation obtained by back extrapolation of east tracks
            StFtpcVertex   *mVertexWest;    // vertex estimation obtained by back extrapolation of west tracks

               TObjArray   *mHit;           // ObjArray of clusters
               TObjArray   *mTrack;         // ObjArray of tracks
                  Bool_t    mHitsCreated;   // indicator if this class created the mHit ObjArray
                  Bool_t    mVertexCreated; // indicator if this class created the mVertex
                Double_t    mMaxDca;        // cut value for momentum fit

public:

            StFtpcTracker();                                  // default constructor
            StFtpcTracker(St_fcl_fppoint *fcl_fppoint, 
			  Double_t vertexPos[6] = 0, 
			  Bool_t bench = (Bool_t)false, 
			  Double_t max_Dca = 100.);           // real constructor
            StFtpcTracker(StFtpcVertex *vertex, 
			  TObjArray *hit, 
			  TObjArray *track, 
			  Bool_t bench = (Bool_t)false, 
			  Double_t max_Dca = 100.);           // constructor if everything is already there
            StFtpcTracker(TObjArray *hits, 
			  StFtpcVertex *vertex, 
			  Bool_t bench = (Bool_t)false, 
			  Double_t max_Dca = 100.);           // constructor to handle arbitrary hits 
            StFtpcTracker(StFtpcVertex *vertex, 
			  St_fcl_fppoint *fcl_fppoint, 
			  St_fpt_fptrack *fpt_fptrack, 
			  Bool_t bench = (Bool_t)false, 
			  Double_t max_Dca = 100.);           // constructor do do refitting

  virtual  ~StFtpcTracker();  // destructor

  void    EstimateVertex(StFtpcVertex *vertex, UChar_t iterations = 1);     // vertex estimation with fit tracks for FTPC east amd west
  void    EstimateVertex(StFtpcVertex *vertex, Char_t hemisphere, UChar_t iterations);  // vertex estimation with fit tracks
  void    CalcEnergyLoss(FDE_FDEPAR_ST *fdepar);                            // calculates dE/dx
  void    Sorter(Double_t *arr, Int_t *index, Int_t len);                   // sorts by dE/dx
  Int_t   FitAnddEdxAndWrite(St_fpt_fptrack *trackTable, 
			     FDE_FDEPAR_ST *fdepar, 
			     Int_t id_start_vertex);                        // does momentum fit, the dEdx calculation and writes tracks to STAF table
  Int_t   FitAndWrite(St_fpt_fptrack *trackTable, Int_t id_start_vertex);   // does momentum fit and writes tracks to STAF table
  Int_t   WriteTracksAndClusters();                                         // writes tracks and clusters in ROOT file

  // getter
       Float_t   GetTime()              { return mTime;                    }  // returns time consumption
       Float_t   GetTime(char name[10]) { return mBench->GetCpuTime(name); }  // returns time consumption for different tracking parts
  StFtpcVertex  *GetVertex()            { return mVertex;                  }  // returns the vertex
  StFtpcVertex  *GetVertexEast()        { return mVertexEast;              }  // returns the vertex estimation of FTPC east
  StFtpcVertex  *GetVertexWest()        { return mVertexWest;              }  // returns the vertex estimation of FTPC west
         Int_t   GetNumberOfClusters()  { return mHit->GetEntriesFast();   }  // returns the number of clusters
         Int_t   GetNumberOfTracks()    { return mTrack->GetEntriesFast(); }  // returns the number of tracks
     TObjArray  *GetClusters()          { return mHit;                     }  // returns ObjArray of clusters
     TObjArray  *GetTracks()            { return mTrack;                   }  // returns ObjArray of tracks
      Double_t   GetMaxDca()      const { return mMaxDca;                  }  // returns cut value for momentum fir

  // setter
          void   SetMaxDca(Double_t f)  { mMaxDca = f;                     }  // sets cut value for momentum fit 

  ClassDef(StFtpcTracker, 1)  //Ftpc tracker interface class
};


inline Int_t StFtpcTracker::WriteTracksAndClusters()
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
