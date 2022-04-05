/*!
 * \class StFtpcTracker 
 *
 * Interface class to call the different track algorithms 
 * for the Ftpc.    
 */


#ifndef STAR_StFtpcTracker
#define STAR_StFtpcTracker

#include "TObject.h"
#include "TBenchmark.h"
#include "StFtpcVertex.hh"
#include "TObjArray.h"

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
                  Bool_t    mTrackCreated; // indicator if this class created the mTrack
                Double_t    mMaxDca;        // cut value for momentum fit

public:

            StFtpcTracker();                                  // default constructor
            StFtpcTracker(StFtpcVertex *vertex, 
			  TObjArray *hit, 
			  TObjArray *track, 
			  Bool_t bench = (Bool_t)kFALSE, 
			  Double_t max_Dca = 100.);           // constructor if everything is already there
            StFtpcTracker(TObjArray *hits, 
			  StFtpcVertex *vertex, 
			  Bool_t bench = (Bool_t)kFALSE, 
			  Double_t max_Dca = 100.);           // constructor to handle arbitrary hits 

  virtual  ~StFtpcTracker();  // destructor

  void    EstimateVertex(StFtpcVertex *vertex, UChar_t iterations = 1);  // vertex estimation with fit tracks for FTPC east amd west
  void    EstimateVertex(StFtpcVertex *vertex, Char_t hemispshere, UChar_t iterations);  // vertex estimation with fit tracks
  StFtpcVertex EstimateVertex(StFtpcVertex *vertex, Char_t hemisphere, 
			      Char_t sector, UChar_t iterations = 1);  // vertex estimation with fit tracks
  StFtpcVertex EstimateVertex(StFtpcVertex *vertex, Char_t hemisphere,
			      Double_t lowAngle, Double_t highAngle,
			      Double_t lowRadius, Double_t highRadius, 
			      UChar_t iterations = 1);  // vertex estimation with fit tracks
  void    CalcEnergyLoss();                                                 // calculates dE/dx
  void    Sorter(Double_t *arr, Int_t *index, Int_t len);                   // sorts by dE/dx
  Int_t   GlobalFitAnddEdx() { return FitAnddEdx(kFALSE); }                 // global momentum fit and dE/dx calculation (no writing!)
  Int_t   PrimaryFitAnddEdx() { return FitAnddEdx(kTRUE); }                 // primary momentum fit and dE/dx calculation (no writing!)
  Int_t   FitAnddEdx(Bool_t primary_fit);                                   // does momentum fit and dE/dx calculation (no writing!)
  Int_t   GlobalFit()  { return Fit(kFALSE); }                              // global momentum fit
  Int_t   PrimaryFit() { return Fit(kTRUE);  }                              // primary momentum fit 
  Int_t   Fit(Bool_t primary_fit);                                          // does momentum fit 

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




#endif

// $Id: StFtpcTracker.hh,v 1.21 2004/09/03 20:36:23 perev Exp $
// $Log: StFtpcTracker.hh,v $
// Revision 1.21  2004/09/03 20:36:23  perev
// Big LeakOff + mem optimisation
//
// Revision 1.20  2004/02/12 19:37:11  oldi
// *** empty log message ***
//
// Revision 1.19  2003/09/16 16:52:51  jeromel
// Multiple constructor entry, zeroing mBench everywhere + doxygenized
//
// Revision 1.18  2003/09/16 15:27:02  jcs
// removed inline as it would leave a few undefined reference
//
// Revision 1.17  2003/05/20 18:35:02  oldi
// Cuts for vertex estimation introduced (globDca < 1 cm, multiplicity >= 200).
//
// Revision 1.16  2002/11/06 13:47:15  oldi
// Vertex handling simplifed.
// Global/primary fit handling simplified.
// Code clean ups.
//
// Revision 1.15  2002/10/31 13:41:54  oldi
// dE/dx parameters read from database, now.
// Vertex estimation for different sectors added.
// Vertex estimation for different areas (angle, radius) added.
//
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
