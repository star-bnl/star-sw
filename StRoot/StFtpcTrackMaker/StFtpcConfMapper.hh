// $Id: StFtpcConfMapper.hh,v 1.3 2000/06/07 11:03:53 oldi Exp $
// $Log: StFtpcConfMapper.hh,v $
// Revision 1.3  2000/06/07 11:03:53  oldi
// Added CreateTrack(StFtpcConfMapPoint *hit) to cleanup ClusterLoop().
// Added TrackLoop().
// Added ExtendTrack(StFtpcTrack *track).
// Added CalcChiSquared(StFtpcTrack *track, StFtpcConfMapPoint *point,
//       Double_t *chi2).
// Added MergeSplitTracks(StFtpcTrack *t1, StFtpcTrack *t2).
//
// Revision 1.2  2000/05/11 15:14:42  oldi
// Changed class names *Hit.* due to already existing class StFtpcHit.cxx in StEvent
//
// Revision 1.1  2000/05/10 13:39:11  oldi
// Initial version of StFtpcTrackMaker
//

//////////////////////////////////////////////////////////////////////////////////
//                                                                              //
// StFtpcConfMapper class - tracking algorithm with conformal mapping           //
//                                                                              //
//////////////////////////////////////////////////////////////////////////////////

#ifndef STAR_StFtpcConfMapper
#define STAR_StFtpcConfMapper

#include "StFtpcTracker.hh"
#include "StFtpcConfMapPoint.hh"
#include "StFtpcTrack.hh"
#include "TObjArray.h"
#include "TBenchmark.h"

class StFtpcConfMapper : public StFtpcTracker {

private:

  TBenchmark *mBench;             // benchmark object (just for run-time measurements)

  // Volume segemnts
  TObjArray *mVolume;             // array of volume (pad, phi, eta) elements
  TObjArray *mSegment;            // array of StFtpcConfMapPoints (all hits of/in one mVolume cell)

  // Number of cells (segments)
  Int_t  mNumRowSegment;  // this is (and has to be at most) the number of padrows
  Int_t  mNumPhiSegment;  // number of phi segments
  Int_t  mNumEtaSegment;  // number of eta segments
  Int_t  mBounds;         // array size
  Char_t mMaxFtpcRow;     // number of rows in one Ftpc
  Char_t mFtpc;           // current Ftpc number (either 1 ore 2)
  
  // Settings ([0]: non vertex tracks, [1]: main vertex tracks)
  Bool_t mVertexConstraint;     // vertex constraint (true or false)
   Int_t mTrackletLength[2];    // minimal length of tracks (up to here different search algorithm)
   Int_t mRowScopeTracklet[2];  // number of row segments to look for the next point of a tracklet
   Int_t mRowScopeTrack[2];     // number of row segments to look for the next point of a track
   Int_t mPhiScope[2];          // number of phi segments to look for the next point
   Int_t mEtaScope[2];          // number of eta segments to look for the next point
   Int_t mMinPoints[2];         // minimum number of points on one track

  // Cuts
  Double_t mMaxAngleTracklet[2];  // limit of angle between to pieces of a tracklet
  Double_t mMaxAngleTrack[2];     // limit of angle between to pieces of a track
  Double_t mMaxCircleDist[2];     // limit of angle phi between to subsequent clusters of one track
  Double_t mMaxLengthDist[2];     // limit of pseudorapidity eta between to subsequent clusters of one track
  
  // Tracking informtion
  Int_t mMainVertexTracks; // number of tracks coming from the main vertex
  Int_t mMergedTracks;     // cluster on a track missing, but tracker was able to merge the pieces
  Int_t mMergedTracklets;  // cluster on a tracklet missing, but tracker was able to merge the pieces
  Int_t mMergedSplits;     // number of merged split tracks
  Int_t mDiffHits;         // circle fit and length fit returned differnet hits
  Int_t mDiffHitsStill;    // different hits from fits could not be resolved
  Int_t mLengthFitNaN;     // argument of arcsin was higher than +1 (lower than -1)
  Int_t mClustersUnused;   // number of unused clusters

  // setter
  void SetTrackletLength(Int_t f)        { mTrackletLength[0] = mTrackletLength[1] = f; }      // sets tracklet lengths (both the same)
  void SetRowScopeTrack(Int_t f)         { mRowScopeTrack[0] = mRowScopeTrack[1] = f; }        // sets row scopes (both the same)
  void SetRowScopeTracklet(Int_t f)      { mRowScopeTracklet[0] = mRowScopeTracklet[1] = f; }  // sets row scopes (both the same)=  
  void SetPhiScope(Int_t f)              { mPhiScope[0] = mPhiScope[1] = f; }                  // sets phi scopes (both the same)
  void SetEtaScope(Int_t f)              { mEtaScope[0] = mEtaScope[1] = f; }                  // sets eta scopes (both the same)
  void SetMinPoints(Int_t f)             { mMinPoints[0] = mMinPoints[1] = f; }                // sets minimum number of points (both the same)
  void SetMaxAngleTrack(Double_t f)      { mMaxAngleTrack[0] = mMaxAngleTrack[1] = f; }        // sets angle cuts (both the same)
  void SetMaxAngleTracklet(Double_t f)   { mMaxAngleTracklet[0] = mMaxAngleTracklet[1] = f; }  // sets angle cuts (both the same)
  void SetMaxCircleDistTrack(Double_t f) { mMaxCircleDist[0] = mMaxCircleDist[1] = f; }        // sets circle cuts (both the same)
  void SetMaxLengthDistTrack(Double_t f) { mMaxLengthDist[0] = mMaxLengthDist[1] = f; }        // sets length cuts (both the same)

  void SetTrackletLength(Int_t f, Int_t g)           { mTrackletLength[1] = f; mTrackletLength[0] = g; }      // sets tracklet lengths (both but different)
  void SetRowScopeTrack(Int_t f, Int_t g)            { mRowScopeTrack[1] = f; mRowScopeTrack[0] = g; }        // sets row scopes for tracks(both but different)
  void SetRowScopeTracklet(Int_t f, Int_t g)         { mRowScopeTracklet[1] = f; mRowScopeTracklet[0] = g; }  // sets row scopes for tracklets(both but different)
  void SetPhiScope(Int_t f, Int_t g)                 { mPhiScope[1] = f; mPhiScope[0] = g; }                  // sets phi scopes (both but different)
  void SetEtaScope(Int_t f, Int_t g)                 { mEtaScope[1] = f; mEtaScope[0] = g; }                  // sets eta scopes (both but different)
  void SetMinPoints(Int_t f, Int_t g)                { mMinPoints[1] = f; mMinPoints[0] = g; }                // sets minimum number of points (both but different)
  void SetMaxAngleTrack(Double_t f, Double_t g)      { mMaxAngleTrack[1] = f; mMaxAngleTrack[0] = g; }        // sets angle cuts (both but different)
  void SetMaxAngleTracklet(Double_t f, Double_t g)   { mMaxAngleTracklet[1] = f; mMaxAngleTracklet[0] = g; }  // sets angle cuts (both but different)
  void SetMaxCircleDistTrack(Double_t f, Double_t g) { mMaxCircleDist[1] = f; mMaxCircleDist[0] = g; }        // sets circle cuts (both but different)
  void SetMaxLengthDistTrack(Double_t f, Double_t g) { mMaxLengthDist[1] = f; mMaxLengthDist[0] = g; }        // sets length cuts (both but different)

  void SetTrackletLength(Int_t f, Bool_t vertex_constraint)        { mTrackletLength[(Int_t)vertex_constraint] = f; }    // sets one tracklet length
  void SetRowScopeTrack(Int_t f, Bool_t vertex_constraint)         { mRowScopeTrack[(Int_t)vertex_constraint] = f; }     // sets one row scope for tracks
  void SetRowScopeTracklet(Int_t f, Bool_t vertex_constraint)      { mRowScopeTracklet[(Int_t)vertex_constraint] = f; }  // sets one row scope for tracklets
  void SetPhiScope(Int_t f, Bool_t vertex_constraint)              { mPhiScope[(Int_t)vertex_constraint] = f; }          // sets one phi scope
  void SetEtaScope(Int_t f, Bool_t vertex_constraint)              { mEtaScope[(Int_t)vertex_constraint] = f; }          // sets one eta scope
  void SetMinPoints(Int_t f, Bool_t vertex_constraint)             { mMinPoints[(Int_t)vertex_constraint] = f; }         // sets one minimum number of points
  void SetMaxAngleTrack(Double_t f, Bool_t vertex_constraint)      { mMaxAngleTrack[(Int_t)vertex_constraint] = f; }     // sets one angle cut
  void SetMaxAngleTracklet(Double_t f, Bool_t vertex_constraint)   { mMaxAngleTracklet[(Int_t)vertex_constraint] = f; }  // sets one angle cut
  void SetMaxCircleDistTrack(Double_t f, Bool_t vertex_constraint) { mMaxCircleDist[(Int_t)vertex_constraint] = f; }     // sets one circle cut
  void SetMaxLengthDistTrack(Double_t f, Bool_t vertex_constraint) { mMaxLengthDist[(Int_t)vertex_constraint] = f; }     // sets one length cut


public:
            StFtpcConfMapper();  // default constructor
            StFtpcConfMapper(St_fcl_fppoint *fcl_fppoint, 
			     Double_t vertexPos[3] = NULL,
			     Bool_t bench = (Bool_t)false,
			     Int_t phi_segments = 100, 
			     Int_t eta_segments = 200);  // constructor
  virtual  ~StFtpcConfMapper();  // destructor

  // setter
  void Settings(Int_t trackletlength1, Int_t trackletlength2, 
		Int_t tracklength1, Int_t tracklength2, 
		Int_t rowscopetracklet1, Int_t rowscopetracklet2,
		Int_t rowscopetrack1, Int_t rowscopetrack2,
		Int_t phiscope1, Int_t phiscope2, 
		Int_t etascope1, Int_t etascope2);                                        // sets all settings of tracker
  void Settings(Int_t trackletlength, Int_t tracklength, Int_t rowscopetracklet, 
		Int_t rowscopetrack, Int_t phiscope, Int_t etascope);                     // sets settings for vertex constraint on or off
  void MainVertexSettings(Int_t trackletlength, Int_t tracklength, Int_t rowscopetracklet, 
			  Int_t rowscopetrack, Int_t phiscope, Int_t etascope);           //  sets settings for vertex constraint on   
  void NonVertexSettings(Int_t trackletlength, Int_t tracklength, Int_t rowscopetracklet, 
			 Int_t rowscopetrack, Int_t phiscope, Int_t etascope);            //  sets settings for vertex constraint off 

  void SetCuts(Double_t maxangletracklet1, Double_t maxangletracklet2, Double_t maxangletrack1, Double_t maxangletrack2, 
	       Double_t maxcircletrack1, Double_t maxcircletrack2, Double_t maxlengthtrack1, Double_t maxlengthtrack2);  // sets all cuts
  void SetCuts(Double_t maxangletracklet, Double_t maxangletrack, Double_t maxcircletrack, Double_t maxlengthtrack);     // sets cuts for vertex constraint on or off
  void SetTrackCuts(Double_t maxangle, Double_t maxcircle, Double_t maxlength, Bool_t vertex_constraint);                // sets cuts of tracks for the given vertex_constraint
  void SetTrackletCuts(Double_t maxangle, Bool_t vertex_constraint);                                                     // sets cut of tracklet for the given vertex_constraint
  void SetVertexConstraint(Bool_t f) { mVertexConstraint = f; }                                                          // sets vertex constraint (on or off)

  // getter
     Int_t  GetNumMainVertexTracks() { return mMainVertexTracks; }  // returns number of main vertex tracks
     Int_t  GetNumMergedTracks()     { return mMergedTracks;     }  // returns number of merged tracks
     Int_t  GetNumMergedTracklets()  { return mMergedTracklets;  }  // returns number of merged tracklets
     Int_t  GetNumMergedSplits()     { return mMergedSplits;     }  // returns number of merged split tracks
     Int_t  GetNumDiffHits()         { return mDiffHits;         }  // returns number of cases where length and circle fit are different
     Int_t  GetNumLengthFitNaN()     { return mLengthFitNaN;     }  // retruns number of settings of argumnet of arcsin to +/-1
     Int_t  GetNumClustersUnused()   { return mClustersUnused;   }  // returns number of unused clusters

     Int_t  GetRowSegm(StFtpcConfMapPoint *hit);                      // returns number of pad segment of a specific hit
     Int_t  GetPhiSegm(StFtpcConfMapPoint *hit);                      // returns number of phi segment of a specific hit
     Int_t  GetEtaSegm(StFtpcConfMapPoint *hit);                      // returns number of eta segment of a specific hit
     Int_t  GetRowSegm(Int_t segm);                                   // returns number of pad segment of a specific segment
     Int_t  GetPhiSegm(Int_t segm);                                   // returns number of phi segment of a specific segment
     Int_t  GetEtaSegm(Int_t segm);                                   // returns number of eta segment of a specific segment
     Int_t  GetSegm(Int_t row_segm, Int_t phi_segm, Int_t eta_segm);  // returns number of segment

  Double_t const  GetPhiDiff(const StFtpcConfMapPoint *hit1, const StFtpcConfMapPoint *hit2);          //returns normalized difference of phi
  Double_t const  GetEtaDiff(const StFtpcConfMapPoint *hit1, const StFtpcConfMapPoint *hit2);          //returns normalized difference of eta
  Double_t const  GetClusterDistance(const StFtpcConfMapPoint *hit1, const StFtpcConfMapPoint *hit2);  // returns distance between to clusters
  Double_t const  GetDistanceFromFit(const StFtpcConfMapPoint *hit);                                   // returns distance of cluster from fit

  // Tracking procedures
              void  ClusterLoop();                                                                           // loops over clusters
              void  CreateTrack(StFtpcConfMapPoint *hit);                                                    // create track with start at hit
StFtpcConfMapPoint *GetNextNeighbor(StFtpcConfMapPoint *start_hit, Double_t *coeff);                         // returns next cluster to start cluster
              void  TrackLoop();                                                                             // Loops over found tracks. Trys to extend them.
            Bool_t  ExtendTrack(StFtpcTrack *track);                                                         // Trys to extend given track.
    Double_t const  TrackAngle(const StFtpcPoint *lasthitoftrack, const StFtpcPoint *hit);                   // returns angle
    Double_t const  TrackletAngle(StFtpcTrack *track, Int_t n = 3);                                          // returns angle
    Double_t const  CalcDistance(const StFtpcConfMapPoint *hit1, const StFtpcConfMapPoint *hit2);            // returns distance of two hits
    Double_t const  CalcDistance(const StFtpcConfMapPoint *hit, Double_t *coeff);                            // returns distance between a hit and straight line
              void  StraightLineFit(StFtpcTrack *track, Double_t *a, Int_t n = 0);                           // calculates a straight line fit for given clusters 
              void  CalcChiSquared(StFtpcTrack *track, StFtpcConfMapPoint *point, Double_t *chi2);           // calculates chi squared for cirlce and length fit
      Bool_t const  VerifyCuts(const StFtpcConfMapPoint *lasttrackhithit, const StFtpcConfMapPoint *newhit); // returns true if phi and eta cut holds
              void  HandleSplitTracks(Double_t max_dist, Double_t ratio_min, Double_t ratio_max);            // loops over tracks and looks for split tracks
              void  MergeSplitTracks(StFtpcTrack *t1, StFtpcTrack *t2);                                      // merges two tracks

  // Start tracking
  void MainVertexTracking(); // tracking of main vertex tracks (vertex constraint on)
  void FreeTracking();       // tracking without vertex constraint 
  void TwoCycleTracking();   // tracking: 1st cylce: with vertex constraint, 2nd cycle: without vertex constraint
  

  // Information
  void  Cout(Int_t width, Int_t figure);     // cout routine with adjustable width for ints
  void  Cout(Int_t width, Double_t figure);  // cout routine with adjustable width for doubles
  void  SettingInfo();  // displays settings
  void  CutInfo();      // displays cust
  void  TrackingInfo(); // displays information about the tracking process (e.g. difficulties during the tracking)

  ClassDef(StFtpcConfMapper, 1)  // Ftpc conformal mapper class
};

#endif
