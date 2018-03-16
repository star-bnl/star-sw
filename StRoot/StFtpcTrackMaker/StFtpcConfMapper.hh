// $Id: StFtpcConfMapper.hh,v 1.23 2018/03/15 22:00:35 smirnovd Exp $
// $Log: StFtpcConfMapper.hh,v $
// Revision 1.23  2018/03/15 22:00:35  smirnovd
// Fix linker error by removing declared but undefined functions
//
// Revision 1.22  2010/06/02 12:12:01  jcs
// undo correction for Bug#1939 since it was incorrect
//
// Revision 1.21  2010/05/19 14:55:29  jcs
// Correction for Bug #1939 - variables with the same name were defined twice;
// once in StFtpcTrackingParams.hh and again in StFtpcConfMapper.hh
// The names of the StFtpcConfMapper variables have been changed to make them unique
//
// Revision 1.20  2007/01/15 08:23:01  jcs
// replace printf, cout and gMesMgr with Logger commands
//
// Revision 1.19  2004/02/12 19:37:10  oldi
// *** empty log message ***
//
// Revision 1.18  2003/09/16 15:27:01  jcs
// removed inline as it would leave a few undefined reference
//
// Revision 1.17  2003/01/20 13:16:23  oldi
// Additional volume segment added as garbage container. Hits which give a
// segment index which is out of range (esp. those ones sitting exactly on the
// beam line) are put in here.
// Handling of function GetSegm() simplified.
//
// Revision 1.16  2002/11/06 13:44:53  oldi
// Vertex handling simplifed.
// Flag for clusters not to be used for tracking introduced.
// Code clean ups.
//
// Revision 1.15  2002/10/11 15:45:03  oldi
// Get FTPC geometry and dimensions from database.
// No field fit activated: Returns momentum = 0 but fits a helix.
// Bug in TrackMaker fixed (events with z_vertex > outer_ftpc_radius were cut).
// QA histograms corrected (0 was supressed).
// Code cleanup (several lines of code changed due to *params -> Instance()).
// cout -> gMessMgr.
//
// Revision 1.14  2002/09/04 13:44:22  oldi
// Typos fixed.
//
// Revision 1.13  2002/04/29 15:49:50  oldi
// All tracking parameters moved to StFtpcTrackingParameters.cc/hh.
// In a future version the actual values should be moved to an .idl file (the
// interface should be kept as is in StFtpcTrackingParameters.cc/hh).
//
// Revision 1.12  2002/04/05 16:50:11  oldi
// Cleanup of MomentumFit (StFtpcMomentumFit is now part of StFtpcTrack).
// Each Track inherits from StHelix, now.
// Therefore it is possible to calculate, now:
//  - residuals
//  - vertex estimations obtained by back extrapolations of FTPC tracks
// Chi2 was fixed.
// Many additional minor (and major) changes.
//
// Revision 1.11  2002/03/15 10:04:41  oldi
// Adjust eta segments not only to z-position of vertex but to x,y as well.
// Avoid tracking if vertex position is outside of the inner radius of the Ftpc.
//
// Revision 1.10  2001/11/21 12:31:15  jcs
// adjust mEtaMax for extended drift maps
//
// Revision 1.9  2001/07/12 08:29:09  oldi
// New constructor introduced to be able to use only cluster which were
// found to be good in a previous run.
// New function NoFieldTracking() introduced.
// Tracking parameters for LaserTracking() changed.
//
// Revision 1.8  2001/01/25 15:21:45  oldi
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
// Revision 1.7  2000/11/10 18:37:02  oldi
// New functions introduced to be able to extend tracks after main vertex tracking.
// This implied many changes in other parts of the class (loop over clusters can run backwards now).
// TBenchmark object ('mBench') moved to StFtpcTracker.
// Cleanup.
//
// Revision 1.6  2000/08/02 10:10:44  oldi
// Changes in function GetSegm() to avoid pointing out of the ObjArry.
// If GetSegm() tries to use a segment out of bounds a warning will be print
// and the last segment will be returned.
// New functions added to be able to calculate the coordinates of a given
// volume segment (used in the warning message above).
//
// Revision 1.5  2000/07/18 21:22:15  oldi
// Changes due to be able to find laser tracks.
// Cleanup: - new functions in StFtpcConfMapper, StFtpcTrack, and StFtpcPoint
//            to bundle often called functions
//          - short functions inlined
//          - formulas of StFormulary made static
//          - avoid streaming of objects of unknown size
//            (removes the bunch of CINT warnings during compile time)
//          - two or three minor bugs cured
//
// Revision 1.4  2000/06/13 14:33:10  oldi
// Added three new funtions (SetEtaMin(), SetEtaMax(), CalcEtaMinMax()) and
// two data members (mEtaMin, mEtaMax) to be able to calculate the
// min./max. possible value of the pseudorapidity eta with the z-position of
// the found main vertex. This solves bug #574 where the min./max. values of eta
// were fixed and could be to small if the found main vertex was to far off from
// z=0.
//
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

#include "StFtpcTrackingParams.hh"
#include "StFtpcTracker.hh"
#include "StFtpcConfMapPoint.hh"
#include "StFtpcTrack.hh"
#include "StMessMgr.h"

#include "TObjArray.h"
#include "TBenchmark.h"

class StFtpcConfMapper : public StFtpcTracker {

private:

      Bool_t  mLaser;             // indicator to know if this is a laser event

  // Volume segemnts
  TObjArray *mVolume;             // array of volume (pad, phi, eta) elements

  // Number of cells (segments)
  Int_t  mNumRowSegment;  // this is (and has to be at most) the number of padrows
  Int_t  mNumPhiSegment;  // number of phi segments
  Int_t  mNumEtaSegment;  // number of eta segments
  Int_t  mBounds;         // array size
  Int_t  mMaxFtpcRow;     // number of rows in one Ftpc
  Char_t mFtpc;           // current Ftpc number (either 1 ore 2)

  // max. eta (pseudorapidity) values
  Double_t mEtaMin;
  Double_t mEtaMax;
  
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
  Double_t mMaxCircleDist[2];     // limit of distance of a new cluster to the circle fit of a track
  Double_t mMaxLengthDist[2];     // limit of distance of a new cluster to the length fit of a track
  
  // Tracking informtion
  Int_t mMainVertexTracks; // number of tracks coming from the main vertex
  Int_t mMergedTracks;     // cluster on a track missing, but tracker was able to merge the pieces
  Int_t mMergedTracklets;  // cluster on a tracklet missing, but tracker was able to merge the pieces
  Int_t mMergedSplits;     // number of merged split tracks
  Int_t mExtendedTracks;   // number of extended tracks
  Int_t mDiffHits;         // circle fit and length fit returned differnet hits
  Int_t mDiffHitsStill;    // different hits from fits could not be resolved
  Int_t mLengthFitNaN;     // argument of arcsin was higher than +1 (lower than -1)
  Int_t mClustersUnused;   // number of unused clusters
  Int_t mBadClusters;      // number of clusters noy to be used for tracking ('bad' flag set)

  // setter
  void SetEtaMin(Double_t f) { mEtaMin = f; }  // sets min. value for eta
  void SetEtaMax(Double_t f) { mEtaMax = f; }  // sets max. value for eta
  void CalcEtaMinMax();                        // calculates the min. and max. value for eta

  void SetTrackletLength(Int_t f)        { mTrackletLength[0] = mTrackletLength[1] = f;     }  // sets tracklet lengths (both the same)
  void SetRowScopeTrack(Int_t f)         { mRowScopeTrack[0] = mRowScopeTrack[1] = f;       }  // sets row scopes (both the same)
  void SetRowScopeTracklet(Int_t f)      { mRowScopeTracklet[0] = mRowScopeTracklet[1] = f; }  // sets row scopes (both the same)=  
  void SetPhiScope(Int_t f)              { mPhiScope[0] = mPhiScope[1] = f;                 }  // sets phi scopes (both the same)
  void SetEtaScope(Int_t f)              { mEtaScope[0] = mEtaScope[1] = f;                 }  // sets eta scopes (both the same)
  void SetMinPoints(Int_t f)             { mMinPoints[0] = mMinPoints[1] = f;               }  // sets minimum number of points (both the same)
  void SetMaxAngleTrack(Double_t f)      { mMaxAngleTrack[0] = mMaxAngleTrack[1] = f;       }  // sets angle cuts (both the same)
  void SetMaxAngleTracklet(Double_t f)   { mMaxAngleTracklet[0] = mMaxAngleTracklet[1] = f; }  // sets angle cuts (both the same)
  void SetMaxCircleDistTrack(Double_t f) { mMaxCircleDist[0] = mMaxCircleDist[1] = f;       }  // sets circle cuts (both the same)
  void SetMaxLengthDistTrack(Double_t f) { mMaxLengthDist[0] = mMaxLengthDist[1] = f;       }  // sets length cuts (both the same)

  void SetTrackletLength(Int_t f, Int_t g)           { mTrackletLength[1] = f; mTrackletLength[0] = g;     }  // sets tracklet lengths (both but different)
  void SetRowScopeTrack(Int_t f, Int_t g)            { mRowScopeTrack[1] = f; mRowScopeTrack[0] = g;       }  // sets row scopes for tracks(both but different)
  void SetRowScopeTracklet(Int_t f, Int_t g)         { mRowScopeTracklet[1] = f; mRowScopeTracklet[0] = g; }  // sets row scopes for tracklets(both but different)
  void SetPhiScope(Int_t f, Int_t g)                 { mPhiScope[1] = f; mPhiScope[0] = g;                 }  // sets phi scopes (both but different)
  void SetEtaScope(Int_t f, Int_t g)                 { mEtaScope[1] = f; mEtaScope[0] = g;                 }  // sets eta scopes (both but different)
  void SetMinPoints(Int_t f, Int_t g)                { mMinPoints[1] = f; mMinPoints[0] = g;               }  // sets minimum number of points (both but different)
  void SetMaxAngleTrack(Double_t f, Double_t g)      { mMaxAngleTrack[1] = f; mMaxAngleTrack[0] = g;       }  // sets angle cuts (both but different)
  void SetMaxAngleTracklet(Double_t f, Double_t g)   { mMaxAngleTracklet[1] = f; mMaxAngleTracklet[0] = g; }  // sets angle cuts (both but different)
  void SetMaxCircleDistTrack(Double_t f, Double_t g) { mMaxCircleDist[1] = f; mMaxCircleDist[0] = g;       }  // sets circle cuts (both but different)
  void SetMaxLengthDistTrack(Double_t f, Double_t g) { mMaxLengthDist[1] = f; mMaxLengthDist[0] = g;       }  // sets length cuts (both but different)

  void SetTrackletLength(Int_t f, Bool_t vertex_constraint)        { mTrackletLength[(Int_t)vertex_constraint] = f;   }  // sets one tracklet length
  void SetRowScopeTrack(Int_t f, Bool_t vertex_constraint)         { mRowScopeTrack[(Int_t)vertex_constraint] = f;    }  // sets one row scope for tracks
  void SetRowScopeTracklet(Int_t f, Bool_t vertex_constraint)      { mRowScopeTracklet[(Int_t)vertex_constraint] = f; }  // sets one row scope for tracklets
  void SetPhiScope(Int_t f, Bool_t vertex_constraint)              { mPhiScope[(Int_t)vertex_constraint] = f;         }  // sets one phi scope
  void SetEtaScope(Int_t f, Bool_t vertex_constraint)              { mEtaScope[(Int_t)vertex_constraint] = f;         }  // sets one eta scope
  void SetMinPoints(Int_t f, Bool_t vertex_constraint)             { mMinPoints[(Int_t)vertex_constraint] = f;        }  // sets one minimum number of points
  void SetMaxAngleTrack(Double_t f, Bool_t vertex_constraint)      { mMaxAngleTrack[(Int_t)vertex_constraint] = f;    }  // sets one angle cut
  void SetMaxAngleTracklet(Double_t f, Bool_t vertex_constraint)   { mMaxAngleTracklet[(Int_t)vertex_constraint] = f; }  // sets one angle cut
  void SetMaxCircleDistTrack(Double_t f, Bool_t vertex_constraint) { mMaxCircleDist[(Int_t)vertex_constraint] = f;    }  // sets one circle cut
  void SetMaxLengthDistTrack(Double_t f, Bool_t vertex_constraint) { mMaxLengthDist[(Int_t)vertex_constraint] = f;    }  // sets one length cut


public:
            StFtpcConfMapper();  // default constructor
            StFtpcConfMapper(TObjArray *inputHits,
			     MIntArray *good_hits,
			     StFtpcVertex *vertex = 0,
			     Bool_t bench = (Bool_t)kFALSE,
			     Int_t phi_segments = 100, 
			     Int_t eta_segments = 200);  // constructor to fill evaluated hits
            StFtpcConfMapper(TObjArray *hits, 
			     StFtpcVertex *vertex = 0, 
			     Bool_t bench = (Bool_t)kFALSE, 
			     Int_t phi_segments = 100, 
			     Int_t eta_segments = 200);  // constructor which takes an CLonesArray of hits
  virtual  ~StFtpcConfMapper();  // destructor

  // setter
  void SetLaser(Bool_t f)    { mLaser = f;  }                                             // set laser flag

  void Settings(TString method);                                                          // set settings by tracking procedure
  void Settings(Int_t method_id);                                                         // set settings by tracking id
  void Settings(Int_t trackletlength1, Int_t trackletlength2, 
		Int_t tracklength1, Int_t tracklength2, 
		Int_t rowscopetracklet1, Int_t rowscopetracklet2,
		Int_t rowscopetrack1, Int_t rowscopetrack2,
		Int_t phiscope1, Int_t phiscope2, 
		Int_t etascope1, Int_t etascope2);                                        // sets all settings of tracker

  void Cuts(TString method);                                                              // set cuts by tracking procedure
  void Cuts(Int_t method_id);                                                             // set cuts by tracking id
  void Cuts(Double_t maxangletracklet1, Double_t maxangletracklet2, Double_t maxangletrack1, Double_t maxangletrack2, 
	    Double_t maxcircletrack1, Double_t maxcircletrack2, Double_t maxlengthtrack1, Double_t maxlengthtrack2);  // sets all cuts

  void SetVertexConstraint(Bool_t f) { mVertexConstraint = f; }                                                       // sets vertex constraint (on or off)

  // getter
  Bool_t    GetLaser()               { return mLaser;            }  // returns laser flag
  
  Double_t  GetEtaMin()              { return mEtaMin;           }  // returns min. value of eta
  Double_t  GetEtaMax()              { return mEtaMax;           }  // returns max. value of eta

     Int_t  GetNumMainVertexTracks() { return mMainVertexTracks; }  // returns number of main vertex tracks
     Int_t  GetNumMergedTracks()     { return mMergedTracks;     }  // returns number of merged tracks
     Int_t  GetNumMergedTracklets()  { return mMergedTracklets;  }  // returns number of merged tracklets
     Int_t  GetNumMergedSplits()     { return mMergedSplits;     }  // returns number of merged split tracks
     Int_t  GetNumExtendedTracks()   { return mExtendedTracks;   }  // returns number of extended tracks
     Int_t  GetNumDiffHits()         { return mDiffHits;         }  // returns number of cases where length and circle fit are different
     Int_t  GetNumLengthFitNaN()     { return mLengthFitNaN;     }  // retruns number of settings of argumnet of arcsin to +/-1
     Int_t  GetNumClustersUnused()   { return mClustersUnused;   }  // returns number of unused clusters
     Int_t  GetNumBadClusters()      { return mBadClusters;      }  // returns number of bad clusters

     Int_t  GetRowSegm(StFtpcConfMapPoint *hit);                      // returns number of pad segment of a specific hit
     Int_t  GetPhiSegm(StFtpcConfMapPoint *hit);                      // returns number of phi segment of a specific hit
     Int_t  GetEtaSegm(StFtpcConfMapPoint *hit);                      // returns number of eta segment of a specific hit
     Int_t  GetSegm(StFtpcConfMapPoint *hit);                         // returns number of segment of a specific hit
     Int_t  GetRowSegm(Int_t segm);                                   // returns number of pad segment of a specific segment
     Int_t  GetPhiSegm(Int_t segm);                                   // returns number of phi segment of a specific segment
     Int_t  GetEtaSegm(Int_t segm);                                   // returns number of eta segment of a specific segment
     Int_t  GetSegm(Int_t row_segm, Int_t phi_segm, Int_t eta_segm);  // returns number of segment
     Int_t  GetRow(Int_t segm);                                       // returns the row of a given row segment
  Double_t  GetPhi(Int_t segm);                                       // returns phi of a given phi segment
  Double_t  GetEta(Int_t segm);                                       // returns eta of a given eta segment

  Double_t const  GetPhiDiff(const StFtpcConfMapPoint *hit1, const StFtpcConfMapPoint *hit2);          //returns normalized difference of phi
  Double_t const  GetEtaDiff(const StFtpcConfMapPoint *hit1, const StFtpcConfMapPoint *hit2);          //returns normalized difference of eta
  Double_t const  GetClusterDistance(const StFtpcConfMapPoint *hit1, const StFtpcConfMapPoint *hit2);  // returns distance between to clusters
  Double_t const  GetDistanceFromFit(const StFtpcConfMapPoint *hit);                                   // returns distance of cluster from fit

  // Tracking procedures
              void  ClusterLoop();                                                                               // loops over clusters
              void  CreateTrack(StFtpcConfMapPoint *hit);                                                        // create track with start at hit
              void  RemoveTrack(StFtpcTrack *track);                                                             // removes track from track array
              void  CompleteTrack(StFtpcTrack *track);                                                           // completes track
StFtpcConfMapPoint *GetNextNeighbor(StFtpcConfMapPoint *start_hit, Double_t *coeff, Bool_t backward);            // returns next cluster to start cluster
              void  ExtendTracks();                                                                              // Loops over found tracks. Trys to extend them.
            Bool_t  TrackExtension(StFtpcTrack *track);                                                          // Trys to extend given track.
      Bool_t const  TestExpression(Int_t sub_row_segm, Int_t end_row, Bool_t backward = (Bool_t)kTRUE);          // increments or decrements *sub_row_segm
              void  LoopUpdate(Int_t *sub_row_segm, Bool_t backward);                                            // tests if loop should be continued
    Double_t const  TrackAngle(const StFtpcPoint *lasthitoftrack, const StFtpcPoint *hit, Bool_t backward);      // returns angle
    Double_t const  TrackletAngle(StFtpcTrack *track, Int_t n = 0);                                              // returns angle
    Double_t const  CalcDistance(const StFtpcConfMapPoint *hit1, const StFtpcConfMapPoint *hit2, Bool_t laser);  // returns distance of two hits
    Double_t const  CalcDistance(const StFtpcConfMapPoint *hit, Double_t *coeff);                                // returns distance between a hit and straight line
              void  StraightLineFit(StFtpcTrack *track, Double_t *a, Int_t n = 0);                               // calculates a straight line fit for given clusters 
              void  CalcChiSquared(StFtpcTrack *track, StFtpcConfMapPoint *point, Double_t *chi2);               // calculates chi squared for cirlce and length fit
      Bool_t const  VerifyCuts(const StFtpcConfMapPoint *lasttrackhithit, 
			       const StFtpcConfMapPoint *newhit, Bool_t backward = (Bool_t)kTRUE);               // returns true if phi and eta cut holds
              void  HandleSplitTracks(Double_t max_dist, Double_t ratio_min, Double_t ratio_max);                // loops over tracks and looks for split tracks
              void  HandleSplitTracks();                                                                         // HandleSplitTracks() with default values
              void  MergeSplitTracks(StFtpcTrack *t1, StFtpcTrack *t2);                                          // merges two tracks
              void  AdjustTrackNumbers(Int_t first_split = 0);                                                                        // renews tracknumbers

  // Start tracking
  void MainVertexTracking(); // tracking of main vertex tracks (vertex constraint on)
  void FreeTracking();       // tracking without vertex constraint 
  void NonVertexTracking();  // same as FreeTracking()
  void TwoCycleTracking();   // tracking: 1st cylce: with vertex constraint, 2nd cycle: without vertex constraint
  void LaserTracking();      // tracks straight tracks (settings optimized for laser data)
  void NoFieldTracking();    // tracks straight tracks originating from main vertex

  // Information
  void  SettingInfo();  // displays settings
  void  CutInfo();      // displays cuts
  void  TrackingInfo(); // displays information about the tracking process (e.g. difficulties during the tracking)

  ClassDef(StFtpcConfMapper, 1)  // Ftpc conformal mapper class
};


#endif
