// $Id: StFtpcTrackEvaluator.hh,v 1.12 2004/02/12 19:37:11 oldi Exp $
// $Log: StFtpcTrackEvaluator.hh,v $
// Revision 1.12  2004/02/12 19:37:11  oldi
// *** empty log message ***
//
// Revision 1.11  2003/09/16 15:27:02  jcs
// removed inline as it would leave a few undefined reference
//
// Revision 1.10  2003/01/16 18:04:34  oldi
// Bugs eliminated. Now it compiles on Solaris again.
// Split residuals for global and primary fit.
//
// Revision 1.9  2002/11/06 13:46:29  oldi
// Code clean ups.
//
// Revision 1.8  2002/10/11 15:45:26  oldi
// Get FTPC geometry and dimensions from database.
// No field fit activated: Returns momentum = 0 but fits a helix.
// Bug in TrackMaker fixed (events with z_vertex > outer_ftpc_radius were cut).
// QA histograms corrected (0 was supressed).
// Code cleanup (several lines of code changed due to *params -> Instance()).
// cout -> gMessMgr.
//
// Revision 1.7  2001/07/12 08:34:31  oldi
// Many new things were developed eg. histograms for time consumption and
// momentum resolution.
// File handling was debugged.
// New constructor to evaluate tracks of good evaluated clusters of a
// previous run.
//
// Revision 1.6  2001/01/25 15:22:19  oldi
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
// Revision 1.5  2000/11/29 08:37:46  jcs
// remove obsolete include statement
//
// Revision 1.4  2000/11/10 18:38:24  oldi
// Short tracks are treated now.
//
// Revision 1.3  2000/07/18 21:22:17  oldi
// Changes due to be able to find laser tracks.
// Cleanup: - new functions in StFtpcConfMapper, StFtpcTrack, and StFtpcPoint
//            to bundle often called functions
//          - short functions inlined
//          - formulas of StFormulary made static
//          - avoid streaming of objects of unknown size
//            (removes the bunch of CINT warnings during compile time)
//          - two or three minor bugs cured
//
// Revision 1.2  2000/06/07 11:58:57  oldi
// Added new histos and Int_t's to count things. :)
// Cleanup.
//
// Revision 1.1  2000/05/10 13:39:27  oldi
// Initial version of StFtpcTrackMaker
//

////////////////////////////////////////////////////////////////////////////////////
//                                                                                //
// StFtpcTrackEvaluator class - evaluates found tracks by comparison to the input //
//                              GEANT clusters and tracks.                        //
//                                                                                //
////////////////////////////////////////////////////////////////////////////////////

#ifndef STAR_StFtpcTrackEvaluator
#define STAR_StFtpcTrackEvaluator

#include "TObject.h"
#include "TObjArray.h"
#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "TProfile2D.h"
#include "MIntArray.h"
#include "TFile.h"

#include "StFtpcVertex.hh"
#include "StFtpcTrack.hh"
#include "StFtpcTracker.hh"
#include "StFtpcDisplay.hh"

#include "tables/St_ffs_gepoint_Table.h"
#include "tables/St_g2t_track_Table.h"
#include "tables/St_g2t_ftp_hit_Table.h"

#include "St_DataSet.h"

class StFtpcTrackEvaluator : public TObject {

private:
                  Bool_t  mObjArraysCreated;        // flag to know which destructor to be called

               TObjArray *mGeantHits;               // ObjArray of geant hits
               TObjArray *mFastSimHits;             // ObjArray of fast simulated hits
               TObjArray *mFoundHits;               // ObjArray of found hits
               TObjArray *mGeantTracks;             // ObjArray of geant tracks
               TObjArray *mFoundTracks;             // ObjArray of found tracks

               MIntArray *mFtpcTrackNum;            // array of numbers of found Ftpc tracks, [geant track]

                   Int_t  mFoundVertexTracks;       // number of found main vertex tracks
                   Int_t  mFoundNonVertexTracks;    // number of found non main vertex tracks

                   Int_t  mGoodGeantPoints;         // number of points on good geant tracks
                   Int_t  mGoodFoundPoints;         // number of points on good found tracks

                   Int_t  mLookLikeGoodTracks;      // number of tracks which look like good tracks but aren't  
                   Int_t  mElectronTracks;          // number of electrons
                   Int_t  mNonVertexTracks;         // number of non main vertex tracks
                   Int_t  mGoodGTracks;             // number of good geant tracks (all tracks - - short tracks - electrons - non vertex tracks)
                   Int_t  mGoodFTracks;             // number of good found tracks (all tracks - - short tracks - electrons - non vertex tracks)  
                   Int_t  mSplitTracks;             // number of split tracks
                   Int_t  mSplitGoodTracks;         // number of split good tracks 
                   Int_t  mSplitLoliGoodTracks;     // number of split look-like good tracks 
                   Int_t  mUncleanTracks;           // number of tracks which have picked up wrong clusters
                   Int_t  mUncleanGoodTracks;       // number of tracks which have picked up wrong clusters and have a good parent track
                   Int_t  mLongTracks;              // number of tracks with more than 10 points
                   Int_t  mLongTrackClusters;       // number of clusters on long tracks
                   Int_t  mShortTracks;             // number of tracks with less than 5 points
                   Int_t  mShortTrackClusters;      // number of clusters on short tracks
                   Int_t  mTooShortTracks;          // number of tracks which should be longer
                   Int_t  mMaxClusters;             // Max. number of clusters on a track

            StFtpcVertex *mVertex;                  // pointer ro the main vertex

                    TH1F *mNumGeantHits;            // number of geant hits per event
                    TH1F *mNumFoundHits;            // number of found hits per event
                    TH1F *mNumGeantTracks;          // number of geant tracks per event
                    TH1F *mNumFoundTracks;          // number of found tracks per event

                    TH1F *mNumFoundVertexTracks;    // number of found main vertex tracks per event
                    TH1F *mNumFoundNonVertexTracks; // number of found non main vertex tracks per event

                    TH1F *mNumElectronTracks;       // number of electrons per event
                    TH1F *mNumNonVertexTracks;      // number of non main vertex tracks per event
                    TH1F *mNumLookLikeGoodTracks;   // number of tracks which look good but aren't
                    TH1F *mNumGoodGTracks;          // number of good geant tracks per event
                    TH1F *mNumGoodFTracks;          // number of good found tracks per event
                    TH1F *mNumSplitTracks;          // number of split tracks per event
                    TH1F *mNumSplitGoodTracks;      // number of split good tracks per event                   
                    TH1F *mNumSplitLoliGoodTracks;  // number of split good tracks per event                   

                    TH1F *mNumUncleanTracks;        // number of unclean tracks per event
                    TH1F *mNumUncleanGoodTracks;    // number of unclean tracks which have a good parent track per event 
                    TH1F *mNumLongTracks;           // number of tracks with more than 10 points per event
                    TH1F *mNumLongTrackClusters;    // number of hits belonging to long tracks per event
                    TH1F *mNumShortTracks;          // number of tracks with less than 5 points per event
                    TH1F *mNumShortTrackClusters;   // number of hits belonging to short tracks per event
                    TH1F *mNumTooShortTracks;       // number of tracks which should be longer

                    TH1F *mGHitsOnTrack;            // number of geant hits per track
                    TH1F *mFHitsOnTrack;            // number of found hits per track
                    
                    TH1F *mNumParents;              // number of parent tracks per track
                    TH1F *mNumWrongHitsAll;         // number of wrong clusters per track
                    TH2F *mNumWrongHits;            // number of wrong clusters per track

                    TH1F *mGoodRatio;               // number of good found tracks divided by the number of good geant tracks
                    TH1F *mContamination;           // number of found tracks looking good divided by the number of found main vertex tracks
                    TH1F *mContaWoSplit;            // number of found tracks looking good minus split tracks divided by the number of found main vertex tracks

                    TH1F *mNumGoodGeantPoints;      // number of points on good geant tracks
		    TH1F *mNumGoodFoundPoints;      // number of points on good found tracks
		    TH1F *mGoodPointRatio;          // ration of points on good geant tracks to points on good found tracks

                    TH2F *mPtot;                    // found total momentum vs. geant total momentum
                    TH2F *mPt;                      // found transverse momentum vs. geant transverse momentum
                    TH2F *mPx;                      // found x momentum vs. geant x momentum
                    TH2F *mPy;                      // found y momentum vs. geant y momentum
                    TH2F *mPz;                      // found z momentum vs. geant z momentum

                    TH1F *mPtotDiff;                // relative difference of total momentum
                    TH1F *mPtDiff;                  // relative difference of transverse momentum
                    TH1F *mPxDiff;                  // relative difference of momentum in x direction
                    TH1F *mPyDiff;                  // relative difference of momentum in y direction
                    TH1F *mPzDiff;                  // relative difference of momentum in z direction

                    TH1F *mPtotAcc;                 // relative accuracy of total momentum
                    TH1F *mPtAcc;                   // relative accuracy of transverse momentum
                    TH1F *mPxAcc;                   // relative accuracy of momentum in x direction
                    TH1F *mPyAcc;                   // relative accuracy of momentum in y direction
                    TH1F *mPzAcc;                   // relative accuracy of momentum in z direction

                    TH3F *mPRelErr;                 // relative error of momentum in pt/eta bins
                    TH3F *mPRelErrqok;              // relative error of momentum in pt/eta bins where the charge sign was measured alright
                    TH3F *mPRelDiff;                // relative difference of momentum in pt/eta bins

                    TH2F *mEtaNghits;               // pseudorapidity vs. number of geant clusters on track
                    TH2F *mEtaNfhits;               // pseudorapidity vs. number of found clusters on track

                    TH2F *mPtEtaF;                  // transverse momentum vs. pseudorapidity of found tracks
                    TH2F *mPtEtaFMes;               // measured transverse momentum vs. pseudorapidity of found tracks
                    TH2F *mPtEtaGood;               // transverse momentum vs. pseudorapidity of good found tracks divided by all found tracks
                    TH2F *mPtEtaBad;                // transverse momentum vs. pseudorapidity of bad found tracks divided by all found tracks
                    TH2F *mPtEtaUnclean;            // transverse momentum vs. pseudorapidity of unclean (found) tracks
                    TH2F *mPtEtaMesUnclean;         // measured transverse momentum vs. measured pseudorapidity of unclean (found) tracks
                    TH2F *mPtEtaUncleanGood;        // transverse momentum vs. pseudorapidity of unclean good (found) tracks
                    TH2F *mPtEtaMesUncleanGood;     // measured transverse momentum vs. measured pseudorapidity of unclean good (found) tracks
                    TH2F *mPtEtaSplit;              // ransverse momentum vs. pseudorapidity of split tracks
                    TH2F *mPtEtaSplitGood;          // ransverse momentum vs. pseudorapidity of split good tracks
                    TH2F *mPtEtaSplitLoliGood;      // ransverse momentum vs. pseudorapidity of split look-like good tracks

                    TH2F *mPtEtaGoodG;              // transverse momentum vs. pseudorapidity of good geant tracks  
                    TH2F *mPtEtaGoodF;              // transverse momentum vs. pseudorapidity of good found tracks
                    TH2F *mPtEtaGoodRatio;          // transverse momentum vs. pseudorapidity of good found tracks divided by good geant tracks
                    TH2F *mPtEtaBadG;               // transverse momentum vs. pseudorapidity of bad geant tracks
                    TH2F *mPtEtaBadF;               // transverse momentum vs. pseudorapidity of bad found tracks
                    TH2F *mPtEtaBadRatio;           // transverse momentum vs. pseudorapidity of bad found tracks divided by bad geant tracks

                    TH2F *mPtEtaFVtx;               // transverse momentum vs. pseudorapidity of found tracks with main vertex tag
                    TH2F *mPtEtaLookLikeGood;       // transverse momentum vs. pseudorapidity of found tracks which look like good tracks but aren't
                    TH2F *mPtEtaContamination;      // transverse momentum vs. pseudorapidity of found tracks which look like good tracks but aren't divided by found tracks with main vertex tag
                    TH2F *mPtEtaContaWoSplits;      // transverse momentum vs. pseudorapidity of found tracks which look like good tracks but aren't minus double counted splits divided by found tracks with main vertex tag

                    TH2F *mPtEta10PointTracks;      // transverse momentum vs. pseudorapidity of 10 point Geant tracks
                    TH2F *mPtEtaWrongCharge;        // transverse momentum vs. pseudorapidity of tracks with wrong charge assigned
                    TH2F *mPtEtaWrongPosCharge;     // transverse momentum vs. pseudorapidity of tracks with wrong positive charge assigned

                    TH2F *mGLengthDistTrackAng;     // length distance vs. track angle of geant tracks
                    TH2F *mGCircleDistTrackAng;     // circle distance vs. track angle of geant tracks
                    TH2F *mFLengthDistTrackAng;     // length distance vs. track angle of found tracks
                    TH2F *mFCircleDistTrackAng;     // circle distance vs. track angle of found tracks

                    TH1F *mGTracklAngAll;           // tracklet angle of geant tracks
                    TH1F *mGTrackAngAll;            // track angle of geant tracks
                    TH1F *mGCircleDistAll;          // circle distance of geant tracks
                    TH1F *mGLengthDistAll;          // length distance of geant tracks

                    TH2F *mGTrackAng;               // track angle of geant tracks
                    TH2F *mGCircleDist;             // circle distance of geant tracks
                    TH2F *mGLengthDist;             // length distance of geant tracks

                    TH2F *mGCircleLength;           // circle distance vs. length distance of geant tracks
  
                    TH1F *mFTracklAngAll;           // tracklet angle of found tracks
                    TH1F *mFTrackAngAll;            // track angle of found tracks
                    TH1F *mFCircleDistAll;          // circle distance of found tracks
                    TH1F *mFLengthDistAll;          // length distance of found tracks

                    TH1F *mDcaFMainVertex;           // distance of closest approach for found main vertex tracks 
                    TH1F *mDcaFNonVertex;            // distance of closest approach for found non vertex tracks
                    TH1F *mDcaGMainVertex;           // distance of closest approach for geant main vertex tracks 
                    TH1F *mDcaGNonVertex;            // distance of closest approach for geant non vertex tracks

                    TH2F *mFTrackAng;               // track angle of found tracks
                    TH2F *mFCircleDist;             // circle distance of found tracks
                    TH2F *mFLengthDist;             // length distance of found tracks

                    TH2F *mFCircleLength;           // circle distance vs. length distance of found tracks

                    TH2F *mPRatioDist;              // point ratio vs. distance of track pairs
                    TH2F *mPRatioDistSplit;         // point ratio vs. distance of split track pairs

                    TH1F *mSetupTime;               // time consumption for initialisation and setup
                    TH1F *mTrackingTime;            // time consumption for main vertex tracking
                    TH1F *mExtensionTime;           // time consumption for track extension
                    TH1F *mSplitTime;               // time consumption for handling split tracks
                    TH1F *mFitTime;                 // time consumption for fitting, dE/dx, and writing
                    TH1F *mTotalTime;               // total time consumption
  
               MIntArray *mParentTrack;             // array of numbers of parent tracks for each cluster, [found track # * 10 + # of cluster on track]
               MIntArray *mParentTracks;            // array of numbers of different parent tracks, [found track # * 10 + # of cluster on track]
               MIntArray *mNumParentTracks;         // array of number of different parent tracks, [found track # * 10 + # of cluster on track]
               MIntArray *mParent;                  // array of number of the actual parent track for each found track

               MIntArray *mClusterArr;              // array to specify the quality of the found clusters (0 = unused, 1 = used, -1 = used wrong)
               MIntArray *mUncleanTracksArr;        // array of numbers of unclean tracks
               MIntArray *mSplitTracksArr;          // array of numbers of split tracks
               MIntArray *mSplitGoodTracksArr;      // array of numbers of split good tracks
               MIntArray *mGoodFastSimHitsArr;      // array of goodness of ffs hits (1 = good, 0 = bad)

                  Bool_t *mUnclean;                 //! array of boolean values: indicates if a found track is a unclean track or not

  void  Setup(St_DataSet *geant, St_DataSet *ftpc_data);                       // Does the setup.
  void  SetupFile(Char_t *filename, Char_t *write_permission);                 // Opens the data file.
  void  SetupHistos();                                                         // Does the setup of the histograms.
  void  CreateHistos();                                                        // Creates the histograms.
  void  DeleteHistos();                                                        // Deletes the histograms.
  void  GeantHitInit(St_g2t_ftp_hit *g2t_ftp_hit);                             // Initializes Geant hits  
  void  GeantTrackInit(St_g2t_track *g2t_track, St_g2t_ftp_hit *g2t_ftp_hit);  // Initializes Geant tracks
  void  FastSimHitInit(St_ffs_gepoint *ffs_hit);                               // Initializes fast simulated hits 
  void  ParentTrackInit();                                                     // Initializes parent tracks
  void  CalcSplitTracks();                                                     // Calculates the number of split tracks
  void  EvaluateGoodness(Int_t t_Counter);                                     // Evaluates if track "t_counter" origins from a good parent  
 
public:
  
   Char_t  *mFilename;                       //! Name of the data file.
   Char_t  *mWritePermission;                //! Write permission of the data file.
   TFile   *mFile;                           // Pointer to the data file.

            StFtpcTrackEvaluator();                                                                    // default constructor
            StFtpcTrackEvaluator(St_DataSet *geant, St_DataSet *ftpc_data, StFtpcTracker *tracker, 
				 Char_t *filename = 0, Char_t *write_permission = 0);                  // real constructor
  virtual  ~StFtpcTrackEvaluator();                                                                    // destructor

  TObjArray *GetGeantHits()   { return mGeantHits;   }
  TObjArray *GetGeantTracks() { return mGeantTracks; }
  TObjArray *GetFastSimHits() { return mFastSimHits; }
  TObjArray *GetFoundHits()   { return mFoundHits;   }
  TObjArray *GetFoundTracks() { return mFoundTracks; }

  MIntArray *GetClusterArr()         { return mClusterArr;         }
  MIntArray *GetSplitTracksArr()     { return mSplitTracksArr;     }
  MIntArray *GetSplitGoodTracksArr() { return mSplitGoodTracksArr; }
  MIntArray *GetUncleanTracksArr()   { return mUncleanTracksArr;   }
  MIntArray *GetGoodFastSimHitsArr() { return mGoodFastSimHitsArr; }

    Int_t   GetNumFoundVertexTracks()   { return mFoundVertexTracks;             }  // returns the number of found main vertex tracks
    Int_t   GetNumFoundNonVertexTracks(){ return mFoundNonVertexTracks;          }  // returns the number of found non main vertex tracks
    Int_t   GetNumLookLikeGoodTracks()  { return mLookLikeGoodTracks;            }  // returns the number of tracks which look good but aren't
    Int_t   GetNumElectronTracks()      { return mElectronTracks;                }  // returns the number of electron tracks
    Int_t   GetNumNonVertexTracks()     { return mNonVertexTracks;               }  // returns the number of non vertex tracks
    Int_t   GetNumGoodGeantTracks()     { return mGoodGTracks;                   }  // returns the number of good geant tracks
    Int_t   GetNumGoodFoundTracks()     { return mGoodFTracks;                   }  // returns the number of good found tracks
    Int_t   GetNumSplitTracks()         { return mSplitTracks;                   }  // returns the number of split tracks
    Int_t   GetNumSplitGoodTracks()     { return mSplitGoodTracks;               }  // returns the number of split good tracks
    Int_t   GetNumSplitLookLikeGoodTracks() { return mSplitLoliGoodTracks;       }  // returns the number of split good tracks
    Int_t   GetNumUncleanTracks()       { return mUncleanTracks;                 }  // returns the number of tracks which have picked up wrong clusters
    Int_t   GetNumUncleanGoodTracks()   { return mUncleanGoodTracks;             }  // returns the number of tracks which have picked up wrong clusters and have a good parent track
    Int_t   GetNumLongTracks()          { return mLongTracks;                    }  // returns number of tracks with more than 10 points in the Ftpc
    Int_t   GetNumLongTrackClusters()   { return mLongTrackClusters;             }  // returns number of clusters on long tracks
    Int_t   GetNumShortTracks()         { return mShortTracks;                   }  // returns number of tracks with less than 5 points in the Ftpc
    Int_t   GetNumShortTrackClusters()  { return mShortTrackClusters;            }  // returns number of clusters on short tracks
    Int_t   GetNumTooShortTracks()      { return mTooShortTracks;                }  // returns number of tracks which should be longer
    Int_t   GetMaxClusters()            { return mMaxClusters;                   }  // returns max. number of clusters on a track

  Int_t GetNumGoodGeantPoints() { return mGoodGeantPoints; }
  Int_t GetNumGoodFoundPoints() { return mGoodFoundPoints; }

    Int_t   GetNumFoundTracks()         { return mFoundTracks->GetEntriesFast(); }  // returns number of found tracks in the Ftpc
    Int_t   GetNumGeantTracks()         { return mGeantTracks->GetEntriesFast(); }  // returns number of geant tracks in the Ftpc
    Int_t   GetNumFoundHits()           { return mFoundHits->GetEntriesFast();   }  // returns number of found hits in the Ftpc
    Int_t   GetNumFastSimHits()         { return mFastSimHits->GetEntriesFast(); }  // returns number of fast simulated hits in the Ftpc
    Int_t   GetNumGeantHits()           { return mGeantHits->GetEntriesFast();   }  // returns number of geant hits in the Ftpc

  
   Bool_t   IsGoodTrack(StFtpcTrack* track);                                        // Evaluates if track is a good track. 
   Bool_t   IsGoodMainVertexTrack(StFtpcTrack* track);                              // Evaluates if track is a good main vertex track. 
   Bool_t   IsGoodNonVertexTrack(StFtpcTrack* track);                               // Evaluates if track is a good non vertex track. 
   Bool_t   IsUncleanTrack(Int_t track_num) { return  mUnclean[track_num];       }  // returns true if track is unclean
   Bool_t   IsCleanTrack(Int_t track_num)   { return !mUnclean[track_num];       }  // returns true if track is clean

     void   Loop();
     void   FillEventHistos();                                    // Fills histograms which are filled only once per event.
     void   FillTimeHistos(StFtpcTracker* tracker);               // Fills histogramms of time consumption
     void   FillParentHistos();                                   // Fills histogram of number of parents
     void   FillParentHistos(Int_t t_counter);                    // Fills histogram of number of parents
     void   FillMomentumHistos();                                 // Fills histograms of rel. momentum difference
     void   FillMomentumHistos(Int_t t_counter);                  // Fills histograms of rel. momentum difference
     void   FillHitsOnTrack(TObjArray *trackarray, Char_t c);     // Fills histogram of hits on tracks
     void   FillHitsOnTrack();                                    // Fills all hits on all tracks
     void   FillFoundHitsOnTrack();                               // Fills found hits on tracks
     void   FillCutHistos();                                      // Fills cut histograms (geant and found tracks)
     void   FillGCutHistos();                                     // Fills cut histograms (geant tracks)
     void   FillFCutHistos();                                     // Fills cut histograms (found tracks)
     void   DivideHistos();                                       // Divides histograms
     void   WriteHistos();                                        // Writes histograms to file

     void   GetGoodHits(TObjArray *good_clusters);                // Fills all good hits in given array    

     void   ShowTracks();    // Displays geant and/or found tracks
     void   GeantInfo();     // Shows information about GEANT output.
     void   ClusterInfo();   // Shows information about geant clusters.
     void   TrackerInfo();   // Shows information about tracker output.
     void   ProblemsInfo();  // Shows information about problems.
     void   InfoAll();       // Shows all information.

  ClassDef(StFtpcTrackEvaluator, 1)  //Ftpc track evaluation class
};





#endif
