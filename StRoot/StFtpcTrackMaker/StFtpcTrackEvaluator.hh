// $Id: StFtpcTrackEvaluator.hh,v 1.1 2000/05/10 13:39:27 oldi Exp $
// $Log: StFtpcTrackEvaluator.hh,v $
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
#include "TClonesArray.h"
#include "TH1.h"
#include "TH2.h"
#include "MIntArray.h"
#include "TFile.h"

#include "StFtpcVertex.hh"
#include "StFtpcTrack.hh"

#include "tables/St_ffs_gepoint_Table.h"
#include "tables/St_g2t_track_Table.h"
#include "tables/St_g2t_ftp_hit_Table.h"

#include "St_DataSet.h"

#include "tables/St_fpt_fptrack_Table.h"
#include "ftpc/St_fde_Module.h"

class StFtpcTrackEvaluator : public TObject {

private:
                  Bool_t  mObjArraysCreated;      // flag to know which destructor to be called

            TClonesArray *mGeantHits;             // ClonesArray of geant hits
            TClonesArray *mFastSimHits;           // ClonesArray of fast simulated hits
            TClonesArray *mFoundHits;             // ClonesArray of found hits
            TClonesArray *mGeantTracks;           // ClonesArray of geant tracks
            TClonesArray *mFoundTracks;           // ClonesArray of found tracks

               MIntArray *mFtpcTrackNum;          // array of numbers of found Ftpc tracks, [geant track]

                   Int_t  mFoundVertexTracks;     // Number of found main vertex tracks
                   Int_t  mFoundNonVertexTracks;  // Number of found non main vertex tracks

                   Int_t  mLookLikeGoodTracks;    // Number of tracks which look like good tracks but aren't  
                   Int_t  mElectronTracks;        // Number of electrons
                   Int_t  mNonVertexTracks;       // Number of non main vertex tracks
                   Int_t  mGoodGTracks;           // Number of good geant tracks (all tracks - - short tracks - electrons - non vertex tracks)
                   Int_t  mGoodFTracks;           // Number of good found tracks (all tracks - - short tracks - electrons - non vertex tracks)  
                   Int_t  mSplitTracks;           // Number of split tracks
                   Int_t  mSplitGoodTracks;       // Number of split good tracks 
                   Int_t  mUncleanTracks;         // Number of tracks which have picked up wrong clusters
                   Int_t  mLongTracks;            // Number of tracks with more than 10 points
                   Int_t  mLongTrackClusters;     // Number of clusters on long tracks
                   Int_t  mShortTracks;           // Number of tracks with less than 5 points
                   Int_t  mShortTrackClusters;    // Number of clusters on short tracks
                   Int_t  mMaxClusters;           // Max. number of clusters on a track

            StFtpcVertex *mVertex;                // pointer ro the main vertex

                    TH1F *mNumGeantHits;          // number of geant hits per event
                    TH1F *mNumFoundHits;          // number of found hits per event
                    TH1F *mNumGeantTracks;        // number of geant tracks per event
                    TH1F *mNumFoundTracks;        // number of found tracks per event

                    TH1F *mNumFoundVertexTracks;  // number of found main vertex tracks per event
                    TH1F *mNumFoundNonVertexTracks;// number of found non main vertex tracks per event

                    TH1F *mNumElectronTracks;     // number of electrons per event
                    TH1F *mNumNonVertexTracks;    // number of non main vertex tracks per event
                    TH1F *mNumLookLikeGoodTracks; // number of tracks which look good but aren't
                    TH1F *mNumGoodGTracks;        // number of good geant tracks per event
                    TH1F *mNumGoodFTracks;        // number of good found tracks per event
                    TH1F *mNumSplitTracks;        // number of split tracks per event
                    TH1F *mNumSplitGoodTracks;    // number of split good tracks per event                   

                    TH1F *mNumUncleanTracks;      // number of unclean tracks per event
                    TH1F *mNumLongTracks;         // number of tracks with more than 10 points per event
                    TH1F *mNumLongTrackClusters;  // number of hits belonging to long tracks per event
                    TH1F *mNumShortTracks;        // number of tracks with less than 5 points per event
                    TH1F *mNumShortTrackClusters; // number of hits belonging to short tracks per event

                    TH1F *mGHitsOnTrack;          // number of geant hits per track
                    TH1F *mFHitsOnTrack;          // number of found hits per track
                    
                    TH1F *mNumParents;            // number of parent tracks per track
                    TH1F *mNumWrongHitsAll;       // number of wrong clusters per track
                    TH2F *mNumWrongHits;          // number of wrong clusters per track

                    TH1F *mGoodPercentage;        // number of good found tracks divided by the number of good geant tracks
  
                    TH2F *mPtot;                  // found total momentum vs. geant total momentum
                    TH2F *mPt;                    // found transverse momentum vs. geant transverse momentum
                    TH2F *mPx;                    // found x momentum vs. geant x momentum
                    TH2F *mPy;                    // found y momentum vs. geant y momentum
                    TH2F *mPz;                    // found z momentum vs. geant z momentum

                    TH1F *mPtotDiff;              // relative difference of total momentum
                    TH1F *mPtDiff;                // relative difference of transverse momentum
                    TH1F *mPxDiff;                // relative difference of momentum in x direction
                    TH1F *mPyDiff;                // relative difference of momentum in y direction
                    TH1F *mPzDiff;                // relative difference of momentum in z direction

                    TH1F *mPtotAcc;               // relative accuracy of total momentum
                    TH1F *mPtAcc;                 // relative accuracy of transverse momentum
                    TH1F *mPxAcc;                 // relative accuracy of momentum in x direction
                    TH1F *mPyAcc;                 // relative accuracy of momentum in y direction
                    TH1F *mPzAcc;                 // relative accuracy of momentum in z direction

                    TH2F *mEtaNghits;             // pseudorapidity vs. number of geant clusters on track
                    TH2F *mEtaNfhits;             // pseudorapidity vs. number of found clusters on track

                    TH2F *mPtEtaF;                // transverse momentum vs. pseudorapidity of found tracks 
                    TH2F *mPtEtaGood;             // transverse momentum vs. pseudorapidity of good found tracks divided by all found tracks
                    TH2F *mPtEtaBad;              // transverse momentum vs. pseudorapidity of bad found tracks divided by all found tracks

                    TH2F *mPtEtaGoodG;            // transverse momentum vs. pseudorapidity of good geant tracks  
                    TH2F *mPtEtaGoodF;            // transverse momentum vs. pseudorapidity of good found tracks
                    TH2F *mPtEtaGoodPercentage;   // transverse momentum vs. pseudorapidity of good found tracks divided by good geant tracks
                    TH2F *mPtEtaBadG;             // transverse momentum vs. pseudorapidity of bad geant tracks
                    TH2F *mPtEtaBadF;             // transverse momentum vs. pseudorapidity of bad found tracks
                    TH2F *mPtEtaBadPercentage;    // transverse momentum vs. pseudorapidity of bad found tracks divided by bad geant tracks

                    TH2F *mPtEtaFVtx;             // transverse momentum vs. pseudorapidity of found tracks with main vertex tag
                    TH2F *mPtEtaLookLikeGood;     // transverse momentum vs. pseudorapidity of found tracks which look like good tracks but aren't
                    TH2F *mPtEtaContamination;    // transverse momentum vs. pseudorapidity of found tracks which look like good tracks but aren't divided by found tracks with main vertex tag

                    TH2F *mGLengthDistTrackAng;   // length distance vs. track angle of geant tracks
                    TH2F *mGCircleDistTrackAng;   // circle distance vs. track angle of geant tracks
                    TH2F *mFLengthDistTrackAng;   // length distance vs. track angle of found tracks
                    TH2F *mFCircleDistTrackAng;   // circle distance vs. track angle of found tracks

                    TH1F *mGTracklAngAll;         // tracklet angle of geant tracks
                    TH1F *mGTrackAngAll;          // track angle of geant tracks
                    TH1F *mGCircleDistAll;        // circle distance of geant tracks
                    TH1F *mGLengthDistAll;        // length distance of geant tracks

                    TH2F *mGTrackAng;             // track angle of geant tracks
                    TH2F *mGCircleDist;           // circle distance of geant tracks
                    TH2F *mGLengthDist;           // length distance of geant tracks

                    TH2F *mGCircleLength;         // circle distance vs. length distance of geant tracks
  
                    TH1F *mFTracklAngAll;         // tracklet angle of found tracks
                    TH1F *mFTrackAngAll;          // track angle of found tracks
                    TH1F *mFCircleDistAll;        // circle distance of found tracks
                    TH1F *mFLengthDistAll;        // length distance of found tracks

                    TH1F *mDcaFMainVertex;         // distance of closest approach for found main vertex tracks 
                    TH1F *mDcaFNonVertex;          // distance of closest approach for found non vertex tracks
                    TH1F *mDcaGMainVertex;         // distance of closest approach for geant main vertex tracks 
                    TH1F *mDcaGNonVertex;          // distance of closest approach for geant non vertex tracks

                    TH2F *mFTrackAng;             // track angle of found tracks
                    TH2F *mFCircleDist;           // circle distance of found tracks
                    TH2F *mFLengthDist;           // length distance of found tracks

                    TH2F *mFCircleLength;         // circle distance vs. length distance of found tracks

               MIntArray *mParentTrack;           // array of numbers of parent tracks for each cluster, [found track # * 10 + # of cluster on track]
               MIntArray *mParentTracks;          // array of numbers of different parent tracks, [found track # * 10 + # of cluster on track]
               MIntArray *mNumParentTracks;       // array of number of different parent tracks, [found track # * 10 + # of cluster on track]
               MIntArray *mParent;                // array of number of the actual parent track for each found track

               MIntArray *mUncleanTracksArr;   // array of numbers of unclean tracks
               MIntArray *mSplitTracksArr;     // array of numbers of split tracks
               MIntArray *mSplitGoodTracksArr; // array of numbers of split good tracks

                  Bool_t *mUnclean;               // array of boolean values: indicates if a found track is a unclean track or not

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
  
   Char_t  *mFilename;                       // Name of the data file.
   Char_t  *mWritePermission;                // Write permission of the data file.
   TFile   *mFile;                           // Pointer to the data file.

            StFtpcTrackEvaluator();                                                                    // default constructor
            StFtpcTrackEvaluator(St_DataSet *geant, St_DataSet *ftpc_data, StFtpcVertex *main_vertex,  
				 St_fcl_fppoint *fcl_fppoint, St_fpt_fptrack *fpt_fptrack, 
				 Char_t *filename = 0, Char_t *write_permission = 0);                  // real constructor
            StFtpcTrackEvaluator(St_DataSet *geant, St_DataSet *ftpc_data, StFtpcVertex *main_vertex, 
				 TClonesArray *hits, TClonesArray *tracks, 
				 Char_t *filename = 0, Char_t *write_permission = 0);                  // another real constructor
  virtual  ~StFtpcTrackEvaluator();                                                                    // destructor

  TClonesArray *GetGeantHits()   { return mGeantHits;   }
  TClonesArray *GetGeantTracks() { return mGeantTracks; }
  TClonesArray *GetFastSimHits() { return mFastSimHits; }
  TClonesArray *GetFoundHits()   { return mFoundHits;   }
  TClonesArray *GetFoundTracks() { return mFoundTracks; }

  MIntArray *GetSplitTracksArr()     { return mSplitTracksArr;     }
  MIntArray *GetSplitGoodTracksArr() { return mSplitGoodTracksArr; }
  MIntArray *GetUncleanTracksArr()   { return mUncleanTracksArr;   }

    Int_t   GetNumFoundVertexTracks()   { return mFoundVertexTracks;             }  // returns the number of found main vertex tracks
    Int_t   GetNumFoundNonVertexTracks(){ return mFoundNonVertexTracks;          }  // returns the number of found non main vertex tracks
    Int_t   GetNumLookLikeGoodTracks()  { return mLookLikeGoodTracks;            }  // returns the number of tracks which look good but aren't
    Int_t   GetNumElectronTracks()      { return mElectronTracks;                }  // returns the number of electron tracks
    Int_t   GetNumNonVertexTracks()     { return mNonVertexTracks;               }  // returns the number of non vertex tracks
    Int_t   GetNumGoodGeantTracks()     { return mGoodGTracks;                   }  // returns the number of good geant tracks
    Int_t   GetNumGoodFoundTracks()     { return mGoodFTracks;                   }  // returns the number of good found tracks
    Int_t   GetNumSplitTracks()         { return mSplitTracks;                   }  // returns the number of split tracks
    Int_t   GetNumSplitGoodTracks()     { return mSplitGoodTracks;               }  // returns the number of split good tracks
    Int_t   GetNumUncleanTracks()       { return mUncleanTracks;                 }  // returns the number of tracks which have picked up wrong clusters
    Int_t   GetNumLongTracks()          { return mLongTracks;                    }  // returns number of tracks with more than 10 points in the Ftpc
    Int_t   GetNumLongTrackClusters()   { return mLongTrackClusters;             }  // returns number of clusters on long tracks
    Int_t   GetNumShortTracks()         { return mShortTracks;                   }  // returns number of tracks with less than 5 points in the Ftpc
    Int_t   GetNumShortTrackClusters()  { return mShortTrackClusters;            }  // returns number of clusters on short tracks
    Int_t   GetMaxClusters()            { return mMaxClusters;                   }  // returns max. number of clusters on a track
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
     void   FillParentHistos();                                   // Fills histogram of number of parents
     void   FillParentHistos(Int_t t_counter);                    // Fills histogram of number of parents
     void   FillMomentumHistos();                                 // Fills histograms of rel. momentum difference
     void   FillMomentumHistos(Int_t t_counter);                  // Fills histograms of rel. momentum difference
     void   FillHitsOnTrack(TClonesArray *trackarray, Char_t c);  // Fills histogram of hits on tracks
     void   FillHitsOnTrack();                                    // Fills all hits on all tracks
     void   FillFoundHitsOnTrack();                               // Fills found hits on tracks
     void   FillCutHistos();                                      // Fills cut histograms (geant and found tracks)
     void   FillGCutHistos();                                     // Fills cut histograms (geant tracks)
     void   FillFCutHistos();                                     // Fills cut histograms (found tracks)
     void   DivideHistos();                                       // Divides histograms
     void   WriteHistos();                                        // Writes histograms to file

     void   ShowTracks();    // Displays geant and/or found tracks
     void   GeantInfo();     // Shows information about GEANT output.
     void   ClusterInfo();   // Shows information about geant clusters.
     void   TrackerInfo();   // Shows information about tracker output.
     void   ProblemsInfo();  // Shows information about problems.
     void   Info();          // Shows all information.

  ClassDef(StFtpcTrackEvaluator, 1)  //Ftpc track evaluation class
};

#endif
