// $Id: StFtpcTrackEvaluator.cc,v 1.22 2007/01/15 08:23:02 jcs Exp $
// $Log: StFtpcTrackEvaluator.cc,v $
// Revision 1.22  2007/01/15 08:23:02  jcs
// replace printf, cout and gMesMgr with Logger commands
//
// Revision 1.21  2004/02/12 19:37:10  oldi
// *** empty log message ***
//
// Revision 1.20  2004/01/28 01:41:32  jeromel
// *** empty log message ***
//
// Revision 1.19  2003/09/16 15:27:02  jcs
// removed inline as it would leave a few undefined reference
//
// Revision 1.18  2003/01/16 18:04:33  oldi
// Bugs eliminated. Now it compiles on Solaris again.
// Split residuals for global and primary fit.
//
// Revision 1.17  2002/11/06 13:46:24  oldi
// Code clean ups.
//
// Revision 1.16  2002/10/24 16:37:46  oldi
// dca (impact parameter) is calculated using StHelix::distance(vertexPos), now.
// Therefore it is the smallest three dimensional distance of the helix to the
// primary vertex.
// dca for primary tracks is filled correctly, now. (Even though this value
// shouldn't be of great use.)
// Code clean-ups.
//
// Revision 1.15  2002/10/11 15:45:22  oldi
// Get FTPC geometry and dimensions from database.
// No field fit activated: Returns momentum = 0 but fits a helix.
// Bug in TrackMaker fixed (events with z_vertex > outer_ftpc_radius were cut).
// QA histograms corrected (0 was supressed).
// Code cleanup (several lines of code changed due to *params -> Instance()).
// cout -> gMessMgr.
//
// Revision 1.14  2002/04/05 16:50:51  oldi
// Cleanup of MomentumFit (StFtpcMomentumFit is now part of StFtpcTrack).
// Each Track inherits from StHelix, now.
// Therefore it is possible to calculate, now:
//  - residuals
//  - vertex estimations obtained by back extrapolations of FTPC tracks
// Chi2 was fixed.
// Many additional minor (and major) changes.
//
// Revision 1.13  2002/03/05 16:53:10  jcs
// force data type definitions to avoid compiler warnings (this is a correct
// but inelegant fix which must be changed)
//
// Revision 1.12  2001/07/12 08:34:27  oldi
// Many new things were developed eg. histograms for time consumption and
// momentum resolution.
// File handling was debugged.
// New constructor to evaluate tracks of good evaluated clusters of a
// previous run.
//
// Revision 1.11  2001/05/02 13:40:07  oldi
// Small change to be able to update the output file again.
//
// Revision 1.10  2001/04/27 10:25:21  oldi
// Protection against crash due to lack of fast simulator hit information.
// The evaluator gives (up to now) only valueable information when tracking was
// performed with the fast simulator. But now it is possile to evaluate slow
// simulated events as well (as far as the necessary information is available).
// ShowTracks() has still a problem due the change of ROOT version.
//
// Revision 1.9  2001/04/25 17:55:10  perev
// HPcorrs
//
// Revision 1.8  2001/03/06 18:06:36  oldi
// Because StFtpcClusterMaker and StFtpcConfMapper have changed two things had
// to be changed here: coeff[4] -> coeff[6], ffs_gepoint -> ffs_fgepoint.
//
// Revision 1.7  2001/01/25 15:22:10  oldi
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
// Revision 1.6  2000/11/10 18:38:24  oldi
// Short tracks are treated now.
//
// Revision 1.5  2000/07/18 21:22:16  oldi
// Changes due to be able to find laser tracks.
// Cleanup: - new functions in StFtpcConfMapper, StFtpcTrack, and StFtpcPoint
//            to bundle often called functions
//          - short functions inlined
//          - formulas of StFormulary made static
//          - avoid streaming of objects of unknown size
//            (removes the bunch of CINT warnings during compile time)
//          - two or three minor bugs cured
//
// Revision 1.4  2000/06/13 14:36:19  oldi
// Changed cout to gMessMgr->Message() (only for non-interactive part).
//
// Revision 1.3  2000/06/07 11:09:50  oldi
// Changed 0 pointers to NULL pointers.
// In SetupHistos(): avoided exit(-1) if write permission is wrong.
// Added new functionality (improvement of output histos).
// Cleanup.
//
// Revision 1.2  2000/05/11 15:14:51  oldi
// Changed class names *Hit.* due to already existing class StFtpcHit.cxx in StEvent
//
// Revision 1.1  2000/05/10 13:39:25  oldi
// Initial version of StFtpcTrackMaker
//

//----------Author:        Markus D. Oldenburg
//----------Last Modified: 17.10.2000
//----------Copyright:     &copy MDO Production 2000

#include "StFtpcTrackEvaluator.hh"
#include "StFtpcConfMapper.hh"
#include "StFtpcPoint.hh"
#include "StFtpcConfMapPoint.hh"
#include "StFtpcTrack.hh"
#include "StFormulary.hh"
#include "StFtpcDisplay.hh"

#include "St_DataSetIter.h"

#include "TFile.h"
#include "TH1.h"
#include "TH2.h"
#include "TAxis.h"

#include "MIntArray.h"

#include "StMessMgr.h"

////////////////////////////////////////////////////////////////////////////////////
//                                                                                //
// StFtpcTrackEvaluator class - evaluates found tracks by comparison to the input //
//                              GEANT clusters and tracks.                        //
//                                                                                //
// As input this class gets the pointers to the ObjArrays of found hits and    //
// tracks and the pointers of the input (to the tracker) geant hits and tracks.   //
// The class should tell then which tracks are found and where are the remaining  //
// problems.                                                                      //
//                                                                                // 
////////////////////////////////////////////////////////////////////////////////////

ClassImp(StFtpcTrackEvaluator)


StFtpcTrackEvaluator::StFtpcTrackEvaluator()
{
  // Default constructor.
  // Sets the pointers.

  mFilename = 0;
  mWritePermission = 0;
  mFile = 0;

  mNumGeantHits = 0;
  mNumFoundHits = 0;
  mNumGeantTracks = 0;
  mNumFoundTracks = 0;

  mClusterArr = 0;
  mUncleanTracksArr = 0;
  mSplitTracksArr = 0;
  mSplitGoodTracksArr = 0;
  mGoodFastSimHitsArr = 0;
  
  mNumFoundVertexTracks = 0;
  mNumFoundNonVertexTracks = 0;

  mNumLookLikeGoodTracks = 0;
  mNumElectronTracks = 0;
  mNumNonVertexTracks = 0;
  mNumGoodGTracks = 0;
  mNumGoodFTracks = 0;
  mGoodRatio = 0;
  mContamination = 0;
  mContaWoSplit = 0;
  mNumSplitTracks = 0;
  mNumSplitGoodTracks = 0;
  mNumSplitLoliGoodTracks = 0;
  mNumUncleanTracks = 0;
  mNumUncleanGoodTracks = 0;
  mNumLongTracks = 0;
  mNumLongTrackClusters = 0;
  mNumShortTracks = 0;
  mNumTooShortTracks = 0;
  mNumShortTrackClusters = 0;

  mVertex = 0;

  mGeantHits = 0;
  mFoundHits = 0;
  mFastSimHits = 0;
  mGeantTracks = 0;
  mFoundTracks = 0;

  mObjArraysCreated = (Bool_t)kFALSE;

  mFtpcTrackNum = 0;

  mSplitTracks = 0;
  mSplitGoodTracks = 0;
  mSplitLoliGoodTracks = 0;
  mUncleanTracks = 0;
  mUncleanGoodTracks = 0;
  mLongTracks = 0;
  mLongTrackClusters = 0;
  mShortTracks = 0;
  mShortTrackClusters = 0;
  mMaxClusters = 0;

  mGoodGeantPoints = 0;
  mGoodFoundPoints = 0;

  mNumGoodGeantPoints = 0;
  mNumGoodFoundPoints = 0;
  mGoodPointRatio = 0;

  mFHitsOnTrack = 0;
  mGHitsOnTrack = 0;
  mNumParents = 0;

  mNumWrongHitsAll = 0;
  mNumWrongHits = 0;

  mPtot = 0;
  mPt = 0;
  mPx = 0;
  mPy = 0;
  mPz = 0;

  mPtotDiff = 0;
  mPtDiff = 0;
  mPxDiff = 0;
  mPyDiff = 0;
  mPzDiff = 0;

  mPtotAcc = 0;
  mPtAcc = 0;
  mPxAcc = 0;
  mPyAcc = 0;
  mPzAcc = 0;

  mPRelErr = 0;
  mPRelErrqok = 0;
  mPRelDiff = 0;
  
  mEtaNghits = 0;
  mEtaNfhits = 0;

  mPtEtaF = 0;
  mPtEtaFMes = 0;
  mPtEtaGood = 0;
  mPtEtaBad = 0;
  mPtEtaUnclean = 0;
  mPtEtaMesUnclean = 0;
  mPtEtaUncleanGood = 0;
  mPtEtaMesUncleanGood = 0;

  mPtEtaSplit = 0;
  mPtEtaSplitGood = 0;
  mPtEtaSplitLoliGood = 0;

  mPtEtaGoodG = 0;
  mPtEtaGoodF = 0;
  mPtEtaGoodRatio = 0;
  mPtEtaBadG = 0;
  mPtEtaBadF = 0;
  mPtEtaBadRatio = 0;

  mPtEtaFVtx = 0;
  mPtEtaLookLikeGood = 0;
  mPtEtaContamination = 0;
  mPtEtaContaWoSplits = 0;

  mPtEta10PointTracks = 0;
  mPtEtaWrongCharge = 0;
  mPtEtaWrongPosCharge = 0;

  mGLengthDistTrackAng = 0;
  mGCircleDistTrackAng = 0;
  mFLengthDistTrackAng = 0;
  mFCircleDistTrackAng = 0;

  mGTracklAngAll = 0;
  mGTrackAngAll = 0;
  mGCircleDistAll = 0;
  mGLengthDistAll = 0; 

  mDcaFMainVertex = 0;
  mDcaFNonVertex = 0;
  mDcaGMainVertex = 0;
  mDcaGNonVertex = 0;

  mGTrackAng = 0;
  mGCircleDist = 0;
  mGLengthDist = 0; 

  mGCircleLength = 0;

  mFTracklAngAll = 0;
  mFTrackAngAll = 0;
  mFCircleDistAll = 0;
  mFLengthDistAll = 0; 

  mFTrackAng = 0;
  mFCircleDist = 0;
  mFLengthDist = 0; 

  mFCircleLength = 0;

  mPRatioDist = 0;
  mPRatioDistSplit = 0;

  mParentTrack = 0;
  mParentTracks = 0;
  mNumParentTracks = 0;

  mUnclean = 0;
  
  mSetupTime = 0;
  mTrackingTime = 0;
  mExtensionTime = 0;
  mSplitTime = 0;
  mFitTime = 0;
  mTotalTime = 0;
}


StFtpcTrackEvaluator::StFtpcTrackEvaluator(St_DataSet *geant, St_DataSet *ftpc_data, StFtpcTracker *tracker, Char_t *filename, Char_t *write_permission)
{
  // Usual used constructor if conformal mapping tracker output available.

  LOG_INFO << "Track evaluating started..." << endm;

  mVertex = tracker->GetVertex();

  mFoundHits = tracker->GetClusters();
  mFoundTracks = tracker->GetTracks();
  mObjArraysCreated = (Bool_t)kFALSE;

  SetupFile(filename, write_permission);
  SetupHistos();
  Setup(geant, ftpc_data);

  St_ffs_gepoint *ffs_gepoint = (St_ffs_gepoint *)ftpc_data->Find("ffs_fgepoint");
 
  if (!ffs_gepoint) {
    // event processed with slow simulator
    LOG_INFO << "This event was processed with the slow simulator!" << endm;
    LOG_INFO << "Most information will be missing due to lack of geant hit information." << endm;
  }
  
  InfoAll();
  FillHitsOnTrack();

  if (ffs_gepoint) {
    // fast simulator was present
    FillParentHistos();
    FillMomentumHistos();
  }

  FillEventHistos();
  FillTimeHistos(tracker);
  FillCutHistos();
  DivideHistos();
  WriteHistos();
}


StFtpcTrackEvaluator::~StFtpcTrackEvaluator()
{
  // Destructor.
  // Deletes ObjArrays and Histograms.
  
  DeleteHistos();

  delete mClusterArr;
  delete mUncleanTracksArr;
  delete mSplitTracksArr;
  delete mSplitGoodTracksArr;
  delete mGoodFastSimHitsArr;
  
  mFile->Close();
  delete mFile;
  delete[] mFilename;
  delete[] mWritePermission;

  if (mGeantHits) {
    mGeantHits->Delete();
    delete mGeantHits;
  }

  if (mGeantTracks) {
    mGeantTracks->Delete();
    delete mGeantTracks;
  }

  if (mFastSimHits) {
    mFastSimHits->Delete();
    delete mFastSimHits;
  }

  delete mFtpcTrackNum;

  delete mParentTrack;
  delete mParentTracks;
  delete mNumParentTracks;  
  delete mParent;

  delete[] mUnclean;

  if (mObjArraysCreated) {
    mFoundHits->Delete();
    mFoundTracks->Delete();
    delete mFoundHits;
    delete mFoundTracks;
  }
}


void StFtpcTrackEvaluator::DeleteHistos()
{
  // Deletes the histograms

  delete mNumGeantHits;
  delete mNumFoundHits;
  delete mNumGeantTracks;
  delete mNumFoundTracks;

  delete mNumFoundVertexTracks;
  delete mNumFoundNonVertexTracks;

  delete mNumLookLikeGoodTracks;
  delete mNumElectronTracks;
  delete mNumNonVertexTracks;
  delete mNumGoodGTracks;
  delete mNumGoodFTracks;
  delete mGoodRatio;
  delete mContamination;
  delete mContaWoSplit;
  delete mNumSplitTracks;
  delete mNumSplitGoodTracks;
  delete mNumSplitLoliGoodTracks;
  delete mNumUncleanTracks;
  delete mNumUncleanGoodTracks;
  delete mNumLongTracks;
  delete mNumLongTrackClusters;
  delete mNumShortTracks;
  delete mNumTooShortTracks;
  delete mNumShortTrackClusters;

  delete mNumGoodGeantPoints;
  delete mNumGoodFoundPoints;
  delete mGoodPointRatio;

  delete mFHitsOnTrack;
  delete mGHitsOnTrack;

  delete mNumParents;
  delete mNumWrongHitsAll;
  delete mNumWrongHits;

  delete mPtot;
  delete mPt;
  delete mPx;
  delete mPy;
  delete mPz;
 
  delete mPtotDiff;
  delete mPtDiff;
  delete mPxDiff;
  delete mPyDiff;
  delete mPzDiff;

  delete mPtotAcc;
  delete mPtAcc;
  delete mPxAcc;
  delete mPyAcc;
  delete mPzAcc;

  delete mPRelErr;
  delete mPRelErrqok;
  delete mPRelDiff;

  delete mEtaNghits;
  delete mEtaNfhits;

  delete mPtEtaF;
  delete mPtEtaFMes;
  delete mPtEtaGood;
  delete mPtEtaBad;
  delete mPtEtaUnclean;
  delete mPtEtaMesUnclean;
  delete mPtEtaUncleanGood;
  delete mPtEtaMesUncleanGood;

  delete mPtEtaSplit;
  delete mPtEtaSplitGood;
  delete mPtEtaSplitLoliGood;

  delete mPtEtaGoodG;
  delete mPtEtaGoodF;
  delete mPtEtaGoodRatio;
  delete mPtEtaBadG;
  delete mPtEtaBadF;
  delete mPtEtaBadRatio;
  
  delete mPtEtaFVtx;
  delete mPtEtaLookLikeGood;
  delete mPtEtaContamination;
  delete mPtEtaContaWoSplits;

  delete mPtEta10PointTracks;
  delete mPtEtaWrongCharge;
  delete mPtEtaWrongPosCharge;

  delete mGLengthDistTrackAng;
  delete mGCircleDistTrackAng;
  delete mFLengthDistTrackAng;
  delete mFCircleDistTrackAng;

  delete mDcaFMainVertex;
  delete mDcaFNonVertex;
  delete mDcaGMainVertex;
  delete mDcaGNonVertex;

  delete mGTracklAngAll;
  delete mGTrackAngAll;
  delete mGCircleDistAll;
  delete mGLengthDistAll;

  delete mGTrackAng;
  delete mGCircleDist;
  delete mGLengthDist;

  delete mGCircleLength;

  delete mFTracklAngAll;
  delete mFTrackAngAll;
  delete mFCircleDistAll;
  delete mFLengthDistAll;

  delete mFTrackAng;
  delete mFCircleDist;
  delete mFLengthDist;

  delete mFCircleLength;

  delete mPRatioDist;
  delete mPRatioDistSplit;

  delete mSetupTime;
  delete mTrackingTime;
  delete mExtensionTime;
  delete mSplitTime;
  delete mFitTime;
  delete mTotalTime;

  return;
}


void StFtpcTrackEvaluator::SetupFile(Char_t *filename, Char_t *write_permission)
{
  // Opens the data file.

  mFilename = new Char_t[50];
  mWritePermission = new Char_t[10];

  if (filename) {
    sprintf(mFilename, "%s", filename);
  }
  
  else {
    sprintf(mFilename, "ftpc_evaluator.root");
  }

  if (write_permission) {
    sprintf(mWritePermission, "%s", write_permission);
  }
  
  else {
    sprintf(mWritePermission, "RECREATE");
  }

  if (strcmp(mWritePermission, "UPDATE") == 0) {
    
    mFile = new TFile(mFilename, "READ");
    
    if (!mFile->IsOpen()) {
      // File not there
      LOG_WARN << "but that's o.k. - I'll create it immediately!" << endm;
      delete mFile;
      mFile = new TFile(mFilename, "RECREATE");
      CreateHistos();
      WriteHistos();
      DeleteHistos();
    }

    else {
      // File is there
      mFile->Close();
      delete mFile;
      mFile = new TFile(mFilename, "UPDATE");
    }
  }

  else {
    // mWritePermission != "UPDATE"
    mFile = new TFile(mFilename, "UPDATE");
  }

  return;
}


void StFtpcTrackEvaluator::Setup(St_DataSet *geant, St_DataSet *ftpc_data)
{
  // Does all the setup which is common to all constructors.
  
  mClusterArr = new MIntArray();

  mUncleanTracksArr = new MIntArray();
  mSplitTracksArr = new MIntArray();
  mSplitGoodTracksArr = new MIntArray();
  mGoodFastSimHitsArr = new MIntArray();

  mFtpcTrackNum = new MIntArray();

  mParentTrack = new MIntArray();
  mParentTracks = new MIntArray();
  mNumParentTracks = new MIntArray();
  mParent = new MIntArray();

  mUnclean = new Bool_t[mFoundTracks->GetEntriesFast()];

  mFoundVertexTracks = 0;
  mFoundNonVertexTracks = 0;

  mLookLikeGoodTracks = 0;
  mElectronTracks = 0;
  mNonVertexTracks = 0;
  mGoodGTracks = 0;
  mGoodFTracks = 0;
  mSplitTracks = 0;
  mSplitGoodTracks = 0;
  mSplitLoliGoodTracks = 0;
  mUncleanTracks = 0;
  mUncleanGoodTracks = 0;
  mLongTracks = 0;
  mLongTrackClusters = 0;
  mShortTracks = 0;
  mShortTrackClusters = 0;
  mTooShortTracks = 0;
  mMaxClusters = 0;

  mGoodGeantPoints = 0;
  mGoodFoundPoints = 0;

  if (geant) {
    // GEANT Table found

    St_DataSetIter  geantI(geant);
    
    GeantHitInit((St_g2t_ftp_hit *)geantI("g2t_ftp_hit"));
    GeantTrackInit((St_g2t_track *)geantI("g2t_track"), (St_g2t_ftp_hit *)geantI("g2t_ftp_hit"));

    St_ffs_gepoint *ffs_gepoint = (St_ffs_gepoint *)ftpc_data->Find("ffs_fgepoint");

    if (ffs_gepoint) {
      // event processed with fast simulator
      FastSimHitInit(ffs_gepoint);
      ParentTrackInit();
      CalcSplitTracks();
    }   
  }
  
  return;
}


void StFtpcTrackEvaluator::SetupHistos()
{
  // Sets up the histograms.
  
  if (strcmp(mWritePermission, "RECREATE") !=0 && strcmp(mWritePermission, "UPDATE") != 0) {
    LOG_WARN << "Wrong write permission! Has to be RECREATE or UPDATE. Set to RECREATE." << endm;
    mWritePermission = (Char_t*)"RECREATE";
  }

  if (strcmp(mWritePermission, "RECREATE") == 0) {
    CreateHistos();
  }

  else if (strcmp(mWritePermission, "UPDATE") == 0) {
    
    mNumGeantHits = (TH1F *)mFile->Get("num_ghits");
    mNumFoundHits = (TH1F *)mFile->Get("num_fhits");
    mNumGeantTracks = (TH1F *)mFile->Get("num_gtracks");
    mNumFoundTracks = (TH1F *)mFile->Get("num_ftracks");

    mNumFoundVertexTracks = (TH1F *)mFile->Get("num_fvtx");
    mNumFoundNonVertexTracks = (TH1F *)mFile->Get("num_fnonvtx");
    
    mNumLookLikeGoodTracks = (TH1F *)mFile->Get("num_loligood");
    mNumElectronTracks = (TH1F *)mFile->Get("num_electron");
    mNumNonVertexTracks = (TH1F *)mFile->Get("num_nvtx");
    mNumGoodGTracks = (TH1F *)mFile->Get("num_goodg");
    mNumGoodFTracks = (TH1F *)mFile->Get("num_goodf");
    mNumSplitTracks = (TH1F *)mFile->Get("num_split");
    mNumSplitGoodTracks = (TH1F *)mFile->Get("num_split_good");
    mNumSplitLoliGoodTracks = (TH1F *)mFile->Get("num_split_loligood");
    mNumUncleanTracks = (TH1F *)mFile->Get("num_unclean");
    mNumUncleanGoodTracks = (TH1F *)mFile->Get("num_unclean_good");
    mNumLongTracks = (TH1F *)mFile->Get("num_long");
    mNumLongTrackClusters  = (TH1F *)mFile->Get("num_longclus");
    mNumShortTracks = (TH1F *)mFile->Get("num_short");
    mNumTooShortTracks = (TH1F *)mFile->Get("num_tooshort");
    mNumShortTrackClusters  = (TH1F *)mFile->Get("num_shortclus");
    
    mNumGoodGeantPoints = (TH1F *)mFile->Get("good_pointsg");
    mNumGoodFoundPoints = (TH1F *)mFile->Get("good_pointsf");
    mGoodPointRatio = (TH1F *)mFile->Get("good_point_ratio");

    mGHitsOnTrack = (TH1F *)mFile->Get("geant_hits");
    mFHitsOnTrack = (TH1F *)mFile->Get("found_hits");

    mNumParents = (TH1F *)mFile->Get("num_parents");
    mNumWrongHits = (TH2F *)mFile->Get("wrong_hits");
    mNumWrongHitsAll = (TH1F *)mFile->Get("wrong_hits_all");

    mGoodRatio = (TH1F *)mFile->Get("good_ratio");
    mContamination = (TH1F *)mFile->Get("contamination");
    mContaWoSplit = (TH1F *)mFile->Get("conta_wo_split");

    mPtot = (TH2F *)mFile->Get("ptot");
    mPt = (TH2F *)mFile->Get("pt");
    mPx = (TH2F *)mFile->Get("px");
    mPy = (TH2F *)mFile->Get("py");
    mPz = (TH2F *)mFile->Get("pz");

    mPtotDiff = (TH1F *)mFile->Get("ptot_diff");
    mPtDiff = (TH1F *)mFile->Get("pt_diff");
    mPxDiff = (TH1F *)mFile->Get("px_diff");
    mPyDiff = (TH1F *)mFile->Get("py_diff");
    mPzDiff = (TH1F *)mFile->Get("pz_diff");

    mPtotAcc = (TH1F *)mFile->Get("ptot_acc");
    mPtAcc = (TH1F *)mFile->Get("pt_acc");
    mPxAcc = (TH1F *)mFile->Get("px_acc");
    mPyAcc = (TH1F *)mFile->Get("py_acc");
    mPzAcc = (TH1F *)mFile->Get("pz_acc");

    mPRelErr = (TH3F *)mFile->Get("p_rel_err");
    mPRelErrqok = (TH3F *)mFile->Get("p_rel_err_q_ok");
    mPRelDiff = (TH3F *)mFile->Get("p_rel_diff");

    mEtaNfhits = (TH2F *)mFile->Get("eta_fhits");
    mEtaNghits = (TH2F *)mFile->Get("eta_ghits");

    mPtEtaF = (TH2F *)mFile->Get("pt_etaf");
    mPtEtaFMes = (TH2F *)mFile->Get("pt_etaf_mes");
    mPtEtaGood = (TH2F *)mFile->Get("pt_eta_good");
    mPtEtaBad = (TH2F *)mFile->Get("pt_eta_bad");
    mPtEtaUnclean = (TH2F *)mFile->Get("pt_eta_unclean");
    mPtEtaMesUnclean = (TH2F *)mFile->Get("pt_eta_mes_unclean");
    mPtEtaUncleanGood = (TH2F *)mFile->Get("pt_eta_unclean_good");
    mPtEtaMesUncleanGood = (TH2F *)mFile->Get("pt_eta_mes_unclean_good");

    mPtEtaSplit = (TH2F *)mFile->Get("pt_eta_split");
    mPtEtaSplitGood = (TH2F *)mFile->Get("pt_eta_split_good");
    mPtEtaSplitLoliGood = (TH2F *)mFile->Get("pt_eta_split_loligood");
   
    mPtEtaGoodG = (TH2F *)mFile->Get("pt_eta_goodg");
    mPtEtaGoodF = (TH2F *)mFile->Get("pt_eta_goodf");
    mPtEtaGoodRatio = (TH2F *)mFile->Get("pt_eta_good_ratio");
    mPtEtaBadG = (TH2F *)mFile->Get("pt_eta_badg");
    mPtEtaBadF = (TH2F *)mFile->Get("pt_eta_badf");
    mPtEtaBadRatio = (TH2F *)mFile->Get("pt_eta_bad_ratio");

    mPtEtaFVtx = (TH2F *)mFile->Get("pt_eta_vtxf");
    mPtEtaLookLikeGood = (TH2F *)mFile->Get("pt_eta_loligood");
    mPtEtaContamination = (TH2F *)mFile->Get("pt_eta_contam");
    mPtEtaContaWoSplits = (TH2F *)mFile->Get("pt_eta_contam_wo_splits");

    mPtEta10PointTracks = (TH2F *)mFile->Get("pt_eta_10point_tracks");
    mPtEtaWrongCharge = (TH2F *)mFile->Get("pt_eta_wrong_charge");
    mPtEtaWrongPosCharge = (TH2F *)mFile->Get("pt_eta_wrong_positive_charge");

    mDcaFMainVertex = (TH1F *)mFile->Get("dca_mainf");
    mDcaFNonVertex = (TH1F *)mFile->Get("dca_nonf");
    mDcaGMainVertex = (TH1F *)mFile->Get("dca_maing");
    mDcaGNonVertex = (TH1F *)mFile->Get("dca_nong");

    mGLengthDistTrackAng = (TH2F *)mFile->Get("glength_ang");
    mGCircleDistTrackAng = (TH2F *)mFile->Get("gcircle_ang");
    mFLengthDistTrackAng = (TH2F *)mFile->Get("flength_ang");
    mFCircleDistTrackAng = (TH2F *)mFile->Get("fcircle_ang");

    mGTracklAngAll = (TH1F *)mFile->Get("gtrackl_angle_all");
    mGTrackAngAll = (TH1F *)mFile->Get("gtrack_angle_all");
    mGCircleDistAll = (TH1F *)mFile->Get("gcircle_dist_all");
    mGLengthDistAll = (TH1F *)mFile->Get("glength_dist_all");

    mGTrackAng = (TH2F *)mFile->Get("gtrack_angle");
    mGCircleDist = (TH2F *)mFile->Get("gcircle_dist");
    mGLengthDist = (TH2F *)mFile->Get("glength_dist");

    mGCircleLength = (TH2F *)mFile->Get("gcircle_length");

    mFTracklAngAll = (TH1F *)mFile->Get("ftrackl_angle_all");
    mFTrackAngAll = (TH1F *)mFile->Get("ftrack_angle_all");
    mFCircleDistAll = (TH1F *)mFile->Get("fcircle_dist_all");
    mFLengthDistAll = (TH1F *)mFile->Get("flength_dist_all");

    mFTrackAng = (TH2F *)mFile->Get("ftrack_angle");
    mFCircleDist = (TH2F *)mFile->Get("fcircle_dist");
    mFLengthDist = (TH2F *)mFile->Get("flength_dist");

    mFCircleLength = (TH2F *)mFile->Get("fcircle_length");
    
    mPRatioDist = (TH2F *)mFile->Get("pratio_dist");
    mPRatioDistSplit = (TH2F *)mFile->Get("pratio_dist_split");

    mSetupTime = (TH1F*)mFile->Get("setup_time");
    mTrackingTime = (TH1F*)mFile->Get("tracking_time");
    mExtensionTime = (TH1F*)mFile->Get("extension_time");
    mSplitTime = (TH1F*)mFile->Get("split_time");
    mFitTime = (TH1F*)mFile->Get("fit_time");
    mTotalTime = (TH1F*)mFile->Get("total_time");
  }

  return;
}


void StFtpcTrackEvaluator::CreateHistos()
{
  // Create histograms.

  mNumGeantHits = new TH1F("num_ghits", "Simulated clusters", 100, 0., 40000.);
  mNumFoundHits = new TH1F("num_fhits", "Found clusters", 100, 0., 40000.);
  mNumGeantTracks = new TH1F("num_gtracks", "Simulated tracks", 100, 0., 5000.);
  mNumFoundTracks = new TH1F("num_ftracks", "Found tracks", 100, 0., 2500.);
  
  mNumFoundVertexTracks = new TH1F("num_fvtx", "Found main vertex tracks", 100, 0., 2500.);
  mNumFoundNonVertexTracks = new TH1F("num_fnonvtx", "Found non main vertex tracks", 100, 0., 2500.);

  mNumLookLikeGoodTracks = new TH1F("num_loligood", "Tracks which look like good tracks", 100, 0., 500.);
  mNumElectronTracks = new TH1F("num_electron", "Electrons", 100, 0., 500.);
  mNumNonVertexTracks = new TH1F("num_nvtx", "Non main vertex tracks", 100, 0., 1000.);
  mNumGoodGTracks = new TH1F("num_goodg", "Good geant tracks", 100, 0., 1000.);
  mNumGoodFTracks = new TH1F("num_goodf", "Good found tracks", 100, 0., 1000.);
  mGoodRatio = new TH1F("good_ratio", "Ratio of good found tracks to good geant tracks", 100, 0., 2.);
  mContamination = new TH1F("contamination", "Ratio of loli good tracks to good main vertex tracks", 100, 0., 2.);
  mContaWoSplit = new TH1F("conta_wo_split", "Ratio of loli good minus split tracks to good main vertex tracks", 100, 0., 2.);
  mNumSplitTracks = new TH1F("num_split", "Split tracks", 100, 0., 300.);
  mNumSplitGoodTracks = new TH1F("num_split_good", "Split good tracks", 100, 0., 300.);
  mNumSplitLoliGoodTracks = new TH1F("num_split_loligood", "Split look-like good tracks", 100, 0., 300.);
  mNumUncleanTracks = new TH1F("num_unclean", "Unclean tracks", 100, 0., 300.);
  mNumUncleanGoodTracks = new TH1F("num_unclean_good", "Unclean good tracks", 100, 0., 300.);
  mNumLongTracks = new TH1F("num_long", "Simulated tracks with more than 10 clusters", 100, 0., 300.);
  mNumShortTracks = new TH1F("num_short", "Simulated tracks with less than 5 clusters", 100, 0., 5000.);
  mNumTooShortTracks = new TH1F("num_tooshort", "Tracks which should be longer", 100, 0., 1000.);

  mNumShortTrackClusters = new TH1F("num_shortclus", "Clusters on tracks with less than 5 clusters", 100, 0., 12000.);
  mNumLongTrackClusters = new TH1F("num_longclus", "Clusters on tracks with more than 10 clusters", 100, 0., 1600.);

  mNumGoodGeantPoints = new TH1F("good_pointsg", "Number of points on good geant tracks", 100, 0., 40000.);
  mNumGoodFoundPoints = new TH1F("good_pointsf", "Number of points on good found tracks", 100, 0., 40000.);
  mGoodPointRatio = new TH1F("good_point_ratio", "Ratio of good points", 100, 0., 1.);
  
  mFHitsOnTrack = new TH1F("found_hits", "Found clusters", StFtpcTrackingParams::Instance()->NumberOfPadRowsPerSide(), 0.5, StFtpcTrackingParams::Instance()->NumberOfPadRowsPerSide() + 0.5);
  mGHitsOnTrack = new TH1F("geant_hits", "Geant clusters", 50, 0.5, 50.5);
  
  mNumParents   = new TH1F("num_parents", "Parent tracks", StFtpcTrackingParams::Instance()->NumberOfPadRowsPerSide(), 0.5, StFtpcTrackingParams::Instance()->NumberOfPadRowsPerSide() + 0.5);
  mNumWrongHits = new TH2F("wrong_hits", "Wrong clusters on tracks", StFtpcTrackingParams::Instance()->NumberOfPadRowsPerSide(), -0.5, 9.5, StFtpcTrackingParams::Instance()->NumberOfPadRowsPerSide(), 0.5, StFtpcTrackingParams::Instance()->NumberOfPadRowsPerSide() + 0.5);
  mNumWrongHitsAll = new TH1F("wrong_hits_all", "Wrong clusters on tracks", StFtpcTrackingParams::Instance()->NumberOfPadRowsPerSide(), -0.5, 9.5);  
  
  mPtot = new TH2F("ptot", "Total momentum", 60, 0., 30., 60, 0., 30.);
  mPt = new TH2F("pt", "Transverse momentum", 50, 0., 2.5, 50, 0., 2.5);
  mPx = new TH2F("px", "Momentum in x direction", 60, -1.5, 1.5, 60, -1.5, 1.5);
  mPy = new TH2F("py", "Momentum in y direction", 60, -1.5, 1.5, 60, -1.5, 1.5);
  mPz = new TH2F("pz", "Momentum in z direction", 100, -25., 25., 100, -25., 25.);
  
  mPtotDiff = new TH1F("ptot_diff", "Rel. difference in p_tot", 200, -1., 1.);
  mPtDiff = new TH1F("pt_diff", "Rel. difference in p_t", 200, -1., 1.);
  mPxDiff = new TH1F("px_diff", "Rel. difference in p_x", 200, -1., 1.);
  mPyDiff = new TH1F("py_diff", "Rel. difference in p_y", 200, -1., 1.);
  mPzDiff = new TH1F("pz_diff", "Rel. difference in p_z", 200, -1., 1.);
  
  mPtotAcc = new TH1F("ptot_acc", "Rel. accuracy in p_tot", 200, 0., 5.);
  mPtAcc = new TH1F("pt_acc", "Rel. accuracy in p_t", 200, 0., 5.);
  mPxAcc = new TH1F("px_acc", "Rel. accuracy in p_x", 200, 0., 5.);
  mPyAcc = new TH1F("py_acc", "Rel. accuracy in p_y", 200, 0., 5.);
  mPzAcc = new TH1F("pz_acc", "Rel. accuracy in p_z", 200, 0., 5.);

  mPRelErr = new TH3F("p_rel_err", "Rel. error of p_tot", 48, 2., 4.4, 25, 0., 5., 50, -1., 1.);
  mPRelErrqok = new TH3F("p_rel_err_q_ok", "Rel. error of p_tot (charge ok)", 48, 2., 4.4, 25, 0., 5., 50, -1., 1.);
  mPRelDiff = new TH3F("p_rel_diff", "Rel. differnce of p_tot", 48, 2., 4.4, 25, 0., 5., 50, -1., 1.);
  
  mEtaNghits = new TH2F("eta_ghits", "Geant tracks", StFtpcTrackingParams::Instance()->NumberOfPadRowsPerSide(), 0.5, StFtpcTrackingParams::Instance()->NumberOfPadRowsPerSide() + 0.5, 96, 2.0, 4.4);
  mEtaNfhits = new TH2F("eta_fhits", "Found tracks", StFtpcTrackingParams::Instance()->NumberOfPadRowsPerSide(), 0.5, StFtpcTrackingParams::Instance()->NumberOfPadRowsPerSide() + 0.5, 96, 2.0, 4.4);

  mPtEtaF = new TH2F("pt_etaf", "Found tracks", 96, 2.0, 4.4, 50, 0., 5.);
  mPtEtaFMes = new TH2F("pt_etaf_mes", "Found tracks", 96, 2.0, 4.4, 50, 0., 5.);
  mPtEtaGood = new TH2F("pt_eta_good", "Ratio of good tracks", 96, 2.0, 4.4, 50, 0., 5.);
  mPtEtaBad = new TH2F("pt_eta_bad", "Ratio of bad tracks", 96, 2.0, 4.4, 50, 0., 5.);
  mPtEtaUnclean = new TH2F("pt_eta_unclean", "Unclean tracks", 96, 2.0, 4.4, 50, 0., 5.);
  mPtEtaMesUnclean = new TH2F("pt_eta_mes_unclean", "Unclean tracks", 96, 2.0, 4.4, 50, 0., 5.);
  mPtEtaUncleanGood = new TH2F("pt_eta_unclean_good", "Unclean good tracks", 96, 2.0, 4.4, 50, 0., 5.);
  mPtEtaMesUncleanGood = new TH2F("pt_eta_mes_unclean_good", "Unclean good tracks", 96, 2.0, 4.4, 50, 0., 5.);

  mPtEtaSplit =  new TH2F("pt_eta_split", "Split tracks", 96, 2.0, 4.4, 50, 0., 5.);
  mPtEtaSplitGood =  new TH2F("pt_eta_split_good", "Split good tracks", 96, 2.0, 4.4, 50, 0., 5.);
  mPtEtaSplitLoliGood =  new TH2F("pt_eta_split_loligood", "Split look-like good tracks", 96, 2.0, 4.4, 50, 0., 5.);

  mPtEtaGoodG = new TH2F("pt_eta_goodg", "Good geant tracks", 96, 2.0, 4.4, 50, 0., 5.);
  mPtEtaGoodF = new TH2F("pt_eta_goodf", "Good found tracks", 96, 2.0, 4.4, 50, 0., 5.);
  mPtEtaGoodRatio = new TH2F("pt_eta_good_ratio", "Ratio of good found tracks", 96, 2.0, 4.4, 50, 0., 5.);
  mPtEtaBadG = new TH2F("pt_eta_badg", "Bad geant tracks", 96, 2.0, 4.4, 50, 0., 5.);
  mPtEtaBadF = new TH2F("pt_eta_badf", "Bad found tracks", 96, 2.0, 4.4, 50, 0., 5.);
  mPtEtaBadRatio = new TH2F("pt_eta_bad_ratio", "Ratio of bad found tracks", 96, 2.0, 4.4, 50, 0., 5.);

  mPtEtaFVtx = new TH2F("pt_eta_vtxf", "Found tracks with main vertex tag", 96, 2.0, 4.4, 50, 0., 5.);
  mPtEtaLookLikeGood = new TH2F("pt_eta_loligood", "Found tracks looking good but are not", 96, 2.0, 4.4, 50, 0., 5.);
  mPtEtaContamination = new TH2F("pt_eta_contam", "Contamination", 96, 2.0, 4.4, 50, 0., 5.);
  mPtEtaContaWoSplits = new TH2F("pt_eta_contam_wo_splits", "Contamination without splits", 96, 2.0, 4.4, 50, 0., 5.);

  mPtEta10PointTracks = new TH2F("pt_eta_10point_tracks", "tracks with 10 points", 96, 2.0, 4.4, 50, 0., 5.);
  mPtEtaWrongCharge = new TH2F("pt_eta_wrong_charge", "tracks with wrong charge", 96, 2.0, 4.4, 50, 0., 5.);
  mPtEtaWrongPosCharge = new TH2F("pt_eta_wrong_positive_charge", "tracks with wrong positiv charge", 96, 2.0, 4.4, 50, 0., 5.);

  mDcaFMainVertex = new TH1F("dca_mainf", "Found main vertex tracks", 100, 0., 10.);
  mDcaFNonVertex = new TH1F("dca_nonf", "Found non vertex tracks", 100, 0., 10.);
  mDcaGMainVertex = new TH1F("dca_maing", "Geant main vertex tracks", 100, 0., 10.);
  mDcaGNonVertex = new TH1F("dca_nong", "Geant non vertex tracks", 100, 0., 10.);

  mGLengthDistTrackAng = new TH2F("glength_ang", "Geant tracks", 100, 0., 0.1, 100, 0., 50.);
  mGCircleDistTrackAng = new TH2F("gcircle_ang", "Geant tracks", 100, 0., 0.1, 100, 0., 0.01);
  mFLengthDistTrackAng = new TH2F("flength_ang", "Found tracks", 100, 0., 0.1, 100, 0., 50.);
  mFCircleDistTrackAng = new TH2F("fcircle_ang", "Found tracks", 100, 0., 0.1, 100, 0., 0.01);
  
  mGTracklAngAll = new TH1F("gtrackl_angle_all", "Geant tracks", 100, 0., 0.1);
  mGTrackAngAll = new TH1F("gtrack_angle_all", "Geant tracks", 100, 0., 0.5);
  mGCircleDistAll = new TH1F("gcircle_dist_all", "Geant tracks", 100, 0., 0.015);
  mGLengthDistAll = new TH1F("glength_dist_all", "Geant tracks", 100, 0., 200);

  mGTrackAng = new TH2F("gtrack_angle", "Geant tracks", 100, 0., 0.5, 8, 2.5, 10.5);
  mGCircleDist = new TH2F("gcircle_dist", "Geant tracks", 100, 0., 0.015, 7, 3.5, 10.5);
  mGLengthDist = new TH2F("glength_dist", "Geant tracks", 100, 0., 200, 7, 3.5, 10.5);

  mGCircleLength = new TH2F("gcircle_length", "Geant tracks", 100, 0., 40., 100, 0., 0.015);

  mFTracklAngAll = new TH1F("ftrackl_angle_all", "Found tracks", 100, 0., 0.1);
  mFTrackAngAll = new TH1F("ftrack_angle_all", "Found tracks", 100, 0., 0.5);
  mFCircleDistAll = new TH1F("fcircle_dist_all", "Found tracks", 100, 0., 0.015);
  mFLengthDistAll = new TH1F("flength_dist_all", "Found tracks", 100, 0., 200);

  mFTrackAng = new TH2F("ftrack_angle", "Found tracks", 100, 0., 0.5, 8, 2.5, 10.5);
  mFCircleDist = new TH2F("fcircle_dist", "Found tracks", 100, 0., 0.015, 7, 3.5, 10.5);
  mFLengthDist = new TH2F("flength_dist", "Found tracks", 100, 0., 200, 7, 3.5, 10.5);

  mFCircleLength = new TH2F("fcircle_length", "Found tracks", 100, 0., 40., 100, 0., 0.015);

  mPRatioDist = new TH2F("pratio_dist", "Found track pairs", 100, 0., 2., 37, 0.25, 1.0);
  mPRatioDistSplit = new TH2F("pratio_dist_split", "Split track pairs", 100, 0., 2., 37, 0.25, 1.0);

  mSetupTime = new TH1F("setup_time", "Setup time", 1000, 0., 2.);
  mTrackingTime = new TH1F("tracking_time", "Tracking time", 1000, 0., 2.);
  mExtensionTime = new TH1F("extension_time", "Extension time", 1000, 0., 2.);
  mSplitTime = new TH1F("split_time", "Splits time", 1000, 0., 2.);
  mFitTime = new TH1F("fit_time", "Fit time", 1000, 0., 2.);
  mTotalTime = new TH1F("total_time", "Total time", 1000, 0., 2.);

  ((TAxis *)mNumGeantHits->GetXaxis())->SetTitle("'F# of clusters");
  ((TAxis *)mNumGeantHits->GetYaxis())->SetTitle("'F# of events");
  ((TAxis *)mNumFoundHits->GetXaxis())->SetTitle("'F# of clusters");
  ((TAxis *)mNumFoundHits->GetYaxis())->SetTitle("'F# of events");
  ((TAxis *)mNumGeantTracks->GetXaxis())->SetTitle("'F# of tracks");
  ((TAxis *)mNumGeantTracks->GetYaxis())->SetTitle("'F# of events");
  ((TAxis *)mNumFoundTracks->GetXaxis())->SetTitle("'F# of tracks");
  ((TAxis *)mNumFoundTracks->GetYaxis())->SetTitle("'F# of events");

  ((TAxis *)mNumFoundVertexTracks->GetXaxis())->SetTitle("'F# of tracks");
  ((TAxis *)mNumFoundVertexTracks->GetYaxis())->SetTitle("'F# of events");
  ((TAxis *)mNumFoundNonVertexTracks->GetXaxis())->SetTitle("'F# of tracks");
  ((TAxis *)mNumFoundNonVertexTracks->GetYaxis())->SetTitle("'F# of events");

  ((TAxis *)mNumLookLikeGoodTracks->GetXaxis())->SetTitle("'F# of tracks");
  ((TAxis *)mNumLookLikeGoodTracks->GetYaxis())->SetTitle("'F# of events");

  ((TAxis *)mNumElectronTracks->GetXaxis())->SetTitle("'F# of tracks");
  ((TAxis *)mNumElectronTracks->GetYaxis())->SetTitle("'F# of events");
  ((TAxis *)mNumNonVertexTracks->GetXaxis())->SetTitle("'F# of tracks");
  ((TAxis *)mNumNonVertexTracks->GetYaxis())->SetTitle("'F# of events");
  ((TAxis *)mNumGoodGTracks->GetXaxis())->SetTitle("'F# of tracks");
  ((TAxis *)mNumGoodGTracks->GetYaxis())->SetTitle("'F# of events");
  ((TAxis *)mNumGoodFTracks->GetXaxis())->SetTitle("'F# of tracks");
  ((TAxis *)mNumGoodFTracks->GetYaxis())->SetTitle("'F# of events");
  ((TAxis *)mGoodRatio->GetXaxis())->SetTitle("ratio");
  ((TAxis *)mGoodRatio->GetYaxis())->SetTitle("'F# of events");
  ((TAxis *)mContamination->GetXaxis())->SetTitle("ratio");
  ((TAxis *)mContamination->GetYaxis())->SetTitle("'F# of events");
  ((TAxis *)mContaWoSplit->GetXaxis())->SetTitle("ratio");
  ((TAxis *)mContaWoSplit->GetYaxis())->SetTitle("'F# of events");

  ((TAxis *)mNumSplitTracks->GetXaxis())->SetTitle("'F# of tracks");
  ((TAxis *)mNumSplitTracks->GetYaxis())->SetTitle("'F# of events");
  ((TAxis *)mNumSplitGoodTracks->GetXaxis())->SetTitle("'F# of tracks");
  ((TAxis *)mNumSplitGoodTracks->GetYaxis())->SetTitle("'F# of events");
  ((TAxis *)mNumSplitLoliGoodTracks->GetXaxis())->SetTitle("'F# of tracks");
  ((TAxis *)mNumSplitLoliGoodTracks->GetYaxis())->SetTitle("'F# of events");
  ((TAxis *)mNumUncleanTracks->GetXaxis())->SetTitle("'F# of tracks");
  ((TAxis *)mNumUncleanTracks->GetYaxis())->SetTitle("'F# of events");
  ((TAxis *)mNumUncleanGoodTracks->GetXaxis())->SetTitle("'F# of tracks");
  ((TAxis *)mNumUncleanGoodTracks->GetYaxis())->SetTitle("'F# of events");
  ((TAxis *)mNumLongTracks->GetXaxis())->SetTitle("'F# of tracks");
  ((TAxis *)mNumLongTracks->GetYaxis())->SetTitle("'F# of events");
  ((TAxis *)mNumLongTrackClusters->GetXaxis())->SetTitle("'F# of clusters");
  ((TAxis *)mNumLongTrackClusters->GetYaxis())->SetTitle("'F# of events");
  ((TAxis *)mNumShortTracks->GetXaxis())->SetTitle("'F# of tracks");
  ((TAxis *)mNumShortTracks->GetYaxis())->SetTitle("'F# of events");
  ((TAxis *)mNumTooShortTracks->GetXaxis())->SetTitle("'F# of tracks");
  ((TAxis *)mNumTooShortTracks->GetYaxis())->SetTitle("'F# of events");
  ((TAxis *)mNumShortTrackClusters->GetXaxis())->SetTitle("'F# of clusters");
  ((TAxis *)mNumShortTrackClusters->GetYaxis())->SetTitle("'F# of events");

  ((TAxis *)mFHitsOnTrack->GetXaxis())->SetTitle("'F# of clusters on track");
  ((TAxis *)mFHitsOnTrack->GetYaxis())->SetTitle("'F# of found tracks");
  ((TAxis *)mGHitsOnTrack->GetXaxis())->SetTitle("'F# of clusters on track");
  ((TAxis *)mGHitsOnTrack->GetYaxis())->SetTitle("'F# of found tracks");

  ((TAxis *)mNumGoodGeantPoints->GetXaxis())->SetTitle("'F# of clusters");
  ((TAxis *)mNumGoodGeantPoints->GetYaxis())->SetTitle("'F# of events");
  ((TAxis *)mNumGoodFoundPoints->GetXaxis())->SetTitle("'F# of clusters");
  ((TAxis *)mNumGoodFoundPoints->GetYaxis())->SetTitle("'F# of events");
  ((TAxis *)mGoodPointRatio->GetXaxis())->SetTitle("Ratio");
  ((TAxis *)mGoodPointRatio->GetYaxis())->SetTitle("'F# of events");
  
  ((TAxis *)mNumParents->GetXaxis())->SetTitle("'F# of parent tracks");
  ((TAxis *)mNumParents->GetYaxis())->SetTitle("'F# of found tracks");
  ((TAxis *)mNumWrongHits->GetXaxis())->SetTitle("'F# of wrong clusters on track");
  ((TAxis *)mNumWrongHits->GetXaxis())->SetTitle("'F# of found clusters on track");
  ((TAxis *)mNumWrongHits->GetZaxis())->SetTitle("'F# of found tracks");
  ((TAxis *)mNumWrongHitsAll->GetXaxis())->SetTitle("'F# of wrong clusters on track");
  ((TAxis *)mNumWrongHitsAll->GetYaxis())->SetTitle("'F# of found tracks");
  
  ((TAxis *)mPtot->GetXaxis())->SetTitle("momentum of geant track");
  ((TAxis *)mPtot->GetYaxis())->SetTitle("momentum of found track");
  ((TAxis *)mPtot->GetZaxis())->SetTitle("'F# of found tracks");
  ((TAxis *)mPt->GetXaxis())->SetTitle("momentum of geant track");
  ((TAxis *)mPt->GetYaxis())->SetTitle("momentum of found track");
  ((TAxis *)mPt->GetZaxis())->SetTitle("'F# of found tracks");
  ((TAxis *)mPx->GetXaxis())->SetTitle("momentum of geant track");
  ((TAxis *)mPx->GetYaxis())->SetTitle("momentum of found track");
  ((TAxis *)mPx->GetZaxis())->SetTitle("'F# of found tracks");
  ((TAxis *)mPy->GetXaxis())->SetTitle("momentum of geant track");
  ((TAxis *)mPy->GetYaxis())->SetTitle("momentum of found track");
  ((TAxis *)mPy->GetZaxis())->SetTitle("'F# of found tracks");
  ((TAxis *)mPz->GetXaxis())->SetTitle("momentum of geant track");
  ((TAxis *)mPz->GetYaxis())->SetTitle("momentum of found track");
  ((TAxis *)mPz->GetZaxis())->SetTitle("'F# of found tracks");
  
  ((TAxis *)mPtotDiff->GetXaxis())->SetTitle("rel. difference");
  ((TAxis *)mPtotDiff->GetYaxis())->SetTitle("'F# of found tracks");
  ((TAxis *)mPtDiff->GetXaxis())->SetTitle("rel. difference");
  ((TAxis *)mPtDiff->GetYaxis())->SetTitle("'F# of found tracks");
  ((TAxis *)mPxDiff->GetXaxis())->SetTitle("rel. difference");
  ((TAxis *)mPxDiff->GetYaxis())->SetTitle("'F# of found tracks");
  ((TAxis *)mPyDiff->GetXaxis())->SetTitle("rel. difference");
  ((TAxis *)mPyDiff->GetYaxis())->SetTitle("'F# of found tracks");
  ((TAxis *)mPzDiff->GetXaxis())->SetTitle("rel. difference");
  ((TAxis *)mPzDiff->GetYaxis())->SetTitle("'F# of found tracks");
  
  ((TAxis *)mPtotAcc->GetXaxis())->SetTitle("rel. accuracy");
  ((TAxis *)mPtotAcc->GetYaxis())->SetTitle("'F# of found tracks");
  ((TAxis *)mPtAcc->GetXaxis())->SetTitle("rel. accuracy");
  ((TAxis *)mPtAcc->GetYaxis())->SetTitle("'F# of found tracks");
  ((TAxis *)mPxAcc->GetXaxis())->SetTitle("rel. accuracy");
  ((TAxis *)mPxAcc->GetYaxis())->SetTitle("'F# of found tracks");
  ((TAxis *)mPyAcc->GetXaxis())->SetTitle("rel. accuracy");
  ((TAxis *)mPyAcc->GetYaxis())->SetTitle("'F# of found tracks");
  ((TAxis *)mPzAcc->GetXaxis())->SetTitle("rel. accuracy");
  ((TAxis *)mPzAcc->GetYaxis())->SetTitle("'F# of found tracks");

  ((TAxis *)mDcaFMainVertex->GetXaxis())->SetTitle("dca [cm]");
  ((TAxis *)mDcaFMainVertex->GetYaxis())->SetTitle("'F# of tracks");
  ((TAxis *)mDcaFNonVertex->GetXaxis())->SetTitle("dca [cm]");
  ((TAxis *)mDcaFNonVertex->GetYaxis())->SetTitle("'F# of tracks");
  ((TAxis *)mDcaGMainVertex->GetXaxis())->SetTitle("dca [cm]");
  ((TAxis *)mDcaGMainVertex->GetYaxis())->SetTitle("'F# of tracks");
  ((TAxis *)mDcaGNonVertex->GetXaxis())->SetTitle("dca [cm]");
  ((TAxis *)mDcaGNonVertex->GetYaxis())->SetTitle("'F# of tracks");

  ((TAxis *)mGTracklAngAll->GetXaxis())->SetTitle("angle of tracklet");
  ((TAxis *)mGTracklAngAll->GetYaxis())->SetTitle("'F# of tracklets");
  ((TAxis *)mGTrackAngAll->GetXaxis())->SetTitle("angle of track endings");
  ((TAxis *)mGTrackAngAll->GetYaxis())->SetTitle("'F# of track endings");
  ((TAxis *)mGCircleDistAll->GetXaxis())->SetTitle("circle distance to track");
  ((TAxis *)mGCircleDistAll->GetYaxis())->SetTitle("'F# of track endings");
  ((TAxis *)mGLengthDistAll->GetXaxis())->SetTitle("length distance to track");
  ((TAxis *)mGLengthDistAll->GetYaxis())->SetTitle("'F# of track endings");

  ((TAxis *)mGTrackAng->GetXaxis())->SetTitle("angle of track endings");
  ((TAxis *)mGTrackAng->GetYaxis())->SetTitle("'F# of last point");
  ((TAxis *)mGTrackAng->GetZaxis())->SetTitle("'F# of track endings");
  ((TAxis *)mGCircleDist->GetXaxis())->SetTitle("circle distance to track");
  ((TAxis *)mGCircleDist->GetYaxis())->SetTitle("'F# of last point");
  ((TAxis *)mGCircleDist->GetZaxis())->SetTitle("'F# of track endings");
  ((TAxis *)mGLengthDist->GetXaxis())->SetTitle("length distance to track");
  ((TAxis *)mGLengthDist->GetYaxis())->SetTitle("'F# of last point");
  ((TAxis *)mGLengthDist->GetZaxis())->SetTitle("'F# of track endings");

  ((TAxis *)mGCircleLength->GetXaxis())->SetTitle("length distance");
  ((TAxis *)mGCircleLength->GetYaxis())->SetTitle("circle distance");
  ((TAxis *)mGCircleLength->GetZaxis())->SetTitle("'F# of track endings");

  ((TAxis *)mFTracklAngAll->GetXaxis())->SetTitle("angle of tracklet");
  ((TAxis *)mFTracklAngAll->GetYaxis())->SetTitle("'F# of tracklets");
  ((TAxis *)mFTrackAngAll->GetXaxis())->SetTitle("angle of track endings");
  ((TAxis *)mFTrackAngAll->GetYaxis())->SetTitle("'F# of track endings");
  ((TAxis *)mFCircleDistAll->GetXaxis())->SetTitle("circle distance to track");
  ((TAxis *)mFCircleDistAll->GetYaxis())->SetTitle("'F# of track endings");
  ((TAxis *)mFLengthDistAll->GetXaxis())->SetTitle("length distance to track");
  ((TAxis *)mFLengthDistAll->GetYaxis())->SetTitle("'F# of track endings");

  ((TAxis *)mFTrackAng->GetXaxis())->SetTitle("angle of track endings");
  ((TAxis *)mFTrackAng->GetYaxis())->SetTitle("'F# of last point");
  ((TAxis *)mFTrackAng->GetZaxis())->SetTitle("'F# of track endings");
  ((TAxis *)mFCircleDist->GetXaxis())->SetTitle("circle distance to track");
  ((TAxis *)mFCircleDist->GetYaxis())->SetTitle("'F# of last point");
  ((TAxis *)mFCircleDist->GetZaxis())->SetTitle("'F# of track endings");
  ((TAxis *)mFLengthDist->GetXaxis())->SetTitle("length distance to track");
  ((TAxis *)mFLengthDist->GetYaxis())->SetTitle("'F# of last point");
  ((TAxis *)mFLengthDist->GetZaxis())->SetTitle("'F# of track endings");

  ((TAxis *)mFCircleLength->GetXaxis())->SetTitle("length distance");
  ((TAxis *)mFCircleLength->GetYaxis())->SetTitle("circle distance");
  ((TAxis *)mFCircleLength->GetZaxis())->SetTitle("'F# of track endings");

  ((TAxis *)mPRatioDist->GetXaxis())->SetTitle("distance [cm]");
  ((TAxis *)mPRatioDist->GetYaxis())->SetTitle("(p1 + p2) / (p1max + p2max)");
  ((TAxis *)mPRatioDist->GetZaxis())->SetTitle("'F# of pairs");
  ((TAxis *)mPRatioDistSplit->GetXaxis())->SetTitle("distance [GeV]");
  ((TAxis *)mPRatioDistSplit->GetYaxis())->SetTitle("(p1 + p2) / (p1max + p2max)");
  ((TAxis *)mPRatioDistSplit->GetZaxis())->SetTitle("'F# of pairs");

  ((TAxis *)mEtaNghits->GetXaxis())->SetTitle("'F# of geant clusters");
  ((TAxis *)mEtaNghits->GetYaxis())->SetTitle("`h#");
  ((TAxis *)mEtaNfhits->GetXaxis())->SetTitle("'F# of found clusters");
  ((TAxis *)mEtaNfhits->GetYaxis())->SetTitle("`h#");

  ((TAxis *)mPtEtaF->GetXaxis())->SetTitle("`h# (of parent)");
  ((TAxis *)mPtEtaF->GetYaxis())->SetTitle("p?'c#! [GeV] (of parent)");
  ((TAxis *)mPtEtaF->GetZaxis())->SetTitle("'F# of tracks");

  ((TAxis *)mPtEtaFMes->GetXaxis())->SetTitle("`h# (measured)");    
  ((TAxis *)mPtEtaFMes->GetYaxis())->SetTitle("p?'c#! [GeV] (measured)");
  ((TAxis *)mPtEtaFMes->GetZaxis())->SetTitle("'F# of tracks");

  ((TAxis *)mPtEtaGood->GetXaxis())->SetTitle("`h# (of parent)");
  ((TAxis *)mPtEtaGood->GetYaxis())->SetTitle("p?'c#! [GeV] (of parent)");
  ((TAxis *)mPtEtaGood->GetZaxis())->SetTitle("'F# of tracks");
  ((TAxis *)mPtEtaBad->GetXaxis())->SetTitle("`h# (of parent)");
  ((TAxis *)mPtEtaBad->GetYaxis())->SetTitle("p?'c#! [GeV] (of parent)");
  ((TAxis *)mPtEtaBad->GetZaxis())->SetTitle("'F# of tracks");
  ((TAxis *)mPtEtaUnclean->GetXaxis())->SetTitle("`h# (of parent)");
  ((TAxis *)mPtEtaUnclean->GetYaxis())->SetTitle("p?'c#! [GeV] (of parent)");
  ((TAxis *)mPtEtaUnclean->GetZaxis())->SetTitle("'F# of tracks");
  ((TAxis *)mPtEtaMesUnclean->GetXaxis())->SetTitle("`h# (of parent)");
  ((TAxis *)mPtEtaMesUnclean->GetYaxis())->SetTitle("p?'c#! [GeV] (of parent)");
  ((TAxis *)mPtEtaMesUnclean->GetZaxis())->SetTitle("'F# of tracks");
  ((TAxis *)mPtEtaUncleanGood->GetXaxis())->SetTitle("`h# (of parent)");
  ((TAxis *)mPtEtaUncleanGood->GetYaxis())->SetTitle("p?'c#! [GeV] (of parent)");
  ((TAxis *)mPtEtaUncleanGood->GetZaxis())->SetTitle("'F# of tracks");
  ((TAxis *)mPtEtaMesUncleanGood->GetXaxis())->SetTitle("`h# (of parent)");
  ((TAxis *)mPtEtaMesUncleanGood->GetYaxis())->SetTitle("p?'c#! [GeV] (of parent)");
  ((TAxis *)mPtEtaMesUncleanGood->GetZaxis())->SetTitle("'F# of tracks");

  ((TAxis *)mPtEtaSplit->GetXaxis())->SetTitle("`h# (of parent)");
  ((TAxis *)mPtEtaSplit->GetYaxis())->SetTitle("p?'c#! [GeV] (of parent)");
  ((TAxis *)mPtEtaSplit->GetZaxis())->SetTitle("'F# of tracks");
  ((TAxis *)mPtEtaSplitGood->GetXaxis())->SetTitle("`h# (of parent)");
  ((TAxis *)mPtEtaSplitGood->GetYaxis())->SetTitle("p?'c#! [GeV] (of parent)");
  ((TAxis *)mPtEtaSplitGood->GetZaxis())->SetTitle("'F# of tracks");
  ((TAxis *)mPtEtaSplitLoliGood->GetXaxis())->SetTitle("`h# (of parent)");
  ((TAxis *)mPtEtaSplitLoliGood->GetYaxis())->SetTitle("p?'c#! [GeV] (of parent)");
  ((TAxis *)mPtEtaSplitLoliGood->GetZaxis())->SetTitle("'F# of tracks");

  ((TAxis *)mPtEtaFVtx->GetXaxis())->SetTitle("`h# (of parent)");
  ((TAxis *)mPtEtaFVtx->GetYaxis())->SetTitle("p?'c#! [GeV] (of parent)");
  ((TAxis *)mPtEtaFVtx->GetZaxis())->SetTitle("'F# of tracks");
  ((TAxis *)mPtEtaLookLikeGood->GetXaxis())->SetTitle("`h# (of parent)");
  ((TAxis *)mPtEtaLookLikeGood->GetYaxis())->SetTitle("p?'c#! [GeV] (of parent)");
  ((TAxis *)mPtEtaLookLikeGood->GetZaxis())->SetTitle("'F# of tracks");
  ((TAxis *)mPtEtaContamination->GetXaxis())->SetTitle("`h# (of parent)");
  ((TAxis *)mPtEtaContamination->GetYaxis())->SetTitle("p?'c#! [GeV] (of parent)");
  ((TAxis *)mPtEtaContamination->GetZaxis())->SetTitle("'F# of tracks");
  ((TAxis *)mPtEtaContaWoSplits->GetXaxis())->SetTitle("`h# (of parent)");
  ((TAxis *)mPtEtaContaWoSplits->GetYaxis())->SetTitle("p?'c#! [GeV] (of parent)");
  ((TAxis *)mPtEtaContaWoSplits->GetZaxis())->SetTitle("'F# of tracks");

  ((TAxis *)mPtEta10PointTracks->GetXaxis())->SetTitle("`h# (of parent)");
  ((TAxis *)mPtEta10PointTracks->GetYaxis())->SetTitle("p?'c#! [GeV] (of parent)");
  ((TAxis *)mPtEta10PointTracks->GetZaxis())->SetTitle("'F# of tracks");
  ((TAxis *)mPtEtaWrongCharge->GetXaxis())->SetTitle("`h# (of parent)");
  ((TAxis *)mPtEtaWrongCharge->GetYaxis())->SetTitle("p?'c#! [GeV] (of parent)");
  ((TAxis *)mPtEtaWrongCharge->GetZaxis())->SetTitle("'F# of tracks");
  ((TAxis *)mPtEtaWrongPosCharge->GetXaxis())->SetTitle("`h# (of parent)");
  ((TAxis *)mPtEtaWrongPosCharge->GetYaxis())->SetTitle("p?'c#! [GeV] (of parent)");
  ((TAxis *)mPtEtaWrongPosCharge->GetZaxis())->SetTitle("'F# of tracks");

  ((TAxis *)mPtEtaGoodG->GetXaxis())->SetTitle("`h# (of parent)");
  ((TAxis *)mPtEtaGoodG->GetYaxis())->SetTitle("p?'c#! [GeV] (of parent)");
  ((TAxis *)mPtEtaGoodG->GetZaxis())->SetTitle("'F# of tracks");
  ((TAxis *)mPtEtaGoodF->GetXaxis())->SetTitle("`h# (of parent)");
  ((TAxis *)mPtEtaGoodF->GetYaxis())->SetTitle("p?'c#! [GeV] (of parent)");
  ((TAxis *)mPtEtaGoodF->GetZaxis())->SetTitle("'F# of tracks");
  ((TAxis *)mPtEtaGoodRatio->GetXaxis())->SetTitle("`h# (of parent)");
  ((TAxis *)mPtEtaGoodRatio->GetYaxis())->SetTitle("p?'c#! [GeV] (of parent)");
  ((TAxis *)mPtEtaGoodRatio->GetZaxis())->SetTitle("ratio");
  ((TAxis *)mPtEtaBadG->GetXaxis())->SetTitle("`h# (of parent)");
  ((TAxis *)mPtEtaBadG->GetYaxis())->SetTitle("p?'c#! [GeV] (of parent)");
  ((TAxis *)mPtEtaBadG->GetZaxis())->SetTitle("'F# of tracks");
  ((TAxis *)mPtEtaBadF->GetXaxis())->SetTitle("`h# (of parent)");
  ((TAxis *)mPtEtaBadF->GetYaxis())->SetTitle("p?'c#! [GeV] (of parent)");
  ((TAxis *)mPtEtaBadF->GetZaxis())->SetTitle("'F# of tracks");
  ((TAxis *)mPtEtaBadRatio->GetXaxis())->SetTitle("`h# (of parent)");
  ((TAxis *)mPtEtaBadRatio->GetYaxis())->SetTitle("p?'c#! [GeV] (of parent)");
  ((TAxis *)mPtEtaBadRatio->GetZaxis())->SetTitle("ratio");

  ((TAxis *)mGLengthDistTrackAng->GetXaxis())->SetTitle("angle of track endings");
  ((TAxis *)mGLengthDistTrackAng->GetYaxis())->SetTitle("length dist. of last point");
  ((TAxis *)mGLengthDistTrackAng->GetZaxis())->SetTitle("'F# of track endings");
  ((TAxis *)mFLengthDistTrackAng->GetXaxis())->SetTitle("angle of track endings");
  ((TAxis *)mFLengthDistTrackAng->GetYaxis())->SetTitle("length dist. of last point");
  ((TAxis *)mFLengthDistTrackAng->GetZaxis())->SetTitle("'F# of track endings");
  ((TAxis *)mGCircleDistTrackAng->GetXaxis())->SetTitle("angle of track endings");
  ((TAxis *)mGCircleDistTrackAng->GetYaxis())->SetTitle("circle dist. of last point");
  ((TAxis *)mGCircleDistTrackAng->GetZaxis())->SetTitle("'F# of track endings");
  ((TAxis *)mFCircleDistTrackAng->GetXaxis())->SetTitle("angle of track endings");
  ((TAxis *)mFCircleDistTrackAng->GetYaxis())->SetTitle("circle dist. of last point");
  ((TAxis *)mFCircleDistTrackAng->GetZaxis())->SetTitle("'F# of track endings");
  ((TAxis *)mSetupTime->GetXaxis())->SetTitle("Zeit [s]");
  ((TAxis *)mSetupTime->GetYaxis())->SetTitle("Ereignisse");
  ((TAxis *)mTrackingTime->GetXaxis())->SetTitle("Zeit [s]");
  ((TAxis *)mTrackingTime->GetYaxis())->SetTitle("Ereignisse");
  ((TAxis *)mExtensionTime->GetXaxis())->SetTitle("Zeit [s]");
  ((TAxis *)mExtensionTime->GetYaxis())->SetTitle("Ereignisse");
  ((TAxis *)mSplitTime->GetXaxis())->SetTitle("Zeit [s]");
  ((TAxis *)mSplitTime->GetYaxis())->SetTitle("Ereignisse");
  ((TAxis *)mFitTime->GetXaxis())->SetTitle("Zeit [s]");
  ((TAxis *)mFitTime->GetYaxis())->SetTitle("Ereignisse");
  ((TAxis *)mTotalTime->GetXaxis())->SetTitle("Zeit [s]");
  ((TAxis *)mTotalTime->GetYaxis())->SetTitle("Ereignisse");

  return;
}


void StFtpcTrackEvaluator::GeantHitInit(St_g2t_ftp_hit *g2t_ftp_hit)
{
  // Initializes Geant hits.

  if(g2t_ftp_hit) {
    
    Int_t NumGeantHits = g2t_ftp_hit->GetNRows();       // number of generated clusters
    g2t_ftp_hit_st *point_st = g2t_ftp_hit->GetTable(); // pointer to generated clusters
    
    mGeantHits = new TObjArray(NumGeantHits);    // create TObjArray

    // Loop ovver all generated clusters
    {for (Int_t i = 0; i < NumGeantHits; i++, point_st++) { 
      mGeantHits->AddAt(new StFtpcConfMapPoint(), i);                              // create StFtpcConfMapPoint
      ((StFtpcConfMapPoint *)mGeantHits->At(i))->SetHitNumber(i);
      ((StFtpcConfMapPoint *)mGeantHits->At(i))->SetNextHitNumber(point_st->next_tr_hit_p-1);
      ((StFtpcConfMapPoint *)mGeantHits->At(i))->SetX(point_st->x[0]);
      ((StFtpcConfMapPoint *)mGeantHits->At(i))->SetY(point_st->x[1]);
      ((StFtpcConfMapPoint *)mGeantHits->At(i))->SetZ(point_st->x[2]);
      ((StFtpcConfMapPoint *)mGeantHits->At(i))->Setup(mVertex);
    }}
  }
}


void StFtpcTrackEvaluator::GeantTrackInit(St_g2t_track *g2t_track, St_g2t_ftp_hit *g2t_ftp_hit)
{
  // Initializes Geant tracks. Therefore the Geant hits are needed as well.
  if (g2t_track) {
    
    Int_t NumGeantTracks = g2t_track->GetNRows();       // number of generated tracks
    g2t_track_st *track_st = g2t_track->GetTable();     // pointer to generated tracks
    mGeantTracks = new TObjArray(NumGeantTracks);       // create TObjArray

    Int_t NumFtpcGeantTracks = 0;
    Int_t ftpc_hits = 0;
    
    mFtpcTrackNum->SetFill(NumGeantTracks, -1);

    // Loop over all generated tracks
    {for (Int_t i = 0; i < NumGeantTracks; i++, track_st++) {
      ftpc_hits = track_st->n_ftp_hit;
      
      if (ftpc_hits) {  // track has hits in Ftpc

	mGHitsOnTrack->Fill(ftpc_hits);
	mGeantTracks->AddAt(new StFtpcTrack(NumFtpcGeantTracks), NumFtpcGeantTracks);     // create StFtpcTrack
	StFtpcTrack *t = (StFtpcTrack*)mGeantTracks->At(NumFtpcGeantTracks);
	TObjArray *points = t->GetHits();

	// set momentum
	t->SetPx(track_st->p[0]);
	t->SetPy(track_st->p[1]);
	t->SetPz(track_st->p[2]);

	// set charge and pid
	t->SetCharge((Int_t)track_st->charge);
	t->SetPid(track_st->ge_pid);
	//	if (t->GetPid() <= 3) {
	//	  mElectronTracks++;
	//	} 

	// set main vertex
	if (track_st->start_vertex_p == 1) {
	  t->ComesFromMainVertex((Bool_t) kTRUE);
	}
	
	else {
	  //	  mNonVertexTracks++;
	  t->ComesFromMainVertex((Bool_t) kFALSE);
	}

	MIntArray *hitnumber = t->GetHitNumbers();
	hitnumber->Set(ftpc_hits);
	((StFtpcConfMapPoint *)mGeantHits->At(track_st->hit_ftp_p - 1))->SetTrackNumber(NumFtpcGeantTracks);
	mFtpcTrackNum->AddAt(NumFtpcGeantTracks, i);
	points->Expand(ftpc_hits);
	points->AddAt(mGeantHits->At(track_st->hit_ftp_p - 1), 0);
	hitnumber->AddAt(track_st->hit_ftp_p - 1, 0);

	// Loop over all hits in Ftpc
	{for(Int_t j = 1; j < ftpc_hits; j++) {
	  Int_t number = ((StFtpcConfMapPoint *)mGeantHits->At(hitnumber->At(j-1)))->GetNextHitNumber();
	  ((StFtpcConfMapPoint *)mGeantHits->At(number))->SetTrackNumber(NumFtpcGeantTracks);
	  points->AddAt(mGeantHits->At(number), j);
	  hitnumber->AddAt(number, j);
	}}

	t->CalculateNMax();
	NumFtpcGeantTracks++;

	if (ftpc_hits>StFtpcTrackingParams::Instance()->NumberOfPadRowsPerSide()) {
	  mLongTracks++;
	  mLongTrackClusters += ftpc_hits;
	  
	  if (ftpc_hits > mMaxClusters) {
	    mMaxClusters = ftpc_hits;
	  }
	}

	if (ftpc_hits<5) {
	  mShortTracks++;
	  mShortTrackClusters += ftpc_hits;
	}

	else {
	  
	  if (IsGoodTrack(t)) {

	    if (ftpc_hits == StFtpcTrackingParams::Instance()->NumberOfPadRowsPerSide()) {
	      mPtEta10PointTracks->Fill(TMath::Abs(t->GetEta()), t->GetPt());
	    }

	    mGoodGTracks++;
	    mGoodGeantPoints += ftpc_hits;
	  }
	}
      }
    }}
  }
}


void StFtpcTrackEvaluator::FastSimHitInit(St_ffs_gepoint *ffs_hit) 
{
  // Initializes fast simulated hits.
  if(ffs_hit) {
    
    Int_t NumFastSimHits = ffs_hit->GetNRows();        // number of generated clusters
    mClusterArr->SetFill(NumFastSimHits, 0);
    mGoodFastSimHitsArr->SetFill(NumFastSimHits, -1);  // goodness of points set to 'undefined'
    ffs_gepoint_st *point_st = ffs_hit->GetTable();    // pointer to fast simulated clusters
    mFastSimHits = new TObjArray(NumFastSimHits);      // create TObjArray
    
    // Loop ovver all generated clusters
    {for (Int_t i = 0; i < NumFastSimHits; i++, point_st++) { 
      mFastSimHits->AddAt(new StFtpcPoint(), i);                             // create StFtpcPoint
      StFtpcPoint *p = (StFtpcPoint*)mFastSimHits->At(i);
      p->SetHitNumber(i);
      p->SetTrackNumber(mFtpcTrackNum->At(point_st->ge_track_p - 1));

      // evaluate goodness of points, fill array with results
      if (IsGoodTrack((StFtpcTrack*)mGeantTracks->At(p->GetTrackNumber()))) {
	mGoodFastSimHitsArr->AddAt(1, i);
      }
      
      else {
	mGoodFastSimHitsArr->AddAt(0, i);
      }
    }}
  }
}


void StFtpcTrackEvaluator::ParentTrackInit()
{
  // Initializes the parents of all tracks.

  mParent->Set(mFoundTracks->GetEntriesFast());
  mNumParentTracks->SetFill(mFoundTracks->GetEntriesFast()*StFtpcTrackingParams::Instance()->NumberOfPadRowsPerSide(), -1);

  Int_t actual_track = -1;

  // loop over tracks
  for (Int_t t_counter = 0; t_counter < mFoundTracks->GetEntriesFast(); t_counter++) {
    StFtpcTrack *track = (StFtpcTrack*)mFoundTracks->At(t_counter);
    TObjArray   *hits  = (TObjArray*) track->GetHits();
    mUnclean[t_counter] = (Bool_t)kFALSE;
    
    if (track->ComesFromMainVertex()) {
      mFoundVertexTracks++;
    }

    else {
      mFoundNonVertexTracks++;
    }
        
    Int_t max_hits = hits->GetEntriesFast();

    for (Int_t h_counter = 0; h_counter < StFtpcTrackingParams::Instance()->NumberOfPadRowsPerSide(); h_counter++) {

      if (h_counter < max_hits) {
	StFtpcPoint *hit = (StFtpcPoint *)hits->At(h_counter);
	mParentTrack->AddLast(((StFtpcPoint*)mFastSimHits->At(hit->GetHitNumber()))->GetTrackNumber());
	mParentTracks->AddLast(mParentTrack->AtLast());
      }
      
      else {
	mParentTrack->AddLast(-1);
	mParentTracks->AddLast(-1);
      }
    }
    
    for (Int_t cluster1 = 0; cluster1 < max_hits; cluster1++) {
	    
      if (mParentTracks->At(t_counter * StFtpcTrackingParams::Instance()->NumberOfPadRowsPerSide() + cluster1) != -1) {
       	mNumParentTracks->AddAt(1, t_counter * StFtpcTrackingParams::Instance()->NumberOfPadRowsPerSide() + cluster1);
	
	for (Int_t cluster2 = cluster1+1; cluster2 < max_hits; cluster2++) {
	  
	  if (mParentTracks->At(t_counter * StFtpcTrackingParams::Instance()->NumberOfPadRowsPerSide() + cluster2) != -1) {

	    if (mParentTracks->At(t_counter * StFtpcTrackingParams::Instance()->NumberOfPadRowsPerSide() + cluster1) == mParentTracks->At(t_counter * StFtpcTrackingParams::Instance()->NumberOfPadRowsPerSide() + cluster2)) {
	      mParentTracks->AddAt(-1, t_counter * StFtpcTrackingParams::Instance()->NumberOfPadRowsPerSide() + cluster2);
	      mNumParentTracks->AddAt(mNumParentTracks->At(t_counter * StFtpcTrackingParams::Instance()->NumberOfPadRowsPerSide() + cluster1) + 1, t_counter * StFtpcTrackingParams::Instance()->NumberOfPadRowsPerSide() + cluster1);
	    }

	    else {  // different parent tracks found
	      if (actual_track != t_counter) {
		mUncleanTracksArr->AddLast(t_counter);
		mUncleanTracks++;
		mUnclean[t_counter] = (Bool_t)kTRUE;
		mPtEtaMesUnclean->Fill(TMath::Abs(((StFtpcTrack *)mFoundTracks->At(t_counter))->GetEta()), ((StFtpcTrack *)mFoundTracks->At(t_counter))->GetPt());
		actual_track = t_counter;
	      }
	    }
	  }
	}
      }
    }

    // Evaluate the most likely parent for this track.
    Int_t max = 0;

    for (Int_t cluster = 0; cluster < StFtpcTrackingParams::Instance()->NumberOfPadRowsPerSide(); cluster++) {

      if (mNumParentTracks->At(t_counter * StFtpcTrackingParams::Instance()->NumberOfPadRowsPerSide() + cluster) >= max) {
	max = mNumParentTracks->At(t_counter * StFtpcTrackingParams::Instance()->NumberOfPadRowsPerSide() + cluster);
	mParent->AddAt(mParentTrack->At(t_counter * StFtpcTrackingParams::Instance()->NumberOfPadRowsPerSide() + cluster), t_counter);
      }
    }

    EvaluateGoodness(t_counter);
  }  

 return;
}


void StFtpcTrackEvaluator::EvaluateGoodness(Int_t t_counter)
{
  // Evaluates if the parent track of the found track no. t_counter is a good track or not.

  StFtpcTrack *track = (StFtpcTrack *)mFoundTracks->At(t_counter);
  StFtpcTrack *parent = (StFtpcTrack *)mGeantTracks->At(mParent->At(t_counter));
  mPtEtaF->Fill(TMath::Abs(parent->GetPseudoRapidity()), parent->GetPt());
  mPtEtaFMes->Fill(TMath::Abs(track->GetPseudoRapidity()), track->GetPt());

  TObjArray *points = track->GetHits();

  Int_t wrong_hits_on_this_track = 0;

  {for (Int_t i=0; i<points->GetEntriesFast(); i++) {
    StFtpcPoint *p = (StFtpcPoint *)points->At(i);
    
    if (mParentTrack->At(t_counter*StFtpcTrackingParams::Instance()->NumberOfPadRowsPerSide()+i) == mParent->At(t_counter)) {
      mClusterArr->AddAt(1, p->GetHitNumber());

      if (i == points->GetEntriesFast()-1) {
	// last point on track was picked up wrong
	
	StFtpcConfMapper c;

	Double_t chi2_1[6];
	Double_t chi2_2[6];

	c.StraightLineFit(track, chi2_1, i);
	c.StraightLineFit(track, chi2_2, i-1);

	//LOG_INFO << " " << chi2_1[4] << " " << chi2_2[4] << " " << chi2_1[4] - chi2_2[4] << endm;
	//LOG_INFO << " " << chi2_1[5] << " " << chi2_2[5] << " " << chi2_1[5] - chi2_2[5] << endm << endm;
      }
    }

    else {
      mClusterArr->AddAt(-1, p->GetHitNumber());
      wrong_hits_on_this_track++;

      if (i == points->GetEntriesFast()-1) {
	// last point on track was picked up wrong
	
	StFtpcConfMapper c;

	Double_t chi2_1[6];
	Double_t chi2_2[6];

	c.StraightLineFit(track, chi2_1, i);
	c.StraightLineFit(track, chi2_2, i-1);

	//LOG_INFO << "u" << chi2_1[4] << " " << chi2_2[4] << " " << chi2_1[4] - chi2_2[4] << endm;
	//LOG_INFO << "u" << chi2_1[5] << " " << chi2_2[5] << " " << chi2_1[5] - chi2_2[5] << endm << endm;
      }

    }
  }}

  if (IsUncleanTrack(t_counter)) {
    mPtEtaUnclean->Fill(TMath::Abs(((StFtpcTrack *)mGeantTracks->At(mParent->At(t_counter)))->GetEta()), ((StFtpcTrack *)mGeantTracks->At(mParent->At(t_counter)))->GetPt());
    
    if (IsGoodTrack(parent) && track->ComesFromMainVertex()) {
      mGoodFoundPoints += (track->GetNumberOfPoints() - wrong_hits_on_this_track);
      mPtEtaUncleanGood->Fill(TMath::Abs(((StFtpcTrack *)mGeantTracks->At(mParent->At(t_counter)))->GetEta()), ((StFtpcTrack *)mGeantTracks->At(mParent->At(t_counter)))->GetPt());
      mPtEtaMesUncleanGood->Fill(TMath::Abs(track->GetEta()), track->GetPt());      
      mUncleanGoodTracks++;
    }
  }
  
  if (IsGoodTrack(parent) && IsCleanTrack(t_counter) && track->ComesFromMainVertex()) {
    mGoodFTracks++;
    mGoodFoundPoints += track->GetNumberOfPoints();
    mPtEtaGoodF->Fill(TMath::Abs(parent->GetPseudoRapidity()), parent->GetPt());
  }

  else {
    mPtEtaBadF->Fill(TMath::Abs(parent->GetPseudoRapidity()), parent->GetPt());

    if (parent->GetPid() <= 3) {
      mElectronTracks++;
    } 

    if(!(parent->ComesFromMainVertex())) {
      mNonVertexTracks++;
    }
  }

  if (track->ComesFromMainVertex()) {
    mPtEtaFVtx->Fill(TMath::Abs(parent->GetPseudoRapidity()), parent->GetPt());

    if ((IsUncleanTrack(t_counter) || (IsCleanTrack(t_counter) && (!IsGoodTrack(parent))))) {
      mPtEtaLookLikeGood->Fill(TMath::Abs(parent->GetPseudoRapidity()), parent->GetPt());
      mLookLikeGoodTracks++;
    }
  }

  return;
}


void StFtpcTrackEvaluator::DivideHistos()
{
  // Divides Histograms to get the efficiency.
  // These hostograms contain always the last event only!

  mPtEtaGood->Divide(mPtEtaGoodF, mPtEtaF);
  mPtEtaBad->Divide(mPtEtaBadF, mPtEtaF);
  mPtEtaGoodRatio->Divide(mPtEtaGoodF, mPtEtaGoodG);
  mPtEtaBadRatio->Divide(mPtEtaBadF, mPtEtaBadG);
  mPtEtaContamination->Divide(mPtEtaLookLikeGood, mPtEtaFVtx);
  // Splits have to be substracted, still. They are available in PtEta up to now but mPtEtaLookLikeGood would be overwritten.
  // mPtEtaContaWoSplits->Divide(mPtEtaLookLikeGood->Add(mPtEtaSplitLoliGood, -1), mPtEtaFVtx);

  return;
}


void StFtpcTrackEvaluator::FillGCutHistos()
{
  // Evaluates the cuts by looking into the distibution od the cut-variables in GEANT tracks.

  StFtpcConfMapper t; 
  Double_t coeff[6];

  for (Int_t t_counter = 0; t_counter < mGeantTracks->GetEntriesFast(); t_counter++) {
    StFtpcTrack *track = (StFtpcTrack*) mGeantTracks->At(t_counter);
    TObjArray   *hits  = (TObjArray*) track->GetHits();

    if (IsGoodMainVertexTrack(track)) {
      mDcaGMainVertex->Fill(track->CalcDca(mVertex, 0));
    }

    if (IsGoodNonVertexTrack(track)) {
      mDcaGNonVertex->Fill(track->CalcDca(mVertex, 0));
    }
    
    if (IsGoodTrack(track)) {
      
      mEtaNghits->Fill(hits->GetEntriesFast(), TMath::Abs(track->GetPseudoRapidity()));
      mPtEtaGoodG->Fill(TMath::Abs(track->GetPseudoRapidity()), track->GetPt());

      for (Int_t h_counter = 2; h_counter < hits->GetEntriesFast() && h_counter < StFtpcTrackingParams::Instance()->NumberOfPadRowsPerSide(); h_counter++) {

	StFtpcConfMapPoint *hit = (StFtpcConfMapPoint *)hits->At(h_counter);
	
	if (h_counter == 2) {
	  mGTracklAngAll->Fill(t.StFtpcConfMapper::TrackletAngle(track, h_counter+1));
	  mGTrackAng->Fill((Float_t)t.StFtpcConfMapper::TrackletAngle(track, h_counter+1), (Float_t)h_counter+1);
	}

	else {
	  mGTrackAngAll->Fill((Float_t)t.StFtpcConfMapper::TrackletAngle(track, h_counter+1));
	  mGTrackAng->Fill((Float_t)t.StFtpcConfMapper::TrackletAngle(track, h_counter+1), (Float_t)h_counter+1);

	  t.StraightLineFit(track, coeff, h_counter-1);
	  hit->SetDist(t.CalcDistance(hit, coeff+0), t.CalcDistance(hit, coeff+2));
	  
	  mGLengthDistTrackAng->Fill((Float_t)t.StFtpcConfMapper::TrackletAngle(track, h_counter+1), (Float_t)hit->GetLengthDist());
	  mGCircleDistTrackAng->Fill((Float_t)t.StFtpcConfMapper::TrackletAngle(track, h_counter+1), (Float_t)hit->GetCircleDist());

	  mGCircleDistAll->Fill(hit->GetCircleDist());
	  mGLengthDistAll->Fill(hit->GetLengthDist());
	  mGCircleDist->Fill((Float_t)hit->GetCircleDist(),(Float_t)h_counter+1);
	  mGLengthDist->Fill((Float_t)hit->GetLengthDist(), (Float_t)h_counter+1);
	  mGCircleLength->Fill((Float_t)hit->GetLengthDist(), (Float_t)hit->GetCircleDist());
	}
      }
    }

    else {
      mPtEtaBadG->Fill(TMath::Abs(track->GetPseudoRapidity()), track->GetPt());
    }
  }

  return;
}


void StFtpcTrackEvaluator::FillFCutHistos()
{
  // Fills cut histograms for found tracks.

  StFtpcConfMapper t; 
  Double_t coeff[6];

  for (Int_t t_counter = 0; t_counter < mFoundTracks->GetEntriesFast(); t_counter++) {
    StFtpcTrack *track = (StFtpcTrack*) mFoundTracks->At(t_counter);
    TObjArray   *hits  = (TObjArray*) track->GetHits();

    // evaluate if the track could be longer
    if (track->GetNumberOfPoints() < track->GetNMax()) {
      mTooShortTracks++;
    }
    
    if (hits->GetEntriesFast() >= 3) {
      
      mEtaNfhits->Fill(hits->GetEntriesFast(), TMath::Abs(track->GetPseudoRapidity()));

      for (Int_t h_counter = 3; h_counter < hits->GetEntriesFast(); h_counter++) {

	StFtpcConfMapPoint *hit = (StFtpcConfMapPoint *)hits->At(h_counter);
	
	if (h_counter == 3) {
	  mFTracklAngAll->Fill(t.StFtpcConfMapper::TrackletAngle(track, h_counter));
	  mFTrackAng->Fill((Float_t)t.StFtpcConfMapper::TrackletAngle(track, h_counter), (Float_t)h_counter);
	}

	else {
	  mFTrackAngAll->Fill(t.StFtpcConfMapper::TrackletAngle(track, h_counter));
	  mFTrackAng->Fill((Float_t)t.StFtpcConfMapper::TrackletAngle(track, h_counter), (Float_t)h_counter);

	  t.StraightLineFit(track, coeff, h_counter-1);
	  hit->SetDist(t.CalcDistance(hit, coeff+0), t.CalcDistance(hit, coeff+2));

	  mFLengthDistTrackAng->Fill((Float_t)t.StFtpcConfMapper::TrackletAngle(track, h_counter+1), (Float_t)hit->GetLengthDist());
	  mFCircleDistTrackAng->Fill((Float_t)t.StFtpcConfMapper::TrackletAngle(track, h_counter+1), (Float_t)hit->GetCircleDist());
	  
	  mFCircleDistAll->Fill(hit->GetCircleDist());
	  mFLengthDistAll->Fill(hit->GetLengthDist());
	  mFCircleDist->Fill((Float_t)hit->GetCircleDist(), (Float_t)h_counter);
	  mFLengthDist->Fill((Float_t)hit->GetLengthDist(), (Float_t)h_counter);
	  mFCircleLength->Fill((Float_t)hit->GetLengthDist(), (Float_t)hit->GetCircleDist());
	}
      }
    }
  }

  return;
}


void StFtpcTrackEvaluator::Loop()
{
  // Usual used loop to loop over tracks and clusters.
  // This is just an example to make it easier to develop new routines.

  for (Int_t t_counter = 0; t_counter < mFoundTracks->GetEntriesFast(); t_counter++) {
    StFtpcTrack *track = (StFtpcTrack*) mFoundTracks->At(t_counter);
    TObjArray   *fhits  = (TObjArray*) track->GetHits();

    for (Int_t h_counter = 0; h_counter < fhits->GetEntriesFast(); h_counter++) {
	
	
      //StFtpcPoint *hit = (StFtpcPoint *) fhits->At(h_counter);
      
    }
  }
}


void StFtpcTrackEvaluator::WriteHistos()
{
  // Writes histograms to file.

  mFile->cd();
  mFile->Delete("*;1");

  mNumGeantHits->Write();
  mNumFoundHits->Write();
  mNumGeantTracks->Write();
  mNumFoundTracks->Write();
  
  mNumFoundVertexTracks->Write();
  mNumFoundNonVertexTracks->Write();

  mNumLookLikeGoodTracks->Write();
  mNumElectronTracks->Write();
  mNumNonVertexTracks->Write();
  mNumGoodGTracks->Write();
  mNumGoodFTracks->Write();
  mGoodRatio->Write();
  mContamination->Write();
  mContaWoSplit->Write();
  mNumSplitTracks->Write();
  mNumSplitGoodTracks->Write();
  mNumSplitLoliGoodTracks->Write();
  mNumUncleanTracks->Write();
  mNumUncleanGoodTracks->Write();
  mNumLongTracks->Write();
  mNumLongTrackClusters->Write();
  mNumShortTracks->Write();
  mNumShortTrackClusters->Write();
  mNumTooShortTracks->Write();

  mGHitsOnTrack->Write();
  mFHitsOnTrack->Write();

  mNumGoodGeantPoints->Write();
  mNumGoodFoundPoints->Write();
  mGoodPointRatio->Write();

  mNumParents->Write();
  mNumWrongHits->Write();
  mNumWrongHitsAll->Write();

  mPtot->Write();
  mPt->Write();
  mPx->Write();
  mPy->Write();
  mPz->Write();

  mPtotDiff->Write();
  mPtDiff->Write();
  mPxDiff->Write();
  mPyDiff->Write();
  mPzDiff->Write();

  mPtotAcc->Write();
  mPtAcc->Write();
  mPxAcc->Write();
  mPyAcc->Write();
  mPzAcc->Write();

  mPRelErr->Write();
  mPRelErrqok->Write();
  mPRelDiff->Write();

  mEtaNghits->Write();
  mEtaNfhits->Write();

  mPtEtaF->Write();
  mPtEtaFMes->Write();
  mPtEtaGood->Write();
  mPtEtaBad->Write();
  mPtEtaUnclean->Write();
  mPtEtaMesUnclean->Write();
  mPtEtaUncleanGood->Write();
  mPtEtaMesUncleanGood->Write();

  mPtEtaSplit->Write();
  mPtEtaSplitGood->Write();
  mPtEtaSplitLoliGood->Write();

  mPtEtaGoodG->Write();
  mPtEtaGoodF->Write();
  mPtEtaGoodRatio->Write();
  mPtEtaBadG->Write();
  mPtEtaBadF->Write();
  mPtEtaBadRatio->Write(); 

  mPtEtaFVtx->Write();
  mPtEtaLookLikeGood->Write();
  mPtEtaContamination->Write();
  mPtEtaContaWoSplits->Write();

  mPtEta10PointTracks->Write();
  mPtEtaWrongCharge->Write();
  mPtEtaWrongPosCharge->Write();

  mDcaFMainVertex->Write();
  mDcaFNonVertex->Write();
  mDcaGMainVertex->Write();
  mDcaGNonVertex->Write();

  mGLengthDistTrackAng->Write();
  mGCircleDistTrackAng->Write();
  mFLengthDistTrackAng->Write();
  mFCircleDistTrackAng->Write();

  mGTracklAngAll->Write();
  mGTrackAngAll->Write();
  mGCircleDistAll->Write();
  mGLengthDistAll->Write();

  mGTrackAng->Write();
  mGCircleDist->Write();
  mGLengthDist->Write();

  mGCircleLength->Write();

  mFTracklAngAll->Write();
  mFTrackAngAll->Write();
  mFCircleDistAll->Write();
  mFLengthDistAll->Write();

  mFTrackAng->Write();
  mFCircleDist->Write();
  mFLengthDist->Write();

  mFCircleLength->Write();

  mPRatioDist->Write();
  mPRatioDistSplit->Write();

  mSetupTime->Write();
  mTrackingTime->Write();
  mExtensionTime->Write();
  mSplitTime->Write();
  mFitTime->Write();
  mTotalTime->Write();

  return;
}


void StFtpcTrackEvaluator::FillMomentumHistos()
{
  // Fill the relative momentum of the found track in comparison to the geant track in histograms.
  
  StFtpcTrack *ftrack;
  StFtpcTrack *gtrack;

  for (Int_t t_counter = 0; t_counter<mFoundTracks->GetEntriesFast(); t_counter++) {
  
    ftrack = (StFtpcTrack *)mFoundTracks->At(t_counter);
    gtrack = (StFtpcTrack *)mGeantTracks->At(mParent->At(t_counter));

    mPtot->Fill((Float_t)gtrack->GetP(), (Float_t)ftrack->GetP());
    mPt->Fill((Float_t)gtrack->GetPt(), (Float_t)ftrack->GetPt());
    mPx->Fill((Float_t)gtrack->GetPx(), (Float_t)ftrack->GetPx());
    mPy->Fill((Float_t)gtrack->GetPy(), (Float_t)ftrack->GetPy());
    mPz->Fill((Float_t)gtrack->GetPz(), (Float_t)ftrack->GetPz());
    
    mPtotDiff->Fill((Float_t)StFormulary::RelDiff(ftrack->GetP(), (Float_t)gtrack->GetP()));
    mPtDiff->Fill((Float_t)StFormulary::RelDiff(ftrack->GetPt(), (Float_t)gtrack->GetPt()));
    mPxDiff->Fill((Float_t)StFormulary::RelDiff(ftrack->GetPx(), (Float_t)gtrack->GetPx()));
    mPyDiff->Fill((Float_t)StFormulary::RelDiff(ftrack->GetPy(), (Float_t)gtrack->GetPy()));
    mPzDiff->Fill((Float_t)StFormulary::RelDiff(ftrack->GetPz(), (Float_t)gtrack->GetPz())); 

    mPtotAcc->Fill(ftrack->GetP()/gtrack->GetP());
    mPtAcc->Fill(ftrack->GetPt()/gtrack->GetPt());
    mPxAcc->Fill(ftrack->GetPx()/gtrack->GetPx());
    mPyAcc->Fill(ftrack->GetPy()/gtrack->GetPy());
    mPzAcc->Fill(ftrack->GetPz()/gtrack->GetPz());

    mPRelErr->Fill(TMath::Abs(gtrack->GetEta()), gtrack->GetPt(), (ftrack->GetP() - gtrack->GetP())/gtrack->GetP());  
    mPRelDiff->Fill(TMath::Abs(gtrack->GetEta()), gtrack->GetPt(), StFormulary::RelDiff(ftrack->GetP(), gtrack->GetP()));

    if (ftrack->GetCharge() != gtrack->GetCharge()) {
      mPtEtaWrongCharge->Fill(TMath::Abs(gtrack->GetEta()), gtrack->GetPt());
      
      if(ftrack->GetCharge() > 0.) {
	mPtEtaWrongPosCharge->Fill(TMath::Abs(gtrack->GetEta()), gtrack->GetPt());
      }
    }
    
    else {
      mPRelErrqok->Fill(TMath::Abs(gtrack->GetEta()), gtrack->GetPt(), (ftrack->GetP() - gtrack->GetP())/gtrack->GetP());
    }
  }

  return;
}


void StFtpcTrackEvaluator::FillMomentumHistos(Int_t t_counter)
{
  // Fill the relative momentum of the found track in comparison to the geant track in histograms.

  StFtpcTrack *ftrack = (StFtpcTrack *)mFoundTracks->At(t_counter);
  StFtpcTrack *gtrack = (StFtpcTrack *)mGeantTracks->At(mParent->At(t_counter));

  mPtot->Fill(gtrack->GetP(), ftrack->GetP());
  mPt->Fill(gtrack->GetPt(), ftrack->GetPt());
  mPx->Fill(gtrack->GetPx(), ftrack->GetPx());
  mPy->Fill(gtrack->GetPy(), ftrack->GetPy());
  mPz->Fill(gtrack->GetPz(), ftrack->GetPz());
    
  mPtotDiff->Fill(StFormulary::RelDiff(ftrack->GetP(), gtrack->GetP()));
  mPtDiff->Fill(StFormulary::RelDiff(ftrack->GetPt(), gtrack->GetPt()));
  mPxDiff->Fill(StFormulary::RelDiff(ftrack->GetPx(), gtrack->GetPx()));
  mPyDiff->Fill(StFormulary::RelDiff(ftrack->GetPy(), gtrack->GetPy()));
  mPzDiff->Fill(StFormulary::RelDiff(ftrack->GetPz(), gtrack->GetPz()));

  mPtotAcc->Fill(ftrack->GetP()/gtrack->GetP());
  mPtAcc->Fill(ftrack->GetPt()/gtrack->GetPt());
  mPxAcc->Fill(ftrack->GetPx()/gtrack->GetPx());
  mPyAcc->Fill(ftrack->GetPy()/gtrack->GetPy());
  mPzAcc->Fill(ftrack->GetPz()/gtrack->GetPz()); 

  mPRelErr->Fill(TMath::Abs(gtrack->GetEta()), gtrack->GetPt(), (ftrack->GetP() - gtrack->GetP())/gtrack->GetP());
  mPRelDiff->Fill(TMath::Abs(gtrack->GetEta()), gtrack->GetPt(), StFormulary::RelDiff(ftrack->GetP(), gtrack->GetP()));

  if (ftrack->GetCharge() != gtrack->GetCharge()) {
    mPtEtaWrongCharge->Fill(TMath::Abs(gtrack->GetEta()), gtrack->GetPt());
    
    if(ftrack->GetCharge() > 0.) {
      mPtEtaWrongPosCharge->Fill(TMath::Abs(gtrack->GetEta()), gtrack->GetPt());
    }
  }
  
  else {
    mPRelErrqok->Fill(TMath::Abs(gtrack->GetEta()), gtrack->GetPt(), (ftrack->GetP() - gtrack->GetP())/gtrack->GetP());
  }
  
  return;
}


void StFtpcTrackEvaluator::FillParentHistos()
{
  // Fill the number of parents per track in histogram.

  for (Int_t t_counter = 0; t_counter < mFoundTracks->GetEntriesFast(); t_counter++) {
    // Loop over tracks
    Int_t num_histo = ((StFtpcTrack *)mFoundTracks->At(t_counter))->GetNumberOfPoints();
    
    Int_t num_parents = 0;
    Int_t num_hits = 0;
    for (Int_t h_counter = 0; h_counter < StFtpcTrackingParams::Instance()->NumberOfPadRowsPerSide(); h_counter++) {
      // Loop over clusters
      
      if (mNumParentTracks->At(t_counter * StFtpcTrackingParams::Instance()->NumberOfPadRowsPerSide() + h_counter) != -1) {
	num_parents++;
	
	if (mParentTracks->At(t_counter * StFtpcTrackingParams::Instance()->NumberOfPadRowsPerSide() + h_counter) != mParent->At(t_counter)) {
	  num_hits += mNumParentTracks->At(t_counter * StFtpcTrackingParams::Instance()->NumberOfPadRowsPerSide() + h_counter);
	}
      } 
    }

    mNumParents->Fill(num_parents);
    mNumWrongHits->Fill((Float_t)num_hits, (Float_t)num_histo);
    mNumWrongHitsAll->Fill(num_hits);
  }
}


void StFtpcTrackEvaluator::FillParentHistos(Int_t t_counter)
{
  // Fill the number of parents per track in histogram.

  Int_t num_histo = ((StFtpcTrack *)mFoundTracks->At(t_counter))->GetNumberOfPoints();
  
  Int_t num_parents = 0;
  Int_t num_hits = 0;
  for (Int_t h_counter = 0; h_counter < StFtpcTrackingParams::Instance()->NumberOfPadRowsPerSide(); h_counter++) {
    // Loop over clusters
    
    if (mNumParentTracks->At(t_counter * StFtpcTrackingParams::Instance()->NumberOfPadRowsPerSide() + h_counter) != -1) {
      num_parents++;
      
      if (mParentTracks->At(t_counter * StFtpcTrackingParams::Instance()->NumberOfPadRowsPerSide() + h_counter) != mParent->At(t_counter)) {
	num_hits += mNumParentTracks->At(t_counter * StFtpcTrackingParams::Instance()->NumberOfPadRowsPerSide() + h_counter);
      }
    } 
  }
  
  mNumParents->Fill(num_parents);
  mNumWrongHits->Fill((Float_t)num_hits, (Float_t)num_histo);
  mNumWrongHitsAll->Fill(num_hits);
}


void StFtpcTrackEvaluator::FillHitsOnTrack(TObjArray *trackarray, Char_t c)
{
  // Fills histogram with the number of points on tracks.
  // If c is set to 'g' it fills the 'geant histo', otherwise the 'found histo'.

  TH1F *histo;

  if (c=='g') {
    histo = mGHitsOnTrack;
  }

  else {
    histo = mFHitsOnTrack;
  }

  {for (Int_t i = 0; i < trackarray->GetEntriesFast(); i++) {
    histo->Fill(((StFtpcTrack*)trackarray->At(i))->GetNumberOfPoints());
  }}
  
  return;
}


void StFtpcTrackEvaluator::CalcSplitTracks()
{
  // Calculates the number of split tracks and returns it. 
  
  Double_t ratio;
  Double_t dist;

  Double_t r1;
  Double_t r2;
  Double_t R1;
  Double_t R2;
  Double_t phi1;
  Double_t phi2;
  Double_t Phi1;
  Double_t Phi2;

  for (Int_t track1 = 0; track1 < mFoundTracks->GetEntriesFast(); track1++) {
    
    for (Int_t track2 = track1+1; track2 < mFoundTracks->GetEntriesFast(); track2++) {
      
      StFtpcTrack *t1 = (StFtpcTrack *)mFoundTracks->At(track1);
      StFtpcTrack *t2 = (StFtpcTrack *)mFoundTracks->At(track2);
     
      r1 = t1->GetRLast();
      r2 = t2->GetRLast();
      phi1 = t1->GetAlphaLast();
      phi2 = t2->GetAlphaLast();
      R1 = t1->GetRFirst();
      R2 = t2->GetRFirst();
      Phi1 = t1->GetAlphaFirst();
      Phi2 = t2->GetAlphaFirst();
      
      dist = (TMath::Sqrt(r2*r2+r1*r1-2*r1*r2*(TMath::Cos(phi1)*TMath::Cos(phi2)+TMath::Sin(phi1)*TMath::Sin(phi2))) +
	      TMath::Sqrt(R2*R2+R1*R1-2*R1*R2*(TMath::Cos(Phi1)*TMath::Cos(Phi2)+TMath::Sin(Phi1)*TMath::Sin(Phi2)))) / 2.;
      ratio = (Double_t)(t1->GetNumberOfPoints() + t2->GetNumberOfPoints()) / (Double_t)(t1->GetNMax() + t2->GetNMax());

      if (!(t1->GetRowsWithPoints() & t2->GetRowsWithPoints()) && (t1->GetHemisphere() == t2->GetHemisphere())) {
	mPRatioDist->Fill(dist, ratio);
      }
      
      if (mParent->At(track1) == mParent->At(track2)) {

	StFtpcTrack *track = (StFtpcTrack *)mGeantTracks->At(mParent->At(track1));
	
	mSplitTracksArr->AddLast(track1);
	mSplitTracksArr->AddLast(track2);
	mSplitTracks++;

	mPtEtaSplit->Fill(TMath::Abs(track->GetEta()), track->GetPt());

	if (ratio) mPRatioDistSplit->Fill(dist, ratio);

	if ((IsUncleanTrack(track1) || (IsCleanTrack(track1) && (!IsGoodTrack(track))))) {
	  mPtEtaSplitLoliGood->Fill(TMath::Abs(track->GetPseudoRapidity()), track->GetPt());
	  mSplitLoliGoodTracks++;
	}

	if (IsGoodTrack(track) && ((StFtpcTrack*)mFoundTracks->At(track1))->ComesFromMainVertex()) {
	  mSplitGoodTracksArr->AddLast(track1);
	  mSplitGoodTracks++;

	  mPtEtaSplitGood->Fill(TMath::Abs(track->GetEta()), track->GetPt());
	}
      }
    }  
  }
  
  return;
}


void StFtpcTrackEvaluator::FillTimeHistos(StFtpcTracker *tracker)
{
  // Fills histograms of time consumption.

  mSetupTime->Fill(tracker->GetTime((Char_t*)"init"));
  mTrackingTime->Fill(tracker->GetTime((Char_t*)"main_vertex"));
  mExtensionTime->Fill(tracker->GetTime((Char_t*)"extend"));
  mSplitTime->Fill(tracker->GetTime((Char_t*)"splits"));
  mFitTime->Fill(tracker->GetTime((Char_t*)"fit"));
  mTotalTime->Fill(tracker->GetTime());

  return;
}


void StFtpcTrackEvaluator::FillEventHistos()
{
  // Fill the histograms which are filled only once per event.


  mNumGeantHits->Fill(GetNumGeantHits());
  mNumGeantTracks->Fill(GetNumGeantTracks());
  mNumFoundHits->Fill(GetNumFoundHits());
  mNumFoundTracks->Fill(GetNumFoundTracks());

  mNumFoundVertexTracks->Fill(GetNumFoundVertexTracks());
  mNumFoundNonVertexTracks->Fill(GetNumFoundNonVertexTracks());

  mNumLookLikeGoodTracks->Fill(GetNumLookLikeGoodTracks());
  mNumElectronTracks->Fill(GetNumElectronTracks());
  mNumNonVertexTracks->Fill(GetNumNonVertexTracks());
  mNumGoodGTracks->Fill(GetNumGoodGeantTracks());
  mNumGoodFTracks->Fill(GetNumGoodFoundTracks());
  mGoodRatio->Fill(((Float_t)GetNumGoodFoundTracks()-(Float_t)GetNumSplitGoodTracks())/(Float_t)GetNumGoodGeantTracks());
  mContamination->Fill((Float_t)GetNumLookLikeGoodTracks()/(Float_t)GetNumFoundVertexTracks());
  mContaWoSplit->Fill((Float_t)(GetNumLookLikeGoodTracks()-GetNumSplitLookLikeGoodTracks())/(Float_t)GetNumFoundVertexTracks());
  mNumSplitTracks->Fill(GetNumSplitTracks());
  mNumSplitGoodTracks->Fill(GetNumSplitGoodTracks());
  mNumSplitLoliGoodTracks->Fill(GetNumSplitLookLikeGoodTracks());
  mNumUncleanTracks->Fill(GetNumUncleanTracks());
  mNumUncleanGoodTracks->Fill(GetNumUncleanGoodTracks());
  mNumLongTracks->Fill(GetNumLongTracks());
  mNumLongTrackClusters->Fill(GetNumLongTrackClusters());
  mNumShortTracks->Fill(GetNumShortTracks());
  mNumShortTrackClusters->Fill(GetNumShortTrackClusters());
  mNumTooShortTracks->Fill(GetNumTooShortTracks());
  mNumGoodGeantPoints->Fill(GetNumGoodGeantPoints());
  mNumGoodFoundPoints->Fill(GetNumGoodFoundPoints());
  mGoodPointRatio->Fill((Float_t)GetNumGoodFoundPoints()/(Float_t)GetNumGoodGeantPoints());

  return;
}

void StFtpcTrackEvaluator::GetGoodHits(TObjArray *good_clusters)
{
  // Fills hits of good simulated tarcks in given TObjArray.
  
  good_clusters->Clear();

  {for (Int_t i = 0; i < mGeantHits->GetEntriesFast(); i++) {
    
    StFtpcConfMapPoint *hit = (StFtpcConfMapPoint*)mGeantHits->At(i);
  
    {for (Int_t j = 0; j < mFtpcTrackNum->GetSize(); j++) {

      if (mFtpcTrackNum->At(j) == hit->GetTrackNumber() && IsGoodTrack((StFtpcTrack*)mGeantTracks->At(j))) {
	LOG_INFO << i << " " << j << " " << mFtpcTrackNum->At(j) << " " << hit->GetTrackNumber() << " " << ((StFtpcTrack*)mGeantTracks->At(j))->GetEta() << endm;
	good_clusters->AddLast(hit);
      }
    }}
  }}
  
  return;
}


void StFtpcTrackEvaluator::GeantInfo()
{
  // Shows information abaout GEANT output.

  LOG_INFO << "Geant hits                    : " << GetNumGeantHits() << endm;
  LOG_INFO << "Geant tracks                  : " << GetNumGeantTracks() << endm;
  LOG_INFO << "Short tracks                  : " << GetNumShortTracks() << endm;
  LOG_INFO << "Long tracks                   : " << GetNumLongTracks() << endm;
  LOG_INFO << "Good tracks                   : " << GetNumGoodGeantTracks() << endm;

  return;
}


void StFtpcTrackEvaluator::ClusterInfo()
{
  // Shows information abaout GEANT clusters.
  
  LOG_INFO << "Short track clusters          : " << GetNumShortTrackClusters() << endm;
  LOG_INFO << "Long track clusters           : " << GetNumLongTrackClusters() << endm;
  LOG_INFO << "Max. track clusters           : " << GetMaxClusters() << endm;
  LOG_INFO << "# of hits on good found tracks: " << GetNumGoodFoundPoints() << endm;
  LOG_INFO << "# of hits on good geant tracks: " << GetNumGoodGeantPoints() << endm;
  LOG_INFO << "Ratio of good found clusters  : " << (Float_t)GetNumGoodFoundPoints()/(Float_t)GetNumGoodGeantPoints() << endm;

  return;
}


void StFtpcTrackEvaluator::ProblemsInfo()
{
  // Shows information about problems.

  LOG_INFO << "Unclean tracks                : " << GetNumUncleanTracks() << endm;
  LOG_INFO << "Unclean good tracks           : " << GetNumUncleanGoodTracks() << endm;
  LOG_INFO << "Split tracks                  : " << GetNumSplitTracks() << endm;
  LOG_INFO << "Split good tracks             : " << GetNumSplitGoodTracks() << endm;
  LOG_INFO << "Split look like good tracks   : " << GetNumSplitLookLikeGoodTracks() << endm;

  return;
}


void StFtpcTrackEvaluator::TrackerInfo()
{
  // Shows information about tracker output.

  LOG_INFO << "Found hits                    : " << GetNumFoundHits() << endm;
  LOG_INFO << "Found tracks (vtx/non vtx)    : " << GetNumFoundTracks() << " (" << GetNumFoundVertexTracks() << "/" << GetNumFoundNonVertexTracks() << ")" << endm;
  LOG_INFO << "Found good tracks             : " << GetNumGoodFoundTracks() << endm;
  LOG_INFO << "Bad found tracks looking good : " << GetNumLookLikeGoodTracks() << endm;
  LOG_INFO << "Found electron tracks         : " << GetNumElectronTracks() << endm;
  LOG_INFO << "Found non main vertex tracks  : " << GetNumNonVertexTracks() << endm;
  LOG_INFO << "Found too short tracks        : " << GetNumTooShortTracks() << endm;
  
  LOG_INFO << "Ratio of good geant tracks    : " << ((Float_t)GetNumGoodFoundTracks()-(Float_t)GetNumSplitGoodTracks())/(Float_t)GetNumGoodGeantTracks() << endm;
  LOG_INFO << "Contamination                 : " << (Float_t)GetNumLookLikeGoodTracks()/(Float_t)GetNumFoundVertexTracks() << endm;   
  LOG_INFO << "Contamination without splits  : " << ((Float_t)GetNumLookLikeGoodTracks()-(Float_t)GetNumSplitLookLikeGoodTracks())/(Float_t)GetNumFoundVertexTracks() << endm; 

  return;
}


void StFtpcTrackEvaluator::InfoAll()
{
  // Shows all information.

  TrackerInfo();
  LOG_INFO << endm;
  GeantInfo();
  LOG_INFO << endm;
  ClusterInfo();
  LOG_INFO << endm;
  ProblemsInfo();

  return;
}

void StFtpcTrackEvaluator::FillCutHistos()
{
  // Fills histograms for cuts. This is done for Geant tracks and found tracks.

  FillGCutHistos();
  FillFCutHistos();

  return;
}


void StFtpcTrackEvaluator::FillHitsOnTrack()
{
  // Fill hits on all tracks in histograms.

  FillHitsOnTrack(mGeantTracks, 'g');
  FillHitsOnTrack(mFoundTracks, 'f');

  return;
}


void StFtpcTrackEvaluator::FillFoundHitsOnTrack()
{
  // Fill hits on found tracks in histograms.

  FillHitsOnTrack(mFoundTracks, 'f');

  return;
}


Bool_t StFtpcTrackEvaluator::IsGoodTrack(StFtpcTrack* track) 
{
  // Returns true if the given track fulfills all requirements to be a "good" track.

  if (track->GetPid() > 3 && track->ComesFromMainVertex() && track->GetNumberOfPoints() >= 5 && track->GetNumberOfPoints() <= StFtpcTrackingParams::Instance()->NumberOfPadRowsPerSide()) {
    return (Bool_t)kTRUE;
  }

  else {
    return (Bool_t)kFALSE;
  }
} 


Bool_t StFtpcTrackEvaluator::IsGoodMainVertexTrack(StFtpcTrack* track) 
{
  // Returns true if the given track fulfills all requirements to be a "good" track.

  if (track->GetPid() > 3 && track->ComesFromMainVertex() && track->GetNumberOfPoints() >= 5 && track->GetNumberOfPoints() <= StFtpcTrackingParams::Instance()->NumberOfPadRowsPerSide()) {
    return (Bool_t)kTRUE;
  }

  else {
    return (Bool_t)kFALSE;
  }
} 


Bool_t StFtpcTrackEvaluator::IsGoodNonVertexTrack(StFtpcTrack* track) 
{
  // Returns true if the given track fulfills all requirements to be a "good" track.

  if (track->GetPid() > 3 && !(track->ComesFromMainVertex()) && track->GetNumberOfPoints() >= 5 && track->GetNumberOfPoints() <= StFtpcTrackingParams::Instance()->NumberOfPadRowsPerSide()) {
    return (Bool_t)kTRUE;
  }

  else {
    return (Bool_t)kFALSE;
  }
} 


void StFtpcTrackEvaluator::ShowTracks()
{
  // Displays the geant and/or found tracks.

  StFtpcDisplay display(GetFoundHits(), GetFoundTracks(), GetGeantHits(), GetGeantTracks());
  display.ShowEvalTracks(mSplitTracksArr, mUncleanTracksArr, mClusterArr);

  return;
}
