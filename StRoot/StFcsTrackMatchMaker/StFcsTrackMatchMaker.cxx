// \class StFcsTrackMatchMaker
// \author Akio Ogawa
//
//  $Id: StFcsTrackMatchMaker.cxx,v 1.1 2021/03/30 13:34:15 akio Exp $
//  $Log: StFcsTrackMatchMaker.cxx,v $

#include "StFcsTrackMatchMaker.h"
#include "StEnumerations.h"
#include "StMessMgr.h"
#include "StEvent/StEvent.h"
#include "StEvent/StFcsCollection.h"
#include "StEvent/StFcsCluster.h"
#include "StEvent/StFwdTrackCollection.h"
#include "StEvent/StFwdTrack.h"

#include "StThreeVectorF.hh"
#include "StFcsDbMaker/StFcsDb.h"
#include "StRoot/StEpdUtil/StEpdGeom.h"

#include "TFile.h"
#include "TH1F.h"
#include "TH2F.h"

ClassImp(StFcsTrackMatchMaker)

StFcsTrackMatchMaker::StFcsTrackMatchMaker(const char *name) : StMaker(name)
{
  mMinEnergy[0] = 0.1; // ~1/3 MIP
  mMinEnergy[1] = 0.5; // ~1/3 MIP
  mMaxDistance[0] = 6.0;
  mMaxDistance[1] = 10.0;
}

StFcsTrackMatchMaker::~StFcsTrackMatchMaker() {

  if (mHdx[0]) {delete mHdx[0]; mHdx[0]=nullptr;}
  if (mHdy[0]) {delete mHdy[0]; mHdy[0]=nullptr;}
  if (mHdr[0]) {delete mHdr[0]; mHdr[0]=nullptr;}
  if (mNtrk[0]) {delete mNtrk[0]; mNtrk[0]=nullptr;}
  if (mNclu[0]) {delete mNclu[0]; mNclu[0]=nullptr;}
  if (mHdx[1]) {delete mHdx[1]; mHdx[1]=nullptr;}
  if (mHdy[1]) {delete mHdy[1]; mHdy[1]=nullptr;}
  if (mHdr[1]) {delete mHdr[1]; mHdr[1]=nullptr;}
  if (mNtrk[1]) {delete mNtrk[1]; mNtrk[1]=nullptr;}
  if (mNclu[1]) {delete mNclu[1]; mNclu[1]=nullptr;}
  if (mNtrk[2]) {delete mNtrk[2]; mNtrk[2]=nullptr;}
  if (mNtrk[3]) {delete mNtrk[3]; mNtrk[3]=nullptr;}
  if (mNclu[2]) {delete mNclu[2]; mNclu[2]=nullptr;}
  if (mNclu[3]) {delete mNclu[3]; mNclu[3]=nullptr;}
  if (mPtEt[0]) {delete mPtEt[0]; mPtEt[0]=nullptr;}
  if (mPtEt[1]) {delete mPtEt[1]; mPtEt[1]=nullptr;}
  if (mPtEt2[0]) {delete mPtEt2[0]; mPtEt2[0]=nullptr;}
  if (mPtEt2[1]) {delete mPtEt2[1]; mPtEt2[1]=nullptr;}
  if (mCharge[0]) {delete mCharge[0]; mCharge[0]=nullptr;}
  if (mCharge[1]) {delete mCharge[1]; mCharge[1]=nullptr;}
  if (mCharge[2]) {delete mCharge[2]; mCharge[2]=nullptr;}
  if (mXY[0]) {delete mXY[0]; mXY[0]=nullptr;}
  if (mXY[1]) {delete mXY[1]; mXY[1]=nullptr;}
  if (mXY[2]) {delete mXY[2]; mXY[2]=nullptr;}

  if (mEpdgeo){
    delete mEpdgeo;
    mEpdgeo = nullptr;
  }

  if (mFile){
    delete mFile;
    mFile = nullptr;
  }


}

int StFcsTrackMatchMaker::Init()
{
  mFcsDb = static_cast<StFcsDb *>(GetDataSet("fcsDb"));
  if (!mFcsDb)
  {
    LOG_ERROR << "StFcsTrackMatchMaker::InitRun Failed to get StFcsDb" << endm;
    return kStFatal;
  }
  mEpdgeo = new StEpdGeom;
  if (mFilename)
  {
    LOG_INFO << "Opening " << mFilename << endm;
    mFile = new TFile(mFilename, "RECREATE");

    mHdx[0] = new TH1F("dx_EcalTrk", "dx Ecal-Track", 100, -50, 50);
    mHdy[0] = new TH1F("dy_EcalTrk", "dy Ecal-Track", 100, -50, 50);
    mHdr[0] = new TH1F("dr_EcalTrk", "dr Ecal-Track", 100, 0, 20);
    mNtrk[0] = new TH1F("NTrk_Ecal", "NTrk_Ecal", 10, 0.0, 10.0);
    mNclu[0] = new TH1F("NEcalClu_Trk", "NEcalClu_Trk", 10, 0.0, 10.0);

    mHdx[1] = new TH1F("dx_HcalTrk", "dx Hcal-Track", 100, -50, 50);
    mHdy[1] = new TH1F("dy_HcalTrk", "dy Hcal-Track", 100, -50, 50);
    mHdr[1] = new TH1F("dr_HcalTrk", "dr Ecal-Track", 100, 0, 20);
    mNtrk[1] = new TH1F("NTrk_Hcal", "NTrk_Hcal", 10, 0.0, 10.0);
    mNclu[1] = new TH1F("NHcalClu_Trk", "NHcalClu_Trk", 10, 0.0, 10.0);

    mNtrk[2] = new TH1F("NTrk", "NTrk/evt", 50, 0.0, 100.0);
    mNtrk[3] = new TH1F("NGoodTrk", "NGoodTrk/evt", 20, 0.0, 20.0);

    mNclu[2] = new TH1F("NEcalClu", "NEcalClu/evt", 30, 0.0, 30.0);
    mNclu[3] = new TH1F("NHcalClu", "NHcalClu/evt", 30, 0.0, 30.0);

    mPtEt[0] = new TH1F("ETovPT_E", "ETovPT_E", 50, 0.0, 2.0);
    mPtEt[1] = new TH1F("ETovPT_EH", "ETovPT_E+H", 50, 0.0, 2.0);
    mPtEt2[0] = new TH2F("ETPT_E", "ETvsPT_E; ET(Ecal); TrkPT", 50, 0.0, 8.0, 50, 0.0, 8.0);
    mPtEt2[1] = new TH2F("ETPT_EH", "ETvsPT_E+H ET(E+H); TrkPT", 50, 0.0, 8.0, 50, 0.0, 8.0);

    mCharge[0] = new TH1F("Charge_E", "Charge_E", 3, -1.5, 1.5);
    mCharge[1] = new TH1F("Charge_H", "Charge_H", 3, -1.5, 1.5);
    mCharge[2] = new TH1F("Charge", "Charge", 3, -1.5, 1.5);

    mXY[0] = new TH2F("XY_E", "XY_E", 50, -130, 130, 50, -110, 110);
    mXY[1] = new TH2F("XY_H", "XY_H", 50, -130, 130, 50, -110, 110);
    mXY[2] = new TH2F("XY", "XY", 50, -130, 130, 50, -110, 110);
  }
  return kStOK;
}

int StFcsTrackMatchMaker::Finish()
{
  if (mFile)
  {
    LOG_INFO << "Closing " << mFilename << endm;
    mFile->Write();
    mFile->Close();
  }
  return kStOK;
}

int StFcsTrackMatchMaker::Make()
{
  StEvent *event = (StEvent *)GetInputDS("StEvent");
  if (!event)
  {
    LOG_ERROR << "StFcsTrackMatchMaker::Make did not find StEvent" << endm;
    return kStErr;
  }
  mFcsColl = event->fcsCollection();
  if (!mFcsColl)
  {
    LOG_ERROR << "StFcsTrackMatchMaker::Make did not find StEvent->StFcsCollection" << endm;
    return kStErr;
  }
  mFwdTrkColl = event->fwdTrackCollection();
  if (!mFwdTrkColl)
  {
    LOG_ERROR << "StFcsTrackMatchMaker::Make did not find StEvent->fwdTrackCollection" << endm;
    return kStErr;
  }

  int ntrk = mFwdTrkColl->numberOfTracks();
  int ngoodtrk = 0;
  int nMatch[2] = {0, 0};
  for (int itrk = 0; itrk < ntrk; itrk++)
  {
    StFwdTrack *trk = mFwdTrkColl->tracks()[itrk];
    if (trk->didFitConvergeFully() == false)
      continue;
    ngoodtrk++;

    // get the projections
    StFwdTrackProjection projECAL, projHCAL, projEPD;
    LOG_INFO << "nProjections: "<< trk->mProjections.size() << endm;
    projECAL = trk->getProjectionFor( kFcsWcalId );
    projHCAL = trk->getProjectionFor( kFcsHcalId );
    projEPD  = trk->getProjectionFor( kFcsPresId );

    if (Debug())
    {
      
      LOG_INFO << Form("Proj0: %6.2f %6.2f %6.2f", projECAL.mXYZ.x(), projECAL.mXYZ.y(), projECAL.mXYZ.z()) << endm;
      LOG_INFO << Form("Proj1: %6.2f %6.2f %6.2f", projHCAL.mXYZ.x(), projHCAL.mXYZ.y(), projHCAL.mXYZ.z()) << endm;
      LOG_INFO << Form("Proj2: %6.2f %6.2f %6.2f", projEPD. mXYZ.x(), projEPD. mXYZ.y(), projEPD. mXYZ.z()) << endm;
    }

    // North or south from track
    int ns = 0;
    if (projECAL.mXYZ.x() > 0.0)
      ns = 1;
    // Look for a Ecal & Hcal match for a track
    for (int ehp = 0; ehp < 2; ehp++)
    {
      StThreeVectorD proj = projECAL.mXYZ;
      if ( ehp == 1 )
        proj = projHCAL.mXYZ;

      int det = ehp * 2 + ns;
      int nclu = mFcsColl->numberOfClusters(det);
      for (int iclu = 0; iclu < nclu; iclu++)
      {
        StFcsCluster *clu = mFcsColl->clusters(det)[iclu];
        float energy = clu->energy();
        if (energy > mMinEnergy[ehp])
        {
          StThreeVectorD xyz = mFcsDb->getStarXYZfromColumnRow(det, clu->x(), clu->y());
          double dx = xyz.x() - proj.x();
          double dy = xyz.y() - proj.y();
          double dr = sqrt(dx * dx + dy * dy);
          if (Debug())
            LOG_INFO << Form("EHP=%1d dx = %6.2f - %6.2f  = %6.2f dy = %6.2f - %6.2f  = %6.2f dr=%6.2f",
                             ehp, xyz.x(), proj.x(), dx, xyz.y(), proj.y(), dy, dr)
                     << endm;
          if (mFile)
          {
            if (fabs(dy) < mMaxDistance[ehp])
              mHdx[ehp]->Fill(dx);
            if (fabs(dx) < mMaxDistance[ehp])
              mHdy[ehp]->Fill(dy);
            mHdr[ehp]->Fill(dr);
          }
          if (dr < mMaxDistance[ehp])
          {
            if (ehp == 0)
            {
              trk->addEcalCluster(clu);
            }
            if (ehp == 1)
            {
              trk->addHcalCluster(clu);
            }
            clu->addTrack(trk);
            nMatch[ehp]++;
          }
        }
      }
    }
  }

  // Sort by ET/PT
  for (int itrk = 0; itrk < ntrk; itrk++)
  {
    StFwdTrack *trk = mFwdTrkColl->tracks()[itrk];
    if (trk->didFitConvergeFully() == false)
      continue;
    trk->sortEcalClusterByET();
    trk->sortHcalClusterByET();
    if (Debug())
    {
      LOG_INFO << Form("TRK pT=%6.2f Cg=%1d NEcal=%lu NHcal=%lu",
                       trk->momentum().perp(), trk->charge(),
                       trk->ecalClusters().size(),
                       trk->hcalClusters().size())
               << endm;
    }
  }
  for (int det = 0; det < 4; det++)
  {
    int ehp = det / 2;
    int nclu = mFcsColl->numberOfClusters(det);
    for (int iclu = 0; iclu < nclu; iclu++)
    {
      StFcsCluster *clu = mFcsColl->clusters(det)[iclu];
      clu->sortTrackByPT();
      if (Debug())
      {
        if (clu->energy() > mMinEnergy[ehp])
        {
          LOG_INFO << Form("FCS DET=%d ET=%6.2f NTrk=%ld",
                           clu->detectorId(), clu->fourMomentum().perp(), clu->tracks().size())
                   << endm;
        }
      }
    }
  }

  // Filling hitograms if file is specified
  if (mFile)
  {
    mNtrk[2]->Fill(ntrk);
    mNtrk[3]->Fill(ngoodtrk);
    for (int itrk = 0; itrk < ntrk; itrk++)
    {
      StFwdTrack *trk = mFwdTrkColl->tracks()[itrk];
      if (trk->didFitConvergeFully() == false)
        continue;

      // get the projections
      StFwdTrackProjection projECAL, projHCAL, projEPD;
      projECAL = trk->getProjectionFor( kFcsWcalId );
      projHCAL = trk->getProjectionFor( kFcsHcalId );
      projEPD = trk->getProjectionFor( kFcsPresId );

      mCharge[2]->Fill(trk->charge());
      mXY[2]->Fill(projECAL.mXYZ.x(), projECAL.mXYZ.y());
      double pt = trk->momentum().perp();
      int ne = trk->ecalClusters().size();
      int nh = trk->hcalClusters().size();
      mNclu[0]->Fill(double(ne));
      mNclu[1]->Fill(double(nh));
      double ete = 0;
      if (ne > 0)
      {
        StFcsCluster *eclu = trk->ecalClusters()[0]; // Take top ET ones
        ete = eclu->fourMomentum().perp();
        mPtEt[0]->Fill(ete / pt);
        mPtEt2[0]->Fill(ete, pt);
        mCharge[0]->Fill(trk->charge());
        mXY[0]->Fill(projECAL.mXYZ.x(), projECAL.mXYZ.y());
      }
      if (nh > 0)
      {
        StFcsCluster *hclu = trk->hcalClusters()[0]; // Take top ET ones
        double eth = hclu->fourMomentum().perp() + ete;
        mPtEt[1]->Fill(eth / pt);
        mPtEt2[1]->Fill(eth, pt);
        mCharge[1]->Fill(trk->charge());
        mXY[1]->Fill(projHCAL.mXYZ.x(), projHCAL.mXYZ.y());
      }
    }

    int nc[2] = {0, 0};
    for (int det = 0; det < 4; det++)
    {
      int ehp = det / 2;
      int nclu = mFcsColl->numberOfClusters(det);
      nc[ehp] += nclu;
      for (int iclu = 0; iclu < nclu; iclu++)
      {
        StFcsCluster *clu = mFcsColl->clusters(det)[iclu];
        if (clu->energy() > mMinEnergy[ehp])
        {
          mNtrk[ehp]->Fill(clu->tracks().size());
        }
      }
    }
    mNclu[2]->Fill(nc[0]);
    mNclu[3]->Fill(nc[1]);

    LOG_INFO << Form("NTrack=%5d NGoodTrack=%3d NEcalMatch=%3d NHcalMatch=%3d", ntrk, ngoodtrk, nMatch[0], nMatch[1]) << endm;
  }
  return kStOK;
}
