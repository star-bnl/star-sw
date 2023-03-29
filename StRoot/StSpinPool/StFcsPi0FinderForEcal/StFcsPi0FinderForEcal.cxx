//class StFcsPi0FinderForEcal
//author Xilin Liang
//
//critical plots for offline QA: h1_inv_mass_cluster , h1_two_cluster_energy_allcut , h1_dgg_cluster  , h2_cluster_dgg_vs_E1pE2
//
//

#include "StFcsPi0FinderForEcal.h"

#include "StEvent/StEnumerations.h"
#include "StEvent/StEvent.h"
#include "StEvent/StFcsCluster.h"
#include "StEvent/StFcsCollection.h"
#include "StEvent/StFcsHit.h"
#include "StEventTypes.h"
#include "StFcsDbMaker/StFcsDbMaker.h"
#include "StMessMgr.h"
#include "StMuDSTMaker/COMMON/StMuTypes.hh"
#include "StSpinPool/StFcsQaMaker/StFcsQaMaker.h"
#include "StSpinPool/StFcsRawDaqReader/StFcsRawDaqReader.h"
#include "StThreeVectorF.hh"
#include "Stypes.h"
#include "TBox.h"
#include "TCanvas.h"
#include "TColor.h"
#include "TFile.h"
#include "TH1.h"
#include "TH2.h"
#include "TLine.h"
#include "TMarker.h"
#include "TROOT.h"
#include "TString.h"
#include "TStyle.h"
#include "TText.h"

#ifndef SKIPDefImp
ClassImp(StFcsPi0FinderForEcal)
#endif

    //------------------------
    StFcsPi0FinderForEcal::StFcsPi0FinderForEcal(const Char_t* name) : StMaker(name) {
}

StFcsPi0FinderForEcal::~StFcsPi0FinderForEcal() {}

//-----------------------
Int_t StFcsPi0FinderForEcal::Init() {
   mFcsDb = static_cast<StFcsDb*>(GetDataSet("fcsDb"));
   if (!mFcsDb) {
      LOG_ERROR << "StFcsEventDisplay::InitRun Failed to get StFcsDbMaker" << endm;
      return kStFatal;
   }

   h1_num_entries = new TH1F("h1_num_entries", "# of entries", 10, 0, 10);
   h1_inv_mass_cluster = new TH1F("h1_inv_mass_cluster", "invariant mass plot for FCS ECal cluster", bins, m_low, m_up);
   h1_inv_mass_cluster->SetXTitle("invariant mass [GeV]");
   h1_Zgg_cluster = new TH1F("h1_Zgg_cluster", "Zgg", bins, 0, .7);
   h1_Zgg_cluster->SetXTitle("Z_{gg}");
   h1_opening_angle_cluster = new TH1F("h1_opening_angle_cluster", "FCS ECal cluster pair opening angle", bins, 0, 0.1);
   h1_opening_angle_cluster->SetXTitle("opening angle [rad]");
   h1_each_cluster_energy = new TH1F("h1_each_cluster_energy", "each cluster energy for FCS ECal", bins, 0, 25);
   h1_each_cluster_energy->SetXTitle("E_{1} [GeV]");
   h1_two_cluster_energy_nocut = new TH1F("h1_two_cluster_energy_nocut", "2 clusters energy(no cut)", bins, 0, 30);
   h1_two_cluster_energy_nocut->SetXTitle("E_{1} + E_{2} [GeV]");
   h1_two_cluster_energy_allcut = new TH1F("h1_two_cluster_energy_allcut", "2 clusters total energy for FCS ECal", bins, 0, 30);
   h1_two_cluster_energy_allcut->SetXTitle("E_{1} + E_{2} [GeV]");
   h1_dgg_cluster = new TH1F("h1_dgg_cluster", "2 cluster distance for FCS ECal", bins, 0, 250);
   h1_dgg_cluster->SetXTitle("d_{gg} [cm]");
   h1_Zgg_nocut_cluster = new TH1F("h1_Zgg_nocut_cluster", "Zgg without cut", bins, 0, 1);
   h1_Zgg_nocut_cluster->SetXTitle("Z_{gg}");
   h1_nCluster = new TH1I("h1_nCluster", "Number of clusters", 40, 0, 40);
   h1_inv_mass_cluster_nocut = new TH1F("h1_inv_mass_cluster_nocut", "invariant mass plot (no cut)", bins, m_low, m_up);
   h1_inv_mass_cluster_nocut->SetXTitle("invariant mass [GeV]");
   h1_nclu_good = new TH1I("h1_nclu_good", "number of good clusters", 50, 0, 50);
   h1_clu_nTowers = new TH1I("h1_clu_nTowers", "number of towers for cluster", 15, 0, 15);
   h1_clu_nTowers->SetXTitle("n Towers");

   //point
   h1_inv_mass_point = new TH1F("h1_inv_mass_point", "invariant mass plot for FCS ECal point", bins, m_low, m_up);
   h1_inv_mass_point->SetXTitle("invariant mass [GeV]");
   h1_Zgg_point = new TH1F("h1_Zgg_point", "energy asymmetry for FCS ECal point", bins, 0, .7);
   h1_Zgg_point->SetXTitle("Z_{gg}");
   h1_opening_angle_point = new TH1F("h1_opening_angle_point", "opening angle plot", bins, 0, 0.1);
   h1_opening_angle_point->SetXTitle("opening angle [rad]");
   h1_each_point_energy = new TH1F("h1_each_point_energy", "each point energy for FCS ECal", bins, 0, 25);
   h1_each_point_energy->SetXTitle("E_{1} [GeV]");
   h1_two_point_energy_nocut = new TH1F("h1_two_point_energy_nocut", "2 points energy(no cut)", bins, 0, 30);
   h1_two_point_energy_nocut->SetXTitle("E_{1} + E_{2} [GeV]");
   h1_two_point_energy_allcut = new TH1F("h1_two_point_energy_allcut", "2 points energy for FCS ECal point", bins, 0, 50);
   h1_two_point_energy_allcut->SetXTitle("E_{1} + E_{2} [GeV]");
   h1_dgg_point = new TH1F("h1_dgg_point", "d_{gg}", bins, 0, 250);
   h1_dgg_point->SetXTitle("d_{gg} [cm]");
   h1_Zgg_nocut_point = new TH1F("h1_Zgg_nocut_point", "Zgg without cut", bins, 0, 1);
   h1_Zgg_nocut_point->SetXTitle("Z_{gg}");
   h1_EcalMult_E1 = new TH1F("h1_EcalMult_E1", "Ecal Mult (E>1)", 50, 0, 50);
   h1_EcalMult_E1->SetXTitle("Ecal Mult");
   h1_nPoint = new TH1I("h1_nPoint", "Number of points", 40, 0, 40);
   h1_inv_mass_point_nocut = new TH1F("h1_inv_mass_point_nocut", "#pi^{0} invariant mass plot (no cut)", bins, m_low, m_up);
   h1_inv_mass_point_nocut->SetXTitle("#pi^{0} invariant mass [GeV]");
   h1_npoi_good = new TH1I("h1_npoi_good", "number of good points", 50, 0, 50);

   for (int i = 0; i < 748; i++) {
      char name_hist[50];
      char title_hist[100];
      sprintf(name_hist, "mass_by_Ntower_%i", i);
      sprintf(title_hist, "invariant mass by %i North tower ", i);
      h1list_mass_by_Ntower[i] = new TH1F(name_hist, title_hist, 50, m_low, m_up);
      h1list_mass_by_Ntower[i]->SetXTitle("invariant mass [GeV]");
      sprintf(name_hist, "mass_by_Stower_%i", i);
      sprintf(title_hist, "invariant mass by %i Sorth tower ", i);
      h1list_mass_by_Stower[i] = new TH1F(name_hist, title_hist, 50, m_low, m_up);
      h1list_mass_by_Stower[i]->SetXTitle("invariant mass [GeV]");
      sprintf(name_hist, "NEtower_%i", i);
      sprintf(title_hist, "North %i tower energy spectrum", i + 1);
      h1list_NEtower[i] = new TH1F(name_hist, title_hist, 200, 0, 20);
      h1list_NEtower[i]->SetXTitle("Energy [GeV]");
      sprintf(name_hist, "SEtower_%i", i);
      sprintf(title_hist, "South %i tower energy spectrum", i + 1);
      h1list_SEtower[i] = new TH1F(name_hist, title_hist, 200, 0, 20);
      h1list_SEtower[i]->SetXTitle("Energy [GeV]");
   }

   h2_EcalMult_vs_TofMult = new TH2F("h2_EcalMult_vs_TofMult", "Ecal Mult vs Tof Mult", 200, 0, 200, 200, 0, 200);
   h2_EcalMult_vs_TofMult->SetXTitle("Tof Multiplicity");
   h2_EcalMult_vs_TofMult->SetYTitle("Ecal Multiplicity");
   h2_cluster_position = new TH2F("h2_cluster_position", "cluster_position", 300, -150, 150, 300, -150, 150);
   h2_cluster_position->SetXTitle("x [cm]");
   h2_cluster_position->SetYTitle("y [cm]");
   h2_cluster_position_cut = new TH2F("h2_cluster_position_cut", "cluster position after cut", 400, -200, 200, 350, -150, 150);
   h2_cluster_position_cut->SetXTitle("x [cm]");
   h2_cluster_position_cut->SetYTitle("y [cm]");
   h2_point_position = new TH2F("h2_point_position", "point_position", 300, -150, 150, 300, -150, 150);
   h2_point_position->SetXTitle("x [cm]");
   h2_point_position->SetYTitle("y [cm]");

   h2_cluster_invmass_vs_dgg = new TH2F("h2_cluster_invmass_vs_dgg", "invariant mass vs d_{gg} (cluster)", 60, 0, 60, 60, 0, 0.3);
   h2_cluster_invmass_vs_dgg->SetXTitle("d_{gg} [cm]");
   h2_cluster_invmass_vs_dgg->SetYTitle("m_{gg} [GeV]");
   h2_cluster_invmass_vs_Zgg = new TH2F("h2_cluster_invmass_vs_Zgg", "invariant mass vs Z_{gg} (cluster)", 35, 0, 0.7, 60, 0, 0.3);
   h2_cluster_invmass_vs_Zgg->SetXTitle("Z_{gg} ");
   h2_cluster_invmass_vs_Zgg->SetYTitle("m_{gg} [GeV]");
   h2_point_invmass_vs_dgg = new TH2F("h2_point_invmass_vs_dgg", "invariant mass vs d_{gg} (cluster)", 60, 0, 60, 60, 0, 0.6);
   h2_point_invmass_vs_dgg->SetXTitle("d_{gg} [cm]");
   h2_point_invmass_vs_dgg->SetYTitle("m_{gg} [GeV]");
   h2_point_invmass_vs_Zgg = new TH2F("h2_point_invmass_vs_Zgg", "invariant mass vs Z_{gg} (cluster)", 35, 0, 0.7, 60, 0, 0.6);
   h2_point_invmass_vs_Zgg->SetXTitle("Z_{gg} ");
   h2_point_invmass_vs_Zgg->SetYTitle("m_{gg} [GeV]");
   h2_cluster_dgg_vs_E1pE2 = new TH2F("h2_cluster_dgg_vs_E1pE2", "2 cluster distance vs total energy for FCS ECal cluster pair", 20, 0, 20, 60, 0, 60);
   h2_cluster_dgg_vs_E1pE2->SetXTitle("E_{1}+E_{2} [GeV]");
   h2_cluster_dgg_vs_E1pE2->SetYTitle("d_{gg} [cm]");
   h2_point_dgg_vs_E1pE2 = new TH2F("h2_point_dgg_vs_E1pE2", "energy asymmetry vs total energy for FCS ECal point pair", 20, 0, 20, 60, 0, 60);
   h2_point_dgg_vs_E1pE2->SetXTitle("E_{1}+E_{2} [GeV]");
   h2_point_dgg_vs_E1pE2->SetYTitle("d_{gg} [cm]");

   return kStOK;
}

//-----------------------
Int_t StFcsPi0FinderForEcal::Finish() {
   if (filename.length() == 0) return kStOk;
   const char* fn = filename.c_str();
   TFile MyFile(fn, "RECREATE");
   h1_num_entries->Write();
   h1_inv_mass_cluster->Write();
   h1_Zgg_cluster->Write();
   h1_opening_angle_cluster->Write();
   h1_each_cluster_energy->Write();
   h1_two_cluster_energy_nocut->Write();
   h1_two_cluster_energy_allcut->Write();
   h1_dgg_cluster->Write();
   h1_Zgg_nocut_cluster->Write();
   h1_inv_mass_cluster_nocut->Write();
   h1_nCluster->Write();
   h1_nclu_good->Write();
   h1_clu_nTowers->Write();

   h1_num_entries->Write();
   h1_inv_mass_point->Write();
   h1_Zgg_point->Write();
   h1_opening_angle_point->Write();
   h1_each_point_energy->Write();
   h1_two_point_energy_nocut->Write();
   h1_two_point_energy_allcut->Write();
   h1_dgg_point->Write();
   h1_Zgg_nocut_point->Write();
   h1_EcalMult_E1->Write();
   h1_inv_mass_point_nocut->Write();
   h1_nPoint->Write();
   h1_npoi_good->Write();

   for (int i = 0; i < 748; i++) {
      h1list_mass_by_Ntower[i]->Write();
      h1list_mass_by_Stower[i]->Write();
      h1list_NEtower[i]->Write();
      h1list_SEtower[i]->Write();
   }

   h2_EcalMult_vs_TofMult->Write();
   h2_cluster_position->Write();
   h2_cluster_position_cut->Write();
   h2_point_position->Write();
   h2_cluster_invmass_vs_dgg->Write();
   h2_cluster_invmass_vs_Zgg->Write();
   h2_point_invmass_vs_dgg->Write();
   h2_point_invmass_vs_Zgg->Write();
   h2_cluster_dgg_vs_E1pE2->Write();
   h2_point_dgg_vs_E1pE2->Write();

   MyFile.Close();
   return kStOK;
}

//----------------------
Int_t StFcsPi0FinderForEcal::Make() {
   cout << "Start using StFcsPi0FinderForECal" << endl;
   StEvent* event = (StEvent*)GetInputDS("StEvent");
   if (!event) {
      LOG_ERROR << "StFcsPi0FinderForEcal::Make did not find StEvent" << endm;
      return kStErr;
   }
   mFcsColl = event->fcsCollection();
   if (!mFcsColl) {
      LOG_ERROR << "StFcsPi0FinderForEcal::Make did not find StEvent->StFcsCollection" << endm;
      return kStErr;
   }

   if (mNAccepted < mMaxEvents) {
      mNEvents++;
      cout << "current event:" << mNEvents << endl;
      if (mFilter == 1 && mFcsColl->numberOfHits(0) + mFcsColl->numberOfHits(1) + mFcsColl->numberOfHits(2) + mFcsColl->numberOfHits(3) == 0) return kStOK;

      //TOF mult cut                                                                                                     
      int tofMult = 0;
      const StTriggerData* trgdata = event->triggerData();
      if(!trgdata && StMuDst::event()) trgdata = StMuDst::event()->triggerData();
      if(trgdata){
	tofMult = trgdata->tofMultiplicity();
	LOG_DEBUG<<"TOF mult="<<tofMult<<endm;
	if (tofMult > 100) return kStOK;
      }else{
	LOG_WARN << "No TriggerData found in StEvent nor Mudst. No TOFMult cut"<<endm;
      }

      //TPC ZVERTEX
      float zTPC=-999.0;
      StPrimaryVertex* tpcvtx = event->primaryVertex();
      if(tpcvtx) {
	zTPC=tpcvtx->position().z();
      }else{
	if (StMuDst::numberOfPrimaryVertices() > 0){
	  StMuPrimaryVertex* mutpcvtx=StMuDst::primaryVertex();
	  if(mutpcvtx) zTPC=mutpcvtx->position().z();
	}
      }

      //BBC ZVERTEX
      float zBBC=-999.0;
      if(trgdata) zBBC = (4096 - trgdata->bbcTimeDifference())*0.016*30.0/2.0;      
      if(zBBC<-200 || zBBC>200) zBBC=-999;

      //VPD ZVERTEX from MuDst(TOF data)
      float zVPD=-999.0;
      if(StMuDst::btofHeader()) zVPD=StMuDst::btofHeader()->vpdVz();

      LOG_INFO << Form("ZTPX = %6.2f ZBBC = %6.2f ZVPD = %6.2f",zTPC,zBBC,zVPD) << endm;

      //test getLorentzVector
      StThreeVectorD xyz(20,0,720);     
      StLorentzVectorD pbbc,ptpc,pvpd;
      StLorentzVectorD p0 = mFcsDb->getLorentzVector((xyz), 10,    0);  LOG_INFO << "Zero " << p0 << endm;  	   
      if(zBBC>-200) {pbbc  = mFcsDb->getLorentzVector((xyz), 10, zBBC);	LOG_INFO << "BBC  " << pbbc << endm;}
      if(zTPC>-200) {ptpc  = mFcsDb->getLorentzVector((xyz), 10, zTPC);	LOG_INFO << "TPC  " << ptpc << endm;}
      if(zVPD>-200) {pvpd  = mFcsDb->getLorentzVector((xyz), 10, zVPD); LOG_INFO << "VPD  " << pvpd << endm;}    

      mNAccepted++;
      int total_nc = 0;
      int total_np = 0;
      int check_fillclu = 0;
      int check_fillpnt = 0;

      int n_EcalMult = 0;
      int n_EcalClustMult = 0;

      int n_EcalClust_cut = 0;
      int n_EcalPoint_cut = 0;
      int n_Ecal_cut = 0;

      float bestclu_invmass = -1;
      float bestclu_totalE = -1;
      float bestclu_dgg = -1;
      float bestclu_Zgg = -1;
      float bestclu_opening_angle = -1;
      float bestpnt_invmass = -1;
      float bestpnt_totalE = -1;
      float bestpnt_dgg = -1;
      float bestpnt_Zgg = -1;
      float bestpnt_opening_angle = -1;
      int best_tower_id1_cluster = -1;
      int best_tower_id2_cluster = -1;
      int best_tower_det_cluster = -1;

      for (int det = 0; det < 2; det++) {
         int check_Ecal = mFcsDb->ecalHcalPres(det);
         if (check_Ecal != 0) continue;

         StSPtrVecFcsCluster& clusters = mFcsColl->clusters(det);
         int nc = mFcsColl->numberOfClusters(det);

         total_nc = nc + total_nc;

         StSPtrVecFcsPoint& points = mFcsColl->points(det);
         int np = mFcsColl->numberOfPoints(det);

         total_np = np + total_np;

         StSPtrVecFcsHit& hits = mFcsColl->hits(det);
         int nh = mFcsColl->numberOfHits(det);
         if (mDebug > 0) LOG_INFO << Form("StFcsEventDisplay Det=%1d nhit=%4d nclu=%3d", det, nh, nc) << endm;

         for (int i = 0; i < nh; i++) {
            StFcsHit* hit = hits[i];
            unsigned short hit_id = hit->id();

            float hit_energy = hit->energy();
            if (det == 0) {
               h1list_NEtower[hit_id]->Fill(hit_energy);
            } else if (det == 1) {
               h1list_SEtower[hit_id]->Fill(hit_energy);
            }  //fill in energy spectrum for tower
            if (hit_energy > E_min) n_Ecal_cut++;
            n_EcalMult++;
         }

         //			//no cut (cluster)
         for (int i = 0; i < nc; i++) {
            StFcsCluster* clu = clusters[i];
            float clu_energy = clu->energy();
            float clu_x = clu->x();
            float clu_y = clu->y();
            StThreeVectorD cluPos = mFcsDb->getStarXYZfromColumnRow(det, clu_x, clu_y);
            float cluPos_x = cluPos.x();
            float cluPos_y = cluPos.y();

            n_EcalClustMult++;
            if ((clu_energy > E_min)) {
               n_EcalClust_cut++;
            }

            h2_cluster_position->Fill(cluPos_x, cluPos_y);
            h1_each_cluster_energy->Fill(clu_energy);
            StThreeVectorD xyz = mFcsDb->getStarXYZfromColumnRow(det, clu_x, clu_y);
            StLorentzVectorD p = mFcsDb->getLorentzVector((xyz), clu_energy,    0);	   
            if (i == nc - 1) continue;
            for (int j = i + 1; j < nc; j++) {
               StFcsCluster* cluj = clusters[j];
               float cluj_energy = cluj->energy();
               float cluj_x = cluj->x();
               float cluj_y = cluj->y();
               StThreeVectorD clujPos = mFcsDb->getStarXYZfromColumnRow(det, cluj_x, cluj_y);

               h1_two_cluster_energy_nocut->Fill(clu_energy + cluj_energy);
               float zgg = (abs(clu_energy - cluj_energy)) / (clu_energy + cluj_energy);
               h1_Zgg_nocut_cluster->Fill(zgg);
               StThreeVectorD xyzj = mFcsDb->getStarXYZfromColumnRow(det, cluj->x(), cluj->y());
               StLorentzVectorD pj = mFcsDb->getLorentzVector((xyzj), cluj->energy(), 0);
               h1_inv_mass_cluster_nocut->Fill((p + pj).m());
            }
         }

         //			//no cut (point)
         for (int i = 0; i < np; i++) {
            StFcsPoint* pnt = points[i];
            float pnt_x = pnt->x();
            float pnt_y = pnt->y();
            float pnt_energy = pnt->energy();
            if ((pnt_energy > E_min)) {
               n_EcalPoint_cut++;
            }
            StThreeVectorD poiPos = mFcsDb->getStarXYZfromColumnRow(det, pnt_x, pnt_y);
            float poiPos_x = poiPos.x();
            float poiPos_y = poiPos.y();
            h2_point_position->Fill(poiPos_x, poiPos_y);
            h1_each_point_energy->Fill(pnt_energy);
            StThreeVectorD xyz = mFcsDb->getStarXYZfromColumnRow(det, pnt_x, pnt_y);
            StLorentzVectorD p = mFcsDb->getLorentzVector((xyz), pnt_energy, 0);
            if (i == np - 1) continue;
            for (int j = i + 1; j < np; j++) {
               StFcsPoint* pntj = points[j];
               float pntj_energy = pntj->energy();

               h1_two_point_energy_nocut->Fill(pnt_energy + pntj_energy);
               float zgg = (abs(pnt_energy - pntj_energy)) / (pnt_energy + pntj_energy);
               h1_Zgg_nocut_point->Fill(zgg);
               StThreeVectorD xyzj = mFcsDb->getStarXYZfromColumnRow(det, pntj->x(), pntj->y());
               StLorentzVectorD pj = mFcsDb->getLorentzVector((xyzj), pntj->energy(), 0);
               h1_inv_mass_point_nocut->Fill((p + pj).m());
            }
         }

         //start cut (cluster)
         for (int i = 0; i < nc; i++) {
            StFcsCluster* clu = clusters[i];
            float clu_x = clu->x();
            float clu_y = clu->y();
            float clu_energy = clu->energy();
            h1_clu_nTowers->Fill(clu->nTowers());
            if ((clu->energy()) < E_min) continue;

            StThreeVectorD xyz = mFcsDb->getStarXYZfromColumnRow(det, clu_x, clu_y);
            StLorentzVectorD p = mFcsDb->getLorentzVector((xyz), clu_energy, 0);
            h2_cluster_position_cut->Fill(xyz.x(), xyz.y());
            for (int j = i + 1; j < nc; j++) {
               StFcsCluster* cluj = clusters[j];
               float cluj_energy = cluj->energy();

               if ((cluj->energy()) < E_min) continue;
               float zgg = (fabs(clu_energy - cluj_energy)) / (clu_energy + cluj_energy);
               if (zgg > 0.7) continue;
               float cluj_x = cluj->x();
               float cluj_y = cluj->y();
               StThreeVectorD xyzj = mFcsDb->getStarXYZfromColumnRow(det, cluj_x, cluj_y);
               StLorentzVectorD pj = mFcsDb->getLorentzVector((xyzj), cluj_energy, 0);

               if ((clu_energy + cluj_energy) > bestclu_totalE) {
                  check_fillclu = 1;
                  bestclu_invmass = ((p + pj).m());
                  bestclu_totalE = (clu_energy + cluj_energy);
                  bestclu_dgg = (sqrt((xyz[0] - xyzj[0]) * (xyz[0] - xyzj[0]) + (xyz[1] - xyzj[1]) * (xyz[1] - xyzj[1]) + (xyz[2] - xyzj[2]) * (xyz[2] - xyzj[2])));
                  bestclu_Zgg = fabs((clu_energy - cluj_energy) / (clu_energy + cluj_energy));
                  bestclu_opening_angle = acos((xyz[0] * xyzj[0] + xyz[1] * xyzj[1] + xyz[2] * xyzj[2]) / (sqrt(xyz[0] * xyz[0] + xyz[1] * xyz[1] + xyz[2] * xyz[2]) * sqrt(xyzj[0] * xyzj[0] + xyzj[1] * xyzj[1] + xyzj[2] * xyzj[2])));

                  int pnti_nTowers = clu->nTowers();
                  int pntj_nTowers = cluj->nTowers();
                  StPtrVecFcsHit& clui_hits = clu->hits();
                  StPtrVecFcsHit& cluj_hits = cluj->hits();
                  float max_energy_tower = -1;
                  unsigned short tower_id = 0;
                  for (int k = 0; k < pnti_nTowers; k++) {
                     if (clui_hits[k]->energy() > max_energy_tower) {
                        max_energy_tower = clui_hits[k]->energy();
                        tower_id = clui_hits[k]->id();
                        best_tower_det_cluster = det;
                     }
                  }
                  best_tower_id1_cluster = tower_id;
                  max_energy_tower = -1;
                  for (int k = 0; k < pntj_nTowers; k++) {
                     if ((cluj_hits[k]->energy()) > max_energy_tower) {
                        max_energy_tower = cluj_hits[k]->energy();
                        tower_id = cluj_hits[k]->id();
                     }
                  }
                  best_tower_id2_cluster = tower_id;
               }
            }
         }

         //
         //
         //			//all cut (point)
         for (int i = 0; i < np - 1; i++) {
            StFcsPoint* pnt = points[i];
            float pnt_x = pnt->x();
            float pnt_y = pnt->y();
            float pnt_energy = pnt->energy();
            if (pnt_energy < E_min) continue;
            StThreeVectorD xyz = mFcsDb->getStarXYZfromColumnRow(det, pnt_x, pnt_y);
            StLorentzVectorD p = mFcsDb->getLorentzVector((xyz), pnt_energy, 0);

            for (int j = i + 1; j < np; j++) {
               StFcsPoint* pntj = points[j];
               float pntj_energy = pntj->energy();

               if ((pntj->energy()) < E_min) continue;
               float zgg = (abs(pnt_energy - pntj_energy)) / (pnt_energy + pntj_energy);
               if (zgg >= 0.7) continue;
               StThreeVectorD xyzj = mFcsDb->getStarXYZfromColumnRow(det, pntj->x(), pntj->y());
               StLorentzVectorD pj = mFcsDb->getLorentzVector((xyzj), pntj->energy(), 0);
               if ((pnt_energy + pntj_energy) > bestpnt_totalE) {
                  check_fillpnt = 1;
                  bestpnt_invmass = ((p + pj).m());
                  bestpnt_totalE = (pnt_energy + pntj_energy);
                  bestpnt_dgg = (sqrt((xyz[0] - xyzj[0]) * (xyz[0] - xyzj[0]) + (xyz[1] - xyzj[1]) * (xyz[1] - xyzj[1]) + (xyz[2] - xyzj[2]) * (xyz[2] - xyzj[2])));
                  bestpnt_Zgg = fabs((pnt_energy - pntj_energy) / (pnt_energy + pntj_energy));
                  bestpnt_opening_angle = acos((xyz[0] * xyzj[0] + xyz[1] * xyzj[1] + xyz[2] * xyzj[2]) / (sqrt(xyz[0] * xyz[0] + xyz[1] * xyz[1] + xyz[2] * xyz[2]) * sqrt(xyzj[0] * xyzj[0] + xyzj[1] * xyzj[1] + xyzj[2] * xyzj[2])));
               }
            }
         }
      }

      h1_nCluster->Fill(total_nc);
      h1_nclu_good->Fill(n_EcalClust_cut);
      h1_nPoint->Fill(total_np);
      h1_npoi_good->Fill(n_EcalPoint_cut);
      h2_EcalMult_vs_TofMult->Fill(tofMult, n_Ecal_cut);
      if (n_Ecal_cut > 40) {
         return kStOK;
      }
      if (check_fillclu == 1) {
         h1_inv_mass_cluster->Fill(bestclu_invmass);
         h1_two_cluster_energy_allcut->Fill(bestclu_totalE);
         h2_cluster_invmass_vs_dgg->Fill(bestclu_dgg, bestclu_invmass);
         h2_cluster_invmass_vs_Zgg->Fill(bestclu_Zgg, bestclu_invmass);
         h1_Zgg_cluster->Fill(bestclu_Zgg);
         h1_opening_angle_cluster->Fill(bestclu_opening_angle);
         h1_dgg_cluster->Fill(bestclu_dgg);
         h2_cluster_dgg_vs_E1pE2->Fill(bestclu_totalE, bestclu_dgg);
         if (best_tower_det_cluster == 0) {
            h1list_mass_by_Ntower[best_tower_id1_cluster]->Fill(bestclu_invmass);
            h1list_mass_by_Ntower[best_tower_id2_cluster]->Fill(bestclu_invmass);
         }
         if (best_tower_det_cluster == 1) {
            h1list_mass_by_Stower[best_tower_id1_cluster]->Fill(bestclu_invmass);
            h1list_mass_by_Stower[best_tower_id2_cluster]->Fill(bestclu_invmass);
         }
      }

      if (check_fillpnt == 1) {
         h1_inv_mass_point->Fill(bestpnt_invmass);
         h1_two_point_energy_allcut->Fill(bestpnt_totalE);
         h2_point_invmass_vs_dgg->Fill(bestpnt_dgg, bestpnt_invmass);
         h2_point_invmass_vs_Zgg->Fill(bestpnt_Zgg, bestpnt_invmass);
         h1_Zgg_point->Fill(bestpnt_Zgg);
         h1_opening_angle_point->Fill(bestpnt_opening_angle);
         h1_dgg_point->Fill(bestpnt_dgg);
         h2_point_dgg_vs_E1pE2->Fill(bestpnt_totalE, bestpnt_dgg);
      }
   }
   return kStOK;
}
