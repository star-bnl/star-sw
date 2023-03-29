// class StFcsPi0FinderForEcal
// author Xilin Liang
//
//

#ifndef STAR_StFcsPi0FinderForEcal_HH
#define STAR_StFcsPi0FinderForEcal_HH

#include "StEnumerations.h"
#include "StMaker.h"
#include "TCanvas.h"
#include "stdio.h"
//#include "TFile.h"
//#include "TH2F.h"
#include "StFcsDbMaker/StFcsDb.h"

class TH1F;
class TH2F;
//class TApplication;
//class StFcsPi0FinderForEcal;
class StFcsDbMaker;
class StFcsCollection;
class StFcsDb;

class StFcsPi0FinderForEcal : public StMaker {
  public:
   StFcsPi0FinderForEcal(const Char_t* name = "FcsPi0F");
   ~StFcsPi0FinderForEcal();
   Int_t Init();
   Int_t Make();
   Int_t Finish();
   void st(Int_t run2) { filename = Form("StFcsPi0invariantmass%i.root", run2); };

  private:
   StFcsDb* mFcsDb = 0;
   //StFcsDbMaker* mFcsDbMaker=0;
   StFcsCollection* mFcsColl = 0;
   TH1F* h1_num_entries = 0;                //h1_num_entries:# of entries
   TH1F* h1_inv_mass_cluster = 0;           //h1_inv_mass_cluster:invariant mass
   TH1F* h1_Zgg_cluster = 0;                //h1_Zgg:Zgg
   TH1F* h1_opening_angle_cluster = 0;      //h1_opening_angle:opening angle
   TH1F* h1_each_cluster_energy = 0;        //h1_each_cluster_energy:each cluster energy(no cut)
   TH1F* h1_two_cluster_energy_nocut = 0;   //h1_two_cluster_energy_nocut:2 cluster energy(no cut)
   TH1F* h1_two_cluster_energy_allcut = 0;  //h1_two_cluster_energy_allcut:2 cluster energy(all cut)
   TH1F* h1_dgg_cluster = 0;                //h1_dgg_cluster:2 dgg(all cut) distance bewteen two clusters at the det.
   TH1F* h1_Zgg_nocut_cluster = 0;          //h1_Zgg_nocut_cluster:Zgg without cut
   TH1I* h1_nCluster = 0;                   //h1_nCluster: number of clusters
   TH1F* h1_inv_mass_cluster_nocut = 0;     //h1_inv_mass_cluster:invariant mass no cut
   TH1I* h1_nclu_good = 0;                  //h1_nclu_good: number of good clusters
   TH1I* h1_clu_nTowers = 0;                //h1_clu_nTowers: number of towers in cluster

   TH1F* h1_inv_mass_point = 0;           //h1_inv_mass_point:invariant mass
   TH1F* h1_Zgg_point = 0;                //h1_Zgg:Zgg
   TH1F* h1_opening_angle_point = 0;      //h1_opening_angle:opening angle
   TH1F* h1_each_point_energy = 0;        //h1_each_point_energy:each point energy(no cut)
   TH1F* h1_two_point_energy_nocut = 0;   //h1_two_point_energy_nocut:2 point energy(no cut)
   TH1F* h1_two_point_energy_allcut = 0;  //h1_two_point_energy_allcut:2 point energy(all cut)
   TH1F* h1_dgg_point = 0;                //h1_dgg_point:2 dgg(all cut) distance bewteen two points at the det.
   TH1F* h1_Zgg_nocut_point = 0;          //h1_Zgg_nocut_point:Zgg without cut:
   TH1F* h1list_mass_by_Ntower[748];      //h1list_mass_by_Ntower: invariant mass sorted by highest energy tower[64]
   TH1F* h1list_mass_by_Stower[748];      //h1list_mass_by_Stower: invariant mass sorted by highest energy tower[64]
   TH1F* h1list_NEtower[748];             //h1list_NEtower: energy spectrum for north Ecal tower (no cut)
   TH1F* h1list_SEtower[748];             //h1list_SEtower: energy spectrum for south Ecal tower (no cut)
   TH1F* h1_EcalMult_E1 = 0;              //h1_EcalMult_E1: Ecal Milt (E>1)
   TH1I* h1_nPoint = 0;                   //h1_nPoint: number of point
   TH1I* h1_npoi_good = 0;                //h1_npoi_good: number of good points
   TH1F* h1_inv_mass_point_nocut = 0;     //h1_inv_mass_point:invariant mass no cut

   TH2F* h2_EcalMult_vs_TofMult = 0;     //h2_EcalMult_vs_TofMult
   TH2F* h2_cluster_position = 0;        //h2_cluster_position
   TH2F* h2_cluster_position_cut = 0;    //h2_cluster_position_cut
   TH2F* h2_point_position = 0;          //h2_point_position
   TH2F* h2_cluster_invmass_vs_dgg = 0;  //h2_cluster_invmass_vs_dgg
   TH2F* h2_cluster_invmass_vs_Zgg = 0;  //h2_cluster_invmass_vs_Zgg
   TH2F* h2_cluster_dgg_vs_E1pE2 = 0;    //h2_cluster_dgg_vs_E1+E2
   TH2F* h2_point_invmass_vs_dgg = 0;    //h2_cluster_invmass_vs_dgg
   TH2F* h2_point_invmass_vs_Zgg = 0;    //h2_cluster_invmass_vs_Zgg
   TH2F* h2_point_dgg_vs_E1pE2 = 0;      //h2_cluster_dgg_vs_E1+E2

   int mDebug = 0;
   int mFilter = 0;
   int mNEvents = -1;
   int mNAccepted = 0;
   int mMaxEvents = 10000;
   int bins = 150;
   string filename;
   float m_low = 0;
   float m_up = 0.4;
   float E_up = 10;
   float E_low = 8;
   float E_min = 1;

#ifndef SKIPDefImp
   ClassDef(StFcsPi0FinderForEcal, 0)
#endif
};

#endif
