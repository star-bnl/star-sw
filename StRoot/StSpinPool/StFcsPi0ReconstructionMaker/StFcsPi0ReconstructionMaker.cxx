//class StFcsPi0ReconstructionMaker
//author Xilin Liang
//
//April 2, 2020: 
//

#include "StFcsPi0ReconstructionMaker.h"
#include "StEvent/StEnumerations.h"
#include "StMessMgr.h"
#include "Stypes.h"
#include "StMuDSTMaker/COMMON/StMuTypes.hh"

#include "StThreeVectorF.hh"
#include "StFcsDbMaker/StFcsDbMaker.h"
#include "StRoot/StSpinPool/StFcsQaMaker/StFcsQaMaker.h"
#include "StRoot/StSpinPool/StFcsRawDaqReader/StFcsRawDaqReader.h"
#include "StRoot/StEvent/StTriggerData.h"
#include "StRoot/StEvent/StFcsCollection.h"
#include "StRoot/StEvent/StFcsHit.h"
#include "StRoot/StEvent/StFcsCluster.h"
#include "StRoot/StEvent/StFcsPoint.h"
#include "StRoot/StEvent/StEvent.h"
#include "StEventTypes.h"
#include "TCanvas.h"
#include "TFile.h"
#include "TH1F.h"
#include "TH2F.h"
#include "TLine.h"
#include "TBox.h"
#include "TMarker.h"
#include "TString.h"
#include "TColor.h"
#include "TStyle.h"
#include "TText.h"
#include "TROOT.h"

ClassImp(StFcsPi0ReconstructionMaker);

//------------------------
StFcsPi0ReconstructionMaker::StFcsPi0ReconstructionMaker(const Char_t* name):
	StMaker(name){}

StFcsPi0ReconstructionMaker::~StFcsPi0ReconstructionMaker(){}

//-----------------------
Int_t StFcsPi0ReconstructionMaker::Init()
{
	mFcsDbMaker=static_cast<StFcsDbMaker*>(GetMaker("fcsDb"));
        if(!mFcsDbMaker){
        	LOG_ERROR  << "StFcsEventDisplay::InitRun Failed to get StFcsDbMaker" << endm;
        return kStFatal;
    	}	
		
		h0=new TH1F("h0","# of entries",10,0,10); //check the number of event;

                h2_EcalMult_vs_TOFMult=new TH2F("h2_EcalMult_vs_TOFMult","Ecal Multiplicity vs TOF Multiplicity",200,-.5,199.5,71,-.5,70.5);//Ecal Multiplicity vs TOF Multiplicity
                h2_EcalMult_vs_TOFMult->SetXTitle("tof Multiplicity");
                h2_EcalMult_vs_TOFMult->SetYTitle("Ecal Multiplicity");

                h2_EcalMult_vs_TOFMult_E_1=new TH2F("h2_EcalMult_vs_TOFMult_E_1","Ecal Multiplicity vs TOF Multiplicity",200,-.5,199.5,70,-.5,69.5);//Ecal Multiplicity vs TOF Multiplicity after counting tower energy > 1 GeV 
                h2_EcalMult_vs_TOFMult_E_1->SetXTitle("TOF Multiplicity");
                h2_EcalMult_vs_TOFMult_E_1->SetYTitle("Ecal Multiplicity(E>1)");

		//for cluster finder
		h1_inv_mass_cluster=new TH1F("h1_inv_mass_cluster","invariant mass plot (for cluster finder analysis)",100,m_low,m_up); // invariant mass for cluster finder analysis
		h1_inv_mass_cluster->SetXTitle("invariant mass [GeV]");	

		h1_Zgg_cluster=new TH1F("h1_Zgg_cluster","Zgg (cluster)",70,0,.7); // energy asymmetry for cluster finder analysis, define as : (abs(E1 - E2) / (E1 + E2)
		h1_Zgg_cluster->SetXTitle("Z_{gg}");

		h1_opening_angle_cluster=new TH1F("h1_opening_angle_cluster","opening angle plot (for cluster finder analysis)",bins,0,0.1); // opening angle
		h1_opening_angle_cluster->SetXTitle("opening angle [rad]");

		h1_each_cluster_energy_nocut=new TH1F("h1_each_cluster_energy_nocut","each cluster energy(without cut for cluster finder)",200,0,200); // store each cluster energy (without cut for cluster finder)
		h1_each_cluster_energy_nocut->SetXTitle("E_{1} [GeV]");

                h1_each_cluster_energy_aftercut=new TH1F("h1_each_cluster_energy_aftercut","each cluster energy(after cut for cluster finder)",200,0,200);//each cluster energy after cut
                h1_each_cluster_energy_aftercut->SetXTitle("E_{1} [GeV]");

		h1_two_cluster_energy_nocut=new TH1F("h1_two_cluster_energy_nocut","E_{1} + E_{2} (without cut for cluster finder)",300,0,300); //E1 +E2 without cut
		h1_two_cluster_energy_nocut->SetXTitle("E_{1} + E_{2} [GeV]");

		h1_two_cluster_energy_aftercut=new TH1F("h1_two_cluster_energy_aftercut","E_{1} + E_{2} (all cuts for cluster finder)",300,2,300);//E1 + E2 after all cuts
		h1_two_cluster_energy_aftercut->SetXTitle("E_{1} + E_{2} [GeV]");

		h1_dgg_cluster=new TH1F("h1_dgg_cluster","d_{gg} (cluster)",80,0,60);//distance bewteen two clusters at the Ecal
		h1_dgg_cluster->SetXTitle("d_{gg} [cm]");

		h2_EcalClustMult_vs_TOFMult=new TH2F("h2_EcalClustMult_vs_TOFMult","Ecal cluster Multiplicity vs tof Multiplicity",200,-.5,199.5,40,-.5,39.5); 
                h2_EcalClustMult_vs_TOFMult->SetXTitle("tof Multiplicity");
                h2_EcalClustMult_vs_TOFMult->SetYTitle("Ecal Cluster Multiplicity");

		h2_EcalClustMult_vs_TOFMult_E_1=new TH2F("h2_EcalClustMult_vs_TOFMult_E_1","Ecal cluster Multiplicity vs TOF Multiplicity",50,-.5,49.5,40,-.5,39.5); //Ecal cluster Multiplicity vs TOF Multiplicity after counting cluster energy > 1 GeV
                h2_EcalClustMult_vs_TOFMult_E_1->SetXTitle("TOF Multiplicity");
                h2_EcalClustMult_vs_TOFMult_E_1->SetYTitle("Ecal Cluster Multiplicity(E>1)");		

		h2_dgg_cluster_vs_E1pE2_cluster=new TH2F("h2_dgg_cluster_vs_E1pE2_cluster","d_{gg} vs E_{1} + E_{2} for cluster finder",45,8,25,60,-.5,59.5);
                h2_dgg_cluster_vs_E1pE2_cluster->SetYTitle("d_{gg}");
                h2_dgg_cluster_vs_E1pE2_cluster->SetXTitle("E_{1} + E_{2} [GeV]");

		h2_mass_cluster_vs_Zgg_cluster=new TH2F("h2_mass_cluster_vs_Zgg_cluster","invariant mass vs Z_{gg} for cluster finder",70,0,.7,80,0,.8);
                h2_mass_cluster_vs_Zgg_cluster->SetXTitle("Z_{gg}");
                h2_mass_cluster_vs_Zgg_cluster->SetYTitle("invariant mass [GeV]");

		h2_cluster_position=new TH2F("h2_cluster_position","cluster position y vs x (cluster finder)", 80,55,135,80,-105,-25);
		h2_cluster_position->SetXTitle("x [cm]");
		h2_cluster_position->SetYTitle("y [cm]");

		h2_mass_cluster_vs_dgg_cluster=new TH2F("h2_mass_cluster_vs_dgg_cluster","invariant mass vs d_{gg}  (cluster finder)",45,5,50,50,m_low,m_up);
		h2_mass_cluster_vs_dgg_cluster->SetXTitle("d_{gg} [cm]");
		h2_mass_cluster_vs_dgg_cluster->SetYTitle("invariant mass [GeV]");

		//for point maker
                h1_inv_mass_point=new TH1F("h1_inv_mass_point","invariant mass plot for point maker",100,m_low,m_up);//invariant mass for point maker
                h1_inv_mass_point->SetXTitle("invariant mass [GeV]");

                h1_Zgg_point=new TH1F("h1_Zgg_point","Zgg (point)",70,0,.7);//energy asymmetry for point maker analysis, define as : (abs(E1 - E2) / (E1 + E2)
                h1_Zgg_point->SetXTitle("Z_{gg}");

                h1_opening_angle_point=new TH1F("h1_opening_angle_point","opening angle plot (point)",bins,0,0.1);//opening angle for point maker
                h1_opening_angle_point->SetXTitle("opening angle [rad]");

                h1_each_point_energy_nocut=new TH1F("h1_each_point_energy_nocut","each point energy(no cut) (point)",200,0,200);//each point energy without cut
                h1_each_point_energy_nocut->SetXTitle("E_{1} [GeV]");

                h1_each_point_energy_aftercut=new TH1F("h1_each_point_energy_aftercut","each point energy(after cut) (point)",200,0,200);//each point energy after cuts
                h1_each_point_energy_aftercut->SetXTitle("E_{1} [GeV]");

                h1_two_point_energy_nocut=new TH1F("h1_two_point_energy_nocut","E_{1} + E_{2} without cut for point maker",300,0,300);
                h1_two_point_energy_nocut->SetXTitle("E_{1} + E_{2} [GeV]");

                h1_two_point_energy_aftercut=new TH1F("h1_two_point_energy_aftercut","E_{1} + E_{2} after cuts for point maker",300,2,300);
                h1_two_point_energy_aftercut->SetXTitle("E_{1} + E_{2} [GeV]");

                h1_dgg_point=new TH1F("h1_dgg_point","d_{gg} (point)",80,0,60);//distance bewteen two points at Ecal.
                h1_dgg_point->SetXTitle("d_{gg} [cm]");

		h1_x_rPosition_point=new TH1F("h1_x_rPosition_point","point position(x) in cell",50,0.0,1.0);//point position(x) in cell
		h1_x_rPosition_point->SetXTitle("point position (x in rPosition) [cm]");
		h1_y_rPosition_point=new TH1F("h1_y_rPosition_point","point position(y) in cell",50,0.0,1.0);//point position(y) in cell
                h1_y_rPosition_point->SetXTitle("point position (y in rPosition) [cm]");

                h2_EcalPointMult_vs_TOFMult=new TH2F("h2_EcalPointMult_vs_TOFMult","Ecal point Multiplicity vs tof Multiplicity",200,-.5,199.5,40,-.5,39.5);//Ecal Point Multiplicity us TOF Multiplicity
                h2_EcalPointMult_vs_TOFMult->SetXTitle("tof Multiplicity");
                h2_EcalPointMult_vs_TOFMult->SetYTitle("Ecal Pointer Multiplicity");

                h2_EcalPointMult_vs_TOFMult_E_1=new TH2F("h2_EcalPointMult_vs_TOFMult_E_1","Ecal point Multiplicity vs TOF Multiplicity",200,-.5,199.5,40,-.5,39.5);//Ecal point Multiplicity vs TOF Multiplicity after counting point energy > 1 GeV
                h2_EcalPointMult_vs_TOFMult_E_1->SetXTitle("TOF Multiplicity");
                h2_EcalPointMult_vs_TOFMult_E_1->SetYTitle("Ecal Pointer Multiplicity(E>1)");

                h2_dgg_point_vs_E1pE2_point=new TH2F("h2_dgg_point_vs_E1pE2_point","d_{gg} vs E_{1} + E_{2} for point maker",45,8,20,60,-.5,59.5);
                h2_dgg_point_vs_E1pE2_point->SetYTitle("d_{gg}");
                h2_dgg_point_vs_E1pE2_point->SetXTitle("E_{1} + E_{2} [GeV]");
                h2_mass_point_vs_Zgg_point=new TH2F("h2_mass_point_vs_Zgg_point","invariant mass vs Z_{gg} for point maker",70,0,.7,80,0,.8);
                h2_mass_point_vs_Zgg_point->SetXTitle("Z_{gg}");
                h2_mass_point_vs_Zgg_point->SetYTitle("invariant mass [GeV]");
                h2_point_position=new TH2F("h2_point_position","point position y vs x for point maker", 80,55,135,80,-105,-25);
                h2_point_position->SetXTitle("x [cm]");
                h2_point_position->SetYTitle("y [cm]");

                h2_mass_point_vs_dgg_point=new TH2F("h2_mass_point_vs_dgg_point","invariant mass vs d_{gg} for point maker",45,5,50,50,m_low,m_up);
                h2_mass_point_vs_dgg_point->SetXTitle("d_{gg} [cm]");
                h2_mass_point_vs_dgg_point->SetYTitle("invariant mass [GeV]");

		//for towers
		for (int i=0;i<64;i++)
		{
			char name_hist[50];
			char title_hist[100];
			sprintf(name_hist,"mass_cluster_by_tower_%i",i);
			sprintf(title_hist,"invariant mass sorted by %i tower for cluster finder",i);
			h1list_mass_cluster_by_tower[i]=new TH1F(name_hist,title_hist,40,m_low,m_up);//invariant mass sorted by #i tower for cluster finder
			h1list_mass_cluster_by_tower[i]->SetXTitle("invariant mass [GeV]");

                        sprintf(name_hist,"mass_point_by_tower_%i",i);
                        sprintf(title_hist,"invariant mass sorted by %i tower (point)",i);
                        h1list_mass_point_by_tower[i]=new TH1F(name_hist,title_hist,40,m_low,m_up);//invariant mass sorted by #i tower for point maker
                        h1list_mass_point_by_tower[i]->SetXTitle("invariant mass [GeV]");

                        sprintf(name_hist,"Etower_%i",i);
                        sprintf(title_hist,"%i tower energy spectrum",i);
			h1list_Etower[i]=new TH1F(name_hist, title_hist,200,0,200);//energy spectrum for towers
			h1list_Etower[i]->SetXTitle("Energy [GeV]");
			
		}

		return kStOK;

	}


	//-----------------------
	Int_t StFcsPi0ReconstructionMaker::Finish()
	{
		const char* fn=filename.c_str();
		TFile* MyFile=new TFile(fn,"RECREATE");
		h0->Write();
		h1_inv_mass_cluster->Write();
		h1_Zgg_cluster->Write();
		h1_opening_angle_cluster->Write();
		h1_each_cluster_energy_nocut->Write();
		h1_each_cluster_energy_aftercut->Write();
		h1_two_cluster_energy_nocut->Write();
		h1_two_cluster_energy_aftercut->Write();
		h1_dgg_cluster->Write();
		h2_EcalMult_vs_TOFMult->Write();
		h2_EcalMult_vs_TOFMult_E_1->Write();
		h2_EcalClustMult_vs_TOFMult->Write();
		h2_EcalClustMult_vs_TOFMult_E_1->Write();
		h2_dgg_cluster_vs_E1pE2_cluster->Write();
                h2_mass_cluster_vs_Zgg_cluster->Write();
		h2_cluster_position->Write();
		h2_mass_cluster_vs_dgg_cluster->Write();
                h1_inv_mass_point->Write();
                h1_Zgg_point->Write();
                h1_opening_angle_point->Write();
                h1_each_point_energy_nocut->Write();
                h1_each_point_energy_aftercut->Write();
                h1_two_point_energy_nocut->Write();
                h1_two_point_energy_aftercut->Write();
                h1_dgg_point->Write();
		h1_x_rPosition_point->Write();
		h1_y_rPosition_point->Write();
                h2_EcalPointMult_vs_TOFMult->Write();
                h2_EcalPointMult_vs_TOFMult_E_1->Write();
                h2_dgg_point_vs_E1pE2_point->Write();
                h2_mass_point_vs_Zgg_point->Write();
                h2_point_position->Write();
                h2_mass_point_vs_dgg_point->Write();
		for (int i=0;i<64;i++) 
		{
			h1list_mass_cluster_by_tower[i]->Write();
			h1list_mass_point_by_tower[i]->Write();
			h1list_Etower[i]->Write();
		}
		MyFile->Close();
		return kStOK;
	}

	//----------------------
	Int_t StFcsPi0ReconstructionMaker::Make()
	{

		StEvent* event = (StEvent*)GetInputDS("StEvent");
		if(!event) {LOG_ERROR << "StFcsPi0ReconstructionMaker::Make did not find StEvent"<<endm; return kStErr;}
		mFcsColl = event->fcsCollection();
		if(!mFcsColl) {LOG_ERROR << "StFcsPi0ReconstructionMaker::Make did not find StEvent->StFcsCollection"<<endm; return kStErr;}

		h0->Fill(1); //count total number of event

		if(mNAccepted < mMaxEvents){
		if(mFilter==1 && mFcsColl->numberOfHits(0)+ mFcsColl->numberOfHits(1)+ mFcsColl->numberOfHits(2)+ mFcsColl->numberOfHits(3)==0) return kStOK;
		mNAccepted++;
		int n_EcalMult=0;
		int n_EcalClustMult=0;
		int n_EcalPointMult=0;
                int n_EcalClust_cut=0;
		int n_EcalPoint_cut=0;
                int n_Ecal_cut=0; 
 
		for(int det=0; det<kFcsNDet; det++)
		{
			int check_ecal= mFcsDbMaker->ecalHcalPres(det);
			if (check_ecal!=0) continue;
			
			// get Ecal Mult,  Ecal Cluster Mult
          		StSPtrVecFcsHit& hits = mFcsColl->hits(det);
			int nh=mFcsColl->numberOfHits(det);
			StSPtrVecFcsCluster& clusters = mFcsColl->clusters(det);
			int nc=mFcsColl->numberOfClusters(det);
                        StSPtrVecFcsPoint& points = mFcsColl->points(det);
                        int np=mFcsColl->numberOfPoints(det);

			//count multiplicity
			if ((det==0) || (det==1)) 
			{
				n_EcalMult=n_EcalMult + nh;
				n_EcalClustMult=n_EcalClustMult + nc;
				n_EcalPointMult=n_EcalPointMult + np;
			};

			for (int i=0;i<nh;i++)
                        {
                                StFcsHit* hit=hits[i];
				unsigned short hit_id=hit->id();
                                float hit_energy=hit->energy();
				h1list_Etower[hit_id]->Fill(hit_energy); //fill in energy spectrum for tower
                                if (hit_energy>1) n_Ecal_cut++;//count Ecal Multiplicity when hit energy >1
                        }
			for (int i=0;i<nc;i++)
                        {
				StFcsCluster* clu=clusters[i];
				float clu_energy=clu->energy();
				if (clu_energy>1) n_EcalClust_cut++;//count Ecal cluster Multiplicity when cluster energy >1
                                
			}
			for (int i=0;i<np;i++)
                        {
                                StFcsPoint* pnt=points[i];
                                float pnt_energy=pnt->energy();
                                if (pnt_energy>1) n_EcalPoint_cut++;////count Ecal point Multiplicity when point energy >1
                        }
		}

		//TOF Mult
		StTriggerData* trg=0;
                StFcsRawDaqReader* fcsraw=(StFcsRawDaqReader*)GetMaker("daqReader");
                StEvent* event= (StEvent*)GetInputDS("StEvent");
                if(fcsraw)
                {
                        trg = fcsraw->trgdata();
                        if(!trg){
                                LOG_INFO << "Canot find Trigger Data from StFcsRawDaqReader" << endm;
                                }
                } else if(event){
                        trg=event->StEvent::triggerData();
                                if(!trg){
                                LOG_INFO << "Canot find Trigger Data from StEvent" << endm;
                                        }
                                }


                int tofmult = trg->tofMultiplicity();

                h2_EcalMult_vs_TOFMult->Fill(tofmult,n_EcalMult);       
 	        h2_EcalMult_vs_TOFMult_E_1->Fill(tofmult,n_Ecal_cut);

                if ((tofmult>TofMult_cut)||(tofmult<=2)) return kStOK;//Tof Mult cut

		if (n_Ecal_cut>EcalMult_cut) return kStOk;//Ecal Mult cut

		float h1_inv_mass_cluster_best=-1;
		float h1_Zgg_cluster_best=-1;
		float h1_opening_angle_cluster_best=-1;
		float h1_two_cluster_energy_aftercut_best=-1;
		float h1_dgg_cluster_best=-1;
		float e1_cluster_best=-1;
		float e2_cluster_best=-1;
		float energy_largest_cluster=-1;
		float clux1_best=-1;
		float cluy1_best=-1;
		float clux2_best=-1;
                float cluy2_best=-1;
		unsigned short best_tower_id1_cluster=0;
		unsigned short best_tower_id2_cluster=0;

		float h1_inv_mass_point_best=-1;
                float h1_Zgg_point_best=-1;
                float h1_opening_angle_point_best=-1;
                float h1_two_point_energy_aftercut_best=-1;
                float h1_dgg_point_best=-1;
                float e1_point_best=-1;
                float e2_point_best=-1;
                float energy_largest_point=-1;
                float pntx1_best=-1;
                float pnty1_best=-1;
                float pntx2_best=-1;
                float pnty2_best=-1;
		float pntx1_rPosition_best=-1;
		float pnty1_rPosition_best=-1;
		float pntx2_rPosition_best=-1;
		float pnty2_rPosition_best=-1;
                unsigned short best_tower_id1_point=0;
                unsigned short best_tower_id2_point=0;

		for(int det=0; det<kFcsNDet; det++)
                {
			int check_ecal= mFcsDbMaker->ecalHcalPres(det);
                        if (check_ecal!=0) continue;
                        StSPtrVecFcsCluster& clusters = mFcsColl->clusters(det);
                        int nc=mFcsColl->numberOfClusters(det);
			StSPtrVecFcsPoint& points = mFcsColl->points(det);
                        int np=mFcsColl->numberOfPoints(det);

			//no cut (cluster finder)

			for (int i=0;i<nc;i++)
			{
				StFcsCluster* clu=clusters[i];
				float clu_energy=clu->energy();
				h1_each_cluster_energy_nocut->Fill(clu_energy);
				
				if (i==nc-1) continue;
				for (int j=i+1;j<nc;j++)
				{
					StFcsCluster* cluj=clusters[j];
					float cluj_energy=cluj->energy();
					h1_two_cluster_energy_nocut->Fill(clu_energy+cluj_energy);
				}
			}

			//no cut (point maker)
			for (int i=0;i<np;i++)
                        {
                                StFcsPoint* pnt=points[i];
                                float pnt_energy=pnt->energy();
                                h1_each_point_energy_nocut->Fill(pnt_energy);

                                if (i==np-1) continue;
                                for (int j=i+1;j<np;j++)
                                {
                                        StFcsPoint* pntj=points[j];
                                        float pntj_energy=pntj->energy();
                                        h1_two_point_energy_nocut->Fill(pnt_energy+pntj_energy);
                                }
                        }

			//start cut (cluster finder)
			for (int i=0; i<(nc-1); i++)
			{

				StFcsCluster* clu=clusters[i];
				float clu_x=clu->x();
				float clu_y=clu->y();
				float clu_energy=clu->energy();
				if (clu_energy<1) continue; //cluster energy cut	
				StThreeVectorD xyz = mFcsDbMaker->getStarXYZfromColumnRow(det,clu_x,clu_y);
				StLorentzVectorD p = mFcsDbMaker->getLorentzVector((xyz),clu_energy,0);

	
				for (int j=i+1;j<nc;j++)
				{
		
					StFcsCluster* cluj=clusters[j];
					float cluj_energy=cluj->energy();

					if ((cluj->energy())<1) continue;
					float zgg=(fabs(clu_energy-cluj_energy))/(clu_energy+cluj_energy);
					if (zgg>=0.7) continue;//Zgg cut
					if (((clu_energy+cluj_energy)<8)||((clu_energy+cluj_energy)>20)) continue;//E1 + E2 cut

					StThreeVectorD xyzj = mFcsDbMaker->getStarXYZfromColumnRow(det,cluj->x(),cluj->y());
                                        StLorentzVectorD pj = mFcsDbMaker->getLorentzVector((xyzj),cluj->energy(),0);
					
					//choose the best pair of cluster	
					if ((clu_energy+cluj_energy)>energy_largest_cluster)
					{	
						energy_largest_cluster=clu_energy+cluj_energy;
						h1_Zgg_cluster_best=zgg;
						h1_inv_mass_cluster_best=(p+pj).m();
						h1_two_cluster_energy_aftercut_best=clu_energy+cluj_energy;
						h1_opening_angle_cluster_best=(acos((xyz[0]*xyzj[0]+xyz[1]*xyzj[1]+xyz[2]*xyzj[2])/(sqrt(xyz[0]*xyz[0]+xyz[1]*xyz[1]+xyz[2]*xyz[2])*sqrt(xyzj[0]*xyzj[0]+xyzj[1]*xyzj[1]+xyzj[2]*xyzj[2]))));
						h1_dgg_cluster_best=(sqrt((xyz[0]-xyzj[0])*(xyz[0]-xyzj[0]) + (xyz[1]-xyzj[1])*(xyz[1]-xyzj[1])));
						e1_cluster_best=clu_energy;
						e2_cluster_best=cluj_energy;
						clux1_best=clu->x();
						cluy1_best=clu->y();
						clux2_best=cluj->x();
						cluy2_best=cluj->y();
		
						StPtrVecFcsHit& clui_hits= clu->hits();
						StPtrVecFcsHit& cluj_hits= cluj->hits();				
						int clui_nTowers= clu->nTowers();
						int cluj_nTowers= cluj->nTowers();
						float max_energy_tower = -1;
						unsigned short tower_id = 0;
						for (int k=0;k<clui_nTowers;k++)
						{
							if (clui_hits[k]->energy()>max_energy_tower) 
							{ max_energy_tower=clui_hits[k]->energy();	
							tower_id = clui_hits[k]->id(); }
						}
						best_tower_id1_cluster=tower_id;
						for (int k=0;k<cluj_nTowers;k++)
                                                {
                                                        if ((cluj_hits[k]->energy())>max_energy_tower) 
							{ max_energy_tower=cluj_hits[k]->energy(); 
							tower_id = cluj_hits[k]->id(); }
                                                }
						best_tower_id2_cluster=tower_id;

					}
				}
			}
		

		//start cut(point)
		for (int i=0; i<(np-1); i++)
                        {

                                StFcsPoint* pnt=points[i];
                                float pnt_x=pnt->x();
                                float pnt_y=pnt->y();
                                float pnt_energy=pnt->energy();
                                if (pnt_energy<1) continue; //point energy cut        
                                StThreeVectorD xyz = mFcsDbMaker->getStarXYZfromColumnRow(det,pnt_x,pnt_y);
                                StLorentzVectorD p = mFcsDbMaker->getLorentzVector((xyz),pnt_energy,0);
				
                                for (int j=i+1;j<np;j++)
                                {

                                        StFcsPoint* pntj=points[j];
                                        float pntj_energy=pntj->energy();

                                        if ((pntj->energy())<1) continue;
                                        float zgg=(fabs(pnt_energy-pntj_energy))/(pnt_energy+pntj_energy);
                                        if (zgg>=0.7) continue;
                                        if (((pnt_energy+pntj_energy)<8)||((pnt_energy+pntj_energy)>20)) continue;

                                        StThreeVectorD xyzj = mFcsDbMaker->getStarXYZfromColumnRow(det,pntj->x(),pntj->y());
                                        StLorentzVectorD pj = mFcsDbMaker->getLorentzVector((xyzj),pntj->energy(),0);

					//choose the best pair of point
                                        if ((pnt_energy+pntj_energy)>energy_largest_point)
                                        {
                                                energy_largest_point=pnt_energy+pntj_energy;
                                                h1_Zgg_point_best=zgg;
                                                h1_inv_mass_point_best=(p+pj).m();
                                                h1_two_point_energy_aftercut_best=pnt_energy+pntj_energy;
                                                h1_opening_angle_point_best=(acos((xyz[0]*xyzj[0]+xyz[1]*xyzj[1]+xyz[2]*xyzj[2])/(sqrt(xyz[0]*xyz[0]+xyz[1]*xyz[1]+xyz[2]*xyz[2])*sqrt(xyzj[0]*xyzj[0]+xyzj[1]*xyzj[1]+xyzj[2]*xyzj[2]))));
                                                h1_dgg_point_best=(sqrt((xyz[0]-xyzj[0])*(xyz[0]-xyzj[0]) + (xyz[1]-xyzj[1])*(xyz[1]-xyzj[1])));
                                                e1_point_best=pnt_energy;
                                                e2_point_best=pntj_energy;
                                                pntx1_best=pnt->x();
                                                pnty1_best=pnt->y();
                                                pntx2_best=pntj->x();
                                                pnty2_best=pntj->y();
						pntx1_rPosition_best=pntx1_best-int(pntx1_best);
						pnty1_rPosition_best=pnty1_best-int(pnty1_best);
						pntx2_rPosition_best=pntx2_best-int(pntx2_best);
						pnty2_rPosition_best=pnty2_best-int(pnty2_best);

 						StFcsCluster* pnt_clu=pnt->cluster();
						StFcsCluster* pntj_clu=pntj->cluster();

                                                StPtrVecFcsHit& pnti_hits= pnt_clu->hits();                                                                        StPtrVecFcsHit& pntj_hits= pntj_clu->hits();
						int pnti_nTowers= pnt_clu->nTowers();
                                                int pntj_nTowers= pntj_clu->nTowers();
                                                float max_energy_tower = -1;
                                                unsigned short tower_id = 0;
                                                for (int k=0;k<pnti_nTowers;k++)
                                                {
                                                        if (pnti_hits[k]->energy()>max_energy_tower)
                                                        { max_energy_tower=pnti_hits[k]->energy();
                                                        tower_id = pnti_hits[k]->id(); }
                                                }
                                                best_tower_id1_point=tower_id;
                                                for (int k=0;k<pntj_nTowers;k++)
                                                {
                                                        if ((pntj_hits[k]->energy())>max_energy_tower)
                                                        { max_energy_tower=pntj_hits[k]->energy();
                                                        tower_id = pntj_hits[k]->id(); }
                                                }
                                                best_tower_id2_point=tower_id;

                                        }
				}

                        
                	}

	 	}	

		//output
		if (h1_inv_mass_cluster_best>-1)
		{

			h1_inv_mass_cluster->Fill(h1_inv_mass_cluster_best);
			h1_Zgg_cluster->Fill(h1_Zgg_cluster_best);
			h1_opening_angle_cluster->Fill(h1_opening_angle_cluster_best);
			h1_each_cluster_energy_aftercut->Fill(e1_cluster_best);
			h1_each_cluster_energy_aftercut->Fill(e2_cluster_best);
			h1_two_cluster_energy_aftercut->Fill(h1_two_cluster_energy_aftercut_best);
			h1_dgg_cluster->Fill(h1_dgg_cluster_best);
			h2_EcalClustMult_vs_TOFMult->Fill(tofmult,n_EcalClustMult);
			h2_EcalClustMult_vs_TOFMult_E_1->Fill(tofmult,n_EcalClust_cut);
			h2_dgg_cluster_vs_E1pE2_cluster->Fill(h1_two_cluster_energy_aftercut_best,h1_dgg_cluster_best);
			h2_mass_cluster_vs_Zgg_cluster->Fill(h1_Zgg_cluster_best,h1_inv_mass_cluster_best);
			h2_cluster_position->Fill(clux1_best,cluy1_best);
			h2_cluster_position->Fill(clux2_best,cluy2_best);
			h2_mass_cluster_vs_dgg_cluster->Fill(h1_dgg_cluster_best,h1_inv_mass_cluster_best);
			h1list_mass_cluster_by_tower[best_tower_id1_cluster]->Fill(h1_inv_mass_cluster_best);
			h1list_mass_cluster_by_tower[best_tower_id2_cluster]->Fill(h1_inv_mass_cluster_best);

		}

		//single event output for point
                if (h1_inv_mass_point_best>-1)
                {       
                        
                        h1_inv_mass_point->Fill(h1_inv_mass_point_best);
                        h1_Zgg_point->Fill(h1_Zgg_point_best);
                        h1_opening_angle_point->Fill(h1_opening_angle_point_best);
                        h1_each_point_energy_aftercut->Fill(e1_point_best);
                        h1_each_point_energy_aftercut->Fill(e2_point_best);
                        h1_two_point_energy_aftercut->Fill(h1_two_point_energy_aftercut_best);
                        h1_dgg_point->Fill(h1_dgg_point_best);
			h1_x_rPosition_point->Fill(pntx1_rPosition_best);
			h1_y_rPosition_point->Fill(pnty1_rPosition_best);
                        h1_x_rPosition_point->Fill(pntx2_rPosition_best);
                        h1_y_rPosition_point->Fill(pnty2_rPosition_best);

                        h2_EcalPointMult_vs_TOFMult->Fill(tofmult,n_EcalPointMult);
                        h2_EcalPointMult_vs_TOFMult_E_1->Fill(tofmult,n_EcalPoint_cut);
                        h2_dgg_point_vs_E1pE2_point->Fill(h1_two_point_energy_aftercut_best,h1_dgg_point_best);
                        h2_mass_point_vs_Zgg_point->Fill(h1_Zgg_point_best,h1_inv_mass_point_best);
                        h2_point_position->Fill(pntx1_best,pnty1_best);
                        h2_point_position->Fill(pntx2_best,pnty2_best);
                        h2_mass_point_vs_dgg_point->Fill(h1_dgg_point_best,h1_inv_mass_point_best);

                        h1list_mass_point_by_tower[best_tower_id1_point]->Fill(h1_inv_mass_point_best);
                        h1list_mass_point_by_tower[best_tower_id2_point]->Fill(h1_inv_mass_point_best);

                }
	}	
 	
	return kStOK;
}
