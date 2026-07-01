#include "StFwdAnaFillEcalClusPoint.h"

ClassImp(StFwdAnaFillEcalClusPoint)

StFwdAnaFillEcalClusPoint::StFwdAnaFillEcalClusPoint()
{
}

StFwdAnaFillEcalClusPoint::~StFwdAnaFillEcalClusPoint()
{
}

UInt_t StFwdAnaFillEcalClusPoint::LoadHists(TFile* file, HistManager* histman, StFwdAnaData* anadata)
{
  UInt_t loaded = 0;
  if( histman==0 ){ return loaded; }
  loaded += histman->AddH2F(file,mH2F_PhotonHeatMap,"H2F_PhotonHeatMap","Distribution of photons in STAR x,y space;x (cm);y (cm)", 400,-200,200, 300,-150,150);
  loaded += histman->AddH2F(file,mH2F_PhotonHeatMapG,"H2F_PhotonHeatMapG","Distribution of photons in STAR x,y space (No Spike);x (cm);y (cm)", 400,-200,200, 300,-150,150);
  loaded += histman->AddH2F(file,mH2F_PhotonHeatMapB,"H2F_PhotonHeatMapB","Distribution of photons in STAR x,y space (Spike);x (cm);y (cm)", 400,-200,200, 300,-150,150);

  loaded += histman->AddH1F(file,mH1F_ClusterEnergy,"H1F_ClusterEnergy","Energy of FCS Clusters;Energy (GeV);", 1000,0,200);
  loaded += histman->AddH1F(file,mH1F_PointEnergy,"H1F_PointEnergy","Energy of FCS Points;Energy (GeV);", 1000,0,200);
  loaded += histman->AddH1F(file,mH1F_ClusterMult,"H1F_ClusterMult","Cluster Multiplicity with only an energy cut;Cluster Multiplicity", 30,0,30);
  loaded += histman->AddH1F(file,mH1F_PointMult,"H1F_PointMult","Point Multiplicity with only an energy cut;Point Multiplicity", 30,0,30);
  return loaded;
}

Int_t StFwdAnaFillEcalClusPoint::DoMake(StFwdAnaData* anadata)
{
  StMuFcsCollection* MuFcsColl = anadata->fcsColl();
  StFcsDb* FcsDb = anadata->fcsDb();
  TClonesArray* PhArr = anadata->getPhArr();
  //Fcs Collection
  if( !MuFcsColl ){ LOG_ERROR << "StFwdAnaFillEcalClusPoint::Make did not find MuFcsCollection" << endm; return kStErr; }

  //TClonesArray* hits = mMuFcsColl->getHitArray();
  //if( hits==0 ){ LOG_INFO << "StFwdAnaFillEcalClusPoint::FillFcsInfo - No FCS hits" << endm; }
  TClonesArray* clusters = anadata->fcsColl()->getClusterArray();
  //if( clusters==0 ){ LOG_INFO << "StFwdAnaFillEcalClusPoint::FillFcsInfo - No FCS clusters" << endm; }
  if( clusters==0 ){ std::cout << "StFwdAnaFillEcalClusPoint::FillFcsInfo - No FCS clusters" << std::endl; }
  TClonesArray* points = anadata->fcsColl()->getPointArray();
  //if( points==0 ){ LOG_INFO << "StFwdAnaFillEcalClusPoint::FillFcsInfo - No FCS points" << endm; }
  if( points==0 ){ std::cout << "StFwdAnaFillEcalClusPoint::FillFcsInfo - No FCS points" << std::endl; }
  
  //std::cout << "|hits:"<<hits << "|clusters:"<<clusters << "|points:"<<points << std::endl;
  Int_t ncandidates = 0;
  if( clusters!=0 ){
    for( UInt_t idet=0; idet<=kFcsEcalSouthDetId; ++idet ){
      //This is to do a fiducial volume cut on the clusters and points, since points and clusters store local coordinates use row, column space designation
      float mincolumncut = 1.0; //Column 1 will give x value between 0 and 1 so want greater than 1
      float minrowcut = 1.0;    //Row 1 will give xy value between 0 and 1 so want greater than 1
      float maxcolumncut = FcsDb->nColumn(idet) - 1; //subtract 1 from max column to get rid of the edge towers
      float maxrowcut = FcsDb->nRow(idet) - 1;       //subtract 1 from max rows to get rid of the edge towers
      //std::cout << "+ |idet:"<<idet << "|maxdet:"<<kFcsNDet;
      unsigned int nc = MuFcsColl->numberOfClusters(idet);
      unsigned int iclus = MuFcsColl->indexOfFirstCluster(idet);
      nc += iclus;
      for( ; iclus<nc; ++iclus){
	StMuFcsCluster* clu = (StMuFcsCluster*)clusters->At(iclus);
	float iclu_x = clu->x();
	float iclu_y = clu->y();
	float iclu_energy = clu->energy();
	if( iclu_energy<anadata->mEnCut ){ continue; }
	if( !(mincolumncut<=iclu_x && iclu_x<=maxcolumncut) ){ continue; }
	if( !(minrowcut<=iclu_y && iclu_y<=maxrowcut) ){ continue; }

	StThreeVectorD iclu_pos = FcsDb->getStarXYZfromColumnRow( idet, iclu_x, iclu_y );
	StLorentzVectorD iclu_p = FcsDb->getLorentzVector( iclu_pos, iclu_energy, 0 );

	StFcsPhotonCandidate* ph = (StFcsPhotonCandidate*) PhArr->ConstructedAt(ncandidates++);
	ph->mFromCluster = true;
	ph->mDetId = idet;
	ph->mX = iclu_pos[0];
	ph->mY = iclu_pos[1];
	ph->mZ = iclu_pos[2];

	ph->mEn = iclu_energy;
	ph->mPxRaw = iclu_p.px();
	ph->mPyRaw = iclu_p.py();
	ph->mPzRaw = iclu_p.pz();
	mH1F_ClusterEnergy->Fill(iclu_energy);

	//std::cout << "Cluster|detid:"<<ph->mDetId << "|mX:"<<ph->mX << "|mY:"<<ph->mY << "|mZ:"<<ph->mZ << std::endl;

	if( anadata->mFoundVertex > 0 ){
	  StLorentzVectorD iclu_p_withz = FcsDb->getLorentzVector( iclu_pos, iclu_energy, anadata->mUseVertex );
	  ph->mPxVert = iclu_p_withz.px();
	  ph->mPyVert = iclu_p_withz.py();
	  ph->mPzVert = iclu_p_withz.pz();
	}
	else{
	  ph->mPxVert = 0;
	  ph->mPyVert = 0;
	  ph->mPzVert = 0;
	}
      }
    }
  }

  //std::cout << "===== EventId:"<< mEvtData->mEvent <<" =====" << std::endl;
  anadata->getEvtData()->mClusterSize = ncandidates;
  Int_t clustersize = ncandidates; //local copy of mEvtData->mClusterSize
  mH1F_ClusterMult->Fill(ncandidates);
  
  if( points!=0 ){
    for( UInt_t idet=0; idet<=kFcsEcalSouthDetId; ++idet ){
      //This is to do a fidicul volume cut on the clusters and points, since points and clusters store local coordinates use row, column space designation
      float mincolumncut = 1.0; //Column 1 will give x value between 0 and 1 so want greater than 1
      float minrowcut = 1.0;    //Row 1 will give xy value between 0 and 1 so want greater than 1
      float maxcolumncut = FcsDb->nColumn(idet) - 1; //subtract 1 from max column to get rid of the edge towers
      float maxrowcut = FcsDb->nRow(idet) - 1;       //subtract 1 from max rows to get rid of the edge towers
      unsigned int np = MuFcsColl->numberOfPoints(idet);
      unsigned int ipoint = MuFcsColl->indexOfFirstPoint(idet);
      np += ipoint;
      for( ; ipoint<np; ++ipoint ){
	StMuFcsPoint* point = (StMuFcsPoint*)points->At(ipoint);
	float ipoi_x = point->x();
	float ipoi_y = point->y();
	float ipoi_energy = point->energy();
	if( ipoi_energy<anadata->mEnCut ){ continue; }
	if( !(mincolumncut<=ipoi_x && ipoi_x<=maxcolumncut) ){ continue; }
	if( !(minrowcut<=ipoi_y && ipoi_y<=maxrowcut) ){ continue; }

	StThreeVectorD ipoi_pos = FcsDb->getStarXYZfromColumnRow( idet, ipoi_x, ipoi_y );
	StLorentzVectorD ipoi_p = FcsDb->getLorentzVector(ipoi_pos, ipoi_energy, 0);

	StFcsPhotonCandidate* ph = (StFcsPhotonCandidate*) PhArr->ConstructedAt(ncandidates++);
	ph->mFromCluster = false;
	ph->mDetId = idet;
	ph->mX = ipoi_pos[0];
	ph->mY = ipoi_pos[1];
	ph->mZ = ipoi_pos[2];
	mH2F_PhotonHeatMap->Fill(ipoi_pos[0],ipoi_pos[1]);
	if( 78.2<=ipoi_energy && ipoi_energy<79.4 ){ mH2F_PhotonHeatMapB->Fill(ipoi_pos[0],ipoi_pos[1]); } //Region with spike
	if( 76.8<=ipoi_energy && ipoi_energy<78.0 ){ mH2F_PhotonHeatMapG->Fill(ipoi_pos[0],ipoi_pos[1]); } //Region near spike

	ph->mEn = ipoi_energy;
	ph->mPxRaw = ipoi_p.px();
	ph->mPyRaw = ipoi_p.py();
	ph->mPzRaw = ipoi_p.pz();
	mH1F_PointEnergy->Fill(ipoi_energy);

	if( anadata->mFoundVertex > 0 ){ 	StLorentzVectorD ipoi_p_withz = FcsDb->getLorentzVector( ipoi_pos, ipoi_energy, anadata->mUseVertex );
	  ph->mPxVert = ipoi_p_withz.px();
	  ph->mPyVert = ipoi_p_withz.py();
	  ph->mPzVert = ipoi_p_withz.pz();
	}
	else{
	  ph->mPxVert = 0;
	  ph->mPyVert = 0;
	  ph->mPzVert = 0;
	}
	//std::cout << "Point|detid:"<<ph->mDetId << "|mX:"<<ph->mX << "|mY:"<<ph->mY << "|mZ:"<<ph->mZ << std::endl;	
      }//i point
    }//fcs dets
  }

  //std::cout << "|ncandidates:"<<ncandidates <<"|clustersize:"<<clustersize <<"|Size:"<<mPhArr->GetEntriesFast() << std::endl;
  Int_t npoints = ncandidates - clustersize; //Don't need to add 1 since including clustersize but not ncandidates
  mH1F_PointMult->Fill(npoints);
  PhArr->Sort(); //Since this is properly sorted with clusters showing up first clustersize is unchanged. Also sorts by energy
  return kStOk;
}

void StFwdAnaFillEcalClusPoint::PaintHeatMap(TCanvas* canv, const char* savename) const
{
  canv->Clear();
  canv->SetLogz();
  mH2F_PhotonHeatMap->Draw("colz");
  canv->Print(savename);
}

void StFwdAnaFillEcalClusPoint::PaintClusPointQa(TCanvas* canv, const char* savename)   const
{
  canv->Clear();
  
  canv->Divide(2,2);
  //canv->cd(1)->SetLogz();
  //mH2F_PhotonHeatMap->Draw("colz");
  canv->cd(1)->SetLogy();
  mH1F_ClusterMult->Draw("hist e");
  canv->cd(2)->SetLogy();
  mH1F_ClusterEnergy->Draw("hist e");
  canv->cd(3)->SetLogy();
  mH1F_PointMult->Draw("hist e");
  canv->cd(4)->SetLogy();
  mH1F_PointEnergy->Draw("hist e");

  canv->Print(savename);
}

void StFwdAnaFillEcalClusPoint::PaintEnergyZoom(TCanvas* canv, const char* savename) const
{
  canv->Clear();

  canv->Divide(2,2);

  canv->cd(1);
  TH1* h1_encopy = (TH1*)mH1F_ClusterEnergy->DrawClone("hist e");
  h1_encopy->GetXaxis()->SetRangeUser(75,85);
  h1_encopy->SetLineColor(kBlack);

  canv->cd(2)->SetLogz();
  mH2F_PhotonHeatMapG->SetStats(0);
  mH2F_PhotonHeatMapG->Draw("colz");

  canv->cd(3)->SetLogz();
  mH2F_PhotonHeatMapB->SetStats(0);
  mH2F_PhotonHeatMapB->Draw("colz");
  
  canv->Print(savename);
}

			  
