#include "StFwdAnaMakeEcalPairs.h"

ClassImp(StFwdAnaMakeEcalPairs)

StFwdAnaMakeEcalPairs::StFwdAnaMakeEcalPairs()
{
  //memset(mH1F_InvMassEpdCuts,0,sizeof(mH1F_InvMassEpdCuts));
}

StFwdAnaMakeEcalPairs::~StFwdAnaMakeEcalPairs()
{
  //delete mH1F_InvMassEpdCuts[0];
  //delete mH1F_InvMassEpdCuts[1];  
}

UInt_t StFwdAnaMakeEcalPairs::LoadHists(TFile* file, HistManager* histman, StFwdAnaData* anadata)
{
  UInt_t loaded = 0;
  if( histman==0 ){ return loaded; }

  //if( mH1F_InvMassEpdCuts[0]==0 ){ mH1F_InvMassEpdCuts[0] = new TObjArray(); }
  //loaded += histman->AddH1FArr(file,mH1F_InvMassEpdCuts[0],NEPDCUTS,"H1F_InvMassEpdCuts_AllTrig","Different EPD NMIP cuts all triggers",500,0,1);
  //if( mH1F_InvMassEpdCuts[1]==0 ){ mH1F_InvMassEpdCuts[1] = new TObjArray(); }
  //loaded += histman->AddH1FArr(file,mH1F_InvMassEpdCuts[1],NEPDCUTS,"H1F_InvMassEpdCuts_EmTrig","Different EPD NMIP cuts EM triggers",500,0,1);
  
  return loaded;
}

Int_t StFwdAnaMakeEcalPairs::DoMake(StFwdAnaData* anadata)
{
  Int_t clustersize = anadata->getEvtInfo()->mClusterSize;
  Int_t npi0candidate = 0;
  TClonesArray* mpharr = anadata->getPhArr();
  TClonesArray* pointpairs = anadata->getPhPairArr();
  //Double_t usevertex = anadata->mUseVertex;
  //Double_t vertexcutlow = anadata->mVertexCutLow;
  //Double_t vertexcuthigh = anadata->mVertexCutHigh;
  //Filling cluster pi0s and cluster photon/elecron epd nmip cut. For clusters only store best pair to speed up code
  if( mMakeClusPairs ){
    for( Int_t ic = 0; ic<clustersize; ++ic ){
      FcsPhotonCandidate* iclus = (FcsPhotonCandidate*) mpharr->UncheckedAt(ic);
      if( !(iclus->mFromCluster) ){ std::cout << "MAJOR ERROR - cluster size of array found a point crashing" << std::endl; exit(0); }
      //std::cout << "|ic:"<<ic << std::endl;
      //std::cout << "  + ";
      //iclus->Print();
      if( !(0<=iclus->mDetId && iclus->mDetId<=kFcsEcalSouthDetId) ){ continue; }
      if( ic==(clustersize-1) ){ continue; }

      ///if( iclus->mEpdHitNmip>-0.1){ //Only include candidates who have their nmip value set
      //if( iclus->mEpdHitNmip<mEpdNmipCut ){ goodclusphotonsidx.emplace_back(ic); }
      //else{ goodcluselectronsidx.emplace_back(ic); }
      //}
      for( Int_t jc=ic+1; jc<clustersize; jc++ ){
	FcsPhotonCandidate* jclus = (FcsPhotonCandidate*) mpharr->UncheckedAt(jc);
	if( !(jclus->mFromCluster) ){ std::cout << "MAJOR ERROR - cluster size of array found a point crashing" << std::endl; exit(0); }
	if( !(0<=jclus->mDetId && jclus->mDetId<=kFcsEcalSouthDetId) ){ continue; }
	TLorentzVector pi0Vert_LV = iclus->lvVert() + jclus->lvVert();
	//if( ic==0 && jc==ic+1 ){ //Since we have a sorted photon array highest two energies are the first two entries
	FcsPairCandidate* pi0c = (FcsPairCandidate*) pointpairs->ConstructedAt(npi0candidate++);
	pi0c->mFromCluster = true;
	pi0c->mFromPh = -1;
	pi0c->mPhoton1Idx = ic;
	pi0c->mPhoton2Idx = jc;
	
	pi0c->mPx = pi0Vert_LV.Px();
	pi0c->mPy = pi0Vert_LV.Py();
	pi0c->mPz = pi0Vert_LV.Pz();
	pi0c->mEn = pi0Vert_LV.E();
	
	pi0c->mEta     = pi0Vert_LV.PseudoRapidity();
	pi0c->mDgg     = FcsPairCandidate::dgg(*iclus,*jclus);
	pi0c->mZgg     = FcsPairCandidate::zgg(*iclus,*jclus);
	pi0c->mAlpha   = FcsPairCandidate::alpha(*iclus,*jclus);
	pi0c->mInvMass = pi0Vert_LV.Mag();
	//std::cout << "|idx1:"<<pi0c->mPhoton1Idx << "|idx2:"<<pi0c->mPhoton2Idx << "|clustermass:"<<pi0c->mInvMass <<  std::endl;
	//}
	/*
	  else{
	  //std::cout << "|idx1:"<<pi0c->mPhoton1Idx << "|idx2:"<<pi0c->mPhoton2Idx << "|pointmass:"<<pi0c->mInvMass <<  std::endl;
	  mH1F_AllPointPairMass->Fill(pi0Vert_LV.Mag());
	  }*/
	//std::cout << "Clus|idx1:"<<pi0c->mPhoton1Idx << "|idx2:"<<pi0c->mPhoton2Idx << "|pointmass:"<<pi0c->mInvMass <<  std::endl;
      }
    }
  }

  //Filling point pi0s and cluster photon/elecron epd nmip cut
  //std::cout << "===== EventId:"<< mEvtInfo->mEvent <<" =====" << std::endl;
  if( mMakePointPairs ){
    //FcsPhotonCandidate* firstphotoncut[NEPDCUTS] = {0};
    //FcsPhotonCandidate* secondphotoncut[NEPDCUTS] = {0};
    //Int_t n_noepdproj = 0;
    //Int_t n_noepdproj_vcut = 0;
    for( Int_t ip = clustersize; ip<mpharr->GetEntriesFast(); ++ip ){
      FcsPhotonCandidate* ipoi = (FcsPhotonCandidate*) mpharr->UncheckedAt(ip);
      if( ipoi->mFromCluster ){ std::cout << "MAJOR ERROR - point size of array found a cluster crashing" << std::endl; exit(0); }
      //std::cout << "|ip:"<<ip << std::endl;
      //std::cout << "  + ";
      //ipoi->Print();
      /*
      if( ipoi->mEpdHitNmip[0]>-0.1){ //Only include candidates who have their nmip value set
	for( short i=0; i<NEPDCUTS; ++i ){
	  if( ipoi->mEpdHitNmip[0] < 0.2+0.1*static_cast<double>(i) ){
	    if( firstphotoncut[i]==0 ){ firstphotoncut[i]=ipoi; }
	    else{ if( secondphotoncut[i]==0 ){ secondphotoncut[i]=ipoi; } }
	  }
	}
      }
      
      else{
	++n_noepdproj;
	if( vertexcutlow<=usevertex && usevertex<=vertexcuthigh ){ ++n_noepdproj_vcut; }
      }
      */
      if( ip==(mpharr->GetEntriesFast()-1) ){ continue; }
      if( !(0<=ipoi->mDetId && ipoi->mDetId<=kFcsEcalSouthDetId) ){ continue; }
      for( Int_t jp=ip+1; jp<mpharr->GetEntriesFast(); ++jp ){
	FcsPhotonCandidate* jpoi = (FcsPhotonCandidate*) mpharr->UncheckedAt(jp);
	if( jpoi->mFromCluster ){ std::cout << "MAJOR ERROR - point size of array found a cluster crashing" << std::endl; exit(0); }
	if( !(0<=jpoi->mDetId && jpoi->mDetId<=kFcsEcalSouthDetId) ){ continue; }
	TLorentzVector pi0Vert_LV = ipoi->lvVert() + jpoi->lvVert();
	//if( ip==clustersize && jp==ip+1 ){ //Since we have a sorted photon array highest two energies are the first two entries
	FcsPairCandidate* pi0c = (FcsPairCandidate*) pointpairs->ConstructedAt(npi0candidate++);
	pi0c->mFromCluster = false;
	pi0c->mFromPh = -1;
	pi0c->mPhoton1Idx = ip;
	pi0c->mPhoton2Idx = jp;
      
	pi0c->mPx = pi0Vert_LV.Px();
	pi0c->mPy = pi0Vert_LV.Py();
	pi0c->mPz = pi0Vert_LV.Pz();
	pi0c->mEn = pi0Vert_LV.E();
      
	pi0c->mEta     = pi0Vert_LV.PseudoRapidity();
	pi0c->mDgg     = FcsPairCandidate::dgg(*ipoi,*jpoi);
	pi0c->mZgg     = FcsPairCandidate::zgg(*ipoi,*jpoi);
	pi0c->mAlpha   = FcsPairCandidate::alpha(*ipoi,*jpoi);
	pi0c->mInvMass = pi0Vert_LV.Mag();
	//std::cout << "Point|idx1:"<<pi0c->mPhoton1Idx << "|idx2:"<<pi0c->mPhoton2Idx << "|pointmass:"<<pi0c->mInvMass <<  std::endl;
      }
    }
  }

  //Handle the epd cuts
  /*
  for( short i=0; i<NEPDCUTS; ++i ){
    if( firstphotoncut[i]!=0 && secondphotoncut[i]!=0 ){
      TLorentzVector pi0Vert_LV = firstphotoncut[i]->lvVert() + secondphotoncut[i]->lvVert();
      if( anadata->mEmTrigFound ){ ((TH1*)mH1F_InvMassEpdCuts[1]->UncheckedAt(i))->Fill(pi0Vert_LV.Mag()); }
      ((TH1*)mH1F_InvMassEpdCuts[0]->UncheckedAt(i))->Fill(pi0Vert_LV.Mag());
    }
  }
  */
  return kStOk;
}

/*
void StFwdAnaMakeEcalPairs::PaintEpdNmipCuts(TCanvas* canv, const char* savename ) const
{
  canv->Clear();
  canv->Divide(2,1);
  for( UInt_t i=0; i<2; ++i ){
    canv->cd(i+1);
    for( Int_t icut=0, ipad=1; icut<NEPDCUTS; ++icut,++ipad ){
      ((TH1*)mH1F_InvMassEpdCuts[i]->UncheckedAt(icut))->SetLineColor(icut+1);//Hack to get rainbow colors since I know I only have 8 histograms
      if( icut==0 ){ ((TH1*)mH1F_InvMassEpdCuts[i]->UncheckedAt(icut))->Draw("hist e"); }
      else{  ((TH1*)mH1F_InvMassEpdCuts[i]->UncheckedAt(icut))->Draw("hist e same"); }
    }
  }
  canv->Print(savename);
}
*/
			  
