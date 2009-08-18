void vertEff(char *file){

  gSystem->Load("$HOME/MyEvent/MyEvent");

  TFile *f=new TFile(file,"OPEN");
  TTree *myEventTree=(TTree*)f->Get("mEventTree");
  MyEvent *ev=new MyEvent();
  myEventTree->SetBranchAddress("branch",&ev);


  TH1F *h_vert_all=new TH1F("h_vert_all","vertex hijing",160,-80.,80.);
  TH1F *h_vert_no=new TH1F("h_vert_no","vertex hijing, no prim.",160,-80.,80.);
  TH1F *h_pion_all=new TH1F("h_pion_all","hijing pions",12,0.,6.);
  TH1F *h_pion_no=new TH1F("h_pion_no","hijing pions, no prim",12,0.,6.);
  TH1F *h_pion_reco=new TH1F("h_pion_reco","hijing pions, prim",12,0.,6.);
  TH1F *h_vert_res=new TH1F("h_vert_res","vertex resolution",200,-50.,50.);

  TH1F *h_gg_all=new TH1F("h_gg_all","good globals",100,0.,100.);
  TH1F *h_gg_reco=new TH1F("h_gg_reco","good globals (vtx reco.)",100,0.,100.);

  h_vert_all->Sumw2();
  h_vert_no->Sumw2();
  h_pion_all->Sumw2();
  h_pion_no->Sumw2();
  h_pion_reco->Sumw2();
  h_vert_res->Sumw2();

  Int_t i=0;

  int n_ev=0;
  int n_ev_no=0;

  while(myEventTree->GetEntry(i)){

    n_ev++;


    TVector3 vert=ev->vertex();

    h_gg_all->Fill(ev->goodGlobals());
    if(fabs(vert.Z())>0.0000001){
      h_gg_reco->Fill(ev->goodGlobals());
    }


    TClonesArray *clP=ev->getMcPionArray();
    if(fabs(vert.Z())>0.0000001){
      h_vert_res->Fill(vert.Z()-ev->zdcVertexZ());
    }

    h_vert_all->Fill(ev->zdcVertexZ());
    for(int i_p=0;i_p<ev->numberOfMcPions();i_p++){
      MyMcTrack *pion=(MyMcTrack*)clP->At(i_p);
      if(pion->momentum().PseudoRapidity()>0.&&pion->momentum().PseudoRapidity()<1.){
	h_pion_all->Fill(pion->momentum().Pt());
	if(fabs(vert.Z())<0.0000001){
	  h_pion_no->Fill(pion->momentum().Pt());
	}
	else h_pion_reco->Fill(pion->momentum().Pt());
      }
    }


    
    if(fabs(vert.Z())<0.0000001){
      n_ev_no++;
      h_vert_no->Fill(ev->zdcVertexZ());
    }
    
    i++;
  }

  TCanvas *c=new TCanvas("c","c",600,900);
  c->Divide(2,3);
  c->cd(1);
  gPad->SetLogy();
  h_vert_all->Draw();
  h_vert_all->SetMinimum(1.);
  h_vert_no->SetLineColor(4);
  h_vert_no->DrawCopy("same");
  c->cd(2);
  h_vert_no->Divide(h_vert_all);
  h_vert_no->Draw();
  c->cd(3);
  gPad->SetLogy();
  h_pion_all->Scale(1./n_ev);
  h_pion_all->Draw();
  h_pion_no->Scale(1./n_ev_no);
  h_pion_no->SetLineColor(4);
  h_pion_no->DrawCopy("same");
  c->cd(4);
  h_pion_reco->Scale(1./n_ev);
  h_pion_reco->Divide(h_pion_all);
  h_pion_reco->Draw();
  c->cd(5);
  gPad->SetLogy();
  h_vert_res->Draw();
  c->SaveAs("hijing_vert.root");
}
