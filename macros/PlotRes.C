void PlotRes() {
  Int_t NF = 0;
  TList *files = (TList *) gROOT->GetListOfFiles();
  if (! files) return;
  Int_t nn = files->GetSize();
  if (! nn) return;
  TIter next(files);
  TFile *f = 0;
  Int_t set = 1;
  TFile *fOut = new TFile("ResPlots.root","recreate");
  TCanvas *c1 = new TCanvas();
  c1->Divide(2,1);
  TProfile *prof = 0;
  TPad *pad;
  pad = (TPad *) c1->cd(1); pad->DrawFrame(0,-.1,150,0.1,"Residual (Inner) vs Z (West)");
  pad = (TPad *) c1->cd(2); pad->DrawFrame(0,-.1,150,0.1,"Residual (Inner) vs Z (East)");
  TLegend *leg[2];
  leg[0] = new TLegend(0.25,0.6,0.9,0.9,"");
  leg[1] = new TLegend(0.25,0.6,0.9,0.9,"");
  while ( (f = (TFile *) next()) ) { 
    cout << "File : " << f->GetName() << endl;
    TNtuple *resNtupleZ = (TNtuple *) f->Get("resNtupleZ");
    if (! resNtupleZ) continue;
    resNtupleZ->SetMarkerColor(set++);
    resNtupleZ->SetMarkerStyle(20);
    TString F(f->GetName());
    F.ReplaceAll("_After","");
    F.ReplaceAll("_Before","");
    F.ReplaceAll(".root","");
    c1->cd(1);
    resNtupleZ->Draw(Form("residual:z>>%sW",F.Data()),
		     "(residualError>0&&residualError<0.1&&padrow<13&&sector<=12&&z<150)/(residualError*residualError)","goffprofg");
    prof = (TProfile *) fOut->Get(Form("%sW",F.Data()));
    prof->SetMaximum(0.1);
    prof->SetMinimum(-.1);
    prof->SetStats(0);
    prof->Draw("same");
    leg[0]->AddEntry(prof,prof->GetName());
    c1->cd(2);
    resNtupleZ->Draw(Form("residual:z>>%sE",F.Data()),
		     "(residualError>0&&residualError<0.1&&padrow<13&&sector>12&&z<150)/(residualError*residualError)","goffprofg");
    prof = (TProfile *) fOut->Get(Form("%sE",F.Data()));
    prof->SetMaximum(0.1);
    prof->SetMinimum(-.1);
    prof->SetStats(0);
    prof->Draw("same");
    leg[1]->AddEntry(prof,prof->GetName());
  }
  c1->cd(1); leg[0]->Draw();
  c1->cd(2); leg[1]->Draw();
}
