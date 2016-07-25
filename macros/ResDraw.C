void ResDraw() {
  Int_t NF = 0;
  TList *files = gROOT->GetListOfFiles();
  if (! files) return;
  Int_t nn = files->GetSize();
  if (! nn) return;
  TFile **FitFiles = new TFile *[nn];
  TIter next(files);
  while ( (f = (TFile *) next()) ) { 
    TString F(f->GetName());
    if (! F.Contains("resXT")) continue;
    FitFiles[NF] = f; cout << "Found file " << NF << "\t" << FitFiles[NF]->GetName() << endl;
    NF++;
  }
  TCanvas *c1 = new TCanvas("c1","Residual for the standard track fit versus row no.");
  c1->SetGrid();
  TH1F *frame = c1->DrawFrame(0,-0.06,46,0.1);
  frame->SetTitle("Residual versus row");
  frame->SetXTitle("Row");
  frame->SetYTitle("Residual (cm)");
  TLegend *leg = new TLegend(0.25,0.6,0.9,0.9,"");
  TString option("");
  for (int i = 0; i<NF; i++) {
    if (FitFiles[i]) { 
      FitFiles[i]->cd();
      TNtuple* FitP = (TNtuple*)  FitFiles[i]->Get("FitP");
      if (! FitP) continue;      TF1 *powfit = new TF1("powfit","[0]*pow(x,[1])",40,80);
      FitP->SetMarkerStyle(20+i);
      for (int k = 0; k < 4; k++) {
	Int_t ss = 12*k;
	FitP->SetMarkerColor(1+k);
	TString Hist(Form("s%i",ss));
	TString Draw(Form("mu:y>>"));
	Draw += Hist;
	TString Cut(Form("i&&j&&i>%i&&i<=%i&&prob>0",ss,ss+12));
	FitP->Draw(Draw.Data(),Cut.Data(),"goffprof");
	cout << "FitP->Draw(" << Draw.Data() << "," << Cut.Data() << ",\"prof\")" << endl;
	TProfile* hist = (TProfile*) gDirectory->Get(Hist.Data());
	if (! hist) continue;
	hist->Draw("same");
	TString Title(gSystem->BaseName(FitFiles[i]->GetName()));
	Title.ReplaceAll(".eventFC_999.root",""); 
	Title.ReplaceAll("resXTGP","");
	//	Int_t Index = Title.Index(".");
	//	if (Index > 0)  Title = TString(Title.Data(),Index);
	Title += Form(" ss (%i,%i]",ss,ss+12);
	leg->AddEntry(hist,Title.Data());
      }
    }
  }
  leg->Draw();
  delete [] FitFiles;
}
