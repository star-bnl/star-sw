void DrawDCA(Int_t opt = 0) {
  if (opt < 0 || opt > 1) return;
  TSeqCollection *files = gROOT->GetListOfFiles();
  if (! files) return;
  Int_t NF = files->GetSize();
  if  (NF != 4) return;
  TFile *Files[4];
  TIter next(files);
  for (Int_t l = 0; l < NF; l++) {
    TFile *file = (TFile *) next();
    Int_t nf = -1;
    if (file) {
      TString FileN(gSystem->BaseName(file->GetName()));
      Int_t index = FileN.Index(".");
      if (index) FileN = TString(FileN.Data(),index);
      if      (FileN == "Sti"  ) nf = 1;
      else if (FileN == "StiCA") nf = 2;
      else if (FileN == "Stv"  ) nf = 3;
      else if (FileN == "StvCA") nf = 4;
      else {continue;}
    }
    Files[nf-1] = file;
  }  
  const Char_t *PlotName[4] = {"dcaXY","dcaZ", "pullXY","pullZ"};
  TF1 *f = new TF1("f","TMath::Sqrt([0]*[0]+([1]*x)*([1]*x))",0.1,5);
  f->SetParameters(0.1,0.1);
  f->SetParLimits(0,0,10);
  f->SetParLimits(1,0,10);
  TCanvas *c1 = new TCanvas("sigma","sigma",800,400);
  c1->Divide(2,1);
  TCanvas *c2 = new TCanvas("fit","fit");
  for (Int_t k = 0; k < 2; k++) {
    TLegend *leg = new TLegend(0.4,0.15,0.9,0.5);
    //      Int_t kl = NF*k+nf;
    Int_t kl = k + 1;
    TH1F *frame = c1->cd(kl)->DrawFrame(0.1,0,5.,1.0);
    frame->SetTitle(FileN);
    //      frame->SetTitle(Form("#sigma of %s versus 1/p for %s",PlotName[k],FileN.Data()));
    frame->SetTitle(Form("Sigma of %s versus 1/p",PlotName[k]));
    frame->SetXTitle("1/P(GeV/c)");
    if (k == 0) 	  frame->SetYTitle("#sigma_{XY} (cm)");
    else 	          frame->SetYTitle("#sigma_{Z} (cm)");
    for (Int_t l = 0; l < NF; l++) {
      TFile *file = Files[l];
      if (! file ) continue;
      file->cd();
      TString FileN(gSystem->BaseName(file->GetName()));
      Int_t index = FileN.Index(".");
      if (index) FileN = TString(FileN.Data(),index);
       //      for (Int_t i = 0; i <= 4; i++) {
      for (Int_t i = 0; i <= 1; i++) {
	c2->cd();
	TString histN(PlotName[k+2*opt]);
	histN += Form("Invp%i",i);
	TH2 *hist = (TH2 *) file->Get(histN);
	if (! hist) continue;
	if (hist->GetEntries() < 100) continue;
	hist->FitSlicesY();
	TH1 *histF = (TH1 *) file->Get(histN + "_2");
	if (! histF) continue;
	histF->SetMarkerStyle(20);
	Int_t color = l+1;
	if (i >= 4) color++;
	histF->SetMarkerColor(color);
	histF->Fit(f,"reqq","",0.1,5.0);
	histF->SetStats(0);
	c1->cd(kl);
	histF->SetTitle("");
	histF->Draw("same");
	if (i > 0) 
	  leg->AddEntry(histF,Form("No. Si points = %i:  #sigma = %4.0f  #oplus %4.0f/P  #mum = %4.0f  #mum @ 1 GeV/c"
				   ,i,1e4*f->GetParameter(0),1e4*f->GetParameter(1),1e4*f->Eval(1.))); 
	else
	  leg->AddEntry(histF,Form("%s:  #sigma = %4.0f  #oplus %4.0f/P  #mum = %4.0f  #mum @ 1 GeV/c"
				   ,FileN.Data(),1e4*f->GetParameter(0),1e4*f->GetParameter(1),1e4*f->Eval(1.))); 
	cout << histF->GetName() << "\t" << 1e4*f->Eval(1.);
	if (i == 0) cout << "\t" << file->GetName();
	cout << endl;
      }
      c1->cd(kl);
      leg->Draw();
    }
  }
}
