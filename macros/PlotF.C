#if 0
TF1 *func  = new TF1("func","sqrt([0]*[0]+[1]*[1]*(209.3-abs(x)))");
func->SetParameters(0.8,1);
func->SetParName(0,"K3");
func->SetParName(1,"D");
OuterPadRcK3_p4->Fit("func")
InnerPadRcK3_p3->Fit("func")
#endif
void PlotF(TString HistName="OuterPadRcQ_4Q", TString FitName="", TString SelName = "") {
  TList *files = gROOT->GetListOfFiles();
  if (! files) return;
  Int_t NF = files->GetSize();
  Int_t NGoodF = 0;
  TFile **FitFiles = new TFile*[NF];
  TIter next(files);
  Double_t xmin =  9999;
  Double_t xmax = -9999;
  Double_t ymin =  9999;
  Double_t ymax = -9999;
  TH1D *hist = 0;
  TH1D *Hist = 0;
  TFile *f = 0;
  while ( (f = (TFile *) next()) ) { 
    TString F(f->GetName());
    hist = (TH1D *) f->Get(HistName.Data());
    if (! hist ) continue;
    if (SelName != "" && ! F.Contains(SelName.Data())) continue;
    Hist = hist;
    FitFiles[NGoodF] = f; NGoodF++;
    TAxis *x = hist->GetXaxis();
    TAxis *y = hist->GetYaxis();
    if (xmin > x->GetXmin()) xmin = x->GetXmin();
    if (xmax < x->GetXmax()) xmax = x->GetXmax();
    if (ymin > hist->GetMinimum()) ymin = hist->GetMinimum();
    if (ymax < hist->GetMaximum()) ymax = hist->GetMaximum();
  }
  ymax += 0.1*(ymax - ymin);
  if (! NGoodF) return;
  TString CName(Form("%s:%s",Hist->GetName(),Hist->GetTitle()));
  TCanvas *c1 = new TCanvas(CName,CName);
  TH1F *frame = c1->DrawFrame(xmin,ymin,xmax,ymax);
  TString Title("Inner");
  if (HistName.Contains("Outer")) Title = "Outer";
  if (HistName.Contains("Time") && HistName.EndsWith("p8")) {
    Title += ":Shaping time vesus Z";
    frame->SetXTitle("Z (cm)");
    frame->SetYTitle("#tau (secs)");
  } else {if (HistName.Contains("Pad") && HistName.EndsWith("p2")) {
    Title += ":#sigma^{2} vesus Z";
    frame->SetXTitle("Z (cm)");
    frame->SetYTitle("#sigma^{2} (pads^{2})");
  } else {
    Title +=" : ";
    Title += Hist->GetTitle();
  }}
  frame->SetTitle(Title);
  TLegend *leg = new TLegend(0.55,0.8,0.9,0.9,"");
  TString same("same");
  for (int i = 0; i<NGoodF; i++) {
    if (FitFiles[i]) { 
      hist = (TH1D *) FitFiles[i]->Get(HistName.Data());
      hist->SetMarkerStyle(20);
      hist->SetMarkerColor(i+1);
      hist->SetLineColor(i+1);
      TString name(FitFiles[i]->GetName());
      name.ReplaceAll(".root"," ");
      if (FitName != "") {
	hist->Fit(FitName.Data(),"0");
	hist->SetStats(0);
	hist->Draw(same.Data()); same = "same";
	name += hist->GetTitle();
	TF1 *func = hist->GetFunction(FitName.Data());
	if (func) {
	  if (HistName.Contains("Time") && HistName.EndsWith("_p8"))
	    name += Form(" = %5.2f +/- %5.2f (ns)",1e9*func->GetParameter(0),1e9*func->GetParError(0)); 
	  if (HistName.Contains("Time") && (HistName.EndsWith("_p5") || HistName.EndsWith("_p6")))
	    name += Form(" = %5.2f +/- %5.2f",func->GetParameter(0),func->GetParError(0)); 
	  func->Draw("same");
	}
      }
      else {
	hist->Draw(same.Data()); same = "same";
	TList *list = hist->GetListOfFunctions();
	TIter next(list);
	TF1 *func = 0;
	while ((func = (TF1 *) next())) {
	  TString funcN(func->GetName());// cout << funcN << endl;
	  if (HistName.Contains("Pad")) {// cout << HistName << endl;
	    if (HistName.EndsWith("_p2")) {
	      TString fName(func->GetName());
	      func->SetLineColor(i+1);
	      if (fName == "gdI" || fName == "gdO") {
		name += ":";
		name += func->GetParName(0);
		name += Form(" = %6.2f +/- %6.2f cm, ",func->GetParameter(0), func->GetParError(0));
		name += func->GetParName(1);
		name += Form(" = %6.2f +/- %6.2f #mum",1e4*func->GetParameter(1), 1e4*func->GetParError(1));
		break;
	      }
	    }
	    else if (funcN == "fConvolution") { 
	      //	      cout << "\t" << funcN << endl;
	      if (HistName.EndsWith("K3")) {
		//		cout << "\t" << HistName << endl;
		Int_t k = 3;
		if (HistName.Contains("OuterPadRcK3")) k = 4;
		name += ":"; name += func->GetParName(k);
		name += Form(" = %6.2f +/- %6.2f cm, ",func->GetParameter(k), func->GetParError(k));
		break;
	      }
	    }
	  }
	}
      }
      cout << name << endl;
      leg->AddEntry(hist,name.Data());
    }
  }
  leg->Draw();
  delete [] FitFiles;
}
