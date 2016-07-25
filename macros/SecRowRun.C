void SecRowRun() {
  TSeqCollection *files = gROOT->GetListOfFiles();
  Int_t nn = files->GetSize();
  if (! nn) return;
  TIter next(files);
  TFile *f = 0;
  TFile *fOut = new TFile("SecRowRun.root","new");
  Int_t i = 0;
  Int_t m = 20;
  while ( (f = (TFile *) next()) ) { 
    TString T(f->GetName());
    if (! T.Contains("SecRowRun_")) continue;
    T.ReplaceAll("SecRow","");
    T.ReplaceAll(".root","");
    TNtuple *F = (TNtuple *) f->Get("FitP");
    if (! F) continue;
    i++;
    if (i == 5) i = 6;
    if (i > 8) {i = 1; m = 21;}
    F->SetMarkerStyle(m);
    F->SetMarkerColor(i);
    if (T.Contains("Run_1") || T.Contains("Run_2")) {
      F->Draw(Form("sigma:y>>%s(45,0.5,45.5)",T.Data()),"","prof");
    } else {
      F->Draw(Form("sigma:y>>%s(45,0.5,45.5)",T.Data()),"1./(dsigma*dsigma)","profg");
    }  
  }
  fOut->Write();
  Draw(fOut);
}
//________________________________________________________________________________
void Draw(TDirectory *fOut) {
  TList *listOfKeys = fOut->GetListOfKeys();
  TList *listOfObjects = fOut->GetList();
  if (! listOfObjects && ! listOfKeys) return;
  TObjArray array;
  TIter nextobj(listOfObjects); 
  TObject *obj = 0;
#if 1
  while ((obj = nextobj())) {
    TString Name(obj->GetName());
    if (Name.Contains("Run")) {
      if (! obj) continue;
      if ( obj->IsA()->InheritsFrom( "TH1" ) ) {
	TH1 *hist = (TH1 *) obj;
	if (hist->GetEntries() > 0) 
	  array.Add(obj);
      }
    }
  }
#endif
  TKey *key = 0;
  TIter nextkey(listOfKeys); 
  while ((key = (TKey*) nextkey())) {
    TString Name(key->GetName());
    if (Name.Contains("Run")) {
      if (array.FindObject(key->GetName())) continue;
      obj = key->ReadObj();
      if (! obj) continue;
      if ( obj->IsA()->InheritsFrom( "TH1" ) ) {
	TH1 *hist = (TH1 *) obj;
	if (hist->GetEntries() > 0) 
	  array.Add(obj);
      }
    }
  }
  Int_t NF = array.GetEntriesFast();
  cout << "no. of histograms " << NF << endl;
  TCanvas *c = new TCanvas();
  TH1F *frame = c->DrawFrame(0,0,46,0.5);
  frame->SetTitle("#sigma versus row no");
  frame->SetYTitle("#sigma");
  frame->SetXTitle("row");
  TLegend *leg = new TLegend(0.1,0.1,0.55,0.25,"");
  Int_t color = 1;
  Int_t marker = 20;
  for (Int_t i = 0; i < NF; i++) {
    TH1 *hist = (TH1*) array.At(i);
    if (color > 8) {color = 1; marker = 21;}
    hist->SetMarkerStyle(marker);
    if (color == 5) color++;
    hist->SetMarkerColor(color);
    color++;
    hist->Draw("same");
    leg->AddEntry(hist, hist->GetName());
  }
  leg->Draw();
  c->Update();
}
