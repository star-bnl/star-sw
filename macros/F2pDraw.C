TLegend *leg = 0;
THStack *F2pDraw(const Char_t *histName = "InnerXTimeRc", const Option_t *opt = "px", Int_t ix = 0) {
  Int_t NF = 0;
  TSeqCollection *files = gROOT->GetListOfFiles();
  Int_t nn = files->GetSize();
  if (! nn) return;
  TIter next(files);
  TFile *f = 0;
  TString Option(opt);
  THStack *hs = new THStack("hs",histName);
  Int_t color = 1;
  if (leg) delete leg;
  leg = new TLegend(0.6,0.6,0.95,0.95);
  while ( (f = (TFile *) next()) ) { 
    TString F(f->GetName());
    //    TString plotName(gSystem->BaseName(F));
    TString plotName(F);
    plotName.ReplaceAll(".Plots.root","");
    plotName.ReplaceAll("/","_");
    TH2 *h2 = (TH2 *) f->Get(histName);
    Int_t ix2 = -1;
    if (ix) ix2 = ix;
    TH1 *h1 = h2->ProjectionX(Form("%s_%s_%i",plotName.Data(),opt,ix),ix,ix2);
    h1->SetMarkerColor(color++);
    hs->Add(h1);
    leg->AddEntry(h1,plotName);
  }
  hs->Draw("nostack,e1p");
  leg->Draw();
  return hs;
}

