struct XPoint_t {
  Float_t x;
  Float_t y;
  Float_t run;
  Float_t mu;  
  Float_t sigma;
  Float_t Entries;
  Float_t Scale;
};
XPoint_t Point;
void SecRowNt(const Char_t *Name="SecRow",const Char_t *FileName="SecRowNt.root") {

  PointP = new TNtuple("PointP","SecRow","sector:row:run:mu:sigma:Entries:Scale");
  
  TIter  next(gROOT->GetListOfFiles());
  TFile *f = 0;
  Int_t i = 0;
  while ((f = (TFile *) next())) {
    cout << "Source file " << i++ << ": " << f->GetName() << endl;
    TProfile2D *hist = (TProfile2D *) f->Get(Name);
    if (! hist) continue;
    Point.Entries = hist->GetEntries();
    sscanf(gSystem->BaseName(f->GetName()),"%f_",&Point.run);
    cout << "Run: " << Point.run << "\tEntries: " << Point.Entries;
    Int_t nx = hist->GetNbinsX();
    Int_t ny = hist->GetNbinsY();
    TAxis *fXaxis = hist->GetXaxis();
    TAxis *fYaxis = hist->GetYaxis();
    Int_t ix, iy;
    Double_t sumw = 0;
    Double_t    w = 0;
    for (iy=0;iy<ny;iy++){
      for (ix=0;ix<nx;ix++){
	Int_t bin = hist->GetBin(ix,iy);
	Double_t cont = hist->GetBinContent(bin);
	Double_t err  = hist->GetBinError(bin);
	if (err < 1.e-9) continue; 
	sumw += cont/(err*err);
	w    +=   1./(err*err);
      }
    }
    if (w < 1.e-9) continue;
    Point.Scale = sumw/w;
    cout << "\tScale: " << Point.Scale << endl;
    for (iy=0;iy<ny;iy++){
      for (ix=0;ix<nx;ix++){
	Int_t bin    = hist->GetBin(ix,iy);
	Point.mu     = hist->GetBinContent(bin) - Point.Scale;
	Point.sigma  = hist->GetBinError(bin);
	if (err < 1.e-9) continue; 
	Point.x      = fXaxis->GetBinCenter(ix);
	Point.y      = fYaxis->GetBinCenter(iy);
	PointP->Fill(&Point.x);
      }
    }    
  }
  f = new TFile(FileName,"RECREATE");
  PointP->Write();
  delete f;
}
