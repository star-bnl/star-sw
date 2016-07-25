struct Tree_t {
  Int_t binx, biny, binz, bin;
  Float_t  x,    y,    z;
  Float_t  C, E;
};
TTree *DumpHist2NT(TH1 *h = 0) {
  if (! h) return 0;
  TString Name("T_"); Name += h->GetName();
  TString Title("TTree for histogram "); Title += h->GetTitle();
  TString Out(gDirectory->GetName());
  Title += " from file "; Title += Out;
  Out.ReplaceAll(".root",".TTree.root");
  TFile *fOut = new TFile(Out,"recreate");
  TTree *tree = new TTree(Name,Title);
  Tree_t DTree;
  tree->Branch("binx",&DTree.binx,"binx/I");
  tree->Branch("biny",&DTree.biny,"biny/I");
  tree->Branch("binz",&DTree.binz,"binz/I");
  tree->Branch("bin",&DTree.bin,"bin/I");
  tree->Branch("x",&DTree.x,"x/F");
  tree->Branch("y",&DTree.y,"y/F");
  tree->Branch("z",&DTree.z,"z/F");
  tree->Branch("C",&DTree.C,"C/F");
  tree->Branch("E",&DTree.E,"E/F");
  Int_t nbinsx = h->GetNbinsX();
  Int_t nbinsy = h->GetNbinsY();
  Int_t nbinsz = h->GetNbinsZ();
  if (h->GetDimension() < 2) nbinsy = -1;
  if (h->GetDimension() < 3) nbinsz = -1;
  //   - Loop on bins (including underflows/overflows)
  Int_t bin, binx, biny, binz;
  Double_t xx[3];
  for (DTree.binz=0;DTree.binz<=nbinsz+1;DTree.binz++) {
    DTree.z = h->GetZaxis()->GetBinCenter(DTree.binz);
    for (DTree.biny=0;DTree.biny<=nbinsy+1;DTree.biny++) {
      DTree.y = h->GetYaxis()->GetBinCenter(DTree.biny);
      for (DTree.binx=0;DTree.binx<=nbinsx+1;DTree.binx++) {
	DTree.x = h->GetXaxis()->GetBinCenter(DTree.binx);
	DTree.bin = DTree.binx +(nbinsx+2)*(DTree.biny + (nbinsy+2)*DTree.binz);
	DTree.C   = h->GetBinContent(DTree.bin);
	DTree.E   = h->GetBinError(DTree.bin);
	tree->Fill();
      }
    }
  }
  return tree;
}
