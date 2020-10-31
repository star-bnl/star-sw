// Clusters
void TbyThits() {
  struct Name_t {
    Char_t *histName;
    Char_t *varName;
    Char_t *cutName;
  };
  Name_t Names[24] = {
    {"PadFC",       "newP.pad:newP.row","oldP.sector<=0&&newP.sector!=20"},
    {"PminFC",     "newP.pmin:newP.row","oldP.sector<=0&&newP.sector!=20"},
    {"PmaxFC",     "newP.pmax:newP.row","oldP.sector<=0&&newP.sector!=20"},
    {"PadFCAll",    "newP.pad:newP.row","newP.sector!=20"},
    {"PminFCAll",  "newP.pmin:newP.row","newP.sector!=20"},
    {"PmaxFCAll",  "newP.pmax:newP.row","newP.sector!=20"},

    {"PadOC",       "oldP.pad:oldP.row","newP.sector<=0&&oldP.sector!=20"},
    {"PminOC",     "oldP.pmin:oldP.row","newP.sector<=0&&oldP.sector!=20"},
    {"PmaxOC",     "oldP.pmax:oldP.row","newP.sector<=0&&oldP.sector!=20"},
    {"PadOCAll",    "oldP.pad:oldP.row","oldP.sector!=20"},
    {"PminOCAll",  "oldP.pmin:oldP.row","oldP.sector!=20"},
    {"PmaxOCAll",  "oldP.pmax:oldP.row","oldP.sector!=20"}

    {"PadFC20",       "newP.pad:newP.row","oldP.sector<=0&&newP.sector==20"},
    {"PminFC20",     "newP.pmin:newP.row","oldP.sector<=0&&newP.sector==20"},
    {"PmaxFC20",     "newP.pmax:newP.row","oldP.sector<=0&&newP.sector==20"},
    {"PadFC20All",    "newP.pad:newP.row","newP.sector==20"},
    {"PminFC20All",  "newP.pmin:newP.row","newP.sector==20"},
    {"PmaxFC20All",  "newP.pmax:newP.row","newP.sector==20"},

    {"PadOC20",       "oldP.pad:oldP.row","newP.sector<=0&&oldP.sector==20"},
    {"PminOC20",     "oldP.pmin:oldP.row","newP.sector<=0&&oldP.sector==20"},
    {"PmaxOC20",     "oldP.pmax:oldP.row","newP.sector<=0&&oldP.sector==20"},
    {"PadOC20All",    "oldP.pad:oldP.row","oldP.sector==20"},
    {"PminOC20All",  "oldP.pmin:oldP.row","oldP.sector==20"},
    {"PmaxOC20All",  "oldP.pmax:oldP.row","oldP.sector==20"}

  };
  TChain *tChain = 0;
  TFile *fOut = 0;
  //  fOut = new TFile("hit.root","update");
  if (! fOut) {
    TDirIter Dir("trackMateFilest_physics_adc*.root");
    TFile *f = 0;
    const Char_t *TreeName = "hitMateComp";
    tChain = new TChain(TreeName);
    Int_t NFiles = 0;
    ULong64_t nEvents = 0;
    ULong64_t nEvTot = 0;
    Char_t *file = 0;
    while ( (file = (Char_t *) Dir.NextFile()) ) {   
      f = new TFile(file);
      if (! f) {cout << "missing file " << file << endl; continue;}
      TTree *tree = (TTree *) f->Get(TreeName);
      cout << "#\t" << NFiles << "\t" << f->GetName();
      if (tree) {
	NFiles++;
	nEvents = tree->GetEntries();
	cout << "\tNo,Events = " << nEvents << endl;
	nEvTot += nEvents;
	tChain->Add(f->GetName());
      } else {
	cout << "\tTTree is missing" << endl;
      }
      delete f; 
    }
    cout	<< "chained " << NFiles  << " files \t" 
		<< "with total " << nEvTot << " events \t" 
		<< "chain returned pointer: " << tChain << endl;
    if (! fOut) fOut = new TFile("hit2D.root","recreate");
    TCanvas *c1 = new TCanvas();
    c1->SetLogz(1);
    for (Int_t k = 0; k < 24; k++) {
      TH2F *hist = (TH2F *) gDirectory->Get("Names[k].histName");
      if ( hist) continue;
      tChain->Draw(Form("%s>>%s(72,0.5,72.5,182,0.5,182.5)",Names[k].varName,Names[k].histName),Form("%s",Names[k].cutName),"goff");
      hist = (TH2F *) gDirectory->Get("Names[k].histName");
      if (! hist) continue;
      hist->SetXTitle("row");
      hist->SetYTitle("pad");
    }
  }
  for (Int_t l = 0; l < 4; l++) {
    for (Int_t k = 0; k < 3; k++) {
      TH2F *P = (TH2F *) gDirectory->Get(Names[6*l+k].histName);
      TH2F *PAll = (TH2F *) gDirectory->Get(Names[6*l+k+3].histName);
      if (! P || ! PAll) continue;
      TH2 *R = new TH2F(*P); // (TH2 *) P->Project3D("yz");
      R->SetName(Form("R%s",R->GetName()));
      //      TH2 *RAll = (TH2 *) PAll->Project3D("yz");
      //      R->Divide(RAll);
      R->Divide(PAll);
      R->SetTitle("Ratio");
      R->Draw("colz");
    }
  }
  fOut->Write();
}

