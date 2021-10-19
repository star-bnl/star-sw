
//____________________________________________________________________________________________________
void merge_chi2_npp_k(const Char_t* inputFileList = "file_chi2.list")
{
	ifstream fin(inputFileList);
	if(!fin){
		Error("merge_chi2", "can't open %s", inputFileList);
		return;
	}

	// 200 GeV
	//  const Int_t nppbin = 20 ;
	//  const Double_t nppmin = 2.18 ;
	//  const Double_t nppmax = 2.38 ;
	//  const Int_t kbin = 100 ;
	//  const Double_t kmin = 0.30 ;
	//  const Double_t kmax = 1.30 ;
	//  const Int_t nppbin = 40 ;
	//  const Double_t nppmin = 2.00 ;
	//  const Double_t nppmax = 2.40 ;
	//  const Int_t kbin = 30 ;
	//  const Double_t kmin = 0.50 ;
	//  const Double_t kmax = 3.50 ;

	// 39 GeV
	//  const Int_t nppbin = 10 ;
	//  const Int_t nppbin = 50 ;
	//  const Double_t nppmin = 1.1 ;
	//  const Double_t nppmax = 1.6 ;
	//  const Double_t nppmin = 1.3 ;
	//  const Double_t nppmax = 1.8 ;
	//  const Double_t nppmin = 1.45 ;
	//  const Double_t nppmax = 1.55 ;

	// 39 GeV
	//  const Int_t kbin = 31 ;
	//  const Double_t kmin = 0.50 ;
	//  const Double_t kmax = 3.50 ;
	//  const Int_t kbin = 40 ;
	//  const Int_t kbin = 20 ;
	//  const Double_t kmin = 1.00 ;
	//  const Double_t kmax = 3.00 ;

	// 11.5 GeV
	//  const Int_t nppbin = 20 ;
	//  const Double_t nppmin = 1.0 ;
	//  const Double_t nppmax = 1.2 ;
	//  const Int_t kbin = 10 ;
	//  const Double_t kmin = 1.00 ;
	//  const Double_t kmax = 2.00 ;
	//  const Int_t kbin = 50 ;
	//  const Double_t kmin = 0.50 ;
	//  const Double_t kmax = 3.00 ;

	// 7.7 GeV
	const Int_t nppbin = 30 ;
	const Double_t nppmin = 0.7 ;
	const Double_t nppmax = 1.0 ;
	const Int_t kbin = 30 ;
	const Double_t kmin = 0.50 ;
	const Double_t kmax = 3.50 ;

	TH2* hChi2 = new TH2D("hchi2Merge", "", nppbin, nppmin, nppmax, kbin, kmin, kmax);
	hChi2->SetXTitle("n_{pp}");
	hChi2->SetYTitle("k");

	Int_t ifile = 0 ;
	Double_t nevents = 0 ;
	TString name("");
	while( fin >> name ){
		TFile* file = TFile::Open(name);
		if(!file || !file->IsOpen()){
			Error("merge_chi2", "can't open %s", name.Data());
			return;
		}
		cout << "OPEN " << file->GetName() << endl;

		TH3* hchi2 = (TH3D*) file->Get("hChi2");
		nevents += hchi2->GetEntries() ;
		for(Int_t x=0; x<hchi2->GetNbinsX(); x++){
			const Int_t y = ifile ;
			hChi2->SetBinContent(x+1, y+1, hchi2->GetBinContent(x+1, 1, 1));
		}

		ifile++;
	}

	hChi2->SetEntries(nevents);

	TFile* output = TFile::Open("chi2_merge.root", "recreate");
	hChi2->Write();
	output->Close();
}


//____________________________________________________________________________________________________
void merge_chi2_npp_x(const Char_t* inputFileList = "file_chi2.list")
{
	ifstream fin(inputFileList);
	if(!fin){
		Error("merge_chi2", "can't open %s", inputFileList);
		return;
	}

	const Int_t nppbin = 50 ;
	//  const Double_t nppmin = 1.1 ;
	//  const Double_t nppmax = 1.6 ;
	const Double_t nppmin = 1.3 ;
	const Double_t nppmax = 1.8 ;
	//  const Int_t xbin = 11 ;
	//  const Double_t xmin = 0.08 ;
	//  const Double_t xmax = 0.19 ;
	const Int_t xbin = 70 ;
	const Double_t xmin = 0.08 ;
	const Double_t xmax = 0.15 ;

	TH2* hChi2 = new TH2D("hchi2Merge", "", nppbin, nppmin, nppmax, xbin, xmin, xmax);
	hChi2->SetXTitle("n_{pp}");
	hChi2->SetYTitle("x");

	Int_t ifile = 0 ;
	Double_t nevents = 0 ;
	TString name("");
	while( fin >> name ){
		TFile* file = TFile::Open(name);
		if(!file || !file->IsOpen()){
			Error("merge_chi2", "can't open %s", name.Data());
			return;
		}
		cout << "OPEN " << file->GetName() << "  nfile=[" << ifile << "]" << endl;

		TH3* hchi2 = (TH3D*) file->Get("hChi2");
		nevents += hchi2->GetEntries() ;
		for(Int_t x=0; x<hchi2->GetNbinsX(); x++){
			const Int_t z = ifile ;
			hChi2->SetBinContent(x+1, z+1, hchi2->GetBinContent(x+1, 1, 1));
		}

		ifile++;
	}
	hChi2->SetEntries(nevents);

	TFile* output = TFile::Open("chi2_merge_x_vs_npp.root", "recreate");
	hChi2->Write();
	output->Close();
}

//____________________________________________________________________________________________________
void merge_chi2_eff(const Char_t* inputFileList = "file_chi2.list")
{
	ifstream fin(inputFileList);
	if(!fin){
		Error("merge_chi2", "can't open %s", inputFileList);
		return;
	}

	// 39 GeV
	//  const Int_t kbin = 30 ;
	//  const Double_t kmin = 0.50 ;
	//  const Double_t kmax = 3.50 ;
	const Int_t nppbin = 50 ;
	const Double_t nppmin = 0.70 ;
	const Double_t nppmax = 1.20 ;

	// 11.5 GeV
	const Int_t nppbin = 20 ;
	const Double_t nppmin = 1.00 ;
	const Double_t nppmax = 1.20 ;

	// For constant efficiency, 7.7 GeV
	//  const Int_t effbin = 40 ;
	//  const Double_t effmin = 0.60 ;
	//  const Double_t effmax = 1.00 ;

	// For mult-dep efficiency, 7.7 GeV
	//  const Int_t effbin = 60 ;
	//  const Double_t effmin = 0.00 ;
	//  const Double_t effmax = 0.60 ;
	//  const Double_t effmin = 0.10 ;
	//  const Double_t effmax = 0.31 ;

	// 11.5 GeV (mult-dep)
	const Int_t effbin = 30 ;
	const Double_t effmin = 0.05 ;
	const Double_t effmax = 0.35 ;

	//  TH2* hChi2 = new TH2D("hchi2Merge", "", kbin, kmin, kmax, effbin, effmin, effmax);
	//  hChi2->SetXTitle("k");
	TH2* hChi2 = new TH2D("hchi2Merge", "", nppbin, nppmin, nppmax, effbin, effmin, effmax);
	hChi2->SetXTitle("n_{pp}");
	hChi2->SetYTitle("#varepsilon");

	Int_t ifile = 0 ;
	Double_t nevents = 0 ;
	TString name("");
	while( fin >> name ){
		TFile* file = TFile::Open(name);
		if(!file || !file->IsOpen()){
			Error("merge_chi2", "can't open %s", name.Data());
			return;
		}
		cout << "OPEN " << file->GetName() << endl;

		TH3* hchi2 = (TH3D*) file->Get("hChi2");
		//    TH1* hchi2k = (TH1D*) hchi2->Project3D("y");
		TH1* hchi2npp = (TH1D*) hchi2->Project3D("x");
		nevents += hchi2->GetEntries() ;
		//    for(Int_t x=0; x<hchi2k->GetNbinsX(); x++){
		for(Int_t x=0; x<hchi2npp->GetNbinsX(); x++){
			const Int_t z = ifile ;
			//      hChi2->SetBinContent(x+1, z+1, hchi2k->GetBinContent(x+1));
			hChi2->SetBinContent(x+1, z+1, hchi2npp->GetBinContent(x+1));
		}

		ifile++;
		file->Close();
	}
	hChi2->SetEntries(nevents);

	TFile* output = TFile::Open("chi2_merge.root", "recreate");
	hChi2->Write();
	output->Close();
	}


