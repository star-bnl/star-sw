
//____________________________________________________________________________________________________
void addNcollVsNpart(
		const Char_t* inputFileList = "./LIST/tree.default.list"
		)
{
	gSystem->Load("St_base");
	gSystem->Load("StUtilities");
	gSystem->Load("StGlauberUtilities");
	gSystem->Load("StCentralityMaker");
	gSystem->Load("StGlauberTree");
	gSystem->Load("StGlauberAnalysisMaker");

	ifstream fin(inputFileList);
	if(!fin)
	{
		Error("addNcollVsNpart", "can't open %s", inputFileList);
		return;
	}

	TChain t("tree");
	TString file;
	while(fin>>file)
	{
		cout << "Add " << file << " into chain" << endl;
		t.Add(file);
	}

	TH2* hNcoll_Npart = new TH2D
		("hNcoll_Npart", "hNcoll_Npart;Npart;Ncoll;",
		 //      StGlauberConstUtilities::GetNpartBin(), 0, StGlauberConstUtilities::GetNpartMax(),
		 //      StGlauberConstUtilities::GetNcollBin(), 0, StGlauberConstUtilities::GetNcollMax()
		 500, 0, 500, 1800, 0, 1800
		);
	t.Draw("ncoll:npart>>hNcoll_Npart", "npart>=2&&ncoll>=1");

	// Write output ROOT file
	TFile* outputFile = TFile::Open("ncoll_npart.root", "recreate");
	hNcoll_Npart->Print();
	hNcoll_Npart->Write();
	outputFile->Close();
}
