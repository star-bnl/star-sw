class StGammaEvent;

TFile *file = 0;
TTree *tree = 0;

StGammaEvent *event = 0;

const Float_t rootS = 200.0;

void readGammaTree( const Char_t *fname = "gamma_tree.root" )
{
  gROOT->LoadMacro("StRoot/StGammaMaker/macros/loadGammaLibs.C");
  loadGammaLibs();

  file = new TFile(fname);
  tree = (TTree*)file->Get("gammas");
  event = new StGammaEvent();
  tree->SetBranchAddress("GammaEvent",&event);


  for ( Long64_t i=0;i<tree->GetEntries();i++ )
    {
      tree->GetEntry(i);

      Int_t run   = event->runNumber();
      Int_t eventid = event->eventNumber();
      Int_t nc    = event->numberOfCandidates();
      UShort_t flags = event->flags();
      Float_t sumPt  = event->sumPt(-2.5,2.5);
      Float_t sumTrackPt = event->sumTrackPt(-2.5,2.5);
      Float_t sumTowerPt = event->sumTowerPt(-2.5,2.5);

      std::cout << "-------------------------------------------------------------" << std::endl;
      std::cout << "run         = " << run   << std::endl;
      std::cout << "event       = " << eventid << std::endl;
      std::cout << "flags       = " << Form("0x%X",flags) << std::endl;      
      std::cout << "pt sum      = " << sumPt << std::endl;
      std::cout << "  + tracks  = " << sumTrackPt << std::endl;
      std::cout << "  + towers  = " << sumTowerPt << std::endl;

      std::cout << "printing n candidates = " << nc << std::endl;
      for ( Int_t ic = 0; ic < nc; ic++ )
	{
	  StGammaCandidate *candidate = event->candidate(ic);
	  if ( !candidate ) continue;

	  // identity
	  Int_t gammaId = candidate->id();
	  Int_t caloId  = candidate->detectorId();

	  // kinematics
	  Float_t pt  = candidate->momentum().Perp();
	  Float_t eta = candidate->momentum().Eta();
	  Float_t phi = candidate->momentum().Phi();
	  Float_t xf  = candidate->momentum().Z() / rootS;

	  // isolation energy sums at three different radii
	  Float_t iso4 = candidate->sumPt(0.4);
	  Float_t iso5 = candidate->sumPt(0.5);
	  Float_t iso7 = candidate->sumPt(0.7);

	  Int_t ntracks4 = candidate->numberOfTracks(0.4);
	  Int_t ntowers4 = candidate->numberOfTowers(0.4);
	  Int_t ntracks5 = candidate->numberOfTracks(0.5);
	  Int_t ntowers5 = candidate->numberOfTowers(0.5);
	  Int_t ntracks7 = candidate->numberOfTracks(0.7);
	  Int_t ntowers7 = candidate->numberOfTowers(0.7);	  

	  Int_t ntracks = candidate->numberOfMyTracks();
	  Int_t ntowers = candidate->numberOfMyTowers();

	  std::cout << " ++++++++++++++++++++++++++++++++";
	  std::cout << " id=" << gammaId <<std::endl;
	  std::cout << " calo=" << caloId << std::endl;
	  std::cout << " pt="<<pt << " eta="<<eta<<" phi="<<phi << std::endl;
	  std::cout << " ntowers=" << ntowers << " ntracks="<<ntracks<<std::endl;

	  std::cout << " iso4="<<iso4<<" iso5="<<iso5<<" iso7="<<iso7<<std::endl;
	  std::cout << " ntr4="<<ntracks4<<" ntr5="<<ntracks5<<" ntr7="<<ntracks7<<std::endl;
	  std::cout << " ntw4="<<ntowers4<<" ntw5="<<ntowers5<<" ntw7="<<ntowers7<<std::endl;

	  Int_t nu = candidate->numberOfSmdu();
	  Int_t nv = candidate->numberOfSmdv(); 
	  std::cout << "nu="<<nu<<" nv="<<nv<<std::endl; 

	  

	}


    }


}
