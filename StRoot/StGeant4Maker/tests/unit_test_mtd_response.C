#include "tests/unit_tests.h"

//___________________________________________________________________
double _eta  = 0; 
double _phid = 0;

//___________________________________________________________________
void unit_test_mtd_response( int nevents=1000 ) {

  gROOT->ProcessLine("initChain();");

  auto* pm = dynamic_cast<StarPrimaryMaker*>( StMaker::GetChain()->GetMaker("PrimaryMaker") );
  pm->SetVertex(0.,0.,0.);
  pm->SetSigma(0.0,0.,0.);

  LOG_TEST << "=======================================================" << std::endl;
  LOG_TEST << "Unit testing of tracks and EPD hits on single muons"     << std::endl;
  LOG_TEST << "=======================================================" << std::endl;

  auto* chain = StMaker::GetChain();  

  TFile* file = TFile::Open("mtd.response.root","recreate");
  TH1F* hOneMuon = new TH1F("hOneMuon","Single muon incident @ E=10 GeV; dE [eV]",100,0.,10.0);
  TH2F* hEtaPhi  = new TH2F("hEtaPhi","Phi vs Eta; #eta; #phi",90,-180.0,180.0,100,-0.5,0.5);

  auto sumHits = [=](const char* name, float sf=1.0  ) -> double {
    assert(sf>0);
    float sum = 0.;    
    auto* table = dynamic_cast<TTable*>( chain->GetDataSet( "g2t_mtd_hit" ) ) ;
    if (table) 
      for ( int i=0;i<table->GetNRows();i++ ){
	const g2t_mtd_hit_st*  hit = static_cast<const g2t_mtd_hit_st*>( table->At(i) );
	sum+=hit->de;	
      };   
    return 1.0*1000.0*1000.*1000.*sum;
    //     GeV  MeV   keV    eV
  };

  

  auto throw_particle   = [=]( std::string type, int n=1, const double E=0.500, double etaMn=-0.45, double etaMx=0.45 ) {
    auto* _kine = dynamic_cast<StarKinematics*>( chain->GetMaker("StarKine") );                                                                                                                                                                                              
    for ( int i=0;i<n;i++ ) {
    auto* _part = _kine->AddParticle(type.c_str());
    auto  _mass = _part->GetMass(); // Get the mass of the proton
    if ( E > _mass ) {
      auto  _pmom = TMath::Sqrt(E*E-_mass*_mass);
      auto  _eta  = gRandom->Rndm() * ( etaMx - etaMn ) + etaMn; // random angle
      auto  _phi  = gRandom->Rndm() * TMath::TwoPi();
      TVector3 unit(0,0,1);
      unit.SetPtEtaPhi(1.0,_eta,_phi);
      unit=unit.Unit();
      auto momentum=_pmom*unit;
      momentum.Print();
      _part->SetPx( momentum[0] );
      _part->SetPy( momentum[1] );
      _part->SetPz( momentum[2] );
      _part->SetVx( 0 );
      _part->SetVy( 0 );
      _part->SetVz( 0 );
    }
    else {
      LOG_INFO << "ERROR: mass > E (no particle simulated)" << endm;
    }
    }
    chain->Clear();
    chain->Make();
  };
  
  for ( int ievent=0;ievent<nevents;ievent++ ) {
    throw_particle( "mu+", 1 );
    hOneMuon->Fill( sumHits( "g2t_mtd_hit" ) );
    auto* table = dynamic_cast<TTable*>( chain->GetDataSet( "g2t_mtd_hit" ) ) ;
    if (table) 
      for ( int i=0;i<table->GetNRows();i++ ){
	const g2t_mtd_hit_st*  hit = static_cast<const g2t_mtd_hit_st*>( table->At(i) );
	//	double de = hit->de * 1.0*1000.0*1000.*1000.0;
	TVector3 pos( hit->x );
	double eta = pos.Eta();
	double phi = pos.Phi() * 180.0/TMath::Pi();
	if ( phi > 180.0 ) phi -= 360.0;
	hEtaPhi->Fill(eta,phi);
      };       
  }

  file->Write();
  delete file;

}
