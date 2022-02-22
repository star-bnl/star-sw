#include "tests/unit_tests.h"
#include <TRandom.h>

//___________________________________________________________________
double _eta  = 0; 
double _phid = 0;
//___________________________________________________________________
void throw_muon_in_epd_tile( double eta, double phid, int charge = 1 ) {

  throw_muon( eta, phid, 10.0, charge ); // energetic
  _eta = eta;
  _phid = phid;

  auto* chain = StMaker::GetChain();
  vertex_table = dynamic_cast<TTable*>( chain->GetDataSet("g2t_vertex")  );
  track_table  = dynamic_cast<TTable*>( chain->GetDataSet("g2t_track")   );
  hit_table    = dynamic_cast<TTable*>( chain->GetDataSet("g2t_epd_hit") );

}

//___________________________________________________________________
void unit_test_epd_response( int nevents=5000 ) {

  gROOT->ProcessLine("initChain();");

  auto* pm = dynamic_cast<StarPrimaryMaker*>( StMaker::GetChain()->GetMaker("PrimaryMaker") );
  pm->SetVertex(0.,0.,0.);
  pm->SetSigma(0.0,0.,0.);

  LOG_TEST << "=======================================================" << std::endl;
  LOG_TEST << "Unit testing of tracks and EPD hits on single muons"     << std::endl;
  LOG_TEST << "=======================================================" << std::endl;

  auto* chain = StMaker::GetChain();  

  auto sumHits = [=](const char* name, float sf=1.0  ) -> double {
    assert(sf>0);
    float sum = 0.;    
    auto* table = dynamic_cast<TTable*>( chain->GetDataSet( name ) ) ;
    if (0!=table) 
      for ( int i=0;i<table->GetNRows();i++ ){
	const g2t_epd_hit_st*  hit = static_cast<const g2t_epd_hit_st*>( table->At(i) );
	if ( hit ) sum+=hit->de;	
      };   
    return 1000.0*sum;
  };

  auto throw_particle   = [=]( std::string type, int n=1, const double E=0.500, double etaMn=2.15, double etaMx=5.08 ) {
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

  TFile* file = TFile::Open("epd.response.root","recreate");
  TH1F* hOneMip = new TH1F("hOneMip","Single muon incident @ 500 MeV; dE [MeV]",100,0.,10.0);  hOneMip->SetLineColor(6);
  TH1F* hTwoMip = new TH1F("hTwoMip","Two muons incident @ 500 MeV; dE [MeV]",100,0.,10.0);    hTwoMip->SetLineColor(3);
  TH1F* hRedMip = new TH1F("hRedMip","Three muons incident @ 500 MeV; dE [MeV]",100,0.,10.0);  hRedMip->SetLineColor(2);
  TH1F* hBluMip = new TH1F("hBluMip","Four muons incident @ 500 MeV; dE [MeV]",100,0.,10.0);   hBluMip->SetLineColor(4);
  TH1F* hSumMip = new TH1F("hSumMip","Four muons incident @ 500 MeV; dE [MeV]",100,0.,10.0);

  for ( int ievent=0;ievent<nevents;ievent++ ) {
    double sum = 0;
    throw_particle( "mu+", 1 );    
    hOneMip->Fill( sum = sumHits( "g2t_epd_hit") );
    hSumMip->Fill( sum );
    throw_particle( "mu+", 2 );
    hTwoMip->Fill( sum = sumHits( "g2t_epd_hit") );
    hSumMip->Fill( sum );
    throw_particle( "mu+", 3 );
    hRedMip->Fill( sum = sumHits( "g2t_epd_hit") );
    hSumMip->Fill( sum );
    throw_particle( "mu+", 4 );
    hBluMip->Fill( sum = sumHits( "g2t_epd_hit") );
    hSumMip->Fill( sum );
  }

  file->Write();
  delete file;

}
