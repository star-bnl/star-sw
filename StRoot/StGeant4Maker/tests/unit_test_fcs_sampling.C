#include "tests/unit_tests.h"
//#include "StFcsDbMaker/StFcsDbMaker.h"

#include <vector>
#include <string>
#include <TRandom.h>

//___________________________________________________________________
double _eta  = 0; 
double _phid = 0;

void unit_test_fcs_sampling( const int nevents=1000 ) {

  gROOT->ProcessLine("initChain();");

  auto* pm = dynamic_cast<StarPrimaryMaker*>( StMaker::GetChain()->GetMaker("PrimaryMaker") );
  pm->SetVertex(0.0,0.0,0.0);
  pm->SetSigma (0.0,0.0,0.0);

  LOG_TEST << "========================================================" << std::endl;
  LOG_TEST << "Test 3.85 GeV protons, neutrons" << std::endl;
  LOG_TEST << "Test 7.7 GeV deuteron" << std::endl;
  LOG_TEST << "Test 15.4 GeV alpha" << std::endl;
  LOG_TEST << "========================================================" << std::endl;

  TFile* file = TFile::Open("output.root","recreate");
  int    nbin=80;
  double mn=0.;
  double mx=20.0;

  auto* chain = StMaker::GetChain();  

  auto sumHits = [=](const char* name, double sf=1.0  ) -> double {
    assert(sf>0);
    double sum = 0.;    
    auto* table = dynamic_cast<TTable*>( chain->GetDataSet( name ) ) ;
    if (table) 
      for ( int i=0;i<table->GetNRows();i++ ){
	const g2t_emc_hit_st*  hit = static_cast<const g2t_emc_hit_st*>( table->At(i) );
	sum+=hit->de / sf;
      };
    return sum;
  };

  auto throw_particle   = [=]( std::string type, const double E=3.85, double etaMn=2.75, double etaMx=3.75 ) {
    auto* _kine = dynamic_cast<StarKinematics*>( chain->GetMaker("StarKine") );                                                                                                                                                                                                       
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
      chain->Clear();
      chain->Make();
    }
    else {
      LOG_INFO << "ERROR: mass > E (no particle simulated)" << endm;
    }
  };

  struct Job {
    std::string particle;
    double energy;
    TH1F* hWCAL;
    TH1F* hHCAL;
    TH1F* hESUM;    
  };

  TH1F* hWCAL[5];
  TH1F* hHCAL[5];
  TH1F* hESUM[5];

  std::vector<Job> jobs = {   
    { "electron",3.85, 0, 0, 0 },
    { "proton",  3.85, 0, 0, 0 },
    { "neutron", 3.85, 0, 0, 0 },
    { "D",       7.70, 0, 0, 0 },
    { "alpha",   15.4, 0, 0, 0 }
  };

  int count = 0;
  for ( auto job : jobs ) {
    std::string name;
    std::string title;

    name  = Form("hWCAL%s",job.particle.c_str());
    title = Form("WCAL response to %5f GeV %s; E [GeV]",job.energy,job.particle.c_str());
    hWCAL[count] = new TH1F(name.c_str(),title.c_str(),500,0.,25.);

    name  = Form("hHCAL%s",job.particle.c_str());
    title = Form("HCAL response to %5f GeV %s; E [GeV]",job.energy,job.particle.c_str());
    hHCAL[count] = new TH1F(name.c_str(),title.c_str(),500,0.,25.);

    name  = Form("hESUM%s",job.particle.c_str());
    title = Form("FCS summed response to %5f GeV %s; E [GeV]",job.energy,job.particle.c_str());
    hESUM[count] = new TH1F(name.c_str(),title.c_str(),500,0.,25.);

    count++;
  }


  for ( int event=0;event<nevents;event++ ) {

    gMessMgr->Info() << "//==========================================================================" << endm;
    gMessMgr->Info() << "Throwing event number " << event << " of " << nevents << endm;
    
    count = 0;
    for ( auto& job : jobs ) {
      throw_particle( job.particle, job.energy );
      double wcal = sumHits( "g2t_wca_hit", 0.2 );
      double hcal = sumHits( "g2t_hca_hit", 0.0145 );
      double sum  = wcal + hcal;      
      LOG_INFO << "Particle = " << job.particle << endm;
      LOG_INFO << "E (wcal) = " << wcal << endm;
      LOG_INFO << "E (hcal) = " << hcal << endm;
      LOG_INFO << "E (totl) = " << sum  << endm;
      hWCAL[count]->Fill(wcal);
      hHCAL[count]->Fill(hcal);
      hESUM[count]->Fill(sum);
      count++;
    }

  }

  file->Write();
  delete file;

}

