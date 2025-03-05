//usr/bin/env root4star -l -b -q $0'('$1', '$2')'; exit $?
// macro to instantiate the Geant3 from within
// STAR  C++  framework and get the starsim prompt
// To use it do
//  root4star starsim.C

class St_geant_Maker;
St_geant_Maker *geant_maker = 0;

class StarGenEvent;
StarGenEvent   *event       = 0;

class StarPrimaryMaker;
StarPrimaryMaker *_primary = 0;

class StarKinematics;
StarKinematics *kinematics = 0;


TH1F* hMll = 0;
bool decayJPsiToElectrons = false;
float numParticles = 1;

// ----------------------------------------------------------------------------
void geometry( TString tag, Bool_t agml=true )
{
  TString cmd = "DETP GEOM "; cmd += tag + " field=-5.0";
  if ( !geant_maker ) geant_maker = (St_geant_Maker *)chain->GetMaker("geant");
  geant_maker -> LoadGeometry(cmd);
  //  if ( agml ) command("gexec $STAR_LIB/libxgeometry.so");
}
// ----------------------------------------------------------------------------
void command( TString cmd )
{
  if ( !geant_maker ) geant_maker = (St_geant_Maker *)chain->GetMaker("geant");
  geant_maker -> Do( cmd );
}
// ----------------------------------------------------------------------------
void trig( Int_t n=1 )
{

  
  for ( Int_t i=0; i<n; i++ ) {

    // Clear the chain from the previous event
    chain->Clear();

    //(Momentum, Energy units are Gev/C, GeV)
    Double_t masses[2] = { 0.00051099895000, 0.00051099895000} ;
  
    if (!decayJPsiToElectrons){
      masses[0] = 0.1056583755;
      masses[1] = 0.1056583755;
    }

    TGenPhaseSpace genEvent;
    TLorentzVector W;
    // W.SetPtEtaPhiM( 0.0, 100.0, 0, 3.096 );
    W.SetXYZM( 0, 0, 30, 3.096 );
    genEvent.SetDecay(W, 2, masses);

    TLorentzVector lv;
    for ( int j = 0; j < numParticles; j++ ){
      Double_t weight = genEvent.Generate();
      TLorentzVector *pElectron = genEvent.GetDecay(0);
      TLorentzVector *pPositron = genEvent.GetDecay(1);
      lv = *pElectron + *pPositron;

      StarGenParticle *ele;
      if ( decayJPsiToElectrons )
        ele = kinematics->AddParticle( "e-" );
      else
        ele = kinematics->AddParticle( "mu-" );
      ele->SetPx(pElectron->Px());
      ele->SetPy(pElectron->Py());
      ele->SetPz(pElectron->Pz());
      ele->SetMass( masses[0] );

      StarGenParticle *pos;
      if ( decayJPsiToElectrons )
        pos = kinematics->AddParticle( "e+" );
      else 
        pos = kinematics->AddParticle( "mu+" );
      pos->SetPx(pPositron->Px());
      pos->SetPy(pPositron->Py());
      pos->SetPz(pPositron->Pz());
      pos->SetMass( masses[0] );

      hMll->Fill( lv.M() );

      cout << "ele eta = " << pElectron->Eta() << endl;
      cout << "pos eta = " << pPositron->Eta() << endl;
    }

    
		// kinematics->Kine( numParticles, nameParticle.Data(), 10.2, 12.0, 2.5, 4.00  );

    // Generate the event
    chain->Make();

    // TTable* hits = chain->GetDataSet("bfc/.make/geant/.data/g2t_stg_hit");
    // if ( hits ) {
    //   double nhits = hits->GetNRows();
    //   hNumHits->Fill( double(i), nhits / 4.0 / numParticles );
    //   std::cout << "N hits  = " << nhits << std::endl;
    // }

    // Print the event
    // command("gprint hits stgh");

  }
}
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
void Kinematics()
{
  
  //  gSystem->Load( "libStarGeneratorPoolPythia6_4_23.so" );
  gSystem->Load( "libKinematics.so");
  kinematics = new StarKinematics();
    
  _primary->AddGenerator(kinematics);
}
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
void jpsi( Int_t nevents=10000, Int_t rngSeed=12352342, bool decayToElectrons = true )
{ 

  hMll = new TH1F("hMll",";Mll;counts [10MeV]", 200, 2.0, 4.0 );
  decayJPsiToElectrons = decayToElectrons;
  cout << "Generating: " << nevents << " events with seed: " << rngSeed << endl;
  if ( decayToElectrons ){
    cout << "Simulating J/psi->e+e-" << endl;
  }  else {
    cout << "Simulating J/psi->mu+mu-" << endl;
  }
  gSystem->Load( "libStarRoot.so" );
  gROOT->SetMacroPath(".:/star-sw/StRoot/macros/:./StRoot/macros:./StRoot/macros/graphics:./StRoot/macros/analysis:./StRoot/macros/test:./StRoot/macros/examples:./StRoot/macros/html:./StRoot/macros/qa:./StRoot/macros/calib:./StRoot/macros/mudst:/afs/rhic.bnl.gov/star/packages/DEV/StRoot/macros:/afs/rhic.bnl.gov/star/packages/DEV/StRoot/macros/graphics:/afs/rhic.bnl.gov/star/packages/DEV/StRoot/macros/analysis:/afs/rhic.bnl.gov/star/packages/DEV/StRoot/macros/test:/afs/rhic.bnl.gov/star/packages/DEV/StRoot/macros/examples:/afs/rhic.bnl.gov/star/packages/DEV/StRoot/macros/html:/afs/rhic.bnl.gov/star/packages/DEV/StRoot/macros/qa:/afs/rhic.bnl.gov/star/packages/DEV/StRoot/macros/calib:/afs/rhic.bnl.gov/star/packages/DEV/StRoot/macros/mudst:/afs/rhic.bnl.gov/star/ROOT/36/5.34.38/.sl73_x8664_gcc485/rootdeb/macros:/afs/rhic.bnl.gov/star/ROOT/36/5.34.38/.sl73_x8664_gcc485/rootdeb/tutorials");

  gROOT->ProcessLine(".L bfc.C");
  {
    TString simple = "sdt20211016 y2024 geant gstar usexgeom agml ";
    bfc(0, simple );
  }

  gSystem->Load( "libVMC.so");

  gSystem->Load( "StarGeneratorUtil.so" );
  gSystem->Load( "StarGeneratorEvent.so" );
  gSystem->Load( "StarGeneratorBase.so" );

  gSystem->Load( "libMathMore.so"   );  
  gSystem->Load( "xgeometry.so"     );

  

  // Setup RNG seed and map all ROOT TRandom here
  StarRandom::seed( rngSeed );
  StarRandom::capture();

  //
  // Create the primary event generator and insert it
  // before the geant maker
  //
  //  StarPrimaryMaker *
  _primary = new StarPrimaryMaker();
  {
    _primary -> SetFileName( "jpsi.root");
    chain -> AddBefore( "geant", _primary );
  }

  Kinematics();

  //
  // Initialize primary event generator and all sub makers
  //
  _primary -> Init();
  _primary->SetSigma( 0.1, 0.1, 0.1 ); // 1mm x 1mm x 1mm smearing at the vertex
  _primary->SetVertex(0.0, 0.0, 0.0 );

  //
  // Setup geometry and set starsim to use agusread for input
  //
  //geometry("y2012");
  command("gkine -4 0");
  command("gfile o jpsi.fzd");


  hNumHits = new TH1F("hNumEvents","Nhits/plane/incident track vs event number",nevents + 1, -0.5, (float)( nevents ) + 0.5 );
  // hNumHits->SetBit(TH1::kCanRebin);


  // command( "DCAY 0" );
  // command( "ANNI 0" );
  // command( "BREM 0" );
  // command( "COMP 0" );
  // command( "HADR 0" );
  // command( "MUNU 0" );
  // command( "PAIR 0" );
  // command( "PFIS 0" );
  // command( "PHOT 0" );
  // command( "RAYL 0" );
  // command( "LOSS 4" );
  // command( "DRAY 0" );
  // command( "MULS 0" );
  // command( "STRA 0" );
  // command( "physi"  );
 
  //
  // Trigger on nevents
  //
  trig( nevents );

  TFile * f = new TFile( "jpsi_gen.root", "RECREATE" );
  f->cd();
  hMll->Write();
  f->Write();

  command("call agexit");  // Make sure that STARSIM exits properly

}
// ----------------------------------------------------------------------------

