// macro to instantiate the Geant3 from within
// STAR  C++  framework and get the starsim prompt
// To use it do
//  root4star starsim_pythia6..C

class St_geant_Maker;
St_geant_Maker *geant_maker = 0;

class StarGenEvent;
StarGenEvent   *event       = 0;

class StarPrimaryMaker;
StarPrimaryMaker *_primary = 0;


//
// This is for testing only and should NOT be used in a production (i.e. batch 
// system) run.  It will spam the heck out of our AFS system, and bring the
// wrath of Jerome down upon you.  We will install data files in appropriate
// path, and you should update when available.
//
TString LHAPDF_DATA_PATH="/afs/cern.ch/sw/lcg/external/lhapdfsets/current/";

// ----------------------------------------------------------------------------
void geometry( TString tag, Bool_t agml=true )
{
  TString cmd = "DETP GEOM "; cmd += tag;
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
    chain->Clear();
    chain->Make();
  }
}
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
void Pythia6( TString mode="pp:W", Int_t tune=370 )
{
  
  //  gSystem->Load( "libStarGeneratorPoolPythia6_4_23.so" );
  if ( LHAPDF_DATA_PATH.Contains("afs") ) {
     cout << "WARNING: LHAPDF_DATA_PATH points to an afs volume" << endl << endl;
     cout << "         You are advised to copy the PDF files you need into a local" << endl;
     cout << "         directory and set the LHAPDF_DATA_PATH to point to it."      << endl;
  }

  gSystem->Setenv("LHAPDF_DATA_PATH", LHAPDF_DATA_PATH.Data() );

  gSystem->Load( "/opt/star/$STAR_HOST_SYS/lib/libLHAPDF.so");
  gSystem->Load( "libPythia6_4_28.so");

  //  gSystem->Load( "StarPythia6.so"   );

  StarPythia6 *pythia6 = new StarPythia6("pythia6");
  if ( mode == "pp:minbias" )
  {
    //pythia6->SetFrame("CMS", 510.0 );
    pythia6->SetFrame("CMS", 200.0 );
    pythia6->SetBlue("proton");
    pythia6->SetYell("proton");
    if ( tune ) pythia6->PyTune( tune );
    //PyPars_t &pypars = pythia6->pypars();
    //int& mstp5 = pypars.mstp(5); mstp5 = 370;
    //float& parp90 = pypars.parp(90); parp90 = 0.213;
  }
  if ( mode == "ep" )
  {
    Double_t pblue[]={0.,0.,30.0};
    Double_t pyell[]={0.,0.,-320.0};
    pythia6->SetFrame("3MOM", pblue, pyell );
    pythia6->SetBlue("e-");
    pythia6->SetYell("proton");
    if ( tune ) pythia6->PyTune( tune );
  }
    
  _primary->AddGenerator(pythia6);
}
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
void starsim( Int_t nevents=1, Int_t rngSeed=0 , Int_t eventnameid=0)
{ 

  if(rngSeed==0) 
  {
    Int_t currenttime=time(0);
    cout<<"time : "<<currenttime<<endl;
    rngSeed=abs((eventnameid +currenttime)*21474836) % (int)(pow(2,20));		// ly: just a simple random seed
  }
  cout<<"Random seed : "<<rngSeed<<endl;

  gROOT->ProcessLine(".L bfc.C");
  {
    TString simple = "y2012 geant gstar agml usexgeom ";
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
    _primary -> SetFileName( Form("pythia6.starsim_%d.root",eventnameid));
    chain -> AddBefore( "geant", _primary );
  }

  //
  // Setup an event generator
  //
  Pythia6( "pp:minbias" ,370);

  //
  // Setup cuts on which particles get passed to geant for
  //   simulation.  
  //
  // If ptmax < ptmin indicates an infinite ptmax.
  // ptmin will always be the low pT cutoff.
  //
  //                    ptmin  ptmax
  _primary->SetPtRange  (0.0,  -1.0);         // GeV
  //
  // If etamax < etamin, there is no cut in eta.
  // otherwise, particles outside of the specified range are cut.
  //
  //                    etamin etamax
  //  _primary->SetEtaRange ( -3.0, +3.0 );
  //
  //  phirange will be mapped into 0 to 2 pi internally.
  //
  //                    phimin phimax
  _primary->SetPhiRange ( 0., TMath::TwoPi() );
  
  
  // 
  // Setup a realistic z-vertex distribution:
  //   x = 0 gauss width = 1mm
  //   y = 0 gauss width = 1mm
  //   z = 0 gauss width = 30cm
  // 
  _primary->SetVertex( 0., 0., 0. );
  //_primary->SetSigma( 0.1, 0.1, 30.0 );
  _primary->SetSigma( 0., 0., 0. );

  //
  // Initialize _primary event generator and all sub makers
  //
  _primary -> Init();

  //
  // Setup geometry and set starsim to use agusread for input
  //
  geometry("y2012");
  command("gkine -4 0");
  command("gfile o pythia6.starsim.fzd");
  

  //
  // Trigger on nevents
  //
  trig( nevents );

  command("call agexit");  // Make sure that STARSIM exits properly
}
// ----------------------------------------------------------------------------

