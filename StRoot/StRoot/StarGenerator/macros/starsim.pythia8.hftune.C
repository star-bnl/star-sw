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

//
// Example macro for loading a "tune" deck.  It is based on work done by
// Thomas Ullrich http://www.star.bnl.gov/protected/heavy/ullrich/pythia8/
//
//
//



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
// trig()  -- generates one event
// trig(n) -- generates n+1 events.
//
// NOTE:  last event generated will be corrupt in the FZD file
//
void trig( Int_t n=1 )
{
  chain->EventLoop(n);
  _primary->event()->Print();
  
}
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
void Pythia8( TString config="pp:W" )
{

  //
  // Create the pythia 8 event generator and add it to 
  // the primary generator
  //
  StarPythia8 *pythia8 = new StarPythia8();    
  if ( config=="pp:W" ) {
      pythia8->SetFrame("CMS", 510.0);
      pythia8->SetBlue("proton");
      pythia8->SetYell("proton");
      
      pythia8->Set("WeakSingleBoson:all=off");
      pythia8->Set("WeakSingleBoson:ffbar2W=on");
      pythia8->Set("24:onMode=0");              // switch off all W+/- decaus
      pythia8->Set("24:onIfAny 11 -11");        // switch on for decays to e+/-
      
    }
  if ( config=="pp:510:minbias" )  {
      pythia8->SetFrame("CMS", 510.0);
      pythia8->SetBlue("proton");
      pythia8->SetYell("proton");            
      pythia8->Set("SoftQCD:minBias = on");
    }

  if ( config=="pp:200:charmonium" ) {
      pythia8->SetFrame("CMS", 200.0);
      pythia8->SetBlue("proton");
      pythia8->SetYell("proton");            
      pythia8->ReadFile( "star_hf_tune_v1.1-1.cmnd" );
      pythia8->Set( "HardQCD:all = off" );
      pythia8->Set( "Charmonium:all=on" );
  }
  if ( config=="pp:200:bottomonium" ) {
      pythia8->SetFrame("CMS", 200.0);
      pythia8->SetBlue("proton");
      pythia8->SetYell("proton");            
      pythia8->ReadFile( "star_hf_tune_v1.1-1.cmnd" );
      pythia8->Set( "HardQCD:all = off" );
      pythia8->Set( "Bottomonium:all=on" );
  }
  if ( config=="pp:200:drellyan" ) {
      pythia8->SetFrame("CMS", 200.0);
      pythia8->SetBlue("proton");
      pythia8->SetYell("proton");            
      pythia8->ReadFile( "star_hf_tune_v1.1-1.cmnd" );
      pythia8->Set( "HardQCD:all = off" );
      pythia8->Set( "WeakSingleBoson:ffbar2gmZ=on" );
  }
  if ( config=="pp:200:minbias" ) {
      pythia8->SetFrame("CMS", 200.0);
      pythia8->SetBlue("proton");
      pythia8->SetYell("proton");            
      pythia8->ReadFile( "star_hf_tune_v1.1-1.cmnd" );
      pythia8->Set( "HardQCD:all = on" );
      pythia8->Set( "SoftQCD:minbias = on" );
  }

  // The TUNE deck may request too many events.  We only need one event
  // per run for simulation.
  pythia8->Set("Main:numberOfEvents = 1 ");

  _primary -> AddGenerator( pythia8 );
  
}
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
void starsim( Int_t nevents=10000, Int_t rngSeed=1234 )
{ 

  gROOT->ProcessLine(".L bfc.C");
  {
    TString simple = "y2012 geant gstar usexgeom agml -detdb -db";
    bfc(0, simple );
  }

  gSystem->Load( "libVMC.so");

  gSystem->Load( "StarGeneratorUtil.so");
  gSystem->Load( "StarGeneratorEvent.so");
  gSystem->Load( "StarGeneratorBase.so" );


  if ( LHAPDF_DATA_PATH.Contains("afs") ) {
    cout << endl << endl;
    cout << "WARNING: LHAPDF_DATA_PATH points to an afs volume" << endl << endl;
    cout << "         You are advised to copy the PDF files you need into a local" << endl;
    cout << "         directory and set the LHAPDF_DATA_PATH to point to it."      << endl;
    cout << endl << endl;
  }

  gSystem->Setenv("LHAPDF_DATA_PATH", LHAPDF_DATA_PATH.Data() );
  gSystem->Load( "/opt/star/$STAR_HOST_SYS/lib/libLHAPDF.so");

  gSystem->Load( "Pythia8_1_62.so");

  gSystem->Load( "libMathMore.so"   );  

  // Force loading of xgeometry
  gSystem->Load( "xgeometry.so"     );

//   // And unloading of geometry
//   TString geo = gSystem->DynamicPathName("geometry.so");
//   if ( !geo.Contains("Error" ) ) {
//     std::cout << "Unloading geometry.so" << endl;
//     gSystem->Unload( gSystem->DynamicPathName("geometry.so") );
//   }

  gSystem->Load( "Pythia8_1_62.so"  );

  // Setup RNG seed and map all ROOT TRandom here
  StarRandom::seed( rngSeed );
  StarRandom::capture();
  
  //
  // Create the primary event generator and insert it
  // before the geant maker
  //
  //  StarPrimaryMaker *
  _primary = new Star_PrimaryMaker();
  {
    _primary -> SetFileName( "pythia8.starsim.root");
    _primary -> SetVertex( 0.1, -0.1, 0.0 );
    _primary -> SetSigma ( 0.1,  0.1, 30.0 );
    chain -> AddBefore( "geant", _primary );
  }

  //
  // Setup an event generator
  //
  Pythia8("pp:200:bottomonium");

  //
  // Setup cuts on which particles get passed to geant for
  //   simulation.  (To run generator in standalone mode,
  //   set ptmin=1.0E9.)
  //                    ptmin  ptmax
  _primary->SetPtRange  (1.0E9,  -1.0);         // GeV
  //  _primary->SetPtRange  (0.0,    -1.0);         // GeV
  //                    etamin etamax
  _primary->SetEtaRange ( -3.0, +3.0 );
  //                    phimin phimax
  _primary->SetPhiRange ( 0., TMath::TwoPi() );
  
  
  // 
  // Setup a realistic z-vertex distribution:
  //   x = 0 gauss width = 1mm
  //   y = 0 gauss width = 1mm
  //   z = 0 gauss width = 30cm
  // 
  _primary->SetVertex( 0., 0., 0. );
  _primary->SetSigma( 0.1, 0.1, 30.0 );

  
  //
  // Initialize primary event generator and all sub makers
  //
  _primary -> Init();

  //
  // Setup geometry and set starsim to use agusread for input
  //
  //geometry("y2012");
  //* AGUSER/GKINE NTRACK ID [ PTLOW PTHIGH YLOW YHIGH PHILOW PHIHIGH ZLOW ZHIGH option ]

  command("gkine -4 0");
  command("gfile o pythia8.starsim.fzd");
  

  //
  // Trigger on nevents
  //
  trig( nevents );

  //
  // Finish the chain
  //
  chain->Finish();

  //
  // EXIT starsim
  //
  command("call agexit");  // Make sure that STARSIM exits properly

}
// ----------------------------------------------------------------------------

