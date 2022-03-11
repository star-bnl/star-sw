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

class StarFilterMaker;
StarFilterMaker *filter = 0;

#define USE_PYTHIA8_DECAYER

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
double _vx      = 0;
double _vy      = 0;
double _vz      = 0;
int    _runId   = 0;
int    _eventId = 0;
void trig( int runId, int eventId, double vz )
{

  _primary -> SetVertex( 0.1, -0.1, vz );
  _primary -> SetSigma ( 0.1E-12,  0.1E-12, 0.1E-12 );

  chain->Clear();
  chain->Make();

  //  command("message gprint kine:");
  //  command("gprint kine");


      
}
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
void Pythia8( TString config="pp:W", Double_t ckin3=0.0, Double_t ckin4=-1.0 )
{

  //
  // Create the pythia 8 event generator and add it to 
  // the primary generator
  //
  StarPythia8 *pythia8 = new StarPythia8();    
  if ( config=="pp:W" )
    {-
      pythia8->SetFrame("CMS", 510.0);
      pythia8->SetBlue("proton");
      pythia8->SetYell("proton");
      
      pythia8->Set("WeakSingleBoson:all=off");
      pythia8->Set("WeakSingleBoson:ffbar2W=on");
      pythia8->Set("24:onMode=0");              // switch off all W+/- decaus
      pythia8->Set("24:onIfAny 11 -11");        // switch on for decays to e+/-
      
    }
  if ( config=="pp:minbias" )
    {
      pythia8->SetFrame("CMS", 500.0);
      pythia8->SetBlue("proton");
      pythia8->SetYell("proton");    

      pythia8->Set("HardQCD:all = on");

    }
  if ( config=="pp:heavyflavor:D0jets" ) 
    {
      pythia8->Set("HardQCD:gg2ccbar = on");
      pythia8->Set("HardQCD:qqbar2ccbar = on");
      pythia8->Set("Charmonium:all = on");

      pythia8->Set("421:mayDecay = 0");

      pythia8->SetFrame("CMS", 200.0);
      pythia8->SetBlue("proton");
      pythia8->SetYell("proton");

    }

  // Setup phase space cuts
  pythia8 -> Set(Form("PhaseSpace:ptHatMin=%f", ckin3 ));
  pythia8 -> Set(Form("PhaseSpace:ptHatMax=%f", ckin4 ));

  _primary -> AddGenerator( pythia8 );

  
}
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
TString runfile = "";

void getNextRun( int& event, int& run, double& z ) {

  static ifstream myfile;
  double x, y;

  // Initialize on first call
  if ( event == 0 ) {
    myfile.open(runfile.Data());
  }

  // Handle end of file
  if ( myfile.eof() ) {
    event = 0;
    run   = 0;
    z     = -999.0;
    return;
  }

  // 15165057 2586838 -0.23291 -0.151908 -2.82513
  myfile >> run >> event >> x >> y >> z;

}

// ----------------------------------------------------------------------------

void starsim( Int_t nevents=10000, Int_t runnumber=15117062, TString runfile_="15117062.dat", int sequence=0, int dummy = 0 )
{ 

  // nevents -- number of events to process
  // runnumber -- the run number this simulation is to be compared to
  // runfile -- a file containing one event per line, specifying the run #, event #, reconstructed vx, vy, vz
  // sequence -- can be used (with stride > 1) to specify independent RNG seeds for the same input file
  // dummy -- is ignored

  const int stride = 1;

  int     rngSeed = runnumber * stride + sequence;
  //          runfile = Form("/gpfs01/star/pwg/droy1/EMBEDDING_2021_D0Analysis/VERTEX/VertexFiles/%i.txt",runnumber);
  //  runfile = Form("INPUTFILES/%i.txt",runnumber);
  runfile = runfile_;

  


  TString basename = Form("rcf22000_%s_%i_%ievts",runfile.Data(),sequence,nevents);

  gROOT->ProcessLine(".L bfc.C");
  {
    TString simple = "y2014x geant gstar usexgeom agml sdt20140530 DbV20150316 misalign ";
    bfc(0, simple );
  }

  gSystem->Load( "libVMC.so");

  gSystem->Load( "StarGeneratorUtil.so");
  gSystem->Load( "StarGeneratorEvent.so");
  gSystem->Load( "StarGeneratorBase.so" );
#ifdef USE_PYTHIA8_DECAYER
  gSystem->Load( "StarGeneratorDecay.so" );
#endif
  gSystem->Load( "Pythia8_3_03.so"  );

  gSystem->Load( "libMathMore.so"   );  

  // Force loading of xgeometry
  gSystem->Load( "xgeometry.so"     );

  gSystem->Load("$OPTSTAR/lib/libfastjet.so");
  gSystem->Load( "StarGeneratorFilt.so" );
  gSystem->Load( "FastJetFilter.so" );

//   // And unloading of geometry
//   TString geo = gSystem->DynamicPathName("geometry.so");
//   if ( !geo.Contains("Error" ) ) {
//     std::cout << "Unloading geometry.so" << endl;
//     gSystem->Unload( gSystem->DynamicPathName("geometry.so") );
//   }


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
    _primary -> SetFileName( "pythia8.starsim.root");
    _primary->SetAttr("beamline", 1);
    chain -> AddBefore( "geant", _primary );
  }

  //
  // Setup an event generator
  //
  double ckin3=3.0;
  double ckin4=-1.0;

  Pythia8("pp:heavyflavor:D0jets", ckin3, ckin4 );

#ifdef USE_PYTHIA8_DECAYER
  //
  // Setup decay manager
  //
  StarDecayManager   *decayMgr = AgUDecay::Manager();
  StarPythia8Decayer *decayPy8 = new StarPythia8Decayer();
  decayMgr->AddDecayer(    0, decayPy8 ); // Handle any decay requested 
  decayPy8->SetDebug(1);
  //  decayPy8->Set("WeakSingleBoson:all = on");
  decayPy8->Set("421:onMode = 0");
  //  decayPy8->Set("421:onIfAll = 321 -211");
  decayPy8->Set("421:onIfMatch = 321 -211");

  // 

  TString name;
  double mass, lifetime, charge;
  int tracktype, pdgcode, g3code;

  // Particle data
  StarParticleData& data = StarParticleData::instance();
  //  One can add a particle to G3 using...
  //data.AddParticleToG3( "MyD0", 0.1865E+01, 0.42800E-12, 0., 3, 421, 37, 0, 0 );
  TParticlePDG* D0     = data.GetParticle("D0");
  TParticlePDG* rho_pl = data.GetParticle("rho+");
  TParticlePDG* rho_mn = data.GetParticle("rho-");
  data.AddParticleToG3( D0, 37 );
  data.AddParticleToG3( rho_pl, 153 );
  data.AddParticleToG3( rho_mn, 154 );
#else
  command("call gstar_part"); 
#endif


#if 1
  //
  // Setup the generator filter
  //
  //  filter = new StDijetFilter();

  filter = new FastJetFilter();
  _primary -> AddFilter( filter );

  // If set to 1, tracks will be saved in the tree on events which were
  // rejected.  If the tree size is too big (because the filter is too
  // powerful) you may want to set this equal to zero.  In which case
  // only header information is saved for the event.
  _primary->SetAttr("FilterKeepAll",     int(0));

  // By default, the primary maker enters an infinite loop and executes
  // the event generator until it yields an event which passes the filter.
  // The big full chain treats this as a single event.
  //
  // If you want the BFC to see an empty event, set the FilterSkipRejects
  // attribute on the primary maker and give it the priveledge it needs
  // to kill the event. 
  //---  primary->SetAttr("FilterSkipRejects", int(1) ); // enables event skipping 
  //---  chain->SetAttr(".Privilege",1,"StarPrimaryMaker::*" );
#endif

  //
  // Setup cuts on which particles get passed to geant for
  //   simulation.  (To run generator in standalone mode,
  //   set ptmin=1.0E9.)
  //                    ptmin  ptmax
  _primary->SetPtRange  (0.0,  -1.0);         // GeV
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
  //  _primary->SetVertex( 0., 0., 0. );
  //_primary->SetSigma( 0.1, 0.1, 30.0 );

  
  //
  // Initialize primary event generator and all sub makers
  //
  _primary -> Init();

  command("gkine -4 0");
  command( Form("gfile o %s.fzd",basename.Data()) );

  int count = 0;
  while (true) {

    int event;
    double z;
    for ( int _s=0;_s<stride;_s++) {
      getNextRun( event, runnumber, z ); 
      if ( 0==runnumber ) {
	std::cout << "End of run file encountered" << std::endl;
	goto DONE;
      }
    }

    // Set run and event number in starsim
    command( Form( "rung %i %i", runnumber, event ) );

    trig( event, runnumber, z );

    if ( count++ > nevents ) {
      std::cout << "Last event requested" << std::endl;
      break;
    }

  }

 DONE:
  std::cout << "Finishing up" << std::endl;



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

