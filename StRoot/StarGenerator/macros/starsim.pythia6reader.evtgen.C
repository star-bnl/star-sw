// macro to instantiate the Geant3 from within
// STAR  C++  framework and get the starsim prompt
// To use it do
//  root4star starsim.C
//
// By Y. Zhang  07/29/2014
// Modified from Jason 's macro
// Added real distributions for pT, y


class St_geant_Maker;
St_geant_Maker *geant_maker = 0;

class StarGenEvent;
StarGenEvent   *event       = 0;

class StarPrimaryMaker;
StarPrimaryMaker *_primary = 0;

//Initialize the settings:
Float_t vx     = 0.;
Float_t vy     = 0.;
Float_t vz     = 0.;
Float_t vx_sig = 0.01;
Float_t vy_sig = 0.01;
Float_t vz_sig = 2.0;
//Float_t minVz  = -5.0;
//Float_t maxVz  = +5.0;
Float_t minPt  = 0.0;
Float_t maxPt  = +20;
Float_t minY   = -1.0;
Float_t maxY   = +1.0;

//_____________________________________________________________________________
void geometry( TString tag, Bool_t agml=true )
{
  TString cmd = "DETP GEOM "; cmd += tag;
  if ( !geant_maker ) geant_maker = (St_geant_Maker *)chain->GetMaker("geant");
  geant_maker -> LoadGeometry(cmd);
  //  if ( agml ) command("gexec $STAR_LIB/libxgeometry.so");
} 
//_____________________________________________________________________________
void command( TString cmd )
{
  if ( !geant_maker ) geant_maker = (St_geant_Maker *)chain->GetMaker("geant");
  geant_maker -> Do( cmd );
}
//_____________________________________________________________________________
void trig( Int_t n=0 )
{
  for ( Int_t i=0; i<n+1; i++ ) {
    chain->Clear();
    chain->Make();
    // Print event generator
    cout << "_____________________________________________________________________" << endl;
    //    _primary -> event() -> Print();
    //    command("gprint kine");
    //    command("gprint vert");
    cout << "_____________________________________________________________________" << endl;


  }
}

void ExtendParticles()
{
  // The Dalitz particle is defined in starsim  in gstar_part.g --
  //
  //  Particle Dalitz    code=10007       TrkTyp=4 mass=0.135  charge=0 tlife=8.4e-17,                     |                                                                                        //                     pdg=100111 bratio= { 1.0,}  mode= {10203,}                                        |                                                                                       //
  // The particle database does not know about this particle, so we have to add it.  It is
  // important that we do not overwrite PDG id = 111, the ID of the standard pi0.  Otherwise,
  // we will have all pi0's in the event decaying by dalitz.

  StarParticleData &data = StarParticleData::instance();

  data.AddParticleToG3("Jpsi",        3.096,  7.48e-21, 0., 4,  443,  160, 0, 0 );
  data.AddParticleToG3("rho",         0.770,  4.35e-24, 0., 3,  113,  152, 0, 0 );
  data.AddParticleToG3("rho_plus",    0.767,  4.35e-24, 1., 4,  213,  153, 0, 0 );
  data.AddParticleToG3("rho_minus",   0.767,  4.35e-24,-1., 4, -213,  154, 0, 0 );
  data.AddParticleToG3("D_star_plus", 2.01027,6.86e-22, 1., 4,  413,   60, 0, 0 );
  data.AddParticleToG3("D_star_minus",2.01027,6.86e-22,-1., 4, -413,   61, 0, 0 );
  data.AddParticleToG3("D_star_0",    2.007,  3.13e-22, 0., 4,  423,   62, 0, 0 );
  data.AddParticleToG3("D_star_0_bar",2.007,  3.13e-22, 0., 4, -423,   63, 0, 0 );

  data.AddParticleToG3("B0",     5.2790, 1.536e-12,  0., 4,  511,   72, 0, 0 );
  data.AddParticleToG3("B0_bar", 5.2790, 1.536e-12,  0., 4, -511,   73, 0, 0 );
  data.AddParticleToG3("B_plus", 5.2790, 1.671e-12,  1., 4,  521,   70, 0, 0 );
  data.AddParticleToG3("B_minus",5.2790, 1.671e-12, -1., 4, -521,   71, 0, 0 );
  data.AddParticleToG3("D0",     1.86484,0.415e-12,  0., 3,  421,   37, 0, 0 );
  data.AddParticleToG3("D0_bar", 1.86484,1.536e-12,  0., 3, -421,   38, 0, 0 );
  data.AddParticleToG3("D_plus", 1.869,  1.057e-12,  1., 4,  411,   35, 0, 0 );
  data.AddParticleToG3("D_minus",1.869,  1.057e-12, -1., 4, -411,   36, 0, 0 );

}

//_____________________________________________________________________________

void Pythia6( TString filename )
{

  gSystem -> Load( "libStarGenEventReader.so" );
  StarGenEventReader* eventreader = new StarGenEventReader();
  eventreader->SetInputFile( filename, "genevents", "primaryEvent" );
  _primary->AddGenerator( eventreader );
  

}

//_____________________________________________________________________________
void reader( int nevents, int index, int rng )
{
  starsim( nevents, index, rng );
}
void starsim( Int_t nevents=10, Int_t Index = 0, Int_t rngSeed=4321 )
{ 

  gROOT->ProcessLine(".L bfc.C");
  {
    TString simple = "y2013_1c geant gstar usexgeom agml ";
    bfc(0, simple );
  }

  gSystem->Load( "libVMC.so");

  gSystem->Load( "StarGeneratorUtil.so" );
  gSystem->Load( "StarGeneratorEvent.so" );
  gSystem->Load( "StarGeneratorBase.so" );
  gSystem->Load( "StarGeneratorDecay.so" );
  gSystem->Load( "libMathMore.so"   );
  gSystem->Load( "libHijing1_383.so");
  gSystem->Load( "libKinematics.so");
  gSystem->Load( "xgeometry.so"     );

  gSystem->Load("libHepMC2_06_09.so");
  gSystem->Load("libPythia8_1_86.so");
  gSystem->Load("libPhotos3_61.so");
  gSystem->Load("libTauola1_1_5.so");
  gSystem->Load("libEvtGen1_06_00.so");


  // Setup RNG seed and map all ROOT TRandom here
  StarRandom::seed( rngSeed );
  StarRandom::capture();

  char rootname[100],fzname[100];
  sprintf(rootname,"st_pythiaevtgen_%d.starsim.root",Index);
  sprintf(fzname,"gfile o st_pythiaevtgen_%d.starsim.fzd",Index);

  //
  // Create the primary event generator and insert it
  // before the geant maker
  //
  _primary = new StarPrimaryMaker();
  {
    _primary -> SetFileName(rootname);
    chain -> AddBefore( "geant", _primary );
  }

  //
  // These should be adjusted to your best vertex estimates
  //
  _primary -> SetVertex( vx,vy,vz );
  _primary -> SetSigma( vx_sig,vy_sig,vz_sig );

  //
  // Setup an event generator
  //
  //  Pythia6("pp:W");
  Pythia6( "pythia6.starsim.root" );

  //
  // Setup decay manager
  //
  StarDecayManager   *decayMgr = AgUDecay::Manager();

  //
  // Output a decay table for taus which specifies Tauola as the decay model
  //
  {
    ofstream out("TAUS.DEC");
    const char* cmds[] = {
      "Decay tau-",      // 1
      "1.0 TAUOLA 0;",   // 2
      "Enddecay",        // 3
      "CDecay tau+",     // 4
      "End"              // 5
    };
    for ( int i=0;i<5;i++ ) 
      {
	out << cmds[i] << endl;
      }
  }
 
  //
  // Setup EvtGen to decay most particles in STAR
  //
  StarEvtGenDecayer *decayEvt = new StarEvtGenDecayer();
  decayEvt->SetDecayTable("TAUS.DEC");
  decayMgr->AddDecayer( 0, decayEvt ); // Handle any decay requested
  decayEvt->SetDebug(0);


  //
  // Setup pythia8 to decay W+ and W- (and possibly others...)
  //
  StarPythia8Decayer *decayPy8 = new StarPythia8Decayer();
  decayMgr->AddDecayer( +24, decayPy8 );
  decayMgr->AddDecayer( -24, decayPy8 );
  decayPy8->SetDebug(1);


  // Allow W to e+ nu or e- nu only
  decayPy8->Set("24:onMode = 0");
  decayPy8->Set("24:onIfAny = 11 -11");

    
  //
  // Initialize primary event generator and all sub makers
  //
  _primary -> Init();

  //return;

  //
  // Setup geometry and set starsim to use agusread for input
  //
  geometry("y2013_1c");
  command("gkine -4 0");
  command(fzname);


  //
  // Limits on eta, pt, ...
  //
  _primary->SetPtRange(0,-1.0); // no limits
  _primary->SetEtaRange(-2.5,2.5);

  //
  // Trigger on nevents
  //
  trig( nevents );

  //  _primary->event()->Print();

  command("call agexit");  // Make sure that STARSIM exits properly
  //  command("gprint kine");
}
//_____________________________________________________________________________

