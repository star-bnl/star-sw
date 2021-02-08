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

class StarKinematics;
StarKinematics *kinematics = 0;

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
void trig( Int_t n=0 )
{
  for ( Int_t i=0; i<n+1; i++ ) {
    chain->Clear();

    // Throw single tau- 
    if (kinematics) kinematics -> Kine( 1, "tau-", 10.0, 20.0, -1.0, 1.0 );

    chain->Make();
  }
}

void Kinematics()
{

  kinematics = new StarKinematics("EvtGen Decay");  
  _primary -> AddGenerator(kinematics);
}

void Pythia6( TString mode="pp:W", Int_t tune=320 )
{
  
  //  gSystem->Load( "libStarGeneratorPoolPythia6_4_23.so" );
  gSystem->Load( "libPythia6_4_28.so");
  //  gSystem->Load( "StarPythia6.so"   );

  StarPythia6 *pythia6 = new StarPythia6("pythia6");
  if ( mode=="pp:W" )
  {
    pythia6->SetFrame("CMS", 510.0 );
    pythia6->SetBlue("proton");
    pythia6->SetYell("proton");
    if ( tune ) pythia6->PyTune( tune );

    // Setup pythia process
    PySubs_t &pysubs = pythia6->pysubs();
    pysubs.msel = 12;
    pysubs.ckin(3)=4.0;

  }
  if ( mode == "pp:minbias" )
  {
    pythia6->SetFrame("CMS", 510.0 );
    pythia6->SetBlue("proton");
    pythia6->SetYell("proton");
    if ( tune ) pythia6->PyTune( tune );
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

void Pythia8( TString config="pp:W" )
{

  //
  // Create the pythia 8 event generator and add it to 
  // the primary generator
  //
  StarPythia8 *pythia8 = new StarPythia8();    
  if ( config=="pp:W" )
    {
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
      pythia8->SetFrame("CMS", 510.0);
      pythia8->SetBlue("proton");
      pythia8->SetYell("proton");            

      pythia8->Set("SoftQCD:minBias = on");
    }

  _primary -> AddGenerator( pythia8 );
  
}

//
void starsim( Int_t nevents=1, Int_t Index = 0, Int_t rngSeed=4321 )
{ 

  gROOT->ProcessLine(".L bfc.C");
  {
    TString simple = "y2014 geant gstar usexgeom agml ";
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


  // force gstar load/call nope
  //  gSystem->Load( "gstar.so" );
  //  command("call gstar");

  // Setup RNG seed and map all ROOT TRandom here
  StarRandom::seed( rngSeed );
  StarRandom::capture();

  TString rootname = "tau.root";
  TString fzname   = "gfile o tau.fzd";

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
  // Setup single particle
  //
  //  Kinematics();
  Pythia8("pp:W");
  //  Pythia6("pp:W");
  

  //
  // Setup decay manager
  //
  StarDecayManager   *decayMgr = AgUDecay::Manager();
  StarEvtGenDecayer  *decayEvt = new StarEvtGenDecayer();
  //decayEvt->SetDecayTable("StRoot/StSimulationMaker/Decay_Table/Jpsi.DEC");
  decayMgr->AddDecayer( 0, decayEvt ); // Handle any decay requested
  //  decayEvt->SetDebug(1);
    

  //
  // Initialize primary event generator and all sub makers
  //
  _primary -> Init();

  //
  // Setup geometry and set starsim to use agusread for input
  //
  //geometry("y2014");
  command("gkine -4 0");
  command(fzname);
 
  //
  // Trigger on nevents
  //
  trig( nevents );

  _primary->event()->Print();

  command("call agexit");  // Make sure that STARSIM exits properly

}
// ----------------------------------------------------------------------------

