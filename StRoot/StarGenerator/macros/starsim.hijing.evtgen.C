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

TF1 *ptDist  = 0;
TF1 *yDist = 0;

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
    // Generate 5 D0 according to a PT and Y distribution
    if (kinematics) kinematics->Kine(   1,  "B0",        0, 48, -1.5, 1.5 );
    //if (kinematics) kinematics->Kine(   2,  "B0_bar",    0, 48, -1.5, 1.5 );
    //if (kinematics) kinematics->Kine(   2,  "B_plus",    0, 48, -1.5, 1.5 );
    //if (kinematics) kinematics->Kine(   2,  "B_minus",   0, 48, -1.5, 1.5 );
    //if (kinematics) kinematics->Kine(   2,  "D0",        0, 24, -1.5, 1.5 );
    //if (kinematics) kinematics->Kine(   2,  "D0_bar",    0, 24, -1.5, 1.5 );
    //if (kinematics) kinematics->Kine(   2,  "D_plus",    0, 24, -1.5, 1.5 );
    //if (kinematics) kinematics->Kine(   2,  "D_minus",   0, 24, -1.5, 1.5 );
    chain->Make();
  }
}

void Kinematics()
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

  kinematics = new StarKinematics("EvtGen Decay");  
  _primary -> AddGenerator(kinematics);
}

// ----------------------------------------------------------------------------
void Hijing()
{
  StarHijing *hijing = new StarHijing("hijing");
  hijing->SetTitle("Hijing 1.383");

  // Setup collision frame, energy and beam species
  hijing->SetFrame("CMS",200.0);
  hijing->SetBlue("Au");
  hijing->SetYell("Au");  
  //hijing->SetImpact(0.0, 30.0);       // Impact parameter min/max (fm)    0.   30.
  hijing->SetImpact(0.0, 15.0);       // Minimum Bias
  hijing->hiparnt().ihpr2(4) = 0;     // Jet quenching (1=yes/0=no)       0
  hijing->hiparnt().ihpr2(3) = 0;     // Hard scattering (1=yes/0=no)
  hijing->hiparnt().hipr1(10) = 2.0;  //    pT jet
  hijing->hiparnt().ihpr2(8)  = 10;   // Max number of jets / nucleon
  hijing->hiparnt().ihpr2(11) = 1;    // Set baryon production
  hijing->hiparnt().ihpr2(12) = 1;    // Turn on/off decay of particles [1=recommended]
  hijing->hiparnt().ihpr2(18) = 1;    // Turn on/off B production
  hijing->hiparnt().hipr1(7) = 5.35;  // Set B production ???? Not really used... Really ????

  // For more configuration options, see the HIJING manual
  // http://ntc0.lbl.gov/~xnwang/hijing/doc.html

  _primary -> AddGenerator(hijing);
  _primary -> SetCuts( 1.0E-6 , -1., -2.6, 2.6 );
  
}
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
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

  // force gstar load/call
  gSystem->Load( "gstar.so" );
  command("call gstar");

  // Setup RNG seed and map all ROOT TRandom here
  StarRandom::seed( rngSeed );
  StarRandom::capture();

  char rootname[100],fzname[100];
  sprintf(rootname,"st_hijingevtgen_%d.starsim.root",Index);
  sprintf(fzname,"gfile o st_hijingevtgen_%d.starsim.fzd",Index);

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
  Hijing();

  //
  // Setup single particle
  //
  Kinematics();

  //
  // Setup decay manager
  //
  StarDecayManager   *decayMgr = AgUDecay::Manager();
  /*
  StarPythia8Decayer *decayPy8 = new StarPythia8Decayer();
  decayMgr->AddDecayer( 0, decayPy8 ); // Handle any decay requested
  decayPy8->SetDebug(1);
  decayPy8->Set("WeakSingleBoson:all = on");
  */
  StarEvtGenDecayer *decayEvt = new StarEvtGenDecayer();
  //decayEvt->SetDecayTable("StRoot/StSimulationMaker/Decay_Table/Jpsi.DEC");
  decayMgr->AddDecayer( 0, decayEvt ); // Handle any decay requested
  decayEvt->SetDebug(1);
    
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

  //Double_t pt0 = 3.0;
  //ptDist = new TF1("ptDist","(x/[0])/(1+(x/[0])^2)^6",0.0,10.0);
  //ptDist->SetParameter(0, pt0);
  //ptDist->Draw();
  ptDist = new TF1("ptDist","[0]*x*TMath::Exp(-x/[1])",minPt,maxPt); //dN/pT/dpT is exp 
  ptDist->SetParameters(1.,1.);//slope = 1.;
  //yDist = new TF1("yDist","-TMath::Erf(x+2.6)*TMath::Erf(x-2.6)",minY,maxY);
  yDist = new TF1("yDist","pol0",minY,maxY);
  yDist->SetParameter(0,1.);
  //phi, default 0 ~ TMath::TwoPi() flat

  //
  // Trigger on nevents
  //
  trig( nevents );

  _primary->event()->Print();

  command("call agexit");  // Make sure that STARSIM exits properly
  command("gprint kine");
}
// ----------------------------------------------------------------------------

