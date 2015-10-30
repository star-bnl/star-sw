// macro to instantiate the Geant3 from within
// STAR  C++  framework and get the starsim prompt
// To use it do
//  root4star starsim.C

// Example of how to add a single pi0 decaying 100% through dalitz decay (e+ e- gamm) to a standard
// hijing event.  




class St_geant_Maker;
St_geant_Maker *geant_maker = 0;

class StarGenEvent;
StarGenEvent   *event       = 0;

class StarPrimaryMaker;
StarPrimaryMaker *_primary = 0;

class StarKinematics;
StarKinematics *kinematics = 0;

Float_t minPt  = +0.5;
Float_t maxPt  = +5.0;
Float_t minEta = -1.5;
Float_t maxEta = +1.5;

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
    // Throw one hypertriton flat in -1 to 1 w/ momentum btwn 0.1 and 1 GeV
    if (kinematics) kinematics->Kine( 1, "Dalitz", minPt, maxPt, minEta, maxEta );
    chain->Make();
    //    command("gprint kine");
  }
}
// ----------------------------------------------------------------------------
void Dalitzdecay()
{ 

  // The Dalitz particle is defined in starsim  in gstar_part.g --
  //
  //  Particle Dalitz    code=10007       TrkTyp=4 mass=0.135  charge=0 tlife=8.4e-17,                     |                                                                                                       
  //                     pdg=100111 bratio= { 1.0,}  mode= {10203,}                                        |                                                                                                       
  //
  // The particle database does not know about this particle, so we have to add it.  It is
  // important that we do not overwrite PDG id = 111, the ID of the standard pi0.  Otherwise,
  // we will have all pi0's in the event decaying by dalitz.
  //
  StarParticleData &data = StarParticleData::instance();
  data.AddParticle("Dalitz","pi0-->e+e-gamma 100%",0.135,0,0,0,"meson",100111,-100111,10007);
  
  kinematics = new StarKinematics("Dalitz Decay");  
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
  hijing->SetImpact(0.0, 30.0);       // Impact parameter min/max (fm)    0.   30.
  HiParnt_t &hiparnt = hijing->hiparnt();
  {
    hijing->hiparnt().ihpr2(4) = 0;     // Jet quenching (1=yes/0=no)       0
    hijing->hiparnt().ihpr2(3) = 0;     // Hard scattering (1=yes/0=no)
    hijing->hiparnt().hipr1(10) = 2.0;  //    pT jet
    hijing->hiparnt().ihpr2(8)  = 10;   // Max number of jets / nucleon
    hijing->hiparnt().ihpr2(11) = 1;    // Set baryon production
    hijing->hiparnt().ihpr2(12) = 1;    // Turn on/off decay of particles [1=recommended]
    hijing->hiparnt().ihpr2(18) = 0;    // 0=Charm, 1=Bottom production
    hijing->hiparnt().hipr1(7) = 5.35;  // Set B production ???? Not really used... Really ????
  };
    

  hijing->hiparnt().ihpr2(50) = 12345; // unused entry

  // For more configuration options, see the HIJING manual
  // http://ntc0.lbl.gov/~xnwang/hijing/doc.html

  _primary -> AddGenerator(hijing);
  _primary -> SetCuts( 1.0E-6 , -1., -2.5, +2.5 );
  
}
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
void starsim( Int_t nevents=10, Int_t rngSeed=4321 )
{ 

  gROOT->ProcessLine(".L bfc.C");
  {
    TString simple = "y2014a geant gstar usexgeom agml ";
    bfc(0, simple );
  }

  gSystem->Load( "libVMC.so");

  gSystem->Load( "StarGeneratorUtil.so" );
  gSystem->Load( "StarGeneratorEvent.so" );
  gSystem->Load( "StarGeneratorBase.so" );
  gSystem->Load( "libMathMore.so"   );  
  gSystem->Load( "libHijing1_383.so");
  gSystem->Load( "libKinematics.so");
  gSystem->Load( "xgeometry.so"     );

  // force gstar load/call
  gSystem->Load( "gstar.so" );
  command("call gstar");

  // Setup RNG seed and map all ROOT TRandom here
  StarRandom::seed( rngSeed );
  StarRandom::capture();

  //
  // Create the primary event generator and insert it
  // before the geant maker
  //
  _primary = new StarPrimaryMaker();
  {
    _primary -> SetFileName( "hijing.starsim.root");
    chain -> AddBefore( "geant", _primary );
  }

  //
  // These should be adjusted to your best vertex estimates
  //
  _primary -> SetVertex( 0., 0., 0. );
  _primary -> SetSigma(  0.3, 0.3, 60.0 );




  // Setup an event generator
  //
  Hijing();
  //
  // Setup single dalitzdecay
  //  
  Dalitzdecay(); 
 

  //
  // Initialize primary event generator and all sub makers
  //
  _primary -> Init();


  //
  // Setup geometry and set starsim to use agusread for input
  //

  command("gkine -4 0");
  command("gfile o hijing.starsim.fzd");
  
  //
  // Trigger on nevents
  //
  trig( nevents );

  _primary->event()->Print();

  //  command("gprint kine");

  command("call agexit");  // Make sure that STARSIM exits properly

}
// ----------------------------------------------------------------------------

