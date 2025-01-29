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


TH1F* hNumHits = 0;
TString nameParticle = "mu+";
int numParticles = 1;
float minPt = 0.0;
float maxPt = 1.0;
float minEta = 2.5;
float maxEta = 4.00;
float minPhi = 0.0;
float maxPhi = 2.0 * TMath::Pi();

float vtxX = 0.0;
float vtxY = 0.0;
float vtxZ = 0.0;

float vtxSigmaX = 0.0001;
float vtxSigmaY = 0.0001;
float vtxSigmaZ = 0.0001;

TString fzdFilename = "sim.fzd";
TString primaryName = "sim.root";

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
void trig_event( Int_t i )
{
  if ( gRandom->Rndm() > 0.5 ) {
    nameParticle = "mu+";
  } else {
    nameParticle = "mu-";
  }
  kinematics->Kine( numParticles, nameParticle.Data(), minPt, maxPt, minEta, maxEta, minPhi, maxPhi );
}
// ----------------------------------------------------------------------------
void trig( Int_t n=1 )
{
  for ( Int_t i=0; i<n; i++ ) {
    // Clear the chain from the previous event
    chain->Clear();
    trig_event( i );
    // Generate the event
    chain->Make();
  }
}
// ----------------------------------------------------------------------------
void Kinematics()
{

  //  gSystem->Load( "libStarGeneratorPoolPythia6_4_23.so" );
  gSystem->Load( "libKinematics.so");
  kinematics = new StarKinematics();
  _primary->AddGenerator(kinematics);
}
// ----------------------------------------------------------------------------
void gen( Int_t nevents=1000, Int_t rngSeed=12352342 )
{

  cout << "Generating: " << nevents << " events with seed: " << rngSeed << endl;
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
    _primary -> SetFileName( primaryName );
    chain -> AddBefore( "geant", _primary );
  }

  Kinematics();

  //
  // Initialize primary event generator and all sub makers
  //
  _primary -> Init();
  _primary->SetSigma( vtxSigmaX, vtxSigmaY, vtxSigmaZ ); // 1mm x 1mm x 1mm smearing at the vertex
  _primary->SetVertex(vtxX, vtxY, vtxZ );

  //
  // Setup geometry and set starsim to use agusread for input
  //
  //geometry("y2012");
  command("gkine -4 0");
  command( TString::Format("gfile o %s", fzdFilename.Data()) );


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
  // StarMagField::setConstBz(true);
  trig( nevents );

  command("call agexit");  // Make sure that STARSIM exits properly

}
// ----------------------------------------------------------------------------
