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
    chain->Make();
  }
}
// ----------------------------------------------------------------------------
void Hijing()
{
  StarHijing *hijing = new StarHijing("hijing");
  hijing->SetTitle("Hijing 1.383");

  // Setup collision frame, energy and beam species
  hijing->SetFrame("CMS",200.0);
  hijing->SetBlue("p");
  hijing->SetYell("He3");  
  hijing->SetImpact(0.0, 30.0);       // Impact parameter min/max (fm)    0.   30.
  HiParnt_t &hiparnt =  hijing->hiparnt();
  {
    hiparnt.ihpr2(4) = 0;     // Jet quenching (1=yes/0=no)       0
    hiparnt.ihpr2(3) = 0;     // Hard scattering (1=yes/0=no)
    hiparnt.hipr1(10) = 2.0;  //    pT jet
    hiparnt.ihpr2(8)  = 10;   // Max number of jets / nucleon
    hiparnt.ihpr2(11) = 1;    // Set baryon production
    hiparnt.ihpr2(12) = 1;    // Turn on/off decay of particles [1=recommended]
    hiparnt.ihpr2(18) = 0;    // 0=charm production, 1=bottom production
    hiparnt.hipr1(7) = 5.35;  // Set B production ???? Not really used... Really ????
  }

  // For more configuration options, see the HIJING manual
  // http://ntc0.lbl.gov/~xnwang/hijing/doc.html

  _primary -> AddGenerator(hijing);
  _primary -> SetCuts( 1.0E-6 , -1., -2.5, +2.5 );
  
}
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
void starsim( Int_t nevents=50, Int_t rngSeed=1234 )
{ 

  gROOT->ProcessLine(".L bfc.C");
  {
    TString simple = "y2012 geant gstar usexgeom agml ";
    //TString full   = "tpcrs TpxRaw y2010a MakeEvent ITTF NoSvtIt NoSsdIt Idst IAna l0 ftpc Sti Tree logger genvtx tpcDB TpcHitMover TpxClu pmd bbcSim tofsim tags emcY2 EEfs evout IdTruth geantout -dstout big fzin MiniMcMk clearmem";
    //  TString full = "y2012 geant gstar tpcrs genvtx tpcDb tpxclu dedx event sdt20120224 ";
    bfc(0, simple );
  }

  gSystem->Load( "libVMC.so");

  gSystem->Load( "StarGeneratorUtil.so" );
  gSystem->Load( "StarGeneratorEvent.so" );
  gSystem->Load( "StarGeneratorBase.so" );
  gSystem->Load( "libMathMore.so"   );  
  gSystem->Load( "libHijing1_383.so");
  gSystem->Load( "xgeometry.so"     );

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
  // Setup an event generator
  //
  Hijing();

  //
  // Initialize primary event generator and all sub makers
  //
  _primary -> Init();

  //
  // Setup geometry and set starsim to use agusread for input
  //
  //geometry("y2012");
  command("gkine -4 0");
  command("gfile o hijing.starsim.fzd");
  
  //
  // Trigger on nevents
  //
  trig( nevents );
  command("gprint kine");

  //  command("call agexit");  // Make sure that STARSIM exits properly

}
// ----------------------------------------------------------------------------

