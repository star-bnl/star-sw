// macro to instantiate Grant3 from within the STAR C++ framework and get the starsim prompt
// To use it do:
// root4star
// [0] .L StarLight.starsim.C

class St_geant_Maker;
St_geant_Maker   *geant_maker = 0;

class StarGenEvent;
StarGenEvent     *event       = 0;

class StarPrimaryMaker;
StarPrimaryMaker *_primary     = 0;

// ----------------------------------------------------------------------------
void geometry( TString tag, Bool_t agm1=true )
{
  TString cmd = "DETP GEOM "; cmd += tag;
  if ( !geant_maker ) geant_maker = (St_geant_Maker *)chain->GetMaker("geant");
  geant_maker -> LoadGeometry(cmd);
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
  for ( Int_t i=0; i<n; i++ )
  {
    chain->Clear();
    chain->Make();
    _primary->event()->Print();
  }
}
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
void STARlight( TString mode="pp" )
{

  gSystem->Load( "libStarLight.so" );

  starlight = new StarLight( "STARlight" );
  if ( mode = "pp" )
  {
    starlight->SetFrame("CMS",510);
    starlight->SetBlue("proton");
    starlight->SetYell("proton");
  }

  _primary->AddGenerator(starlight);
}
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
void starsim( Int_t nevents=1, Int_t rngSeed=1234 )
{

  gROOT->ProcessLine( ".L bfc.C" );
  {
    TString simple = "y2012 geant gstar usexgeom agml ";
    bfc( 0, simple );
  }

  gSystem->Load(    "libVMC.so"     );

  gSystem->Load( "StarGeneratorUtil.so" );
  gSystem->Load( "StarGeneratorEvent.so" );
  gSystem->Load( "StarGeneratorBase.so" );

  gSystem->Load(  "libMathMore.so"  );
  gSystem->Load(   "xgeometry.so"   );

  // Setup RNG seed and map all ROOT TRandom here
  StarRandom::seed( rngSeed );
  StarRandom::capture();

  //
  _primary = new StarPrimaryMaker();
  {
    _primary->SetFileName( "StarLight.starsim.root" );
    chain->AddBefore("geant",_primary);
  }

  STARlight( "pp" );

  _primary->Init();

  geometry( "y2012" );
  command( "gkine -4 0" );
  command( "gfile o StarLight.starsim.fzd" );

  trig( nevents );

  command("call agexit");  // Make sure that STARSIM exits properly


}
// ----------------------------------------------------------------------------
