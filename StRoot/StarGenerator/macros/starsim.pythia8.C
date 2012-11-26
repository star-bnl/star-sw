// macro to instantiate the Geant3 from within
// STAR  C++  framework and get the starsim prompt
// To use it do
//  root4star starsim.C

class St_geant_Maker;
St_geant_Maker *geant_maker = 0;

class StarGenEvent;
StarGenEvent   *event       = 0;

class StarPrimaryMaker;
StarPrimaryMaker *primary = 0;

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
void trig( Int_t n=0 )
{
  chain->EventLoop(n+1);
  //  command("gprint kine");
  //  command("gprint hits");
  primary->event()->Print();
  
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
    }

  primary -> AddGenerator( pythia8 );
  
}
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
void starsim( Int_t nevents=10 )
{ 

  gROOT->ProcessLine(".L bfc.C");
  {
    TString simple = "y2012 geant gstar";
    bfc(0, simple );
  }

  gSystem->Load( "libVMC.so");

  gSystem->Load( "StarGeneratorUtil.so");
  gSystem->Load( "StarGeneratorEvent.so");
  gSystem->Load( "StarGeneratorBase.so" );

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
 
  
  //
  // Create the primary event generator and insert it
  // before the geant maker
  //
  //  StarPrimaryMaker *
  primary = new StarPrimaryMaker();
  {
    primary -> SetFileName( "pythia8.starsim.root");
    chain -> AddBefore( "geant", primary );
  }

  //
  // Setup an event generator
  //
  Pythia8("pp:W");

  //
  // Initialize primary event generator and all sub makers
  //
  primary -> Init();

  //
  // Setup geometry and set starsim to use agusread for input
  //
  //geometry("y2012");
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
  command("EXIT");

}
// ----------------------------------------------------------------------------

