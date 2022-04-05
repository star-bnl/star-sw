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

// ----------------------------------------------------------------------------
void geometry( TString tag, Bool_t agml=true )
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
// trig()  -- generates one event
// trig(n) -- generates n events.
void trig( Int_t n=1 )
{
  chain->EventLoop(n);
  
}
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
void Pythia8( TString config="pp:W", Double_t ckin3=0.0, Double_t ckin4=-1.0 )
{

  gSystem->Load( "Pythia8_1_62.so"  );

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
      pythia8->SetFrame("CMS", 500.0);
      pythia8->SetBlue("proton");
      pythia8->SetYell("proton");    

      pythia8->Set("HardQCD:all = on");

    }

  // Setup phase space cuts
  pythia8 -> Set(Form("PhaseSpace:ptHatMin=%f", ckin3 ));
  pythia8 -> Set(Form("PhaseSpace:ptHatMax=%f", ckin4 ));

  _primary -> AddGenerator( pythia8 );
  
}
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
void Pythia6( TString mode="pp:minbias", Double_t ckin3=0.0, Double_t ckin4=-1.0, Int_t tune=320, Int_t rngSeed=1234 )
{
  
  //  gSystem->Load( "libStarGeneratorPoolPythia6_4_23.so" );
  gSystem->Load( "libPythia6_4_23.so");
  //  gSystem->Load( "StarPythia6.so"   );

  // Setup RNG seed and captuire ROOT TRandom
  StarRandom::seed(rngSeed);
  StarRandom::capture();
 
  StarPythia6 *pythia6 = new StarPythia6("pythia6");

  //
  // Common blocks for configuration
  //
  PySubs_t &pysubs = pythia6->pysubs();

  if ( mode=="pp:W" )
  {
    pythia6->SetFrame("CMS", 510.0 );
    pythia6->SetBlue("proton");
    pythia6->SetYell("proton");
    if ( tune ) pythia6->PyTune( tune );

    // Setup pythia process

    pysubs.msel = 12;

  }
  if ( mode == "pp:minbias" )
  {
    pythia6->SetFrame("CMS", 510.0 );
    pythia6->SetBlue("proton");
    pythia6->SetYell("proton");
    pythia6->PyTune( tune );
    
    // Setup pythia process

    pysubs.msel = 1;

  }

  pysubs.ckin(3)=ckin3;
  pysubs.ckin(4)=ckin4;

  _primary->AddGenerator(pythia6);

}

// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
void starsim( Int_t nevents=1, Double_t ckin3=7.0, Double_t ckin4=-1.0  )
{ 

  gROOT->ProcessLine(".L bfc.C");
  {
    TString simple = "y2012 geant gstar usexgeom agml ";
    bfc(0, simple );
  }

  gSystem->Load( "libVMC.so");

  gSystem->Load( "StarGeneratorUtil.so");
  gSystem->Load( "StarGeneratorEvent.so");
  gSystem->Load( "StarGeneratorBase.so" );
  gSystem->Load( "libMathMore.so"   );  



  //  gSystem->Load("StarFilterMaker.so") ;
  gSystem->Load( "StarGeneratorFilt.so" );
 
  gMessMgr->SetLevel(999);

  //
  // Create the primary event generator and insert it
  // before the geant maker
  //
  _primary = new StarPrimaryMaker();
  {
    _primary -> SetFileName( Form("filter_%f_%f.gener.root",ckin3,ckin4) );
    _primary -> SetVertex( 0.1, -0.2, 0.0 );
    _primary -> SetSigma ( 0.1,  0.1, 30.0 );
    chain -> AddBefore( "geant", _primary );
  }

  //
  // Setup an event generator
  //
  Pythia8("pp:minbias", ckin3, ckin4 );
  //  Pythia6("pp:minbias", ckin3, ckin4 );

  //
  // Setup the generator filter
  //
  filter = new StDijetFilter();
  _primary -> AddFilter( filter );

  // If set to 1, tracks will be saved in the tree on events which were
  // rejected.  If the tree size is too big (because the filter is too
  // powerful) you may want to set this equal to zero.  In which case
  // only header information is saved for the event.
  _primary->SetAttr("FilterKeepAll",     int(1));

  // By default, the primary maker enters an infinite loop and executes
  // the event generator until it yields an event which passes the filter.
  // The big full chain treats this as a single event.
  //
  // If you want the BFC to see an empty event, set the FilterSkipRejects
  // attribute on the primary maker and give it the priveledge it needs
  // to kill the event. 
  //---  primary->SetAttr("FilterSkipRejects", int(1) ); // enables event skipping 
  //---  chain->SetAttr(".Privilege",1,"StarPrimaryMaker::*" );


  //
  // Initialize primary event generator and all sub makers
  //
  _primary -> Init();

  //
  // Setup geometry and set starsim to use agusread for input
  //

  command("gkine -4 0");
  command( Form("gfile o filter_%f_%f.starsim.fzd",ckin3,ckin4) );


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

