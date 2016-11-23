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

class StarKinematics;
StarKinematics *kinematics = 0;

// Set magnetic field (reversed full)
Float_t field = -5.0;
//Float_t field = 0.0;

// ----------------------------------------------------------------------------
void geometry( TString tag, Bool_t agml=true ){
  TString cmd = "DETP GEOM "; cmd += tag; cmd += Form(" field=%f", field);
  if ( !geant_maker ) geant_maker = (St_geant_Maker *)chain->GetMaker("geant");
  geant_maker -> LoadGeometry(cmd);
  //  if ( agml ) command("gexec $STAR_LIB/libxgeometry.so");
}
// ----------------------------------------------------------------------------
void command( TString cmd ){
  if ( !geant_maker ) geant_maker = (St_geant_Maker *)chain->GetMaker("geant");
  geant_maker -> Do( cmd );
}
// ----------------------------------------------------------------------------
void trig( Int_t n=1, const char* pid="mu-", int print=0){
    for ( Int_t i=0; i<n; i++ ) {
	cout << "==== Event="<<i<<"===="<<endl;
	// Clear the chain from the previous event
	chain->Clear();
	// In kinematics, generates single particle before Make is called.	
	kinematics->Kine( 1, pid, 2.99, 3.01, 2.50, 4.60, 0.0, 3.141592654*2.0);
	//kinematics->Kine( 1, pid, 2.99, 3.01, 2.50, 4.20, 0.0, 3.141592654/2.0);
	// Generate the event
	chain->Make();
	// Print the event
	if(print>0) primary->event()->Print();
	if(print>1) {
	    TIter Iterator = primary->event()->IterAll();
	    StarGenParticle *p = 0;
	    while( ( p = (StarGenParticle*)Iterator.Next() ) ){
		TLorentzVector v = p->momentum();
		cout << Form(" ===> pt=%7.3f eta=%7.3f phi=%7.3f\n",v.Pt(),v.Eta(),v.Phi());
	    }
	}
	if(print>2) command("gpri hits");
    }
}
// ----------------------------------------------------------------------------
void Kinematics(){
  //  gSystem->Load( "libStarGeneratorPoolPythia6_4_23.so" );
  gSystem->Load( "libKinematics.so");
  kinematics = new StarKinematics();    
  primary->AddGenerator(kinematics);
}
// ----------------------------------------------------------------------------
void runSimFlat( Int_t nevents=1000, Int_t run=1, const char* pid="muon", int print=0){ 
  gROOT->ProcessLine(".L bfc.C");{
    TString simple = "y2015 geant gstar agml usexgeom";
    bfc(0, simple );
  }
  gSystem->Load( "libVMC.so");
  gSystem->Load( "StarGeneratorUtil.so" );
  gSystem->Load( "StarGeneratorEvent.so" );
  gSystem->Load( "StarGeneratorBase.so" );
  gSystem->Load( "libMathMore.so"   );  
  // gSystem->Load( "xgeometry.so"     );
  
  // Create the primary event generator and insert it
  // before the geant maker
  primary = new StarPrimaryMaker();
  {
    chain   -> AddBefore( "geant", primary );
    // Set the output filename for the event record
    primary -> SetFileName(Form("test_%s_run%i.root",pid,run));
    // Set the x,y,z vertex and distribution
    primary -> SetVertex( 0., 0., 0. );
    primary -> SetSigma ( 0., 0., 0. );
  }

  // Initialize primary event generator and all sub makers
  Kinematics();
  primary -> Init();

  // Initialize random number generator
  StarRandom::capture();
  StarRandom::seed(run);

  // Setup geometry and set starsim to use agusread for input
  geometry("y2015");
  command("gkine -4 0");
  command(Form("gfile o test_%s_run%i.fzd",pid,run));

  // Switch off some physics
  const Char_t *cmds[] = { 
    "CKOV 0"
  };
  for ( UInt_t i=0;i<sizeof(cmds)/sizeof(Char_t*); i++ ){command( cmds[i] );}
    
  // Trigger on nevents
  trig(nevents, pid, print);

  command("call agexit");
}
// ----------------------------------------------------------------------------

