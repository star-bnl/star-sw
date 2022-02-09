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
void trig( Int_t n=1, char* pid="ele", float e=0.0, float pt=1.5, int npart=1, int print=0){
    char* PID = pid;
    char* ele="e-";
    char* pos="e+";
    if(pid[0]=='e' && pid[1]=='l' && pid[2]=='e') PID=ele;
    if(pid[0]=='p' && pid[1]=='o' && pid[2]=='s') PID=pos;
    for ( Int_t i=0; i<n; i++ ) {
	double pi=3.141592654;
	cout << "==== Event="<<i<<"===="<<endl;
	// Clear the chain from the previous event
	chain->Clear();
	if(e>0.0){
	    kinematics->SetAttr("energy",1);
	    // In kinematics, generates single particle before Make is called.	
	    //kinematics->Kine(npart, PID, e-0.01, e+0.01, 2.2,  4.2, 0.0, 3.141592654*2.0);
	    //kinematics->Kine(npart, PID, e-0.01, e+0.01, 3.0,  3.01, 0.0, 3.141592654*2.0);
	    kinematics->Kine(npart, PID, e-0.01, e+0.01, 3.0,  3.01, -pi/2, pi);	    
	    //kinematics->Kine(npart, PID, e-0.01, e+0.01, 2.2,  4.0, 0.0, 2*pi);	    
	    //kinematics->Kine(npart, PID, e-0.01, e+0.01, 2.60, 4.0, 0.0, 3.141592654*2.0);
	    //kinematics->Kine( 1, PID, 2.99, 3.01, 2.50, 4.20, 0.0, 3.141592654/2.0);
	}else{
	    kinematics->SetAttr("energy",0);
	    kinematics->Kine(npart, PID, pt-0.001, pt+0.001, 2.5, 4.0, 0.0, 2*pi);
	    //	    kinematics->Kine(npart, PID, pt-0.01, pt+0.01, 2.9, 3.1, pi/4, pi/2);	    
	    //kinematics->Kine(npart, PID, pt-0.01, pt+0.01, 2.9, 3.1, -2.0*pi/8.0, -pi/8.0);	    
	}

	// Generate the event
	chain->Make();
	// Print the event
	if(print>0) primary->event()->Print();
	if(print>1) {
	    TIter Iterator = primary->event()->IterAll();
	    StarGenParticle *p = 0;
	    while( ( p = (StarGenParticle*)Iterator.Next() ) ){
	      TLorentzVector v = p->momentum();
	      cout << Form(" ===> e=%6.3f pt=%7.3f eta=%7.3f phi=%7.3f\n",v.E(),v.Pt(),v.Eta(),v.Phi());
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
void runSimFlat( Int_t nevents=1000, Int_t run=1, 
		 char* pid="mu+", float e=0.0, float pt=4.0, float vz=0.0, int npart=1, 
		 int ecal=1, int print=0){ 
  gROOT->ProcessLine(".L bfc.C");{
    TString simple = "y2022 geant gstar agml usexgeom";
    bfc(0, simple );
  }
  gSystem->Load( "libVMC.so");
  gSystem->Load( "StarGeneratorUtil.so" );
  gSystem->Load( "StarGeneratorEvent.so" );
  gSystem->Load( "StarGeneratorBase.so" );
  gSystem->Load( "libMathMore.so"   );  
  gSystem->Load( "xgeometry.so"     );
  
  // Create the primary event generator and insert it
  // before the geant maker
  primary = new StarPrimaryMaker();
  {
    chain   -> AddBefore( "geant", primary );
    // Set the output filename for the event record
    if(e>0.0){
	primary -> SetFileName(Form("%s.e%d.vz%d.run%i.root",pid,(int)e,(int)vz,run));
    }else{
	primary -> SetFileName(Form("%s.pt%3.1f.vz%d.run%i.root",pid,pt,(int)vz,run));
    }
    // Set the x,y,z vertex and distribution
    primary -> SetVertex( 0., 0., vz );
    primary -> SetSigma ( 0., 0., 0. );
  }

  // Initialize primary event generator and all sub makers
  Kinematics();
  primary -> Init();

  // Initialize random number generator
  StarRandom::capture();
  StarRandom::seed(run);

  // Setup geometry and set starsim to use agusread for input
  geometry("dev2022=1");
  command("gkine -4 0");
  if(e>0.0){
      command(Form("gfile o %s.e%d.vz%d.run%d.fzd",pid,(int)e,(int)vz,run));
  }else{
      command(Form("gfile o %s.pt%3.1f.vz%d.run%d.fzd",pid,pt,(int)vz,run));
  }
  if(ecal==0){
      cout << "TURNING OFF ECAL!!!"<<endl;
      command("DETP WCAL wver.active=0");
  }

  // Switch off some physics
  const Char_t *cmds[] = { 
    "CKOV 0"
  };
  for ( UInt_t i=0;i<sizeof(cmds)/sizeof(Char_t*); i++ ){command( cmds[i] );}
    
  // Trigger on nevents
  trig(nevents, pid, e, pt, npart, print);

  command("call agexit");
}
// ----------------------------------------------------------------------------

