// macro to instantiate the Geant3 from within
// STAR  C++  framework and get the starsim prompt
// To use it do
//  root4star starsim.C

/*!
  This macro illustrates how to add a particle to the STAR particle database, and 
  use that particle in simulation.  The method "Particles()" adds two particles:
  the omega(772) and the Lambda Xi dibaryon. 

  The particle must exist in the gstar library.  In order to add a new particle
  to the gstar library, you should 

  $ cvs co pams/sim/gstar
  $ edit pams/sig/gstar/gstar_part.g
  ... add your particle
  $ cons

  Future versions of the code will eliminate this last step.

 */


class St_geant_Maker;
St_geant_Maker *geant_maker = 0;

class StarGenEvent;
StarGenEvent *event = 0;

class StarPrimaryMaker;
StarPrimaryMaker *_primary = 0;

class StarKinematics;
StarKinematics *kinematics = 0;

TF1 *ptDist  = 0;
TF1 *etaDist = 0;

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
void Particles()
{
  StarParticleData &data = StarParticleData::instance();
  //	TParticlePDG *myNewParticle = new TParticlePDG( "X(2430)", "Lambda-xi0",
  //					2430.543, true, 2.5e-15, 0,
  //					"dibaryon", 700, 0, 
  //					/* G3 ID */ 40009);
  
  //char* name; char* title;
  //double mass(GeV); bool stable; dobule width(GeV); double charge;
  //char* particleClass; int pdgCode; int anti; int tracking Code;

  
  if ( 1 ) 
    {
      TString name  = "lamXi0";
      TString title = "Lambda Xi0 Dibaryon";
      Double_t mass    = 2.430543;
      Bool_t   stable  = false;
      Double_t width   = 2.5E-15;
      Double_t charg3  = 0;
      TString  _class  = "dibaryon";
      Int_t    pdgCode = 2033223122;
      Int_t    g3Code  = 60002; // must exist in gstar_part.g
      
      TParticlePDG *part = data.AddParticle(name, title, mass, stable, width, charg3, _class, pdgCode, 0, g3Code );
  }

  if ( 1 ) 
    {
      //  ! omega(782) --> e+ e- 100%
      //  Particle omega  code=10150 TrkTyp=3 mass=.782   charge=0  tlife=7.79E-23,
      //                  pdg=0    bratio  = { 1.00, },  
      //                           mode    = { 0203, }
      TString name  = "omega(782)";
      TString title = "omgea(782)";
      Double_t mass    = 0.782;
      Bool_t   stable  = false;
      Double_t width   = 0.;
      Double_t charg3  = 0.;
      TString  _class  = "meson";
      Int_t    pdgCode = 223;
      Int_t    g3Code  = 10150; // must exist in gstar_part.g
      
      TParticlePDG *part = data.AddParticle(name, title, mass, stable, width, charg3, _class, pdgCode, 0, g3Code );
  }

  //  command("gexe $STAR_LIB/gstar.so");
  //  gSystem->Load(".$STAR_HOST_SYS/lib/gstar.so");
  gSystem->Load("libgstar.so");
  command("call gstar");

}

// ----------------------------------------------------------------------------
void trig( Int_t n=1 )
{
	for ( Int_t i=0; i<n; i++ )
	{
		// Clear the chain from the previous event
		chain->Clear();

		kinematics->Kine(1, "lamXi0", 0.0, 10.0, -1.0, +1.0 );// number; particle; pt low high; eta low high
		// kinematics->Kine(5, "omega(782)", 0.0, 10.0, -1.0, +1.0 );// number; particle; pt low high; eta low high

		// Generate 4 muons flat in pT and eta 
		//		kinematics->Kine(5, "proton", 0., 10., -1.0, +1.0 );

		// Generate 4 muons according to a PT and ETA distribution
		//    kinematics->Dist(4, "mu-", ptDist, etaDist );

		// Generate the event
		chain->Make();

		// Print the event
		_primary->event()->Print();
	}
}
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
void Kinematics()
{
	//  gSystem->Load( "libStarGeneratorPoolPythia6_4_23.so" );
	gSystem->Load( "libKinematics.so");
	kinematics = new StarKinematics();

	_primary->AddGenerator(kinematics);
}
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
void starsim( Int_t nevents=1, Int_t rngSeed=1234 )
{ 
	gROOT->ProcessLine(".L bfc.C");
	{
		TString simple = "y2012 geant gstar usexgeom agml ";
		bfc(0, simple );
	}
	
	gSystem->Load( "libVMC.so");
	
	gSystem->Load( "StarGeneratorUtil.so" );
	gSystem->Load( "StarGeneratorEvent.so" );
	gSystem->Load( "StarGeneratorBase.so" );
	
	gSystem->Load( "libMathMore.so"   );
	gSystem->Load( "xgeometry.so"     );

	// Setup RNG seed and captuire ROOT TRandom
	StarRandom::seed(rngSeed);
	StarRandom::capture();
 
	//
	// Create the primary event generator and insert it
	// before the geant maker
	//
	//  StarPrimaryMaker *
	_primary = new StarPrimaryMaker();
	{
		_primary -> SetFileName( "kinematics.starsim.root");
		chain -> AddBefore( "geant", _primary );
	}
	
	Particles();	
	Kinematics();
	
	//
	// Initialize primary event generator and all sub makers
	//
	_primary -> Init();
	
	//
	// Setup geometry and set starsim to use agusread for input
	//
	geometry("y2012");
	command("gkine -4 0");
	command("gfile o pythia6.starsim.fzd");
	
	//
	// Setup PT and ETA distributions
	//
	
	Double_t pt0 = 3.0;
	ptDist = new TF1("ptDist","(x/[0])/(1+(x/[0])^2)^6",0.0,10.0);
	ptDist->SetParameter(0, pt0);
	ptDist->Draw();
	
	etaDist = new TF1("etaDist","-TMath::Erf(x+2.6)*TMath::Erf(x-2.6)",-0.8,+0.8);
	
	//
	// Trigger on nevents
	//
	trig( nevents );
	
	//	command("call agexit");  // Make sure that STARSIM exits properly
}
// ----------------------------------------------------------------------------
