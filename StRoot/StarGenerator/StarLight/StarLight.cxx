#include "StarLight.h"
ClassImp(StarLight);

#include "randomgenerator.h"
#include "TDatabasePDG.h"
#include "TParticlePDG.h"

#include "StarGenerator/UTIL/StarRandom.h"
#include "StarGenerator/EVENT/StarGenAAEvent.h"
#include "StarGenerator/EVENT/StarGenPPEvent.h"
#include "StarGenerator/EVENT/StarGenParticle.h"

#include "inputParameters.h"
#include "TString.h"

// ----------------------------------------------------------------------------
// Remap STARlight's random number generator to Star Random
extern "C" {
  Double_t rndm_( Int_t *idummy ){
    return StarRandom::Instance().flat();
  };
};
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
StarLight::StarLight(const Char_t *name) : StarGenerator(name),
					   ParametersDouble(),
					   ParametersInt(),
					   _parameters(0),
					   mSTARlight(0)
{


  mSTARlight = new starlight();

   /// Default values for the initialization parameters.
   ParametersInt["BEAM_1_Z"]       =  79;       // Z of projectile //Can be set from mBlue
   ParametersInt["BEAM_1_A"]       =  197;      // A of projectile //Can be set from mBlue
   ParametersInt["BEAM_2_Z"]       =  79;       // Z of target //Can be set from mYell
   ParametersInt["BEAM_2_A"]       =  197;      // A of target //Can be set from mYell
   ParametersDouble["BEAM_GAMMA"]  =  106;      // Gamma of the colliding ions
   ParametersDouble["W_MAX"]       =  4.0;      // Max value of w
   ParametersDouble["W_MIN"]       =  0.0;      // Min value of w
   ParametersInt["W_N_BINS"]       =  40;       // Bins i w
   ParametersDouble["RAP_MAX"]     =  4.;       // max y
   ParametersInt["RAP_N_BINS"]     =  80;       // Bins i y
   ParametersInt["CUT_PT"]         =  0;        // Cut in pT? 0  (no, 1  yes)
   ParametersDouble["PT_MIN"]      =  1.0;      // Minimum pT in GeV
   ParametersDouble["PT_MAX"]      =  3.0;      // Maximum pT in GeV
   ParametersInt["CUT_ETA"]        =  0;        // Cut in pseudorapidity? (0  no, 1  yes)
   ParametersInt["ETA_MIN"]        =  -10;      // Minimum pseudorapidity
   ParametersInt["ETA_MAX"]        =  10;       // Maximum pseudorapidity
   ParametersInt["PROD_MODE"]      =  2;        // gg or gP switch (1  2-photon, 2  vector meson (narrow), 3  vector meson (wide) )
   ParametersInt["PROD_PID"]       =  444;      // Channel of interest
   ParametersInt["BREAKUP_MODE"]   =  5;        // Controls the nuclear breakup
   ParametersInt["INTERFERENCE"]   =  0;        // Interference (0  off, 1  on)
   ParametersDouble["IF_STRENGTH"] =  1.;       // % of intefernce (0.0 - 0.1)
   ParametersInt["COHERENT"]       =  1;        // Coherent1,Incoherent0
   ParametersDouble["INCO_FACTOR"] =  1.;       // percentage of incoherence
   ParametersDouble["BFORD"]       =  9.5;      // I honestly don't know what this does
   ParametersDouble["INT_PT_MAX"]  =  0.24;     // Maxvoid imum pt considered, when interference is turned on
   ParametersInt["INT_PT_N_BINS"]  =  120;      // Number of pt bins when interference is turned on
   ParametersInt["RND_SEED"]       =  12345;    // This isn't actually used, but starlight looks for it
   ParametersInt["OUTPUT_FORMAT"]  =  0;        // Again, I don't think this is used, but starlight looks for it
  // JFN 9/11/12 5:24pm : check if there is an object in the StarGenerator class which stores pt or eta cuts. If there is, reset the cut parameters right here accordingly.

}
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
Int_t StarLight::Init()
{
  // Proton mass:
  Double_t ProtonMass  = 0.938272046;
  // Neutron mass:
  Double_t NeutronMass = 0.939565378;

  // Map typical species run at RHIC
  map<TString,Int_t> A, Z;
  A["p"]  =   1;    Z["p"]  =  1;
  A["n"]  =   1;    Z["n"]  =  0;
  A["d"]  =   2;    Z["d"]  =  1;
  A["He3"] =  3;    Z["He3"] = 2;
  A["Au"] = 197;    Z["Au"] = 79;
  A["Cu"] =  64;    Z["Cu"] = 29;
  A["U"]  = 238;    Z["U"]  = 92;
  
  
  A["proton"]   =1;    Z["proton"]   =1;
  A["neutron"]  =1;    Z["neutron"]  =0;
  A["deuteron"] =2;    Z["deuteron"] =1;  
  A["e-"]       =0;    Z["e-"]       =0;
  A["electron"] =0;    Z["electron"] =0;
  A["e+"]       =0;    Z["e+"]       =0;
  A["positron"] =0;    Z["positron"] =0;


  TString myBlue = mBlue;
  TString myYell = mYell;


  //
  // Initialize STARlight based on the frame and registerd beam momenta
  //

  ParametersInt["BEAM_1_A"] = A[myBlue];
  ParametersInt["BEAM_1_Z"] = Z[myBlue];
  ParametersInt["BEAM_2_A"] = A[myYell];
  ParametersInt["BEAM_2_Z"] = Z[myYell];

  // Here we set the BEAM_GAMMA value. Gamma = E/M = mRootS/(massBlue+massYell).
  if( mFrame == "CMS" )
  {
    // if mFrame == "CMS" then we get mRootS to work with
    ParametersDouble["BEAM_GAMMA"] = mRootS/(((Z[myBlue]+Z[myYell])*ProtonMass)+((A[myBlue]-Z[myBlue]+A[myYell]-Z[myYell])*NeutronMass));
  }
  if( (mFrame == "3MOM") || (mFrame == "4MOM") || (mFrame == "5MOM") )
  {
    // if mFrame == "#MOM" then we get momentum vectors to work with
    // 4MOM = ( px, py, pz, E/c = sqrt( m**2*c**2 + |p|**2) )
    // JFN 9/28/12 9:15am - I'm a little fuzy on my relativistic mechaincs, so this next should be double checked:
    // Also, the units need to be checked. c=1?
    mRootS = ( sqrt(pow(((Z[myBlue]*ProtonMass)+((A[myBlue]-Z[myBlue])*NeutronMass)),2) + sqrt( pow(mBlueMomentum.Px(),2) + pow(mBlueMomentum.Py(),2) + pow(mBlueMomentum.Pz(),2))) + sqrt(pow(((Z[myYell]*ProtonMass)+((A[myYell]-Z[myYell])*NeutronMass)),2) + sqrt( pow(mYellMomentum.Px(),2) + pow(mYellMomentum.Py(),2) + pow(mYellMomentum.Pz(),2))));
    ParametersDouble["BEAM_GAMMA"] = mRootS/(((Z[myBlue]+Z[myYell])*ProtonMass)+((A[myBlue]-Z[myBlue]+A[myYell]-Z[myYell])*NeutronMass));
  }

  //  TDatabasePDG &pdg  = (*TDatabasePDG::Instance());
  //  TParticlePDG *blue = pdg.GetParticle(myBlue); assert(blue);
  //  TParticlePDG *yell = pdg.GetParticle(myYell); assert(yell);

  //
  // Setup event record based upon the beam species
  //

  if      ( (mBlue == "Au") || (mBlue == "Cu") || (mBlue == "U") ) mEvent = new StarGenAAEvent();
  else if ( (mBlue == "proton") && (mYell == "proton") )           mEvent = new StarGenPPEvent();
  else                                                             assert(0); // figure this out


  //
  // Initialize STARlight with the parameters specifed
  //
  ProcessParameters();

  //
  return StMaker::Init();
  //
}
// ----------------------------------------------------------------------------
Int_t StarLight::Generate()
{

  //
  // Generate the event.
  //
  upcEvent event = mSTARlight -> produceEvent();

  // JFN 9/12/12 10:10am: go figure out what this does and why it should be here
  // Rset hepevt
  //mSTARlight ->pushTrackReset();

  if ( (mBlue == "Au") || (mBlue == "Cu") || (mBlue == "U") )  FillAA( mEvent );
  if ( (mBlue == "proton") && (mYell == "proton") )            FillPP( mEvent );
  else /* ever make it onto this branch, assert in init        FillPP( mEvent ); */ assert(0);

  //
  // Get number of particles
  //
  const std::vector<starlightParticle> &particles = *(event.getParticles());

    // Loop over all particles in the event
  for ( UInt_t i=0;i<particles.size();i++ )
      { 
        Int_t q         = particles[i].getCharge();
        Int_t id        = particles[i].getPdgCode();
        Int_t stat      = StarGenParticle::kFinal;    // JFN 9/12/12 3:47pm: I don't think STARlight assigns particle status codes, but I'm also hedging my bet that it only gives final state particles. This may be a problem.
        Int_t mother1   = ParametersInt["PROD_PID"];
        Int_t mother2   = ParametersInt["PROD_PID"];
        Int_t daughter1 = 0;                          // I don't think that there are any decays
        Int_t daughter2 = 0;
        Double_t px     = particles[i].GetPx();
        Double_t py     = particles[i].GetPy();
        Double_t pz     = particles[i].GetPz();
        Double_t energy = particles[i].GetE();
        Double_t mass   = particles[i].M(); 
        Double_t vx     = 0;                          // As far as I can deduce, there is no vertex information provided
        Double_t vy     = 0;
        Double_t vz     = 0;
        Double_t vt     = 0;

        //
        // STARlight does not follow the PDG naming convention.  They drop the sign of antiparticles.  
        //
        assert( id > 0 );// If this fails, starlight group has changed something
        id *= q / abs(q);

        mEvent -> AddParticle( stat, id, mother1, mother2, daughter1, daughter2, px, py, pz, energy, mass, vx, vy, vz, vt );

      }

  return kStOK;
}

// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
void StarLight::FillAA( StarGenEvent *myevent )
{
  //
  // Fill event-wise information
  //
  // JFN 9/12/12 3:48pm: At the moment I am including this mostly for completeness, and I am padding out the values with nulls. Some of these parameters are meaningless for STARlight, and others will be filled in at a later time
  // JFN 9/12/12 3:52pm: At the later moment in time when I come back to fix this, these "Fill??()" functions may need to also to a pointed to the upcEvent which has just been generated. I don't know how else they will get the event data.
  StarGenAAEvent  *event = (StarGenAAEvent *)myevent;

  event -> idBlue     = 1;
  event -> idYell     = 1;
  event -> process    = ParametersInt["PROD_PID"];
  event -> subprocess = 0;

  event -> idParton1  = 0;
  event -> idParton2  = 0;
  event -> xParton1   = 0;
  event -> xParton2   = 0;
  event -> xPdf1      = 0;
  event -> xPdf2      = 0;
  event -> Q2fac      = 0;
  event -> Q2ren      = 0;
  event -> valence1   = 0;
  event -> valence2   = 0;
  
  event -> sHat       = 0;
  event -> tHat       = 0;
  event -> uHat       = 0;
  event -> ptHat      = 0;
  event -> thetaHat   = 0;
  event -> phiHat     = 0;

  event -> weight     = 0;

}
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
void StarLight::FillPP( StarGenEvent *myevent )
{
  //
  // Fill event-wise information
  //
  // JFN 9/12/12 3:48pm: At the moment I am including this mostly for completeness, and I am padding out the values with nulls. Some of these parameters are meaningless for STARlight, and others will be filled in at a later time
  // JFN 9/12/12 3:52pm: At the later moment in time when I come back to fix this, these "Fill??()" functions may need to also to a poointed to the upcEvent which has just been generated. I don't know how else they will get the event data.
  StarGenPPEvent  *event = (StarGenPPEvent *)myevent;

  event -> idBlue     = 1;
  event -> idYell     = 1;
  event -> process    = ParametersInt["PROD_PID"];
  event -> subprocess = 0;

  event -> idParton1  = 0;
  event -> idParton2  = 0;
  event -> xParton1   = 0;
  event -> xParton2   = 0;
  event -> xPdf1      = 0;
  event -> xPdf2      = 0;
  event -> Q2fac      = 0;
  event -> Q2ren      = 0;
  event -> valence1   = 0;
  event -> valence2   = 0;
  
  event -> sHat       = 0;
  event -> tHat       = 0;
  event -> uHat       = 0;
  event -> ptHat      = 0;
  event -> thetaHat   = 0;
  event -> phiHat     = 0;

  event -> weight     = 0;

}
// ----------------------------------------------------------------------------
void StarLight::ProcessParameters()
{
  // First we print all of the parameters to a file
  std::ofstream params_out;
  params_out.open( "starlight.in" );
  for(map<TString,Double_t>::iterator i = ParametersDouble.begin(); i != ParametersDouble.end(); i++)
  {
    params_out << i->first << " = " << i->second << endl;
  }
  for(map<TString,Int_t>::iterator i = ParametersInt.begin(); i != ParametersInt.end(); i++)
  {
    params_out << i->first << " = " << i->second << endl;
  }
  params_out.close();

  // Then we tell STARlight to read it in
  _parameters = new inputParameters();
  _parameters -> init( "starlight.in" );
  mSTARlight -> setInputParameters( _parameters );
  //_parameters->interactionType() = "PHOTONPHOTON";
  mSTARlight -> init();
}
// ----------------------------------------------------------------------------
void StarLight::SetEtaCut( Double_t low, Double_t high )
{
  ParametersDouble["ETA_MIN"] = low;
  ParametersDouble["ETA_MAX"] = high;
  ParametersInt["CUT_ETA"]    = 1;    // Turn the cut on
}
// ----------------------------------------------------------------------------
void StarLight::SetPtCut( Double_t low, Double_t high )
{
  ParametersDouble["PT_MIN"] = low;
  ParametersDouble["PT_MAX"] = high;
  ParametersInt["CUT_PT"]    = 1;     // Turn the cut on
}
// ----------------------------------------------------------------------------
void StarLight::SetRapidityValues( Double_t high, Int_t bins )
{
  ParametersDouble["RAP_MAX"] = high;
  ParametersInt["RAP_N_BINS"] = bins;
}
// ----------------------------------------------------------------------------
void StarLight::SetWValues( Double_t low, Double_t high, Int_t bins )
{
  ParametersDouble["W_MIN"] = low;
  ParametersDouble["W_MAX"] = high;
  ParametersInt["W_N_BINS"] = bins;
}
// ----------------------------------------------------------------------------
void StarLight::SetProductionMode( Int_t mode )
{
  ParametersInt["PROD_MODE"] = mode;
}
// ----------------------------------------------------------------------------
void StarLight::SetProductionPID( Int_t pid )
{
  ParametersInt["PROD_PID"] = pid;
}
// ----------------------------------------------------------------------------
void StarLight::SetBreakupMode( Int_t mode )
{
  ParametersInt["BREAKUP_MODE"] = mode;
}
// ----------------------------------------------------------------------------
void StarLight::SetInterference( Double_t percent )
{
  if( (percent > 1) || (percent<0) )
  {
    cout << "starSTARlight::SetInterference( Double_t percent ) : percent must be a value between 0 and 1." << endl;
    assert( (percent > 1) || (percent<0) );
  }

  ParametersDouble["IF_STRENGTH"] = percent;
  ParametersInt["INTERFERENCE"]   = 1;
}
// ----------------------------------------------------------------------------
void StarLight::SetIncoherence( Double_t percent )
{
  if( (percent > 1) || (percent<0) )
  {
    cout << "starSTARlight::SetIncoherence( Double_t percent ) : percent must be a value between 0 and 1." << endl;
    assert( (percent > 1) || (percent<0) );
  }

  ParametersDouble["INCO_FACTOR"] = percent;
  ParametersInt["COHERENT"]       = 0;
}
// ----------------------------------------------------------------------------
void StarLight::SetBFORD( Double_t value )
{
  ParametersDouble["BFORD"] = value;
}
// ----------------------------------------------------------------------------
void StarLight::SetInterferencePtValues( Double_t high, Int_t bins )
{
  ParametersDouble["INT_PT_MAX"] = high;
  ParametersInt["INT_PT_N_BINS"] = bins;
}
