#include "StarHerwig6.h"
ClassImp(StarHerwig6);

#include "StarCallf77.h"
#include "StarGenerator/EVENT/StarGenPPEvent.h"
#include "StarGenerator/EVENT/StarGenEPEvent.h"
#include "StarGenerator/EVENT/StarGenParticle.h"

#include "StarGenerator/UTIL/StarRandom.h"

#include <map>
#include <iostream>

// ---------------------------------------------------------------------------
/// Remapping Herwigs random number generator to StarRandom
// JFN 9/10/12 7:58pm: In principle the '_' at the end of 'hwrgen_' shouldn't be necessary, but it seems to be. FYI
#define hwrgen F77_NAME(hwrgen, HWRGEN)
extern "C" {
  Double_t hwrgen( Int_t *idummy ){
    return StarRandom::Instance().flat();
  };
};
  
// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
StarHerwig6::StarHerwig6( const Char_t *name ) : StarGenerator(name)
{

  /// Setting up a map between Herwig's status codes and the HepMC status codes used in StarEvent
  mStatusCode[0]   = StarGenParticle::kNull;
  mStatusCode[1]   = StarGenParticle::kFinal;
  mStatusCode[2]   = StarGenParticle::kDocumentation;
  mStatusCode[3]   = StarGenParticle::kDocumentation;
  /// See the Herwig manual, section 8.3.1: Status Codes
  // First we set 100-201 to kDocumentation in bulk...
  for ( UInt_t i=0; i<101; i++)
    {
      mStatusCode[i+100] = StarGenParticle::kDocumentation;
    }
  // ...then go back and fix the ones we know.
  /// Beam
  mStatusCode[101] = StarGenParticle::kIncident;
  /// Target
  mStatusCode[102] = StarGenParticle::kIncident;


}
// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
/// This sets the Herwig process, for details see the Herwig manual chapter 4, particulary 4.2-4.12
void StarHerwig6::SetProcess( Int_t proccess )
{
  hwproc().iproc = proccess;
}

Int_t StarHerwig6::Init()
{

  //
  // Create a new event record for either pp or ep events
  //
  if ( ( mBlue == "proton" ) && ( mYell == "proton" ) )                 mEvent = new StarGenPPEvent();
  else                                                                  mEvent = new StarGenEPEvent();

  /// Remapt the ROOT names to Herwig names
  std::map< TString, string > particle;
  /// Ex: particle["ROOT name"]="Herwig name";
  particle["proton"] = "P       ";
  particle["e-"]     = "E-      ";

  /// Set up frames
  if ( mFrame=="COM" )
    {
      Fatal(GetName(),"COM is an invalid frame for Herwig. Exiting");
      assert(0); // JFN 9/7/12: Note: if this wont compile use error(0);
    }
  if ( mFrame=="3MOM" || mFrame=="4MOM" || mFrame=="5MOM" )
    {
      // All Herwig asks for is the "momentum of the beams", so I am giving it p_z
      hwproc().pbeam1 = mBlueMomentum.Pz();
      hwproc().pbeam2 = -mYellMomentum.Pz();
    }

  /// Initialize Herwig:
  /// beam particles: PART,PART2
  SetBeams( particle[mBlue], particle[mYell] );

  /// Set particles to not decay
  std::vector<string> StableParticles;
  // PI0 111
  StableParticles.push_back("PI0     ");
  // PI+ 211
  StableParticles.push_back("PI+     ");
  // ETA 221
  StableParticles.push_back("ETA     ");
  // K+ 321
  StableParticles.push_back("K+      ");
  // K_SHORT 310
  StableParticles.push_back("K_S0    ");
  // K_LONG 130
  StableParticles.push_back("K_L0    ");
  // LAMBDA0 3122
  StableParticles.push_back("LAMBDA  ");
  // JFN 9/10/12: I looked up the particle names in herwig6520.F and I am 99.999% sure the following two are correct, but they make the code throw a fit and aren't recognized.
  // SIGMA0 3212
  //StableParticles.push_back("SIMGA0  ");
  // SIGMA- 3112
  //StableParticles.push_back("SIMGA-  ");
  // SIGMA+ 3222
  StableParticles.push_back("SIGMA+  ");
  // Xi- 3312
  StableParticles.push_back("XI-     ");
  // Xi0 3322
  StableParticles.push_back("XI0     ");
  // OMEGA- 3334
  StableParticles.push_back("OMEGA-  ");

  HWInit( StableParticles );

  return StMaker::Init();
}
// ----------------------------------------------------------------------------
//
// ----------------------------------------------------------------------------
Int_t StarHerwig6::Generate()
{
  /// Generate an event
  HWGenerate();

  // Blue beam is a proton, running PP
  if ( ( mBlue == "proton" ) && ( mYell == "proton" ) )    FillPP( mEvent );
  // Otherwise, runnin EP
  else                                                     FillEP( mEvent );

  //Do Stuff with the particles
  mNumberOfParticles = hepevt().nhep;
  for ( Int_t idx=1; idx<=mNumberOfParticles; idx++ )
    {

      Int_t    id = hepevt().idhep(idx);
      Int_t    stat = mStatusCode[ hepevt().isthep(idx) ];
      if ( !stat ) {
	stat = StarGenParticle::kUnknown;
      }
      Int_t    m1 = hepevt().jmohep(idx,1);
      Int_t    m2 = hepevt().jmohep(idx,2);
      Int_t    d1 = hepevt().jdahep(idx,1);
      Int_t    d2 = hepevt().jdahep(idx,2);
      Double_t px = hepevt().phep(idx,1);
      Double_t py = hepevt().phep(idx,2);
      Double_t pz = hepevt().phep(idx,3);
      Double_t E  = hepevt().phep(idx,4);
      Double_t M  = hepevt().phep(idx,5);
      Double_t vx = hepevt().vhep(idx,1);
      Double_t vy = hepevt().vhep(idx,2);
      Double_t vz = hepevt().vhep(idx,3);
      Double_t vt = hepevt().vhep(idx,4);

      mEvent -> AddParticle( stat, id, m1, m2, d1, d2, px, py, pz, E, M, vx, vy, vz, vt );
    }

  return kStOK;
}
// ----------------------------------------------------------------------------
//
// ----------------------------------------------------------------------------
void StarHerwig6::Clear( const Option_t *opts )
{

  /// Terminate elementary processes
  HWFinish();
  //.... may be better to move to Finalize() ?

  return;
}
// ----------------------------------------------------------------------------
//
// ----------------------------------------------------------------------------
void StarHerwig6::FillPP( StarGenEvent *event)
{

  // Fill the event-wise information
  StarGenPPEvent *myevent = (StarGenPPEvent *)event;
  myevent -> idBlue     = hwbeam().ipart1;
  myevent -> idYell     = hwbeam().ipart2;
  myevent -> process    = hwproc().iproc;
  myevent -> subprocess = 0;

  myevent -> idParton1  = -999;
  myevent -> idParton2  = -999;
  myevent -> xParton1   = hwhard().xx(1);
  myevent -> xParton2   = hwhard().xx(2);
  myevent -> xPdf1      = -999;
  myevent -> xPdf2      = -999;
  myevent -> Q2fac      = -999;
  myevent -> Q2ren      = -999;
  myevent -> valence1   = 0;
  myevent -> valence2   = 0;

  myevent -> sHat       = custom().hwmans;
  myevent -> tHat       = custom().hwmant;
  myevent -> uHat       = custom().hwmanu;
  myevent -> ptHat      = -999;
  myevent -> thetaHat   = -999;
  myevent -> phiHat     = -999;
  
  myevent -> weight     = -999;

}
// ----------------------------------------------------------------------------
//
// ----------------------------------------------------------------------------
void StarHerwig6::FillEP( StarGenEvent *event)
{

  // Fill the event-wise information
  StarGenPPEvent *myevent = (StarGenPPEvent *)event;
  myevent -> idBlue     = hwbeam().ipart1;
  myevent -> idYell     = hwbeam().ipart2;
  myevent -> process    = hwproc().iproc;
  myevent -> subprocess = 0;

  myevent -> idParton1  = -999;
  myevent -> idParton2  = -999;
  myevent -> xParton1   = hwhard().xx(1);
  myevent -> xParton2   = hwhard().xx(2);
  myevent -> xPdf1      = -999;
  myevent -> xPdf2      = -999;
  myevent -> Q2fac      = -999;
  myevent -> Q2ren      = -999;
  myevent -> valence1   = 0;
  myevent -> valence2   = 0;

  myevent -> sHat       = custom().hwmans;
  myevent -> tHat       = custom().hwmant;
  myevent -> uHat       = custom().hwmanu;
  myevent -> ptHat      = -999;
  myevent -> thetaHat   = -999;
  myevent -> phiHat     = -999;
  
  myevent -> weight     = -999;

}
