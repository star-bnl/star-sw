#include "StarUrQMD.h"
ClassImp(StarUrQMD);

#include "StarCallf77.h"
#include "StarGenerator/EVENT/StarGenPPEvent.h"
#include "StarGenerator/EVENT/StarGenEPEvent.h"
#include "StarGenerator/EVENT/StarGenParticle.h"

#include "StarGenerator/UTIL/StarRandom.h"
#include <map>
#include <iostream>

// ---------------------------------------------------------------------------
/// Remap UrQMD's and Pythia's random number generator to StarRandom
extern "C" {
  Double_t ranfstar_( Int_t *idummy ){    return StarRandom::Instance().flat(); };
  Double_t pyrstar_ ( Int_t *idummy ){    return StarRandom::Instance().flat(); };  
  //  StarRandom &ranf_ = StarRandom::Instance();
  //  StarRandom &pyr_  = StarRandom::Instance();
};
  
// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
StarUrQMD::StarUrQMD( const Char_t *name ) : StarGenerator(name)
{


  assert( 2+2==5 ); // UrQMD is not ready for prime time

  /// Setting up a map between UrQMD's status codes and the HepMC status codes used in StarEvent
  //JFN 11/19/12 15:50- I can't find any documentation on UrQMD status codes (or even if the status information is stored) so we are going to do this in a very general way just to clean it up for later.
  for ( UInt_t i=0; i<200; i++)
    {
      mStatusCode[i+100] = StarGenParticle::kFinal;
    }
  //JFN 11/19/12 15:53- This next bit is for reference
  /*mStatusCode[0]   = StarGenParticle::kNull;
  mStatusCode[1]   = StarGenParticle::kFinal;
  mStatusCode[2]   = StarGenParticle::kDocumentation;
  mStatusCode[3]   = StarGenParticle::kDocumentation;*/

}
// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
Int_t StarUrQMD::Init()
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
  A["He3"]=   3;    Z["He3"]=  2;
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

  stringstream Blue;
  stringstream Yell;

  Blue << A[myBlue] << " " << Z[myBlue];
  Yell << A[myYell] << " " << Z[myYell];

  InputParametersString["pro"] = Blue.str().c_str();
  InputParametersString["tar"] = Yell.str().c_str();

  stringstream impactParameters;
  impactParameters << mImpactMin << " " << mImpactMax;
  InputParametersString["IMP"] = impactParameters.str().c_str();

  //
  // Create a new event record for either pp or eo events
  //
  if ( ( mBlue == "proton" ) && ( mYell == "proton" ) )                 mEvent = new StarGenPPEvent();
  else                                                                  mEvent = new StarGenEPEvent();

  /// Remapt the ROOT names to UrQMD names
  std::map< TString, string > particle;
  /// Ex: particle["ROOT name"]="UrQMD name";
  particle["proton"] = "P       ";
  particle["e-"]     = "E-      ";

  /// Set up frames
  if ( mFrame=="COM" )
    {
      InputParametersDouble["ecm"]=mRootS;
    }
  if ( mFrame=="3MOM" || mFrame=="4MOM" || mFrame=="5MOM" )
    {
      ///JFN 11/21/12 4:11pm- I believe this calculation of mRootS from the momentum is correct, but I wouldn't stake my life on it. Additionally, I think mRootS should be something that should be calculated by the StarGenerator framework (ie, I shouldn't have to do it)
      mRootS = ( sqrt(pow(((Z[myBlue]*ProtonMass)+((A[myBlue]-Z[myBlue])*NeutronMass)),2) + sqrt( pow(mBlueMomentum.Px(),2) + pow(mBlueMomentum.Py(),2) + pow(mBlueMomentum.Pz(),2))) + sqrt(pow(((Z[myYell]*ProtonMass)+((A[myYell]-Z[myYell])*NeutronMass)),2) + sqrt( pow(mYellMomentum.Px(),2) + pow(mYellMomentum.Py(),2) + pow(mYellMomentum.Pz(),2))));
      InputParametersDouble["ecm"]=mRootS;
    }

  /// Set particles to not decay
  ///JFN 11/20/12 12:36pm- I have figured out how to set particles to not decay; they must be listed in the input file in the form "stb [itpy#]". I need to figure out the convention for ityp id numbers.
  ///JFN 11/20/12 12:46pm- See the UrQMD manual, page 12, tables 2 and 3 for particle IDs
  ///JFN 11/25/12 1:17pm- ityp doesn't seem to be completely specific for defingin particle type. Also, "antibaryons carry a negative sign", so some of these may need to be negative.
  // PI0 111
  // PI+ 211
  StableParticles.push_back("101");
  // ETA 221
  StableParticles.push_back("102");
  // K+ 321
  StableParticles.push_back("");
  // K_SHORT 310
  StableParticles.push_back("");
  // K_LONG 130
  StableParticles.push_back("");
  // LAMBDA0 3122
  StableParticles.push_back("27");
  // SIGMA0 3212
  // SIGMA- 3112
  // SIGMA+ 3222
  StableParticles.push_back("40");
  // Xi- 3312
  // Xi0 3322
  StableParticles.push_back("49");
  // OMEGA- 3334
  StableParticles.push_back("55");

  /// Initialize UrQMD:
  InitializeUrQMD();

  return StMaker::Init();
}
// ----------------------------------------------------------------------------
//
// ----------------------------------------------------------------------------
Int_t StarUrQMD::Generate()
{
  /// Generate an event
  GenerateEvent();

  // Blue beam is a proton, running PP
  if ( ( mBlue == "proton" ) && ( mYell == "proton" ) )    FillPP( mEvent );
  // Otherwise, runnin EP
  else                                                     FillEP( mEvent );

  //Do Stuff with the particles
  mNumberOfParticles = 1;
  //mNumberOfParticles = isys().ncoll(1); //JFN 11/28/12- this is wrong
  for ( Int_t idx=1; idx<=mNumberOfParticles; idx++ )
    {

      Int_t    id = isys().uid(idx); // or isys().itypd(idx). It isn't clear which is right
      Int_t    stat = StarGenParticle::kFinal; //JFN 11/25/12 12:28pm- for the moment I am setting every status to be a final state particle.
      //Int_t    stat = mStatusCode[ hepevt().isthep(idx) ]; */@
      //    if ( !stat ) {
      //	stat = StarGenParticle::kUnknown;
      //    }
      Int_t    m1 = itdelay().ityptd(idx,1);
      Int_t    m2 = itdelay().ityptd(idx,2);
      Int_t    d1 = 0; //JFN- I don't think daughter information is preserved
      Int_t    d2 = 0;
      Double_t px = coor().px(idx);
      Double_t py = coor().py(idx);
      Double_t pz = coor().pz(idx);
      Double_t E  = coor().p0(idx);
      Double_t M  = coor().fmass(idx);
      Double_t vx = px/M;
      Double_t vy = py/M;
      Double_t vz = pz/M;
      Double_t vt = sqrt(E*2/M); //E=m*v^2/2, v=sqrt(E*2/M)

      mEvent -> AddParticle( stat, id, m1, m2, d1, d2, px, py, pz, E, M, vx, vy, vz, vt );
    }

  return kStOK;
}
// ----------------------------------------------------------------------------
//
// ----------------------------------------------------------------------------
///JFN 11/18/12 13:24 - I think having a clear function is optional, and UrQMD doesn't have any explicit cleanup. Although, we could toss the un-needed output files here.
/*Int_t StarUrQMD::Clear()
{
  return kStOK;
}*/
// ----------------------------------------------------------------------------
//
// ----------------------------------------------------------------------------
void StarUrQMD::FillPP( StarGenEvent *event)
{

  // Fill the event-wise information
  StarGenPPEvent *myevent = (StarGenPPEvent *)event;
  myevent -> idBlue     = 0;
  myevent -> idYell     = 0;
  myevent -> process    = 0;
  /*myevent -> idBlue     = hwbeam().ipart1; // Int //JFN 11/26/12- "Blue beam ID". I don't know the ID convention
  myevent -> idYell     = hwbeam().ipart2;
  myevent -> process    = hwproc().iproc; //JFN 11/26/12- in principle there are process and subprocess ids because they get written in the headers of the output files, but I cant figure out where they are stored*/
  myevent -> subprocess = 0;

  myevent -> idParton1  = -999;
  myevent -> idParton2  = -999;
  myevent -> xParton1   = 0;
  myevent -> xParton2   = 0;
  myevent -> xPdf1      = -999;
  myevent -> xPdf2      = -999;
  myevent -> Q2fac      = -999;
  myevent -> Q2ren      = -999;
  myevent -> valence1   = 0;
  myevent -> valence2   = 0;

  myevent -> sHat       = 0;
  myevent -> tHat       = 0;
  myevent -> uHat       = 0;
  myevent -> ptHat      = -999;
  myevent -> thetaHat   = -999;
  myevent -> phiHat     = -999;
  
  myevent -> weight     = -999;

}
// ----------------------------------------------------------------------------
//
// ----------------------------------------------------------------------------
void StarUrQMD::FillEP( StarGenEvent *event)
{
    ///JFN 11/25/12 12:31pm- when FillPP is done, just copy the contents in here.
}
// ----------------------------------------------------------------------------
//
// ----------------------------------------------------------------------------
///JFN 11/19/12 7:54am- to initialize UrQMD we first have to create input files for UrQMD to read and set enviormental variables, then we call UrQMD's init function
///JFN 11/19/12 7:37pm- In hindsight, I think I might rather have these functions defined in StarUrQMD.cxx.... maybe
///JFN 11/20/12 12:09pm- I have moved these two function (InitializeUrQMD and GenerateEvent) to StarUrQMD.cxx
void StarUrQMD::InitializeUrQMD()
  {
    /// Set the enviormental variables
    ///JFN 11/19/12 7:55am- Honestly, it pains me to set enviormental variables just to run this. I need to check if there is a way I can just set common block variables
    ///JFN 11/19/12 7:38pm- I would like to be able to just define the name on the input output files....
    ///JFN 11/21/12 5:19pm- I have found where the enviormental variables are loaded for UrQMD: line 359 of input.F. I see no easy way to spoof this behavior without modifying the source code, which I am trying to keep to an absolute minimum,
    //SetEnVars();

    /// Print the input file
    std::ofstream inputfile;
    inputfile.open( "UrQMD.in" );
    for(map<TString,Int_t>::iterator i = InputParametersInt.begin(); i != InputParametersInt.end(); i++)
      { 
        inputfile << i->first << " " << i->second << endl;
      }
    for(map<TString,Double_t>::iterator i = InputParametersDouble.begin(); i != InputParametersDouble.end(); i++)
      { 
        inputfile << i->first << " " << i->second << endl;
      }
    for(map<TString,TString>::iterator i = InputParametersString.begin(); i != InputParametersString.end(); i++)
      { 
        inputfile << i->first << " " << i->second << endl;
      }
    /// Set particles to not decay
    for(unsigned int i = 0; i< StableParticles.size(); i++)
      {
        inputfile << "stb " << StableParticles[i] << endl;
      }
    /// Define calculation time: total time span, interval at which output is written (both in fm/c)
    inputfile << "tim 200 200" << endl;
    /// This supresses the output files
    inputfile << "f13 \n f14 \n f15 \n f16 \n f17 \n f18 \n f19 \n f20" << endl;
    /// This sets the number of events for UrQMD to run (but we aren't using this, so it doens't matter)
    inputfile << "nev 1000" << endl;
    /// This sets the random number generator seed (but we aren't using UrQMD's random number generator, so this doens't matter)
    inputfile << "rsd 111" << endl;
    /// This marks the end of the input file
    inputfile << "xxx and done" << endl;
    inputfile.close();

    /// Initialize UrQMD
    iurqmd();
  }
// ----------------------------------------------------------------------------
//
// ----------------------------------------------------------------------------
///JFN 11/19/12 7:57am- I think we can grab the event information right from the common blocks, so there may be no need to process anything, or for this function
///JFN 11/21/12 5:28pm- just for reference, the first calls for setting up output files in RunFunctin.F is at line 169. Final output is done at line 408.
/*void GenerateEvent()
  {
    /// Call the UrQMD function to make a new event
    genevt();

    /// Process the results
    //ProcessEvent();
  }*/
