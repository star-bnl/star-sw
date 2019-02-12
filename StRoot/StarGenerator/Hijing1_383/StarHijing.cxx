
#include "StarHijing.h"
ClassImp(StarHijing);

#include "StarCallf77.h"
#include "StarGenerator/EVENT/StarGenAAEvent.h"
#include "StarGenerator/EVENT/StarGenParticle.h"

#include "StarGenerator/UTIL/StarRandom.h"
#include "TString.h"

#include <map>
#include <iostream>
using namespace std;

#include "TGenericTable.h"

StMaker *_maker = 0;

TGenericTable *regtable( const Char_t *type, const Char_t *name, void *address )
{
  TGenericTable *table = new TGenericTable(type,name);
  table->Adopt( 1, address );
  _maker -> AddData( table, ".const" );
  return table;
};

// ----------------------------------------------------------------------------
// Remap hijing's random number generator to StarRandom
extern "C" {
  float rlustar_( Int_t *idummy ){    return StarRandom::Instance().flat();  };
  float rndmstar_( Int_t *idummy ){    return StarRandom::Instance().flat();  };
};
Double_t rndm(){ return StarRandom::Instance().flat(); }
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
StarHijing::StarHijing( const Char_t *name ) : StarGenerator(name)
{

  _maker = this;

  // Register configuration commons
  regtable("HiParnt_t", "hiparnt", (void *)address_of_hiparnt() );
  regtable("HiMain1_t", "himain1", (void *)address_of_himain1() );
  //regtable("HiMain2_t", "himain2", (void *)address_of_himain2() ); // Probably too big to be useful
  regtable("Ludat3_t",  "ludat3",  (void *)address_of_hiparnt() );

  // Setup a map between HIJING status codes and HepMC status codes
  // katt(i,4)... all other codes should map to kUnknown
  mStatusCode[1]  = StarGenParticle::kFinal;
  mStatusCode[11] = StarGenParticle::kDecayed;


  // Mapping between jetset particle IDs and PDG particle IDs
  mParticleCode[ 10551 ] = 551;
  mParticleCode[ 20443 ] = 10443;
  mParticleCode[ 30443 ] = 20443;
  mParticleCode[ 30553 ] = 20553;
  mParticleCode[  4232 ] = 4332;
  mParticleCode[  4332 ] = 4232;

}

Int_t StarHijing::pdgid( const Int_t &jetid )
{
  // Unknown particles
  if ( jetid ==   551 ) return 0;
  if ( jetid == 10443 ) return 0;

  // K0/K0bar = 0.5 K0 short + 0.5 K0 long
  if ( jetid==311 || jetid==-311 )
    {
      if ( rndm() > 0.5 ) return 130; // K0 long
      else                return 310; // K0 short
    }

  int id = mParticleCode[jetid];



  // Return ID if it was found in the table
  if ( id )
    return id;

  // If the particle isn't in the table, it corresponds to the PDG code
  return jetid;
}



Int_t StarHijing::LuComp( Int_t jetsetid )
{
  return Lucomp( jetsetid );
};


Int_t StarHijing::Init()
{

  mEvent = new StarGenAAEvent();

  // Number of spectators
  // Initialized per event
  for ( Int_t i=0; i<2; i++ ){
    mNumberOfSpectatorProtons[i]=0;
    mNumberOfSpectatorNeutrons[i]=0;
  }

   /**
   *
   *  These particles will be decayed by geant instead of hijing
   *
   **/ 
#define STABLE(x) ludat3().mdcy( Lucomp( x ), 1 ) = 0
  STABLE( 111 );
  STABLE( 221 );
  STABLE( 3122 );
  STABLE( 3212 );
  STABLE( 3112 );
  STABLE( 3222 );
  STABLE( 3312 );
  STABLE( 3322 );
  STABLE( 3334 );
#undef STABLE





  /*
  ludat3().mdcy(102,1)=0; // PI0 111
  ludat3().mdcy(109,1)=0; // ETA 221
  ludat3().mdcy(164,1)=0; // LAMBDA0 3122
  ludat3().mdcy(167,1)=0; // SIGMA0 3212
  ludat3().mdcy(162,1)=0; // SIGMA- 3112
  ludat3().mdcy(169,1)=0; // SIGMA+ 3222
  ludat3().mdcy(172,1)=0; // Xi- 3312
  ludat3().mdcy(174,1)=0; // Xi0 3322
  ludat3().mdcy(176,1)=0; // OMEGA- 3334
  */
  // Double check indexing here
  /*
  ludat3().mdcy(106,1)=0; // PI+ 211   (not decayed anyhow)
  ludat3().mdcy(112,1)=1; // K_SHORT 310   ... decay these ...
  ludat3().mdcy(105,1)=1; // K_LONG 130
  ludat3().mdcy(116,1)=0; // K+ 321
  */

 
  //
  // Check the frame.  Only CMS is supported at this time
  //
  if (!( mFrame == "CMS" || mFrame =="FIXT"))
    {
      cout << "StarHijing: Only CMS / FIXT frame supported for now.  Kill me now." << endl;
    }

  ////////////////////////////////////
  // Initialize the hijing
  ///////////////////////////////////
  
  // Map typical species run at RHIC
  map<TString,Int_t> A, Z;  map<TString,string> type;
  A["p"]  =1;    Z["p"]  =1;   type["p"]  ="P       "; 
  A["n"]  =1;    Z["n"]  =0;   type["n"]  ="N       ";
  A["d"]  =2;    Z["d"]  =1;   type["d"]  ="A       ";
  A["He3"]=3;    Z["He3"]=2;   type["He3"]="A       ";


  A["Au"]=197;  Z["Au"]=79;  type["Au"]="A       ";
  A["Cu"]=63;   Z["Cu"]=29;  type["Cu"]="A       ";
  A["U"] =238;  Z["U"]=92;   type["U"] ="A       ";
  A["Al"]=27;   Z["Al"]=13;  type["Al"]="A       ";

  A["proton"]   =1;    Z["proton"]   =1;   type["proton"]   ="P       "; // important to map size of type onto character*8
  A["neutron"]  =1;    Z["neutron"]  =0;   type["neutron"]  ="N       ";
  A["deuteron"] =2;    Z["deuteron"] =1;   type["deuteron"] ="A       ";

  A["Zr96"]= 96;  Z["Zr96"]=40;  type["Zr96"]="A       ";
  A["Ru96"]= 96;  Z["Ru96"]=44;  type["Ru96"]="A       ";

  hiparnt().ihpr2(12) = 1; // 0=particle decays on 1=off

  string frame = mFrame.Data();
  if(frame =="FIXT") frame="LAB";

  float  roots = TMath::Abs( mRootS );
  Hijset( roots, frame, type[mBlue], type[mYell], A[mBlue], Z[mBlue], A[mYell], Z[mYell] );

  mNumberOfBeamProtons[0]=Z[mBlue];
  mNumberOfBeamProtons[1]=Z[mYell];
  mNumberOfBeamNeutrons[0]=A[mBlue]-mNumberOfBeamProtons[0];
  mNumberOfBeamNeutrons[1]=A[mYell]-mNumberOfBeamProtons[1];

  //
  // Grab the computed maximum impact parameter for the collision and report it.  Test
  // against user's impact parameter and report if it's too large, and truncate.
  //
  Float_t bmax = hiparnt().hipr1(34)+hiparnt().hipr1(35);
  if ( mImpactMax < 0 )
    {
      mImpactMax = bmax;
    }
  cout << Form("HIJING impact parametr allowed range is 0 .. %f fm", bmax) << endl;
  if ( mImpactMax > bmax )
    {
      cout << Form("     >>> user's value too large, truncating to %f fm <<< ", bmax) << endl;
      mImpactMax = bmax;
    }
  cout <<      "------------------------------------------------------------------------------" << endl;
  cout <<      "------------------------------------------------------------------------------" << endl;
  cout << Form("HIJING events will be generated with impact parameter %f .. %f fm", mImpactMin, mImpactMax ) << endl;
  cout <<      "------------------------------------------------------------------------------" << endl;
  cout <<      "------------------------------------------------------------------------------" << endl;
  //
  // PDG id for heavy ions
  //
  //if ( mBlue != "p" && mBlue != "n" )  mBlueId = 10 * A[mBlue] + 10000 * Z[mBlue] + 1000000000;
  //if ( mYell != "p" && mYell != "n" )  mBlueId = 10 * A[mYell] + 10000 * Z[mYell] + 1000000000;

  //
  // Keep all information for all particles, even those which decay
  //
  hiparnt().ihpr2(21)=1;
  hiparnt().ihpr2(10)=1; // show error msgs

  return StMaker::Init();

}
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
extern "C" {
  void hijing_( const char *frame, float &bmin, float &bmax, int sframe );
}

Int_t StarHijing::Generate()
{


  cout << "-----------------> Generate() <--------------------" << endl;

  // Generate one hijing event
  {
    //    string frame = mframe;

    string frame = "CMS     ";

    if(mFrame == "FIXT")
      frame="LAB     ";

    cout << mFrame.Data() << " " << frame.data() << endl;

    Float_t bmin = mImpactMin;
    Float_t bmax = mImpactMax;
    //    Hijing( frame, bmin, bmax );
    hijing_( frame.c_str(), bmin, bmax, frame.size() );
  }


  // Number of spectators
  // Initialized per event
  //
  for ( Int_t i=0; i<2; i++ ){
    mNumberOfSpectatorProtons[i]=0;
    mNumberOfSpectatorNeutrons[i]=0;
  }

  //
  // Loop over all particles in the event
  //
  mNumberOfParticles = himain1().natt;
  StarGenParticle *particles[ mNumberOfParticles + 1]; // temporary list of particles [1<=idx<=mNumber]
  StarGenParticle *current = 0;
  for ( Int_t idx=1; idx<=mNumberOfParticles; idx++ )
    {

      Int_t jsid = himain2().katt(idx, 1); // this is jetset 7.2 id
      Int_t id = ( pdgid(jsid) );

      Int_t stat      = himain2().katt(idx, 4);
      if ( !stat ) {
	stat = StarGenParticle::kUnknown;
      }
      Int_t m1 = himain2().katt(idx, 3);
      Int_t m2 = -1;
      Int_t d1 = -1; // daughters will be set below
      Int_t d2 = -1; // 
      Double_t px = himain2().patt(idx, 1);
      Double_t py = himain2().patt(idx, 2);
      Double_t pz = himain2().patt(idx, 3);  pz *= mDirect;
      Double_t E  = himain2().patt(idx, 4);
      Double_t M  = Ulmass(jsid); // Need to lookup mass here... from hijing library ... ulmass in hipyset
      Double_t vx = himain2().vatt(idx, 1);
      Double_t vy = himain2().vatt(idx, 2);
      Double_t vz = himain2().vatt(idx, 3);
      Double_t vt = himain2().vatt(idx, 4);

      particles[idx] = mEvent -> AddParticle( stat, id, m1, m2, d1, d2, px, py, pz, E, M, vx, vy, vz, vt );
      if ( m1 > 0 ) { // 
	current = particles[m1]; 
	assert(current);
	// Set first daughter if it hasn't been set
	if (  -1 == current->GetFirstDaughter() ) current->SetFirstDaughter(idx);
	// Set last daughter if it's larger than the current idx
	if ( idx  > current->GetLastDaughter()  ) current->SetLastDaughter(idx);
      }
	

      // count spectators
      Int_t code = himain2().katt(idx, 2 );

      if ( code == 0 || code == 1 )   // blue beam spectator
	{
	  if ( id == 2212 ) mNumberOfSpectatorProtons[0]++;
	  if ( id == 2112 ) mNumberOfSpectatorNeutrons[0]++;
	}

      if ( code == 10 || code == 11 ) // yellow beam spectator
	{
	  if ( id == 2212 ) mNumberOfSpectatorProtons[1]++;
	  if ( id == 2112 ) mNumberOfSpectatorNeutrons[1]++;
	}

    }



  FillAA(mEvent);

  return kStOK;
  
}
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
void StarHijing::FillAA( StarGenEvent *_event )
{

  StarGenAAEvent *event = (StarGenAAEvent *)_event;
  
  event -> process    = -999;
  event -> subprocess = -999;
  event -> idParton1  = hiparnt().ihnt2(9);
  event -> idParton2  = hiparnt().ihnt2(10);
  event -> xParton1   = -999;
  event -> xParton2   = -999;
  event -> xPdf1      = -999;
  event -> xPdf2      = -999;
  event -> Q2fac      = -999;
  event -> Q2ren      = -999;
  event -> valence1   = 0;
  event -> valence2   = 0;
  event -> sHat       = -999;
  event -> tHat       = -999;
  event -> uHat       = -999;
  event -> ptHat      = -999;
  event -> thetaHat   = -999;
  event -> phiHat     = -999;

  event -> impactParameter = hiparnt().hint1(19); // in fm
  event -> reactionPlane   = hiparnt().hint1(20); // in rad
  
  // Number of particpant protons is Z - n spectators
  // Number of particpant neutrons is ( A - Z ) - nspectators

  event -> numberOfBinary = himain1().n0;  
  event -> numberOfParticipantNeutrons[0] = mNumberOfBeamNeutrons[0] - mNumberOfSpectatorNeutrons[0];
  event -> numberOfParticipantNeutrons[1] = mNumberOfBeamNeutrons[1] - mNumberOfSpectatorNeutrons[1];
  event -> numberOfParticipantProtons[0]  = mNumberOfBeamProtons[0]  - mNumberOfSpectatorProtons[0];
  event -> numberOfParticipantProtons[1]  = mNumberOfBeamProtons[1]  - mNumberOfSpectatorProtons[1];
  event -> numberRejected = -999;
  event -> numberWounded[0] = himain1().nwounded_blue;
  event -> numberWounded[1] = himain1().nwounded_yell;
  event -> numberOfJets     = himain1().jatt;

  event -> weight = 1.0;

}
