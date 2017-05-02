#include "StarPythia6.h"
ClassImp(StarPythia6);

#include "StarCallf77.h"
#include "StarGenerator/EVENT/StarGenPPEvent.h"
#include "StarGenerator/EVENT/StarGenEPEvent.h"
#include "StarGenerator/EVENT/StarGenParticle.h"

#include "StarGenerator/UTIL/StarRandom.h"
#include <map>
#include <iostream>

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
// Remap pythia's random number generator to StarRandom
extern "C" {
  Double_t pyrstar_( Int_t *idummy ){    return StarRandom::Instance().flat();  };
};
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
StarPythia6::StarPythia6( const Char_t *name ) : StarGenerator(name)
{

  // Setup a map between pythia6 status codes and the HepMC status codes
  // used in StarEvent
  mStatusCode[0] = StarGenParticle::kNull;
  mStatusCode[1] = StarGenParticle::kFinal;
  const Int_t decays[] = { 2, 3, 4, 5, 11, 12, 13, 14, 15 };
  for ( UInt_t i=0;i<sizeof(decays)/sizeof(Int_t); i++ )
    {
      mStatusCode[decays[i]]=StarGenParticle::kDecayed;
    }
  const Int_t docum[] = { 21, 31, 32, 41, 42, 51, 52 };
  for ( UInt_t i=0;i<sizeof(docum)/sizeof(Int_t); i++ )
    {
      mStatusCode[docum[i]]=StarGenParticle::kDocumentation;
    }

  _maker = this;

  regtable("PyJets_t", "pyjets", address_of_pyjets() );
  regtable("PySubs_t", "pysubs", address_of_pysubs() );
  regtable("PyDat3_t", "pydat3", address_of_pydat3() );
  regtable("PyPars_t", "pypars", address_of_pypars() );
  regtable("PyInt5_t", "pyint5", address_of_pyint5() );

}
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
void StarPythia6::PyTune( Int_t tune ){  ::PyTune(tune); }
void StarPythia6::PyStat( Int_t stat ){  ::PyStat(stat); }
void StarPythia6::PyList( Int_t list ){  ::PyList(list); }

// ----------------------------------------------------------------------------
Int_t StarPythia6::Init()
{

  //
  // Create a new event record for either pp or ep events
  //
  if ( mBlue == "proton" )                 mEvent = new StarGenPPEvent();
  else                                     mEvent = new StarGenEPEvent();

  /**
   *
   *  These particles will be decayed by geant instead of pythia 
   *
   **/
  pydat3().mdcy(102,1)=0; // PI0 111
  pydat3().mdcy(106,1)=0; // PI+ 211
  pydat3().mdcy(109,1)=0; // ETA 221
  pydat3().mdcy(116,1)=0; // K+ 321
  pydat3().mdcy(112,1)=0; // K_SHORT 310
  pydat3().mdcy(105,1)=0; // K_LONG 130
  pydat3().mdcy(164,1)=0; // LAMBDA0 3122
  pydat3().mdcy(167,1)=0; // SIGMA0 3212
  pydat3().mdcy(162,1)=0; // SIGMA- 3112
  pydat3().mdcy(169,1)=0; // SIGMA+ 3222
  pydat3().mdcy(172,1)=0; // Xi- 3312
  pydat3().mdcy(174,1)=0; // Xi0 3322
  pydat3().mdcy(176,1)=0; // OMEGA- 3334


  // Remap the ROOT names to pythia6 names
  std::map< TString, string > particle;
  particle["proton"]="p";
  particle["e-"]="e-";

  // For frames other than CMS, we need to set the beam momenta in pyjets
  if ( mFrame=="3MOM" || mFrame=="4MOM" || mFrame=="5MOM" )
    {
      for ( Int_t i=0;i<3;i++ ) pyjets().p(1,i+1) = mBlueMomentum[i];
      for ( Int_t i=0;i<3;i++ ) pyjets().p(2,i+1) = mYellMomentum[i];
    }
  if ( mFrame=="4MOM" || mFrame=="5MOM" )
    { Int_t i=3;
      pyjets().p(1,i+1) = mBlueMomentum[i];
      pyjets().p(2,i+1) = mYellMomentum[i];
    }


  // Initialize pythia
  PyInit( mFrame.Data(), particle[mBlue], particle[mYell], mRootS );

  return StMaker::Init();
}
// ----------------------------------------------------------------------------
//
// ----------------------------------------------------------------------------
Int_t StarPythia6::Generate()
{

  // Generate the event
  PyEvnt();

  // Blue beam is a proton, running PP
  if ( mBlue == "proton" )                   FillPP( mEvent );
  // Otherwise, runnin EP
  else                                       FillEP( mEvent );

  // Loop over all particles in the event
  mNumberOfParticles = pyjets().n;
  for ( Int_t idx=1; idx<=mNumberOfParticles; idx++ )
    {

      Int_t    id = pyjets().k(idx,2);
      Int_t    stat = mStatusCode[ pyjets().k(idx,1) ];
      if ( !stat ) {
	stat = StarGenParticle::kUnknown;
      }
      Int_t    m1 = pyjets().k(idx,3);
      Int_t    m2 = 0;
      Int_t    d1 = pyjets().k(idx,4);
      Int_t    d2 = pyjets().k(idx,5);
      Double_t px = pyjets().p(idx,1);
      Double_t py = pyjets().p(idx,2);
      Double_t pz = pyjets().p(idx,3);
      Double_t E  = pyjets().p(idx,4);
      Double_t M  = pyjets().p(idx,5);
      Double_t vx = pyjets().v(idx,1);
      Double_t vy = pyjets().v(idx,2);
      Double_t vz = pyjets().v(idx,3);
      Double_t vt = pyjets().v(idx,4);

      mEvent -> AddParticle( stat, id, m1, m2, d1, d2, px, py, pz, E, M, vx, vy, vz, vt );

    }

  return kStOK;
}
// ----------------------------------------------------------------------------
//
// ----------------------------------------------------------------------------
void StarPythia6::FillPP( StarGenEvent *event )
{

  // Fill the event-wise information
  StarGenPPEvent *myevent = (StarGenPPEvent *)event;

  myevent -> idBlue     = pypars().msti(11);
  myevent -> idYell     = pypars().msti(12);
  myevent -> process    = pysubs().msel;  
  myevent -> subprocess = pypars().msti(1);

  myevent -> idParton1  = pypars().msti(15);
  myevent -> idParton2  = pypars().msti(16);
  myevent -> xParton1   = pypars().pari(31);
  myevent -> xParton2   = pypars().pari(32);
  myevent -> xPdf1      = 0;
  myevent -> xPdf2      = 0;
  myevent -> Q2fac      = pypars().pari(22);
  myevent -> Q2ren      = 0.;
  myevent -> valence1   = -1;
  myevent -> valence2   = -1;

  myevent -> sHat       = pypars().pari(14);
  myevent -> tHat       = pypars().pari(15);
  myevent -> uHat       = pypars().pari(16);
  myevent -> ptHat      = pypars().pari(17);
  myevent -> thetaHat   = TMath::ACos( pypars().pari(41) );
  myevent -> phiHat     = -999;
  
  myevent -> weight     = pypars().pari(7);

}
// ----------------------------------------------------------------------------
//
// ----------------------------------------------------------------------------
void StarPythia6::FillEP( StarGenEvent *event )
{

  // Fill the event-wise information
  StarGenEPEvent *myevent = (StarGenEPEvent *)event;

  myevent -> idBlue     = pypars().msti(11);
  myevent -> idYell     = pypars().msti(12);
  myevent -> process    = pysubs().msel;
  myevent -> subprocess = pypars().msti(1);   

  // ID of the colliding parton.  If MSTI(15) isn't a parton, grab MSTI(16)
  Int_t      id = pypars().msti(15); 
  Double_t    x = pypars().pari(31);
  if ( TMath::Abs(id)>6 ) { id = pypars().msti(16); x = pypars().pari(32); }
  myevent -> idParton   = id;
  myevent -> xParton    = x;

  myevent -> xPdf       = 0;

  myevent -> Q2         = pypars().pari(22);  
  myevent -> weight     = pypars().pari(7);

}

StarGenStats StarPythia6::Stats()
{
  StarGenStats stats("Pythia6Stats","Pythia 6 Run Statistics");
  PyStat(1);

  stats.nTried         = pyint5().ngen(0,1);
  stats.nSelected      = pyint5().ngen(0,2);
  stats.nAccepted      = pyint5().ngen(0,3);
  stats.sigmaGen       = pyint5().xsec(0,3); //xsection
  // ... we may want to extend StarGenStats to add these, if they are useful ...
  //  stats.xxxxxxxx       = pyint5().xsec(0,1); //estimated maximum differential cross section
  //  stats.xxxxxxxxxxxx   = pyint5().xsec(0,2); //gives the sum of differential cross sections phase-space points evaluated so far

  stats.nFilterSeen    = stats.nAccepted;
  stats.nFilterAccept  = stats.nAccepted;

  //stats.Dump();

  // Return a copy of the class we just created
  return stats;
}

void StarPythia6::SetDecayFlag ( const int kf, const int flagw ) 
{
  int kc = PyComp( kf );
  if ( kc ) {
    pydat3().mdcy(kc,1)=0;
  }
  else {
    LOG_WARN << "Incorrect kf = " << kf << endm;
  }

}
