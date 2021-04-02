#include "StarDpmjet3.h"
#include "StarCallf77.h"
#include "StarGenerator/EVENT/StarGenAAEvent.h"
#include "StarGenerator/EVENT/StarGenParticle.h"
#include "StarGenerator/UTIL/StarRandom.h"
#include <map>
#include <iostream>
#include "TGenericTable.h"
StMaker *_maker = 0;

TGenericTable *regtable( const Char_t *type, const Char_t *name, void *address ){
  TGenericTable *table = new TGenericTable(type,name);
  table->Adopt( 1, address );
  _maker -> AddData( table, ".const" );
  return table;
};

// ----------------------------------------------------------------------------
#if 0
// Remap random number generator to StarRandom
extern "C" {
  Double_t star_rndm_( Int_t *idummy ){ return StarRandom::Instance().flat();  };
};
#endif
ClassImp(StarDpmjet3);

// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
StarDpmjet3::StarDpmjet3( const Char_t *name ) : StarGenerator(name)
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
  mStatusCode[  -1] = StarGenParticle::kFinal;   //nucleons, deuterons, H-3, He-3, He-4 evaporated 
  mStatusCode[1000] = StarGenParticle::kUnknown; //residual nucleus (ground state)
  mStatusCode[1001] = StarGenParticle::kUnknown; //residual nucleus (ground state)

  _maker = this;

  regtable("DtEvt1_t", "dtevt1", address_of_dtevt1() );
  regtable("DtGlcp_t", "dtglcp", address_of_dtglcp() );
}

// ----------------------------------------------------------------------------
Int_t StarDpmjet3::Init(){
  mEvent = new StarGenAAEvent();

  // Initialize DPMJET3
  DpmjetInit(mBeam,mEnergy);

  return StMaker::Init();
}
// ----------------------------------------------------------------------------
//
// ----------------------------------------------------------------------------
Int_t StarDpmjet3::Generate(){
  // Generate the event
  DpmjetAAEvent();

  //eventwise info
  StarGenAAEvent* aaevt = (StarGenAAEvent*)mEvent;
  aaevt->impactParameter =  dtglcp().BIMPAC;

  // Loop over all particles in the event
  mNumberOfParticles = dtevt1().NHKK;
  for(int i=1; i<=mNumberOfParticles; i++){      
      int id = dtevt1().id(i);
      int st = dtevt1().st(i); 
      float m= dtevt1().p(i,5);
      if(id==80000){ //fragments
	  if     (fabs(m-1.8756)<0.0001) {id=1000010020;} //Deuteron (GID=45)
	  else if(fabs(m-2.8093)<0.0001) {id=1000010030;} //Tritium  (GID=46)
	  else if(fabs(m-2.8089)<0.0001) {id=1000020030;} //He3      (GID=49)
	  else if(fabs(m-3.7274)<0.0001) {id=1000020020;} //Alpha    (GID=47)
      }
      Int_t    stat = mStatusCode[st];
      if ( !stat ) {
	  stat = StarGenParticle::kUnknown;
      }      
      Int_t    m1 = dtevt1().mo(i,1);
      Int_t    m2 = dtevt1().mo(i,2);
      Int_t    d1 = dtevt1().da(i,1);
      Int_t    d2 = dtevt1().da(i,2);
      Double_t px = dtevt1().p(i,1);
      Double_t py = dtevt1().p(i,2);
      Double_t pz = dtevt1().p(i,3);
      Double_t E  = dtevt1().p(i,4);
      Double_t M  = dtevt1().p(i,5);
      Double_t vx = dtevt1().v(i,1);
      Double_t vy = dtevt1().v(i,2);
      Double_t vz = dtevt1().v(i,3);
      Double_t vt = dtevt1().v(i,4);
      mEvent->AddParticle( stat, id, m1, m2, d1, d2, px, py, pz, E, M, vx, vy, vz, vt );
  }
  return kStOK;
}
