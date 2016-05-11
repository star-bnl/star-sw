#include "AgStarReader.h"
ClassImp(AgStarReader);

#include "StarCallf77.h"
#include "TDatabasePDG.h"
#include "TParticlePDG.h"

#include "St_geant_Maker/St_geant_Maker.h"
#include "TLorentzVector.h"
#include <vector>
#include <map>
using namespace std;

#include "TGiant3.h"

#include "StarGenerator/UTIL/StarParticleData.h"

AgStarReader *AgStarReader::mInstance = 0;

// TODO: Refactor TDatabasePDG to StarParticleData 

//
// Setup the interface with starsim
//
#define agusread F77_NAME(agusread,AGUSREAD)
#define agsvert  F77_NAME(agsvert, AGSVERT)
#define agskine  F77_NAME(agskine, AGSKINE)

extern "C" {
  void type_of_call agusread() {    AgStarReader::Instance().ReadEvent();  }
  void type_of_call agsvert( Float_t *vertex, Int_t *nb, Int_t *nt, Float_t *ubuf, Int_t *nu, Int_t *nv );
  void type_of_call agskine( Float_t *plab,   Int_t *ip, Int_t *nv, Float_t *ubuf, Int_t *nb, Int_t *nt );
};

// ----------------------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------------------
AgStarReader::AgStarReader() : TObject(), mStack(0), mParticleData(0)
{ 
  mParticleData = &StarParticleData::instance();
}
// ----------------------------------------------------------------------------------------------------
//
// ----------------------------------------------------------------------------------------------------
AgStarReader &AgStarReader::Instance()
{
  if ( !mInstance ) mInstance = new AgStarReader();
  return (*mInstance);
}
// ----------------------------------------------------------------------------------------------------
//
// ----------------------------------------------------------------------------------------------------
void AgStarReader::ReadEvent()
{
  
  TGiant3 *geant3 = St_geant_Maker::instance()->Geant3();

  TParticle *part = 0;    // particle 
  Int_t      itrk = -1;   // track number
  static Int_t _debug = 0;

  struct Vertex_t {
    Double_t x, y, z, t;
    Int_t idx;
  };

  Int_t idvtx = 0;
  map<Int_t, Int_t> start_vtx;
  Float_t ubuf[1] = {0};
  Int_t nb = 0;
  Int_t nu = 0;
  Int_t nv = 0;
  Int_t nt = 0;
  Int_t nin = 0;
  Int_t nvold = -1;
  
  while(  (part=mStack->PopNextTrack(itrk)) )
    {

      // First check that the status of the particle is stable.  If not,
      // continue on to the next particle.
      if ( part->GetStatusCode() != 1 ) continue;

#if 0
      // Get the parent particle and lookup the vertex id
      Int_t parent = part->GetFirstMother();
      Int_t myvtx  = start_vtx[parent];
      if ( !myvtx )
	{
	  start_vtx[parent]=++idvtx;

	  // Build the vertex
	  Float_t v[] = {
	    Float_t(part->Vx())  ,
	    Float_t(part->Vy())  ,
	    Float_t(part->Vz())
	  };

	  myvtx = geant3->Gsvert( v, 0, 0 );
	  assert(myvtx==idvtx);

	}
#else
      nin++;
      if (_debug) {cout << nin << "\t"; part->Print();}
	  Float_t v[4] = {
	    Float_t(part->Vx())  ,
	    Float_t(part->Vy())  ,
	    Float_t(part->Vz())  ,
	    0
	  };
	  SetVert(v, 0, 0, ubuf, nu, nv);
#endif
      // Now connect the particle to the vertex
      Float_t plab[] = { 
	Float_t(part->Px())  ,
	Float_t(part->Py())  ,
	Float_t(part->Pz())
      };
      Int_t   ipdg    = part->GetPdgCode();
      Int_t   g3id    = 0;
      {
	TParticlePDG *pdg = mParticleData -> GetParticle( ipdg );     assert(pdg);
	g3id = pdg->TrackingCode();
	if ( g3id < 1 )
	  {
	    Warning(GetName(),Form("Particle %s with PDG id=%i has no G3 code.  Skipped.",pdg->GetName(),ipdg));
	  }
      }
#if 0
      geant3->Gskine( plab, g3id, myvtx );
#else
      SetKine(plab, g3id, nv, ubuf, nb, nt);
#endif
    }
  if (_debug) {
    nv = 0; 
    geant3->Gpvert(nv);
    geant3->Gpkine(nv);
  }
}
// ----------------------------------------------------------------------------------------------------
//
// ----------------------------------------------------------------------------------------------------
void AgStarReader::SetVert( Float_t *vertex, Int_t ntbeam, Int_t nttarg, Float_t *ubuf, Int_t nu, Int_t &nv )
{
  /* call */ agsvert( vertex, &ntbeam, &nttarg, ubuf, &nu, &nv );
}
// ----------------------------------------------------------------------------------------------------
//
// ----------------------------------------------------------------------------------------------------
void AgStarReader::SetKine( Float_t *plab,   Int_t idpart, Int_t nv,     Float_t *ubuf, Int_t nb, Int_t &nt )
{
  /* call */ agskine( plab, &idpart, &nv, ubuf, &nb, &nt );
}
// ----------------------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------------------
