#include <assert.h>
#include <vector>
#include <map>
#include "Riostream.h"
#include "StarParticleReader.h"
#include "TDatabasePDG.h"
#include "TLorentzVector.h"
#include "TGeant3.h"
#include "StarGenerator/BASE/StarParticleStack.h"
#include "StarGenerator/UTIL/StarParticleData.h"
using namespace std;

StarParticleReader *StarParticleReader::mInstance = 0;
ClassImp(StarParticleReader);

// ----------------------------------------------------------------------------------------------------
StarParticleReader::StarParticleReader() : TObject(), mStack(0), mParticleData(0)
{ 
  mParticleData = &StarParticleData::instance();
}
// ----------------------------------------------------------------------------------------------------
//
// ----------------------------------------------------------------------------------------------------
StarParticleReader &StarParticleReader::Instance()
{
  if ( !mInstance ) mInstance = new StarParticleReader();
  return (*mInstance);
}
// ----------------------------------------------------------------------------------------------------
//
// ----------------------------------------------------------------------------------------------------
void StarParticleReader::ReadEvent()
{
  
  TGeant3 *geant3 = (TGeant3 *) TVirtualMC::GetMC();

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
#if 0
  if (_debug) {
    nv = 0; 
    geant3->Gpvert(nv);
    geant3->Gpkine(nv);
  }
#endif
}
// ----------------------------------------------------------------------------------------------------
//
// ----------------------------------------------------------------------------------------------------
void StarParticleReader::SetVert( Float_t *vertex, Int_t ntbeam, Int_t nttarg, Float_t *ubuf, Int_t nu, Int_t &nv )
{
  /* call */ /*  agsvert( vertex, &ntbeam, &nttarg, ubuf, &nu, &nv ); */
}
// ----------------------------------------------------------------------------------------------------
//
// ----------------------------------------------------------------------------------------------------
void StarParticleReader::SetKine( Float_t *plab,   Int_t idpart, Int_t nv,     Float_t *ubuf, Int_t nb, Int_t &nt )
{
  /* call */ /* agskine( plab, &idpart, &nv, ubuf, &nb, &nt ); */
}
// ----------------------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------------------
