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
  void type_of_call agsvert( float *vertex, int *nb, int *nt, float *ubuf, int *nu, int *nv );
  void type_of_call agskine( float *plab,   int *ip, int *nv, float *ubuf, int *nb, int *nt );
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
  int      itrk = -1;   // track number


  struct Vertex_t {
    Double_t x, y, z, t;
    int idx;
  };

  int idvtx = 0;
  map<int, int> start_vtx;

  while(  (part=mStack->PopNextTrack(itrk)) )
    {

      // First check that the status of the particle is stable.  If not,
      // continue on to the next particle.
      if ( part->GetStatusCode() != 1 ) continue;

      //      part->Print();

      // Get the parent particle and lookup the vertex id
      int parent = part->GetFirstMother();
      int myvtx  = start_vtx[parent];
      if ( !myvtx )
	{
	  start_vtx[parent]=++idvtx;

	  // Build the vertex
	  float v[] = {
	    float(part->Vx())  ,
	    float(part->Vy())  ,
	    float(part->Vz())
	  };

	  myvtx = geant3->Gsvert( v, 0, 0 );
	  assert(myvtx==idvtx);

	  // Set time of flight
	  geant3->Gctrak()->tofg = part->T();

	}

      // Now connect the particle to the vertex
      float plab[] = { 
	float(part->Px())  ,
	float(part->Py())  ,
	float(part->Pz())
      };

      int   ipdg    = part->GetPdgCode();
      int   g3id    = 0;
      {
	TParticlePDG *pdg = mParticleData -> GetParticle( ipdg );     assert(pdg);
	g3id = pdg->TrackingCode();
	if ( g3id < 1 )
	  {
	    Warning(GetName(),Form("Particle %s with PDG id=%i has no G3 code.  Skipped.",pdg->GetName(),ipdg));
	  }
      }


      geant3->Gskine( plab, g3id, myvtx );

    }

}
// ----------------------------------------------------------------------------------------------------
//
// ----------------------------------------------------------------------------------------------------
void AgStarReader::SetVert( float *vertex, int ntbeam, int nttarg, float *ubuf, int nu, int &nv )
{
  /* call */ agsvert( vertex, &ntbeam, &nttarg, ubuf, &nu, &nv );
}
// ----------------------------------------------------------------------------------------------------
//
// ----------------------------------------------------------------------------------------------------
void AgStarReader::SetKine( float *plab,   int idpart, int nv,     float *ubuf, int nb, int &nt )
{
  /* call */ agskine( plab, &idpart, &nv, ubuf, &nb, &nt );
}
// ----------------------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------------------
