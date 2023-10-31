#include "AgStarReader.h"
ClassImp(AgStarReader);

#include "StarCallf77.h"
#include "TDatabasePDG.h"
#include "TParticlePDG.h"

#include "TLorentzVector.h"
#include <vector>
#include <map>
using namespace std;

#include <cassert>

#include "StarGenerator/UTIL/StarParticleData.h"
#include "StarGenerator/BASE/StarParticleStack.h"

AgStarReader *AgStarReader::mInstance = 0;

// TODO: Refactor TDatabasePDG to StarParticleData 

//
// Setup the interface with starsim and direct to geant3
//
#define ageventread F77_NAME(ageventread,AGEVENTREAD)
#define agsvert  F77_NAME(agsvert, AGSVERT)
#define agskine  F77_NAME(agskine, AGSKINE)
#define  gskine	 F77_NAME(gskine,  GSKINE)
#define  gsvert	 F77_NAME(gsvert,  GSVERT)

// Comis routine for obtaining the address of common blocks
#define  gcomad	 F77_NAME(gcomad,GCOMAD)

extern "C" {
  void type_of_call ageventread() {    AgStarReader::Instance().ReadEvent();  }
  void type_of_call agsvert( float *vertex, int *nb, int *nt, float *ubuf, int *nu, int *nv );
  void type_of_call agskine( float *plab,   int *ip, int *nv, float *ubuf, int *nb, int *nt );
  void type_of_call  gsvert( float *, int &, int &, float *, int &, int &); 
  void type_of_call  gskine( float *, int &, int &, float *, int &, int &); 
  void type_of_call gcomad(DEFCHARD, int*& DEFCHARL); 
};

//
//----------GCTRAK ... normally I would rather poke my eye out rather than hardcode 
// this... but this structure has existed w/in GEANT3 unchanged for 3 decades or so.
// Think we can get away with this here...
//
#define MAXMEC 30 
typedef struct { 
  float  vect[7]; 
  float  getot; 
  float  gekin; 
  int    vout[7]; 
  int    nmec; 
  int    lmec[MAXMEC]; 
  int    namec[MAXMEC]; 
  int    nstep; 
  int    maxnst; 
  float  destep; 
  float  destel; 
  float  safety; 
  float  sleng; 
  float  step; 
  float  snext; 
  float  sfield; 
  float  tofg; 
  float  gekrat; 
  float  upwght; 
  int    ignext; 
  int    inwvol; 
  int    istop; 
  int    igauto; 
  int    iekbin; 
  int    ilosl; 
  int    imull; 
  int    ingoto; 
  int    nldown; 
  int    nlevin; 
  int    nlsav; 
  int    istory; 
} Gctrak_t; 

Gctrak_t* fGctrak = 0;


// ----------------------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------------------
AgStarReader::AgStarReader() : TObject(), mStack(0), mParticleData(&StarParticleData::instance())
{ 

}
// ----------------------------------------------------------------------------------------------------
//
// ----------------------------------------------------------------------------------------------------
AgStarReader &AgStarReader::Instance()
{
  if ( !mInstance ) {
    mInstance = new AgStarReader();
    gcomad(PASSCHARD("GCTRAK"),(int*&) fGctrak  PASSCHARL("GCTRAK"));    
  }
  return (*mInstance);
}
// ----------------------------------------------------------------------------------------------------
//
// ----------------------------------------------------------------------------------------------------
void AgStarReader::ReadEvent()
{
  
  TParticle *part = 0;    // particle 
  int      itrk = -1;   // track number

  struct Vertex_t {
    double x, y, z, t;
    int idx;
  };

  int idvtx = 0;
  map<int, int> start_vtx;

  float* ubuf = 0;
  int nwbuf=0;

  while(  (part=mStack->PopNextTrack(itrk)) )
    {

      // First check that the status of the particle is stable.  If not,
      // continue on to the next particle.
      if ( part->GetStatusCode() != 1 ) continue;

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

	  // Set the vertex of the track
	  {
	    int nt = 0;
	    int nb = 0;
	    gsvert(v, nt, nb, ubuf, nwbuf, myvtx);
	    assert(myvtx==idvtx);
	  }

	  // Set time of flight
	  assert(fGctrak);
	  fGctrak->tofg = part->T();

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
	TParticlePDG *pdg = mParticleData->GetParticle( ipdg );     assert(pdg);
	g3id = pdg->TrackingCode();
	if ( g3id < 1 )
	  {
	    Warning(GetName(),Form("Particle %s with PDG id=%i has no G3 code.  Skipped.",pdg->GetName(),ipdg));
	  }
      }

      // And add the particle
      { 
	int nt = 0;
	gskine( plab, g3id, myvtx, ubuf, nwbuf, nt );
      }

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
