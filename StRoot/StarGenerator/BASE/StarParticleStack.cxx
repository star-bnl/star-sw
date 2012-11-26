#include "StarParticleStack.h"
ClassImp(StarParticleStack);

#include <assert.h>

// Interface to STARSIM particle stack
#define agsvert F77_NAME(agsvert,AGSVERT)
#define agskine F77_NAME(agskine,AGSKINE)

#include "TMCProcess.h"
using namespace std;

extern "C" 
{
  void type_of_call agsvert( Float_t *vertex, Int_t *ntbeam, Int_t *nttarg, Float_t *ubuf, Int_t *nu, Int_t *nv );
  void type_of_call agskine( Float_t *plab,   Int_t *iparti, Int_t *nv,     Float_t *ubuf, Int_t *nb, Int_t *nt );
};

const Int_t kDefaultStackSize = 400;
const Int_t kDefaultArraySize = 4000;

// ----------------------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------------------
StarParticleStack::StarParticleStack( const Char_t *name ) : 
  TVirtualMCStack(),
  mNumPrimary(0),
  mCurrent(-1),
  mArraySize(0),
  mArray(0),
  mStackSize(0)
{

  mArray     = new TClonesArray("TParticle", kDefaultArraySize );

}
// ----------------------------------------------------------------------------------------------------
//
// ----------------------------------------------------------------------------------------------------
StarParticleStack::~StarParticleStack()
{
  if ( mArray ) delete mArray; 
}
// ----------------------------------------------------------------------------------------------------
//
// ----------------------------------------------------------------------------------------------------
void StarParticleStack::PushTrack( Int_t toDo, Int_t parent, Int_t pdg, 
					   Double_t px, Double_t py, Double_t pz, Double_t energy,
					   Double_t vx, Double_t vy, Double_t vz, Double_t vt,
					   Double_t polx, Double_t poly, Double_t polz,
					   TMCProcess mech, Int_t& ntr, Double_t weight,
					   Int_t is )
{

  
  //
  // Add a new particle to the array.  Note:  VMC standard PushTrack does not specify the children
  // (or 2nd parent) of particles.  So these are entered with value of -1.
  //
  TClonesArray &array = (*mArray);
  TParticle *particle = new(array[mArraySize]) TParticle( pdg, is, parent, -1, -1, -1, 
						      px, py, pz, energy, vx, vy, vz, vt);
  particle->SetPolarisation( polx, poly, polz );
  particle->SetWeight(weight);
  particle->SetUniqueID(mech);


  // Increment primary track count
  if ( parent<0 )
    {
      mNumPrimary++;
    }

  // Add to the stack of particles
  if ( toDo )
    {

      mStack.push_back( particle );
      mStackIdx.push_back( mArraySize ); // store stack ID

    }  
  ntr = mArraySize; // guess this is supposed to be track number (index in array)

  // Increment mArraySize
  mArraySize++;

}
// ----------------------------------------------------------------------------------------------------
//
// ----------------------------------------------------------------------------------------------------
TParticle *StarParticleStack::PopNextTrack( Int_t &itrack )
{

  // Start with invalid track index
  itrack = -1;

  // The stack is empty.  Signal the end.
  if ( mStack.empty() ) 
    {
      return NULL;
    }

  // Get the particle on the top of the stack
  TParticle *particle = mStack.front();    mStack.pop_front();
  itrack              = mStackIdx.front(); mStackIdx.pop_front();
  mCurrent            = itrack;
  
  return particle;

}
// ----------------------------------------------------------------------------------------------------
//
// ----------------------------------------------------------------------------------------------------
TParticle *StarParticleStack::PopPrimaryForTracking( Int_t i ) 
{
  assert(i<mArraySize);
  return (TParticle *)(*mArray)[i];
}
// ----------------------------------------------------------------------------------------------------
//
// ----------------------------------------------------------------------------------------------------
void StarParticleStack::SetCurrentTrack( Int_t tn )
{
  mCurrent = tn;
}
// ----------------------------------------------------------------------------------------------------
//
// ----------------------------------------------------------------------------------------------------
Int_t StarParticleStack::GetNtrack() const
{
  return mArray->GetEntriesFast();
}
// ----------------------------------------------------------------------------------------------------
//
// ----------------------------------------------------------------------------------------------------
TParticle *StarParticleStack::GetCurrentTrack() const
{
  return GetParticle(mCurrent);
}
// ----------------------------------------------------------------------------------------------------
//
// ----------------------------------------------------------------------------------------------------
Int_t StarParticleStack::GetCurrentTrackNumber() const
{
  return mCurrent;
}
// ----------------------------------------------------------------------------------------------------
//
// ----------------------------------------------------------------------------------------------------
Int_t StarParticleStack::GetCurrentParentTrackNumber() const
{
  TParticle *current = GetCurrentTrack();
  return (current)? current->GetFirstMother() : -1;
}
// ----------------------------------------------------------------------------------------------------
//
// ----------------------------------------------------------------------------------------------------
TParticle *StarParticleStack::GetParticle( Int_t idx ) const
{
  return (TParticle *)(*mArray)[idx];
}
// ----------------------------------------------------------------------------------------------------
//
// ----------------------------------------------------------------------------------------------------
void StarParticleStack::Clear( const Option_t *opts )
{
  mArray->Clear();
  mStack.clear();
  mCurrent = -1;
  mArraySize = 0;
  mStackSize = 0;
}
// ----------------------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------------------
