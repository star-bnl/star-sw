#include "StarGenEvent.h"
ClassImp(StarGenEvent);
#include "StarGenParticle.h"

#include "TDatabasePDG.h"
#include "TParticlePDG.h"
#include <assert.h>
#include "TMath.h"
#include <iostream>

using namespace std;

// ----------------------------------------------------------------------------
StarGenEvent::StarGenEvent(const Char_t *name, const Char_t *title ) : TObject(),
  mName(name),
  mTitle(title),
  mParticles(0),
  mGeneratorId(0),
  mProcessId(0),
  mOffset(0),
  mEventNumber(0),
  mRunNumber(0),
  mDaqRunNumber(0),
  mDaqFileNumber(0),
  mBlueId(0),
  mYellId(0),
  mCmsEnergy(0),
  mFilterResult( StarGenEvent::kAccept ),// default accept
  mNumParticles(0)
{
  for ( Int_t i=0;i<3;i++ ) mNumRejected[i]=0; 
  mParticles = new TClonesArray( "StarGenParticle", 1000 );
}
// ----------------------------------------------------------------------------
StarGenEvent::~StarGenEvent()
{
  if ( mParticles ) delete mParticles; mParticles=0;
}
// ----------------------------------------------------------------------------
void StarGenEvent::Clear( Option_t *opts )
{
  TString Opts = opts;
  if ( Opts.Contains("part") ) {
    mParticles->Clear();
    mNumParticles = 0;
  }
  if ( Opts.Contains("data") ) {
    mWeights.clear();
    mProcessId    = 0;
    mFilterResult = StarGenEvent::kAccept; // Default is always accept
    mCmsEnergy    = 0;
    for ( Int_t i=0;i<3;i++ ) mNumRejected[i]=0; 
  }
}
// ----------------------------------------------------------------------------
StarGenParticle *StarGenEvent::AddParticle()
{
  TClonesArray &particles = *mParticles;
  StarGenParticle *particle = new( particles[ mNumParticles ] ) StarGenParticle();
  particle->SetIndex( mNumParticles++ );
  return particle;
}
StarGenParticle *StarGenEvent::AddParticle( Int_t status, Int_t pdg, Int_t m1, Int_t m2, Int_t d1, Int_t d2, 
				Double_t px, Double_t py, Double_t pz, Double_t E, Double_t M,
				Double_t vx, Double_t vy, Double_t vz, Double_t vt )
{

  StarGenParticle *particle = AddParticle();
  particle->SetStatus(status);
  particle->SetId(pdg);
  particle->SetFirstMother(m1);
  particle->SetLastMother(m2);
  particle->SetFirstDaughter(d1);
  particle->SetLastDaughter(d2);
  particle->SetPx(px);
  particle->SetPy(py);
  particle->SetPz(pz);
  particle->SetEnergy(E);
  particle->SetMass(M);
  particle->SetVx(vx);
  particle->SetVy(vy);
  particle->SetVz(vz);
  particle->SetTof(vt);

  return particle;
}
// ----------------------------------------------------------------------------
StarGenParticle *StarGenEvent::AddParticle( StarGenParticle *p )
{
  //  Double_t P[]={p->GetPx(),p->GetPy(),p->GetPz()};
  //  Double_t V[]={p->GetVx(),p->GetVy(),p->GetVz()};
  //  Double_t tof=p->GetTof();
  return AddParticle( p->GetStatus(), p->GetId(), p->GetFirstMother(), p->GetLastMother(), p->GetFirstDaughter(), p->GetLastDaughter(), 
		      p->GetPx(), p->GetPy(), p->GetPz(), p->GetEnergy(), p->GetMass(),
		      p->GetVx(), p->GetVy(), p->GetVz(), p->GetTof() );
}
// ----------------------------------------------------------------------------
void StarGenEvent::Print( const Option_t *opts )const
{
  TString Opts = opts;

  if ( Opts.Contains("head") )
    {
      cout << "----------------------------------------------------------------------------- " 
	   << mName.Data() << endl;
      cout << "Run number:   " << mRunNumber << endl;
      cout << "Event number: " << mEventNumber << endl;
      cout << "Generator:    " << mGeneratorId << endl;
      cout << "Offset:       " << mOffset << endl;
      cout << Form("Filter:       0x%04x",int(mFilterResult)) << mOffset << endl;
      cout << "----------------------------------------------------------------------------- " 
	   << endl;
    }

  for ( Int_t i=0;i<mParticles->GetEntriesFast();i++ )
    {
      StarGenParticle *part = (StarGenParticle *)(*mParticles)[i];
      if ( !Opts.Contains("simu")||part->Simulate() )
	part->Print();
    }

}
// ----------------------------------------------------------------------------
