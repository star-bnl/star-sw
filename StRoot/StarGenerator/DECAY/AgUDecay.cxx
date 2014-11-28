#include "AgUDecay.h"
#include "StMessMgr.h"
#include "TVirtualMCDecayer.h"
#include "TParticle.h"
#include "TClonesArray.h"
#include "StarDecayManager.h"
#include "St_geant_Maker/St_geant_Maker.h"
#include "StarGenerator/UTIL/StarParticleData.h"

#define geant3 St_geant_Maker::instance()->Geant3()

StarParticleData &pdb = StarParticleData::instance();

AgUDecay AgUDecay::sInstance;
//
// --------------------------------------------------------------------------------------------------
//
extern "C" {

  void agudcay_() 
  { static AgUDecay &decay = AgUDecay::instance();

    // Decay current particle on G3 stack
    decay();

  };

}
//
// --------------------------------------------------------------------------------------------------
//
Int_t AgUDecay::operator()()
{
  LOG_INFO << "decay() called" << endm;
  if (0==mDecayer) return 0; // no decayer registerd

  //  Int_t np = 0;

  mArray   -> Clear();
  mDecayer -> ForceDecay();

  Int_t idGeant3 = geant3->Gckine()->ipart;

  Double_t pmom = Double_t( geant3->Gctrak()->vect[6] );
  Double_t px   = Double_t( geant3->Gctrak()->vect[3] ) * pmom;
  Double_t py   = Double_t( geant3->Gctrak()->vect[4] ) * pmom;
  Double_t pz   = Double_t( geant3->Gctrak()->vect[5] ) * pmom;
  Double_t E    = Double_t( geant3->Gctrak()->getot   );

  mP[0] = px; mP[1] = py; mP[2] = pz; mP[3] = E;

  // Convert geant3 id to "standard" id
  // Long64_t idPart = StarParticleData::ConvertG3Id( idGeant3 );

  // Extract PDG ID from idPart
  Int_t idPdg = pdb.GetParticleG3( idGeant3 )->PdgCode();

  // Perform the decay
  mDecayer -> Decay( idPdg, &mP );

  // Retrieve the particles into the clones array
  Int_t np = mDecayer -> ImportParticles( mArray ); if ( np<1 ) return np;

  // Flag deselected particles
  vector<Int_t> flags(np);

  for ( Int_t i=1 /* first daughter */; i < np; i++ )
    {

      TParticle    *particle = (TParticle *)mArray->At(i);
      Int_t         first    = particle->GetFirstDaughter();
      Int_t         last     = particle->GetLastDaughter();
      Int_t         pdgid    = particle->GetPdgCode();
      Int_t         status   = particle->GetStatusCode();
      TParticlePDG *particlePDG = pdb.GetParticle(pdgid); 
      Int_t         g3id        = particlePDG->TrackingCode();


      // LOG_INFO << "-- particle i= " << i 
      // 	       << " first=" << first 
      // 	       << " last=" << last
      // 	       << " -----------------------------------" << endm;
      // particlePDG->Print();

      // If the particle has been deselected skip and deselect its daughters as well
      if ( 1 == flags[i] )
      	{
      	  if (first>0) for ( Int_t j=first-1;j<last;j++ ) flags[j]=1;
      	  continue;
      	}

      // Long lived particles are stacked for further tracking.  
      if ( 1 != status )
      	{
      	  Double_t lifetime = mDecayer->GetLifetime(pdgid); 
      	  if ( true /* lifetime > Double_t( 1.0E-15 ) */ )
      	    {
      	      // Particle is stacked, skip daughters
      	      if (first>0) for (Int_t j=first;j<=last;j++ ) flags[j]=1;
      	      LOG_INFO << "Stack particle, skip daughters tlife=" << lifetime 
      		       << " first=" << first 
      		       << " last=" << last
      		       << endm;
      	      particlePDG->Print();
      	    }
      	  else if (first>0)
      	    {
      	      // Particle is decayed (skipped), stack daughters
      	      LOG_INFO << "Decay particle, stack daughters tlife=" << lifetime << endm;
      	      particlePDG->Print();
      	      flags[i] = 1; 
      	      continue; 
      	    }
      	  else {
      	    LOG_INFO << "Stack particle, stable tlife=" << lifetime << endm;
      	  }
	  
      	}

      // Neutrinos are skipped
      if ( pdgid == 12 || pdgid == -12 ){ flags[i]=1; continue; }
      if ( pdgid == 14 || pdgid == -14 ){ flags[i]=1; continue; }
      if ( pdgid == 16 || pdgid == -16 ){ flags[i]=1; continue; }

      // This is the current stack position
      Int_t &index = geant3->Gcking()->ngkine;

      // Throw particle on the stack
      (geant3->Gcking()->gkin[index][0]) = particle->Px();
      (geant3->Gcking()->gkin[index][1]) = particle->Py();
      (geant3->Gcking()->gkin[index][2]) = particle->Pz();
      (geant3->Gcking()->gkin[index][3]) = particle->Energy();

      (geant3->Gcking()->gkin[index][4]) = Float_t(g3id);
      particlePDG->Print();

      // Decay vertex
      (geant3->Gckin3()->gpos[index][0]) = geant3->Gctrak()->vect[0];
      (geant3->Gckin3()->gpos[index][1]) = geant3->Gctrak()->vect[1];
      (geant3->Gckin3()->gpos[index][2]) = geant3->Gctrak()->vect[2];

      // time of flight offset (mm)... (huh?)
      (geant3->Gcking()->tofd[index])    = 0.;

      // And increase stack counter
      index++;

    }

  return np;
}

AgUDecay::AgUDecay() : mDecayer( 0 ), 
		       mArray( new TClonesArray("TParticle",1000) ), 
		       mP()
{

}
