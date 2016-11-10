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
  
  
  Gctrak_t& gctrak = *(geant3->Gctrak()); // kinematics of current track
  Gcking_t& gcking = *(geant3->Gcking()); // kinematics of decay products
  Gckin3_t& gckin3 = *(geant3->Gckin3()); // vertex of decay products

  float x = gctrak.vect[0];
  float y = gctrak.vect[1];
  float z = gctrak.vect[2];

  //  LOG_INFO << Form(">>> decay() called x=%f y=%f z=%f <<<",x,y,z) << endm;
  if (0==mDecayer) return 0; // no decayer registerd

  //  int np = 0;

  mArray   -> Clear();
  mDecayer -> ForceDecay();

  int idGeant3 = geant3->Gckine()->ipart;

  double pmom = double( gctrak.vect[6] );
  double px   = double( gctrak.vect[3] ) * pmom;
  double py   = double( gctrak.vect[4] ) * pmom;
  double pz   = double( gctrak.vect[5] ) * pmom;
  double E    = double( gctrak.getot   ); // Input energy

  mP[0] = px; mP[1] = py; mP[2] = pz; mP[3] = E;

  // Convert geant3 id to "standard" id
  // Long64_t idPart = StarParticleData::ConvertG3Id( idGeant3 );

  // Extract PDG ID from idPart
  int idPdg = pdb.GetParticleG3( idGeant3 )->PdgCode();

  // Perform the decay
  mDecayer -> Decay( idPdg, &mP );

  // Retrieve the particles into the clones array
  int np = mDecayer -> ImportParticles( mArray ); if ( np<1 ) return np;

  // Flag deselected particles
  //vector<int> flags(np); // this causes a mystery crash when flags dtor is called
  int flags[np];

  //  mArray -> Print();

  for ( int i=1 /* first daughter */; i < np; i++ )
    {

      TParticle    *particle    = (TParticle *)mArray->At(i);
      int           first       = particle->GetFirstDaughter();
      int           last        = particle->GetLastDaughter();
      int           pdgid       = particle->GetPdgCode();
      int           status      = particle->GetStatusCode();
      TParticlePDG *particlePDG = pdb.GetParticle(pdgid); 
      int           g3id        = particlePDG->TrackingCode();

      // If the particle has been deselected skip and deselect its daughters as well
      if ( 1 == flags[i] )
      	{
      	  if (first>0) { for ( int j=first;j<=last;j++ ) flags[j]=1;
	    //LOG_INFO << "i = " << i << " deselect: j=" << first << " to " << last << endm; 
	  }
      	  continue;
      	}

      // If the particle is not known to G3 we skip it, and make sure the daughters
      // are selected... or we add it to the database dynamically
      if ( 0 == g3id ) 
	{

	  //
	  // kDecay Policy -- On discovery of unknown G3 id, push the daughters onto the stack
	  //
	  if ( kDecay == mDiscovery ) 
	    {
	      if (first>0) { for ( int j=first;j<last+1;j++ ) flags[j]=0;
		//LOG_INFO << "i = " << i << " DESELECT: j=" << first << " to " << last << endm; 
	      }
	      // NOTE: should probably flag this particle before hitting continue...
	      //    ... who decides which particles get stacker?
	      continue;
	    }

	  //
	  // kSpawn Policy -- On discovery of unknown G3 id, add the particle state
	  //
	  else if ( kSpawn == mDiscovery ) 
	    {
	      pdb.AddParticleToG3( particlePDG, mNextG3id++ ); assert(mNextG3id < 60000);
	    }


	}


      // Long lived particles are stacked for further tracking.  
      if ( 1 != status )
      	{
      	  double lifetime = mDecayer->GetLifetime(pdgid); 
      	  if ( true /* lifetime > double( 1.0E-15 ) */ )
      	    {
      	      // Particle is stacked, skip daughters
      	      if (first>0) for (int j=first;j<=last;j++ ) flags[j]=1;
      	      // LOG_INFO << "Stack particle, skip daughters tlife=" << lifetime 
      	      // 	       << " first=" << first 
      	      // 	       << " last=" << last
      	      // 	       << endm;
      	      // particlePDG->Print();
      	    }
      	  else if (first>0)
      	    {
      	      // Particle is decayed (skipped), stack daughters
      	      //LOG_INFO << "Decay particle, stack daughters tlife=" << lifetime << endm;
      	      particlePDG->Print();
      	      flags[i] = 1; 
      	      continue; 
      	    }
      	  else {
	    //      	    LOG_INFO << "Stack particle, stable tlife=" << lifetime << endm;
      	  }
	  
      	}


      // Neutrinos are skipped
      if ( pdgid == 12 || pdgid == -12 ){ flags[i]=1; continue; }
      if ( pdgid == 14 || pdgid == -14 ){ flags[i]=1; continue; }
      if ( pdgid == 16 || pdgid == -16 ){ flags[i]=1; continue; }

      // This is the current stack position
      int &index = gcking.ngkine;

      // Throw particle on the stack
      (gcking.gkin[index][0]) = particle->Px();
      (gcking.gkin[index][1]) = particle->Py();
      (gcking.gkin[index][2]) = particle->Pz();
      (gcking.gkin[index][3]) = particle->Energy();

      (gcking.gkin[index][4]) = float(g3id);
      //      particlePDG->Print();

      // Decay vertex
      (gckin3.gpos[index][0]) = gctrak.vect[0];
      (gckin3.gpos[index][1]) = gctrak.vect[1];
      (gckin3.gpos[index][2]) = gctrak.vect[2];

      // time of flight offset (mm)... (huh?)
      (gcking.tofd[index])    = 0.;

      // And increase stack counter
      index++;

    }


  return np;
}

AgUDecay::AgUDecay() : mDecayer( 0 ), 
		       mArray( new TClonesArray("TParticle",1000) ), 
		       mP(),
		       mDiscovery( kDecay ),
		       mNextG3id( 12345 ) // dynamic G3 id
{

}
