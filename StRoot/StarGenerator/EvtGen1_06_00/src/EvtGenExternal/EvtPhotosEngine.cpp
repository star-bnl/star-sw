#ifdef EVTGEN_PHOTOS
//--------------------------------------------------------------------------
//
// Environment:
//      This software is part of the EvtGen package. If you use all or part
//      of it, please give an appropriate acknowledgement.
//
// Copyright Information: See EvtGen/COPYRIGHT
//      Copyright (C) 2011      University of Warwick, UK
//
// Module: EvtPhotosEngine
//
// Description: Interface to the PHOTOS external generator
//
// Modification history:
//
//    John Back       May 2011            Module created
//
//------------------------------------------------------------------------

#include "EvtGenExternal/EvtPhotosEngine.hh"

#include "EvtGenBase/EvtPDL.hh"
#include "EvtGenBase/EvtVector4R.hh"
#include "EvtGenBase/EvtPhotonParticle.hh"
#include "EvtGenBase/EvtReport.hh"
#include "EvtGenBase/EvtRandom.hh"

#include "Photos/Photos.h"
#include "Photos/PhotosHepMCEvent.h"
#include "Photos/PhotosHepMCParticle.h"
#include "Photos/PhotosParticle.h"

#include "HepMC/SimpleVector.h"
#include "HepMC/Units.h"

#include <iostream>
#include <sstream>
#include <vector>

using std::endl;

EvtPhotosEngine::EvtPhotosEngine(std::string photonType, bool useEvtGenRandom) {

  _photonType = photonType;
  _gammaId = EvtId(-1,-1);
  _gammaPDG = 22; // default photon pdg integer
  _mPhoton = 0.0;

  EvtGenReport(EVTGEN_INFO,"EvtGen")<<"Setting up PHOTOS."<<endl;

  if (useEvtGenRandom == true) {
      
    EvtGenReport(EVTGEN_INFO,"EvtGen")<<"Using EvtGen random number engine also for Photos++"<<endl;

    Photospp::Photos::setRandomGenerator(EvtRandom::Flat);

  }

  Photospp::Photos::initialize();

  // Increase the maximum possible value of the interference weight
  Photospp::Photos::maxWtInterference(64.0); // 2^n, where n = number of charges (+,-)
  Photospp::Photos::setInterference(true);
  Photospp::Photos::setExponentiation(true); // Sets the infrared cutoff at 1e-7
  // Reset the minimum photon energy, if required, in units of half of the decaying particle mass.
  // This must be done after exponentiation! Keep the cut at 1e-7, i.e. 0.1 keV at the 1 GeV scale,
  // which is appropriate for B decays
  Photospp::Photos::setInfraredCutOff(1.0e-7);

  _initialised = false;

}

EvtPhotosEngine::~EvtPhotosEngine() {

}

void EvtPhotosEngine::initialise() {

  if (_initialised == false) {

    _gammaId = EvtPDL::getId(_photonType);

    if (_gammaId == EvtId(-1,-1)) {
      EvtGenReport(EVTGEN_INFO,"EvtGen")<<"Error in EvtPhotosEngine. Do not recognise the photon type "
			   <<_photonType<<". Setting this to \"gamma\". "<<endl;
      _gammaId = EvtPDL::getId("gamma");
    }

    _gammaPDG = EvtPDL::getStdHep(_gammaId);
    _mPhoton = EvtPDL::getMeanMass(_gammaId);

    _initialised = true;
 
  }

}

bool EvtPhotosEngine::doDecay(EvtParticle* theMother) {

  if (_initialised == false) {this->initialise();}

  if (theMother == 0) {return false;}

  // Create a dummy HepMC GenEvent containing a single vertex, with the mother
  // assigned as the incoming particle and its daughters as outgoing particles.
  // We then pass this event to Photos for processing.
  // It will return a modified version of the event, updating the momentum of
  // the original particles and will contain any new photon particles. 
  // We add these extra photons to the mother particle daughter list.

  // Skip running Photos if the particle has no daughters, since we can't add FSR.
  // Also skip Photos if the particle has too many daughters (>= 10) to avoid a problem
  // with a hard coded upper limit in the PHOENE subroutine.
  int nDaug(theMother->getNDaug());
  if (nDaug == 0 || nDaug >= 10) {return false;}

  // Create the dummy event.
  HepMC::GenEvent* theEvent = new HepMC::GenEvent(HepMC::Units::GEV, HepMC::Units::MM);

  // Create the decay "vertex".
  HepMC::GenVertex* theVertex = new HepMC::GenVertex();
  theEvent->add_vertex(theVertex);

  // Add the mother particle as the incoming particle to the vertex.
  HepMC::GenParticle* hepMCMother = this->createGenParticle(theMother, true);
  theVertex->add_particle_in(hepMCMother);

  // Find all daughter particles and assign them as outgoing particles to the vertex.
  // Keep track of the number of photons already in the decay (e.g. we may have B -> K* gamma)
  int iDaug(0), nGamma(0);
  for (iDaug = 0; iDaug < nDaug; iDaug++) {

    EvtParticle* theDaughter = theMother->getDaug(iDaug);
    HepMC::GenParticle* hepMCDaughter = this->createGenParticle(theDaughter, false);
    theVertex->add_particle_out(hepMCDaughter);

    if (theDaughter) {
      int daugId = theDaughter->getPDGId();
      if (daugId == _gammaPDG) {nGamma++;}
    }

  }

  // Now pass the event to Photos for processing
  // Create a Photos event object
  Photospp::PhotosHepMCEvent photosEvent(theEvent);

  // Run the Photos algorithm
  photosEvent.process();    

  // Find the number of (outgoing) photons in the event
  int nPhotons = this->getNumberOfPhotons(theVertex);

  // See if Photos has created additional photons. If not, do nothing extra
  int nDiffPhotons = nPhotons - nGamma;
  int iLoop(0);

  if (nDiffPhotons > 0) {

    // We have extra particles from Photos; these would have been appended
    // to the outgoing particle list

    // Get the iterator of outgoing particles for this vertex
    HepMC::GenVertex::particles_out_const_iterator outIter;
    for (outIter = theVertex->particles_out_const_begin();
	 outIter != theVertex->particles_out_const_end(); ++outIter) {

      // Get the next HepMC GenParticle
      HepMC::GenParticle *outParticle = *outIter;

      // Get the three-momentum Photos result for this particle, and the PDG id
      double px(0.0), py(0.0), pz(0.0);
      int pdgId(0);

      if (outParticle != 0) {
	HepMC::FourVector HepMCP4 = outParticle->momentum();
	px = HepMCP4.px();
	py = HepMCP4.py();
	pz = HepMCP4.pz();
	pdgId = outParticle->pdg_id();
      }

      // Create an empty 4-momentum vector for the new/modified daughters
      EvtVector4R newP4;

      if (iLoop < nDaug) {

	// Original daughters
	EvtParticle* daugParticle = theMother->getDaug(iLoop);
	if (daugParticle != 0) {

	  // Keep the original particle mass, but set the three-momentum
	  // according to what Photos has modified. However, this will 
	  // violate energy conservation (from what Photos has provided).
	  double mass = daugParticle->mass();
	  double energy = sqrt(mass*mass + px*px + py*py + pz*pz);
	  newP4.set(energy, px, py, pz);
	  // Set the new four-momentum (FSR applied)
	  daugParticle->setP4WithFSR(newP4);

	}

      } else if (pdgId == _gammaPDG) {

	// Extra photon particle. Setup the four-momentum object
	double energy = sqrt(_mPhoton*_mPhoton + px*px + py*py + pz*pz);
	newP4.set(energy, px, py, pz);

	// Create a new photon particle and add it to the list of daughters
	EvtPhotonParticle* gamma = new EvtPhotonParticle();
	gamma->init(_gammaId, newP4);
	// Set the pre-FSR photon momentum to zero
	gamma->setFSRP4toZero();
	// Let the mother know about this new photon
	gamma->addDaug(theMother);
	// Set its particle attribute to specify it is a FSR photon
	gamma->setAttribute("FSR", 1); // it is a FSR photon
	gamma->setAttribute("ISR", 0); // it is not an ISR photon

      }

      // Increment the loop counter for detecting additional photon particles
      iLoop++;

    }    

  }

  // Cleanup
  theEvent->clear();
  delete theEvent;

  return true;

}

HepMC::GenParticle* EvtPhotosEngine::createGenParticle(EvtParticle* theParticle, bool incoming) {

  // Method to create an HepMC::GenParticle version of the given EvtParticle.
  if (theParticle == 0) {return 0;}

  // Get the 4-momentum (E, px, py, pz) for the EvtParticle
  EvtVector4R p4(0.0, 0.0, 0.0, 0.0);

  if (incoming == true) {
    p4 = theParticle->getP4Restframe();
  } else {
    p4 = theParticle->getP4();
  }
  
  // Convert this to the HepMC 4-momentum
  double E = p4.get(0);
  double px = p4.get(1);
  double py = p4.get(2);
  double pz = p4.get(3);

  HepMC::FourVector hepMC_p4(px, py, pz, E);

  int PDGInt = EvtPDL::getStdHep(theParticle->getId());

  // Set the status flag for the particle. This is required, otherwise Photos++ 
  // will crash from out-of-bounds array index problems.
  int status = Photospp::PhotosParticle::HISTORY;
  if (incoming == false) {status = Photospp::PhotosParticle::STABLE;}

  HepMC::GenParticle* genParticle = new HepMC::GenParticle(hepMC_p4, PDGInt, status);

  return genParticle;

}

int EvtPhotosEngine::getNumberOfPhotons(const HepMC::GenVertex* theVertex) const {

  // Find the number of photons from the outgoing particle list

  if (!theVertex) {return 0;}

  int nPhotons(0);

  // Get the iterator of outgoing particles for this vertex
  HepMC::GenVertex::particles_out_const_iterator outIter;
  for (outIter = theVertex->particles_out_const_begin();
       outIter != theVertex->particles_out_const_end(); ++outIter) {

    // Get the next HepMC GenParticle
    HepMC::GenParticle *outParticle = *outIter;

    // Get the PDG id
    int pdgId(0);
    if (outParticle != 0) {pdgId = outParticle->pdg_id();}
    
    // Keep track of how many photons there are
    if (pdgId == _gammaPDG) {nPhotons++;}

  }

  return nPhotons;

}

#endif
