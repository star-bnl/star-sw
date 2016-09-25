/**
 * Example of use of tauola C++ interface. e+, e- -> tau + tau -
 * HepMC events are constructed. Taus are subsequently decay via
 * tauola.
 *
 * @author Nadia Davidson
 * @date 17 June 2008
 */

#include "Tauola/Tauola.h"
#include "Tauola/TauolaHepMCEvent.h"

//HepMC headers
#include "HepMC/GenEvent.h"
#include "HepMC/GenParticle.h"

#include "tauola_print_parameters.h"

//#include "tauola_set_seed.h" // uncomment for old releases 
// and use setSeed( 47238, 985439, 0 ); 
// In new libraries Tauola::setSeed( 47238, 985439, 0 );
// is available.

using namespace std;
using namespace Tauolapp;

/** Create a simple HepMC::GenEvent 
 ** e+ + e- -> tau+ tau - 
 **/
HepMC::GenEvent * make_simple_tau_event(){

  HepMC::GenEvent * event = new HepMC::GenEvent();

  //Create some four vectors for the electrons
  double ele_mass_sqr = parmas_.amell*parmas_.amell;
  HepMC::FourVector momentum_e1(0,0,0,0);
  HepMC::FourVector momentum_e2(0,0,0,0);

  momentum_e1.setPz(-2); //change these
  momentum_e2.setPz(3.5); //as needed

  momentum_e1.setE(sqrt(momentum_e1.pz()*momentum_e1.pz()+ele_mass_sqr));
  momentum_e2.setE(sqrt(momentum_e2.pz()*momentum_e2.pz()+ele_mass_sqr));

  //make taus
  double tau_mass = parmas_.amtau;

  //make tau's four vector
  HepMC::FourVector momentum_tau1(0,0,0,0);
  HepMC::FourVector momentum_tau2(0,0,0,0);

  //make particles
  HepMC::GenParticle * e1 = new HepMC::GenParticle(momentum_e1,-11,3);
  HepMC::GenParticle * e2 = new HepMC::GenParticle(momentum_e2,11,3);
  HepMC::GenParticle * tau1 = new HepMC::GenParticle(momentum_tau1,-15,1);
  HepMC::GenParticle * tau2 = new HepMC::GenParticle(momentum_tau2,15,1);
  
  //set the masses
  e1->set_generated_mass(parmas_.amell);
  e2->set_generated_mass(parmas_.amell);
  tau1->set_generated_mass(tau_mass);
  tau2->set_generated_mass(tau_mass);
 
  //make the vertex
  HepMC::GenVertex * vertex = new HepMC::GenVertex();
  vertex->add_particle_in(e1);
  vertex->add_particle_in(e2);
  vertex->add_particle_out(tau1);
  vertex->add_particle_out(tau2);

  event->add_vertex(vertex);

  //calculate center of mass frame
  HepMC::FourVector cms(0,0,(momentum_e1.pz()+momentum_e2.pz()),
			  momentum_e1.e()+momentum_e2.e());

  HepMC::GenParticle * cms_particle = new HepMC::GenParticle(cms,0,0);

  //Make TauolaParticles for doing boosting:
  TauolaHepMCParticle * cms_boost = new TauolaHepMCParticle(cms_particle);

  TauolaHepMCParticle first_tau(tau1);
  TauolaHepMCParticle second_tau(tau2);

  double tau_energy = 0.5*sqrt(cms.e()*cms.e() - (cms.px()*cms.px()
			+ cms.py()*cms.py()+cms.pz()*cms.pz()));
		
  first_tau.setE(tau_energy);
  first_tau.setPx((1.0/sqrt(2.0))*sqrt(tau_energy*tau_energy-tau_mass*tau_mass));
  first_tau.setPy((1.0/sqrt(2.0))*sqrt(tau_energy*tau_energy-tau_mass*tau_mass));

  second_tau.setE(tau_energy);
  second_tau.setPx(-1*(1.0/sqrt(2.0))*sqrt(tau_energy*tau_energy-tau_mass*tau_mass));
  second_tau.setPy(-1*(1.0/sqrt(2.0))*sqrt(tau_energy*tau_energy-tau_mass*tau_mass));

  first_tau.boostFromRestFrame(cms_boost);
  second_tau.boostFromRestFrame(cms_boost);

  //clean up
  delete cms_boost;
  delete cms_particle;

  return event;
}

/** example main for decaying a stop with modified tauola */
int main(void){

  int NumberOfEvents = 10;

  //These three lines are not really necessary since they are the default
  Tauola::setDecayingParticle(15);
  Tauola::setSameParticleDecayMode(0); 
  Tauola::setOppositeParticleDecayMode(0);

  // Change TAUOLA-FORTRAN random generator seed:
  // Tauola::setSeed( 47238, 985439, 0 ); 

  // for older releases use   setSeed( 47238, 985439, 0 ); // and  tauola_set_seed.h

  Tauola::initialize();

  tauola_print_parameters(); // Prints TAUOLA  parameters (residing inside its library): e.g. to test user interface

  // Begin event loop. Generate event.
  for (int iEvent = 0; iEvent < NumberOfEvents; ++iEvent) {

    // Convert event record to Tau format

    HepMC::GenEvent * event = make_simple_tau_event();

    cout << "BEFORE:"<<endl;
    event->print();
    TauolaHepMCEvent * t_event = new TauolaHepMCEvent(event);
    t_event->decayTaus();

    cout << "AFTER:"<<endl;
    event->print();

    //clean up
    delete event;
    delete t_event;
  }

  // This is an access to old FORTRAN info on generated tau sample. 
  // That is why it refers to old version number (eg. 2.7) for TAUOLA.
  //Tauola::summary();
}

