/**
 * Example of use of tauola C++ interface.
 * e+, e- -> tau + tau - HEPEVT events are constructed.
 * Taus are subsequently decayed via tauola.
 *
 * @author Tomasz Przedzinski
 * @date 24 November 2011
 */

#include "Tauola/Tauola.h"
#include "Tauola/TauolaHEPEVTParticle.h"
#include "Tauola/TauolaHEPEVTEvent.h"

#include "tauola_print_parameters.h"
using namespace std;
using namespace Tauolapp;

/** Create a simple e+ + e- -> tau+ tau- HEPEVT event **/
TauolaHEPEVTEvent* make_simple_tau_event(){

  TauolaHEPEVTEvent * evt = new TauolaHEPEVTEvent();

  // Create some four vectors for the electrons
  double e_mass_sq   = parmas_.amell*parmas_.amell;
  double tau_mass_sq = parmas_.amtau*parmas_.amtau;

  double e1_pz = -2.0; //change these
  double e2_pz =  3.5; //as needed
  double e1_e  = sqrt(e1_pz*e1_pz + e_mass_sq);
  double e2_e  = sqrt(e2_pz*e2_pz + e_mass_sq);



  // TauolaHEPEVTParticle prepares single particle to be written into event record 
  // Arguments are as follows:
  // (PDG_id, status, p_x, p_y, p_z, energy, mass, position of first mother, position of second mother,
  //  position first daugter, last daughter)
  // New particles are added to the end of the event record
  // In HEPEVT style these arguments mean:  
  // (IDHEP(),ISTAT(),PHEP(1,),PHEP(2,),PHEP(3,),PHEP(4,),PHEP(5,),JMOHEP(1,),JMOHEP(2,),JDAHEP(1,),JDAHEP(2,)

  // Make TauolaParticles for boosting
  TauolaHEPEVTParticle *cms_boost  = new TauolaHEPEVTParticle(  0, 0, 0., 0., e1_pz+e2_pz,e1_e+e2_e,0.,            -1, -1, -1, -1);

  TauolaHEPEVTParticle *first_e    = new TauolaHEPEVTParticle(-11, 3, 0., 0., e1_pz,      e1_e,     parmas_.amell, -1, -1,  2,  3);
  TauolaHEPEVTParticle *second_e   = new TauolaHEPEVTParticle( 11, 3, 0., 0., e2_pz,      e2_e,     parmas_.amell, -1, -1,  2,  3);
  TauolaHEPEVTParticle *first_tau  = new TauolaHEPEVTParticle(-15, 1, 0., 0., 0.,         0.,       parmas_.amtau,  0,  1, -1, -1);
  TauolaHEPEVTParticle *second_tau = new TauolaHEPEVTParticle( 15, 1, 0., 0., 0.,         0.,       parmas_.amtau,  0,  1, -1, -1);

  // NOW we add the following particles to the list (our event record): 
  // Order matters!
  evt->addParticle(first_e   );
  evt->addParticle(second_e  );
  evt->addParticle(first_tau );
  evt->addParticle(second_tau);
  // Appropriate method to copy our event to HEPEVT is available. 
  // To activate uncoment appropriate line in TauolaHEPEVTEvent.h
  // Useful names: read_event_from_HEPEVT, write_event_to_HEPEVT.

  double tau_energy = 0.5*sqrt( (e1_e+e2_e)*(e1_e+e2_e) - (e1_pz+e2_pz)*(e1_pz+e2_pz) );

  first_tau->setE  (tau_energy);
  first_tau->setPx ((1.0/sqrt(2.0))*sqrt(tau_energy*tau_energy-tau_mass_sq));
  first_tau->setPy ((1.0/sqrt(2.0))*sqrt(tau_energy*tau_energy-tau_mass_sq));

  second_tau->setE (tau_energy);
  second_tau->setPx(-1*(1.0/sqrt(2.0))*sqrt(tau_energy*tau_energy-tau_mass_sq));
  second_tau->setPy(-1*(1.0/sqrt(2.0))*sqrt(tau_energy*tau_energy-tau_mass_sq));

  first_tau ->boostFromRestFrame(cms_boost);
  second_tau->boostFromRestFrame(cms_boost);

  //clean up
  delete cms_boost;

  return evt;
}

/** Example of using Tauola to decay taus stored in HEPEVT-like event record */
int main(void){

  int NumberOfEvents = 10;

  //These three lines are not really necessary since they are the default
  Tauola::setDecayingParticle(15);
  Tauola::setSameParticleDecayMode(0);
  Tauola::setOppositeParticleDecayMode(0);

  Tauola::initialize();

  tauola_print_parameters(); // Prints TAUOLA  parameters (residing inside its library): e.g. to test user interface

  // Begin event loop. Generate event.
  for (int iEvent = 0; iEvent < NumberOfEvents; ++iEvent) {

    // Create simple event
    TauolaHEPEVTEvent * t_event = make_simple_tau_event();

    cout << "event BEFORE decays:"<<endl;
    t_event->print();

    t_event->decayTaus();

    cout << "event AFTER decays:"<<endl;
    t_event->print();

    //clean up
    delete t_event;
  }

  // This is an access to old FORTRAN info on generated tau sample.
  // That is why it refers to old version number (eg. 2.7) for TAUOLA.
  //Tauola::summary();
}

