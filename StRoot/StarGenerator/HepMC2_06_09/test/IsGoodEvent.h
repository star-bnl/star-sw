//////////////////////////////////////////////////////////////////////////
// IsGoodEvent.h
//
// garren@fnal.gov, May 2007
//
// Events containing a photon of pT > 25 GeV are considered good
//////////////////////////////////////////////////////////////////////////

//! used in the tests

/// \class  IsGoodEvent
/// event selection predicate. returns true if the event contains
/// a photon with pT > 50 GeV
class IsGoodEvent {
public:
    bool operator()( const HepMC::GenEvent* evt ) { 
	for ( HepMC::GenEvent::particle_const_iterator p 
		  = evt->particles_begin(); p != evt->particles_end(); ++p ){
	    if ( (*p)->pdg_id() == 22 && (*p)->momentum().perp() > 25. ) {
		return 1;
	    }
	}
	return 0;
    }
};
