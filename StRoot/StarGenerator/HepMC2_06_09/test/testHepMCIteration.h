//////////////////////////////////////////////////////////////////////////
// testHepMCIteration.h
//
// garren@fnal.gov, May 2007
//
// Define methods and classes used by testHepMCIteration
//////////////////////////////////////////////////////////////////////////

/// returns true if the GenParticle particle is a photon with pT > 10 GeV
bool IsPhoton( const HepMC::GenParticle* p ) { 
    if ( p->pdg_id() == 22 
	 && p->momentum().perp() > 10. ) return true;
    return false;
}

/// returns true if the GenParticle is a W+/W-
bool IsWBoson( const HepMC::GenParticle* p ) { 
    if ( abs(p->pdg_id()) == 24 ) return true;
    return false;
}

/// \class  IsFinalState
/// this predicate returns true if the input has no decay vertex
class IsFinalState {
public:
    /// returns true if the GenParticle does not decay
    bool operator()( const HepMC::GenParticle* p ) { 
	if ( !p->end_vertex() && p->status()==1 ) return true;
	return false;
    }
};

/// \class  PrintPhoton
/// prints the particle if it is a photon
class PrintPhoton {
public:
    PrintPhoton( std::ostream & os ) : m_out( os ) {}
    void operator()( const HepMC::GenParticle* p ) { 
	if ( IsPhoton(p) ) p->print( m_out );
    }
private:
   std::ostream & m_out;
};

/// \class  PrintParticle
/// prints the particle
class PrintParticle {
public:
    PrintParticle( std::ostream & os ) : m_out( os ) {}
    void operator()( const HepMC::GenParticle* p ) { 
	m_out << "\t";
	p->print( m_out );
    }
private:
   std::ostream & m_out;
};

//! test class

/// \class  PrintChildren
/// prints the particle
class PrintChildren {
public:
    PrintChildren( std::ostream & os ) : m_out( os ) {}
    void operator()( HepMC::GenParticle* p ) { 
	// make a copy
	HepMC::GenParticle* cp = p;
	// use the copy and the original
	m_out << "\t\t\t (id,barcode,status) " 
	      << cp->pdg_id() << " " 
              << p->barcode() << " "
              << cp->status() << std::endl;
    }
private:
   std::ostream & m_out;
};

//! test class

/// \class  PrintDescendants
/// prints the particle
class PrintDescendants {
public:
    PrintDescendants( std::ostream & os ) : m_out( os ) {}
    void operator()( const HepMC::GenParticle* p ) { 
	m_out << "\t\t";
	p->print( m_out );
    }
private:
   std::ostream & m_out;
};
