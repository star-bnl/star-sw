// HepMCInterface.h is a part of the PYTHIA event generator.
// Copyright (C) 2012 Mikhail Kirsanov, Torbjorn Sjostrand.
// PYTHIA is licenced under the GNU GPL version 2, see COPYING for details.
// Please respect the MCnet Guidelines, see GUIDELINES for details.

// Header file for I_Pythia8 class,
// which converts a PYTHIA event record to the standard HepMC format.

#ifndef Pythia8_HepMCInterface_H
#define Pythia8_HepMCInterface_H

#include <set>
#include <vector>
#include "HepMC/IO_BaseClass.h"
#include "Pythia.h"

namespace HepMC {

// Forward references to some classes.
class GenEvent;
class GenVertex;
class GenParticle;
class ParticleDataTable;

//==========================================================================

// The I_Pythia8 class.

class I_Pythia8 : public IO_BaseClass {

public:

  // Constructor and destructor.
  I_Pythia8();
  virtual ~I_Pythia8();

  // The key methods to convert Pythia events into HepMC ones.
  bool fill_next_event( Pythia8::Event& pyev, GenEvent* evt, 
    int ievnum = -1 );
  bool fill_next_event( Pythia8::Pythia& pythia, GenEvent* evt,
    int ievnum = -1, bool convertGluonTo0 = false );
  void put_pdf_info( GenEvent* evt, Pythia8::Pythia& pythia,
    bool convertGluonTo0 = false );

  // Read out values for some switches.
  bool trust_both_mothers_and_daughters() const;
  bool trust_mothers_before_daughters() const;
  bool print_inconsistency_errors() const;

  // Set values for some switches.
  void set_trust_mothers_before_daughters( bool b = true );
  void set_trust_both_mothers_and_daughters( bool b = false );
  void set_print_inconsistency_errors( bool b = true );
  void set_crash_on_problem( bool b = false );
  void set_freepartonwarnings( bool b = true );
  void set_convert_to_mev( bool b = false );

private: 

  // Following are not (yet?) implemented for this class.
  virtual bool fill_next_event( GenEvent*  ) { return 0; }
  virtual void write_event( const GenEvent* ) {;}
  virtual void write_particle_data_table( const ParticleDataTable* ) {}
  virtual bool fill_particle_data_table( ParticleDataTable* ) { return 0; }

  // Use of copy constructor is not allowed
  I_Pythia8( const I_Pythia8& ) : IO_BaseClass() {}

  // Data members.
  bool   m_trust_mothers_before_daughters;
  bool   m_trust_both_mothers_and_daughters;
  bool   m_print_inconsistency_errors; 
  bool   m_crash_on_problem;
  bool   m_freepartonwarnings;
  bool   m_convert_to_mev;
  double m_mom_scale_factor;
  int    m_internal_event_number;

};

// Inline access methods.

inline bool I_Pythia8::trust_both_mothers_and_daughters() const 
  { return m_trust_both_mothers_and_daughters; }
    
inline bool I_Pythia8::trust_mothers_before_daughters() const 
  { return m_trust_mothers_before_daughters; }

inline bool I_Pythia8::print_inconsistency_errors() const
  { return m_print_inconsistency_errors; }
 
inline void I_Pythia8::set_trust_both_mothers_and_daughters( bool b )
  { m_trust_both_mothers_and_daughters = b; }

inline void I_Pythia8::set_trust_mothers_before_daughters( bool b )
  { m_trust_mothers_before_daughters = b; }

inline void I_Pythia8::set_print_inconsistency_errors( bool b  )
  { m_print_inconsistency_errors = b; }

inline void I_Pythia8::set_crash_on_problem( bool b  )
  { m_crash_on_problem = b; }

inline void I_Pythia8::set_convert_to_mev( bool b  )
  { m_convert_to_mev = b; m_mom_scale_factor = (b) ? 1000. : 1.; }

inline void I_Pythia8::set_freepartonwarnings( bool b  )
  { m_freepartonwarnings = b; }

//==========================================================================

} // end namespace HepMC

#endif  // end Pythia8_HepMCInterface_H
