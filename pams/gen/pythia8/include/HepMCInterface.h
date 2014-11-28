// HepMCInterface.h is a part of the PYTHIA event generator.
// Copyright (C) 2008 Mikhail Kirsanov, Torbjorn Sjostrand.
// PYTHIA is licenced under the GNU GPL version 2, see COPYING for details.
// Please respect the MCnet Guidelines, see GUIDELINES for details.

//--------------------------------------------------------------------------
#ifndef Pythia8_HepMCInterface_H
#define Pythia8_HepMCInterface_H

//////////////////////////////////////////////////////////////////////////
// Mikhail.Kirsanov@Cern.CH
// Pythia8 I class
//////////////////////////////////////////////////////////////////////////
//

#include <set>
#include <vector>
#include "HepMC/IO_BaseClass.h"
#include "Pythia.h"

namespace HepMC {

  class GenEvent;
  class GenVertex;
  class GenParticle;
  class ParticleDataTable;

  class I_Pythia8 : public IO_BaseClass {
    public:
                        I_Pythia8();
      virtual           ~I_Pythia8();
      bool              fill_next_event( Pythia8::Event& pyev, GenEvent* evt,
                                         int ievnum = -1 );
      bool              fill_next_event( Pythia8::Pythia& pythia, GenEvent* evt,
                                         int ievnum = -1,
                                         bool convertGluonTo0 = false );
      void              put_pdf_info( GenEvent* evt, Pythia8::Pythia& pythia,
                                      bool convertGluonTo0 = false );

	// see comments below for these switches.
      bool              trust_both_mothers_and_daughters() const;
      bool              trust_mothers_before_daughters() const;
      bool              print_inconsistency_errors() const;
      void              set_trust_mothers_before_daughters( bool b = 1 );
      void              set_trust_both_mothers_and_daughters( bool b = 0 );
      void              set_print_inconsistency_errors( bool b = 1 );
      void              set_crash_on_problem( bool b = 1 );
      void              set_convert_to_mev( bool b = 1 );

    private: // following are not (yet?) implemented for this class
      virtual bool fill_next_event( GenEvent*  ) { return 0; }
      virtual void write_event( const GenEvent* ) {;}
      virtual void write_particle_data_table( const ParticleDataTable* ){}
      virtual bool fill_particle_data_table( ParticleDataTable* )
        { return 0; }

    private: // use of copy constructor is not allowed
      I_Pythia8( const I_Pythia8& ) : IO_BaseClass() {}

    private: // data members

      bool m_trust_mothers_before_daughters;
      bool m_trust_both_mothers_and_daughters;
      bool m_print_inconsistency_errors; 
      int m_internal_event_number;
      bool m_crash_on_problem;
      bool m_convert_to_mev;
      float m_mom_scale_factor;

  };

  ////////////////////////////
  // INLINES access methods //
  ////////////////////////////
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
  { m_convert_to_mev = b; m_mom_scale_factor = 1000.; }

} // HepMC

#endif  // Pythia8_HepMCInterface_H

//--------------------------------------------------------------------------
