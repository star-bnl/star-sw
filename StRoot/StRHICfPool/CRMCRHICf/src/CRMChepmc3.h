// -*- mode: C++ -*-
/**
 * @file      src/CRMChepmc3.h
 * @author    Christian Holm Christensen <cholm@nbi.dk>
 * @date      Feb 1, 2021
 * 
 * @brief  Helper to fill in HepMC3::GenEvent
 */
#ifndef CRMChepmc3_h
#define CRMChepmc3_h
#include <HepMC3/GenHeavyIon.h>
#include <HepMC3/GenCrossSection.h>
#include <HepMC3/GenEvent.h>
#include "CRMCoptions.h"
#include "CRMCinterface.h"

/** 
 * Helper class to encode full HepMC3::GenEvent 
 *
 * Filling from the Fortran HEPEVT common block is done by the helper class 
 * CRCMhepevt. 
 */
struct CRMChepmc3
{
  /** Constructor */
  CRMChepmc3() :_ion(0), _xsec(0) {}

  /** 
   * Initialize this helper.  Queries the configuration if the
   * collisions are ion-like and then creates the heavy-ion header if
   * that is the case.  
   */
  void init(const CRMCoptions& cfg)
  {
    if (cfg.GetProjectileId() > 1 || cfg.GetTargetId() > 1)
      _ion  = std::make_shared<HepMC3::GenHeavyIon>();
    _xsec = std::make_shared<HepMC3::GenCrossSection>();
  }

  /** 
   * Fill in event information.  Note, particles and vertices are
   * assumed to be filled in already by the CRMChepevt helper or
   * similar.
   */
  void fillInEvent(const CRMCoptions& cfg,
		   int                evno,
		   HepMC3::GenEvent&  event)
  {
    event.set_event_number(evno);

    _xsec->set_cross_section(1e9 * (_ion ?
				    gCRMC_data.sigineaa : 
				    gCRMC_data.sigine), 0);
    event.set_cross_section(_xsec);
    
    if (_ion) {
      _ion->Ncoll_hard                   = gCRMC_data.kohevt;
      _ion->Npart_proj                   = gCRMC_data.npjevt;
      _ion->Npart_targ                   = gCRMC_data.ntgevt;
      _ion->Ncoll                        = gCRMC_data.kolevt;
      _ion->N_Nwounded_collisions        = gCRMC_data.ng1evt;
      _ion->Nwounded_N_collisions        = gCRMC_data.ng2evt;
      _ion->Nwounded_Nwounded_collisions = gCRMC_data.nglevt;
      _ion->impact_parameter             = gCRMC_data.bimevt;
      _ion->event_plane_angle            = gCRMC_data.phievt;
      _ion->sigma_inel_NN                = gCRMC_data.sigine*1e9;
      _ion->Nspec_proj_n                 = gCRMC_data.npnevt;
      _ion->Nspec_targ_n                 = gCRMC_data.ntnevt;
      _ion->Nspec_proj_p                 = gCRMC_data.nppevt;
      _ion->Nspec_targ_p                 = gCRMC_data.ntpevt;
      event.set_heavy_ion(_ion);
    }
  }
  /** Heavy-ion header */
  HepMC3::GenHeavyIonPtr _ion;
  /** Cross-section */
  HepMC3::GenCrossSectionPtr _xsec;
};
#endif
//
// EOF
//
