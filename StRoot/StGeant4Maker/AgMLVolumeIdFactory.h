#ifndef __AgMLVolumeIdFactory_h__
#define __AgMLVolumeIdFactory_h__

#include "TString.h"
#include <map>

#include <StarVMC/StarAgmlLib/AgMLExtension.h>
#include <AgMLTpcVolumeId.h>
#include <AgMLFstVolumeId.h>
#include <AgMLStgVolumeId.h>
#include <AgMLWcaVolumeId.h>
#include <AgMLHcaVolumeId.h>
#include <AgMLPreVolumeId.h>
#include <AgMLEpdVolumeId.h>
#include <AgMLEmcVolumeId.h>
#include <AgMLEEmcVolumeId.h>
#include <AgMLFmsVolumeId.h>
#include <AgMLBTofVolumeId.h>
#include <AgMLMtdVolumeId.h>
#include <AgMLVpdVolumeId.h>
#include <AgMLETofVolumeId.h>
#include <AgMLMtdVolumeId.h>
#include <AgMLBbcVolumeId.h>
#include <AgMLVpdVolumeId.h>


/**
 * @class AgMLVolumeIdFactory
 * @brief A factory for creating detector-specific volume identifier objects.
 *
 * This class provides a static factory method, `Create`, which returns an
 * appropriate AgMLVolumeId-derived object based on the family name of a
 * sensitive volume. It manages a cache of created instances to avoid
 * unnecessary object creation.
 */
class AgMLVolumeIdFactory {
public:

  // TODO: Identify what the test flag is for and (probably) remove it 
  static AgMLVolumeId* Create( TString name, bool test=false ) { 

    static std::map<TString,AgMLVolumeId*> VolumeId;

    AgMLExtension* agml = AgMLExtension::get( name );

    AgMLVolumeId* id = VolumeId[name];
    if ( 0==id ) {
      if      ( name == "TPAD" )  
	id = new AgMLTpcVolumeId;
      else if ( name == "FTUS" )
	id = new AgMLFstVolumeId;
      else if ( name == "TGCG" )
	id = new AgMLStgVolumeId;
      else if ( name == "WSCI" )
	id = new AgMLWcaVolumeId;
      else if ( name == "HSCI" )
	id = new AgMLHcaVolumeId;
      else if ( name == "PSCI" )
	id = new AgMLPreVolumeId;
      else if ( name == "EPDT" )
	id = new AgMLEpdVolumeId;
      else if ( name == "CSCI" )
	id = new AgMLEmcVolumeId;
      else if ( name == "ESCI" )
	id = new AgMLEEmcVolumeId;
      else if ( name == "BRSG" ) 
	id = new AgMLBtofVolumeId;
      else if ( name == "ECEL" ) 
	id = new AgMLEtofVolumeId;
      else if ( name == "MIGG" ) 
	id = new AgMLMtdVolumeId;
      else if ( name == "VRAD" ) 
	id = new AgMLVpdVolumeId;
      else if ( name == "FLGR" )
	id = new AgMLFmsVolumeId( "FLGR" );
      else if ( name == "FLXF" )
	id = new AgMLFmsVolumeId( "FLXF" );
      else if ( name == "MXSA" )
	id = new AgMLMtdVolumeId();
      else if ( name == "BPOL" )
	id = new AgMLBbcVolumeId();
      else if ( name == "VRAD" )
	id = new AgMLVpdVolumeId();

      VolumeId[name] = id;

    }
    
    return id;

  };
  
};

#endif
