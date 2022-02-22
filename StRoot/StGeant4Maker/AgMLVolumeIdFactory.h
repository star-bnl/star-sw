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
#include <AgMLBTofVolumeId.h>
#include <AgMLMtdVolumeId.h>
#include <AgMLVpdVolumeId.h>

class AgMLVolumeIdFactory {
public:

  // TODO: Identify what the test flag is for and (probably) remove it 
  static AgMLVolumeId* Create( TString name, bool test=false ) { 

    static std::map<TString,AgMLVolumeId*> VolumeId;

    AgMLVolumeId* id = VolumeId[name];
    if ( 0==id ) {
      if      ( name == "TPAD" && test==false )  
	id = new AgMLTpcVolumeId;
      else if ( name == "TPAD" && test==true )
	id = new AgMLTpcVolumeIdTest__;
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
      else if ( name == "MIGG" ) 
	id = new AgMLMtdVolumeId;
      else if ( name == "VRAD" ) 
	id = new AgMLVpdVolumeId;
      VolumeId[name] = id;
    }
    
    return id;

  };
  
};

#endif
