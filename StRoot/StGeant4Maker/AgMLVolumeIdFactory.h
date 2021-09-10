#ifndef __AgMLVolumeIdFactory_h__
#define __AgMLVolumeIdFactory_h__

#include "TString.h"
#include <map>

#include <StarVMC/StarAgmlLib/AgMLExtension.h>
#include <StRoot/StGeant4Maker/AgMLTpcVolumeId.h>
#include <StRoot/StGeant4Maker/AgMLFstVolumeId.h>
#include <StRoot/StGeant4Maker/AgMLStgVolumeId.h>
#include <StRoot/StGeant4Maker/AgMLWcaVolumeId.h>
#include <StRoot/StGeant4Maker/AgMLHcaVolumeId.h>
#include <StRoot/StGeant4Maker/AgMLPreVolumeId.h>
#include <StRoot/StGeant4Maker/AgMLEpdVolumeId.h>
#include <StRoot/StGeant4Maker/AgMLEmcVolumeId.h>
#include <StRoot/StGeant4Maker/AgMLEEmcVolumeId.h>
#include <StRoot/StGeant4Maker/AgMLBTofVolumeId.h>
#include <StRoot/StGeant4Maker/AgMLMtdVolumeId.h>
#include <StRoot/StGeant4Maker/AgMLVpdVolumeId.h>

class AgMLVolumeIdFactory {
public:

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
