#include <TGeoManager.h>

#include "StBFChain/StBFChain.h"
#include "StarVMC/StarAgmlLib/AgModule.h"
#include "StarVMC/StarAgmlLib/StarTGeoStacker.h"
#include "StarVMC/StarGeometry/StarGeo.h"

extern StBFChain* chain;

/**
 * Adapted from StarDb/VmcGeometry/CreateGeometry.h
 */
TDataSet *CreateGeometry(const Char_t *name="y2011") {
  TObjectSet *geom = 0;
  if ( gGeoManager ) { 
    cout << "AgML geometry:  Existing TGeoManager " << gGeoManager->GetName() 
	 << " detected, ignoring request for " 
	 << name << endl;
    return geom;
  }

  // Cache geometry to a TFile.  Geometry will be restored from TFile on subsequent calls.
  TString filename = "";  
  if  (chain)  { filename = chain->GetFileOut(); if ( filename=="" ) filename = chain->GetFileIn();  }
  else { filename = name;  }

  // Strip out @ symbol
  filename = filename.ReplaceAll("@",""); 
  // Strip off the last extention in the filename
  filename = filename( 0, filename.Last('.') );
  // Append geom.root to the extentionless filename
  filename+=".geom.root";

  // Detect second call to the system
  if ( AgModule::Find("HALL") ) {
    if ( chain->GetOption("Sti")    ||
	 chain->GetOption("StiCA")  ||
	 chain->GetOption("StiVMC") ){
      cout << "AgML geometry:  HALL exists.  Restore from cache file " 
	   << filename.Data() << endl;
      gGeoManager = 0;
      assert(0);
      TGeoManager::Import( filename );
      assert(gGeoManager);
    }
    return geom;
  }

  cout << "AgML: Building geometry " << name << " " << endl;

  // Create the geometry using TGeo
  AgBlock::SetStacker( new StarTGeoStacker() ); 

  Geometry *build = new Geometry();  

  // Suppress copious ROOT warnings 
  Long_t save = gErrorIgnoreLevel; gErrorIgnoreLevel = 9999;
  build->ConstructGeometry(name);
  gErrorIgnoreLevel = save;

  if ( gGeoManager ) 
    {
      gGeoManager->CloseGeometry();
      geom = new TObjectSet("Geometry",gGeoManager, false );
      geom -> SetTitle( Form("AgML Geometry: %s",name) );

      TFile *file = new TFile( filename, "recreate" );
      file->cd();
      gGeoManager->Write();
      file->Close();
      delete file;
    }

  return (TDataSet *)geom;  
}
