#include <TGeoManager.h>

#if !defined(__CINT__) || defined(__CLING__)
#include "StBFChain/StBFChain.h"
#include "StarVMC/StarGeometry/StarGeo.h"
#endif

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

  if ( gGeoManager ) {
    if ( chain->GetOption("Sti")    || 	 chain->GetOption("StiCA")  ) {
      gGeoManager = 0;
      assert(0);      // this assert should be called in our Sti reconstruction chains... why not?
      TGeoManager::Import( filename );
    }
  }
  cout << "AgML: Building geometry " << name << " " << endl;


  Geometry *build = new Geometry();  
  build->InitAgML( "StarTGeoStacker" );

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

