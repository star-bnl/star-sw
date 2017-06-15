// $Id: StMCConstructGeometry.cxx,v 1.2 2010/10/26 19:39:57 jwebb Exp $
//
//
// Class StMCConstructGeometry
// ------------------
// Base class for Magnetic field calculation

#include <stdio.h>
#include <assert.h>
#include "Stiostream.h"
#include "StMessMgr.h"
#include "StMCConstructGeometry.h"
#include "TVirtualMC.h"
#include "TVirtualMCApplication.h"
#include "TROOT.h"
#include "TSystem.h"
#include "TGeoManager.h"
#include "TEnv.h"
#include "StMCStack.h"

ClassImp(StMCConstructGeometry)

//_____________________________________________________________________________
StMCConstructGeometry::StMCConstructGeometry(const char *gy)
  : GCall(gy,"StMCConstructGeometry")
{
}   
//_____________________________________________________________________________
int  StMCConstructGeometry::Fun()
{
  if (! gGeoManager) 
    {
      TString ts,tsn(GetName()),star("${STAR}");    
      if (tsn.Contains("yf")) {
	star = "${STAR_PATH}/.DEV2";
	TString aaa(".include ${STAR_PATH}/.DEV2/StarDb/VmcGeometry");
	gSystem->ExpandPathName(aaa);
	gROOT->ProcessLine(aaa);	
	aaa = gEnv->GetValue("Unix.*.Root.MacroPath",0);
	aaa+= ":"; aaa+= star; aaa+="$/StarDb/VmcGeometry/";
	gEnv->SetValue("Unix.*.Root.MacroPath",aaa);
	tsn.ReplaceAll("yf","");      
      }
      if (!ts.Contains(".C")) {
	ts=star;
	ts+="/StarDb/VmcGeometry/Geometry.";
	ts+=tsn; ts+=".C";
      }      
      gSystem->ExpandPathName(ts);
      int fail=gROOT->LoadMacro(ts);
      if (fail) Printf("operator(): fail=%d path=%s",fail,ts.Data());
      assert(fail==0);
      gROOT->ProcessLine("CreateTable()");
    }
  else
    {
      std::cout << "Exisiting geometry detected... use it." << std::endl;
    }

  TGeoVolume *cave = gGeoManager->GetVolume("CAVE");
  gGeoManager->SetTopVolume(cave);

  gMC->SetRootGeometry();

  return 0;
}





		
		
		
