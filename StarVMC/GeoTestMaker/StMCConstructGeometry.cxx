// $Id: StMCConstructGeometry.cxx,v 1.6 2015/08/04 21:13:40 jwebb Exp $
//
//
// Class StMCConstructGeometry
// ------------------
// Base class for Magnetic field calculation

#include <stdio.h>
#include <assert.h>
#include "Stiostream.h"
//#include "StMessMgr.h"
#include "StMCConstructGeometry.h"
#include "TVirtualMC.h"
#include "TVirtualMCApplication.h"
#include "TROOT.h"
#include "TSystem.h"
#include "TGeoManager.h"
#include "TGeoShape.h"
#include "TGeoBBox.h"
#include "TGeoMatrix.h"
#include "TEnv.h"
#include "TRegexp.h"
#include "StMCStack.h"

#include "StMessMgr.h"

ClassImp(StMCConstructGeometry)

//_____________________________________________________________________________
StMCConstructGeometry::StMCConstructGeometry(const char *gy)
  : GCall(gy,"StMCConstructGeometry")
{
}   
//_____________________________________________________________________________
int  StMCConstructGeometry::Fun()
{
  TString ts,tsn(GetName()),star("${STAR}"),top;
  if (! gGeoManager) {
    top = tsn(TRegexp("(.*)"));
    if (top[0]=='(') {tsn.ReplaceAll(top,""); top = top(1, top.Length()-2); }  
    
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
    //if (fail) Printf("operator()","fail=%d path=%s",fail,ts.Data());
    if (fail) LOG_FATAL << "operator() fail=" << fail << " path=" << ts.Data() << endm;
    assert(fail==0);
    gROOT->ProcessLine("CreateTable()");
  }
  if (top.Length()) SetTop(top);

  gMC->SetRootGeometry();

  return 0;
}
//_____________________________________________________________________________
void StMCConstructGeometry::SetTop(const char *topName)
{
  TGeoVolume *vol= gGeoManager->GetVolume(topName);
  assert(vol);
  if (strcmp(topName,"CAVE")==0) {gGeoManager->SetTopVolume(vol);return;}

  TGeoMedium *meVac = gGeoManager->GetMedium("Vacuum");
  if(!meVac) {
    const TGeoMaterial *maVac = gGeoManager->GetMaterial("Vacuum");
    if (!maVac)         maVac = new TGeoMaterial("Vacuum", 0,0,0);
    meVac = new TGeoMedium("Vacuum",1000000,maVac);
  }
  TGeoBBox *bb = (TGeoBBox*)vol->GetShape();
  bb->ComputeBBox();
  TString myName("."); myName+=vol->GetName(); 
  TGeoVolume *top = gGeoManager->MakeBox(myName,meVac
                     ,bb->GetDX(),bb->GetDY(),bb->GetDZ());

  top->AddNode(vol,1,new TGeoTranslation(bb->GetOrigin()[0]
                                        ,bb->GetOrigin()[1]
					,bb->GetOrigin()[2]));
  gGeoManager->SetTopVolume(top);
}




		
		
		
