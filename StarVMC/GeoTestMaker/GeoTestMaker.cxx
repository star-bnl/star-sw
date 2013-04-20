//*-- Author : Victor Perev
// 
// $Id: GeoTestMaker.cxx,v 1.5 2013/04/20 21:53:41 perev Exp $
// $Log: GeoTestMaker.cxx,v $
// Revision 1.5  2013/04/20 21:53:41  perev
// Rename StTGeoHelper ==> StTGeoProxy
//
// Revision 1.4  2010/01/27 23:02:57  perev
// Development
//
// Revision 1.3  2009/08/29 21:19:04  perev
// 100 * 10000 tracks
//
// Revision 1.2  2009/06/07 02:28:36  perev
// 1st reasonable version with orth2
//
// Revision 1.1  2009/03/25 23:15:10  perev
// New VMC maker
//
// Revision 1.9  2009/02/03 15:55:44  fisyak
// synchronize with .DEV2
//
// Revision 1.2  2009/01/24 00:21:43  fisyak
// Fix debug flag
//
// Revision 1.1.1.1  2008/12/10 20:45:49  fisyak
// Merge with macos version
//
// Revision 1.8  2008/03/05 13:15:56  fisyak
// comply Skip signuture with base class
//
// Revision 1.7  2007/04/07 19:33:09  perev
// Check for input file added
//
// Revision 1.6  2007/01/09 04:53:20  potekhin
// New input modes
//
// Revision 1.4  2005/09/13 21:34:29  fisyak
// Move initialization from Init to InitRun, add conversion TGeoVolume to TVolume for StEventDisplayMaker and TofMatchers
//
// Revision 1.3  2005/06/17 18:35:45  fisyak
// Add flow diagram
//
// Revision 1.2  2005/06/09 20:14:40  fisyak
// Set Run number (=1 D)
//
// Revision 1.1  2005/05/24 22:58:08  fisyak
// The first version
//
//
// Rewritten by V.Perev

/* Flow diagram:
   Load(); // shared libraries
   GetVMC(); // define gGeoManager
------------------
GeoTestMaker::Init()
------------------

*/

#include <assert.h>
#include "TROOT.h"
#include "TSystem.h"
#include "TGeometry.h"
#include "TGeoManager.h"
#include "TObjectSet.h"
#include "GeoTestMaker.h"
#include "Stiostream.h"
#include "StarMagField.h"
#include "StVMCApplication.h"
#include "StMCStack.h"
#include "StMessMgr.h"
#include "TVirtualMC.h"
#include "TGeant3TGeo.h"
#include "StMCInitApp.h"
//#include "StMCSteppingHist.h"
#include "StMCStepping2Hist.h"
#include "StTGeoProxy.h"

ClassImp(GeoTestMaker);

//_____________________________________________________________________________
GeoTestMaker::GeoTestMaker(const char *name,const char *gy, int trig)
  :StMaker(name),fNTrig(trig),fGeo(gy)
{
gSystem->AddIncludePath("${STAR}2/StarDb/VmcGeometry");
}

//_____________________________________________________________________________
int GeoTestMaker::Init() 
{
  StVMCApplication *app = new StVMCApplication(fGeo, "StVMC application");
  StMCInitApp *ini = new StMCInitApp();

  if (*SAttr("NPrim" )) {ini->SetNPrim(IAttr( "NPrim"));}
  if (*SAttr("PtMin" )) {ini->SetPt (DAttr( "PtMin"),DAttr( "PtMax"));}
  if (*SAttr("EtaMin")) {ini->SetEta(DAttr("EtaMin"),DAttr("EtaMax"));}
  if (*SAttr("PhiMin")) {ini->SetEta(DAttr("PhiMin"),DAttr("PhiMax"));}
  if (*SAttr("ZMin"  )) {ini->SetZ  (DAttr(  "ZMin"),DAttr(  "ZMax"));}
  app->SetInit(ini);
//StMCSteppingHist *steps =   new StMCSteppingHist(fGeo);
  StMCStepping2Hist *steps =   new StMCStepping2Hist(fGeo);
  app->SetStepping(steps);
  if (*SAttr("SteppingDebug")) {steps->SetDebug(IAttr("SteppingDebug"));}
  
  app->Init();

  StTGeoProxy::Instance()->Init(1);

  return StMaker::Init();
}
//_____________________________________________________________________________
int GeoTestMaker::InitRun  (int runumber)
{
  return kStOK;
}
//_____________________________________________________________________________
int GeoTestMaker::Make()
{
  TVirtualMC::GetMC()->ProcessRun(fNTrig);
  return kStOK;
}
//_____________________________________________________________________________
int GeoTestMaker::Finish()
{
  //  StMCSteppingHist::Instance()->Finish();
//StMCSteppingHist::Instance()->Finish();
  StMCStepping2Hist::Instance()->Finish();
  return StMaker::Finish();
}

//________________________________________________________________________________
void GeoTestMaker::SetDebug(int l) {
  StMaker::SetDebug(l);
}
