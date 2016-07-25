// $Id: LoadGeanE.C,v 1.1 2009/03/24 21:59:02 fisyak Exp $
// $Log: LoadGeanE.C,v $
// Revision 1.1  2009/03/24 21:59:02  fisyak
// Freeze
//
//  GeanE test 
#if !defined(__CINT__)
#include "Riostream.h"
#include <stdio.h>
#include "TROOT.h"
#include "TSystem.h"
#include "TMath.h"
#include "TGeant3TGeo.h"
#include "StarVMC/StarVMCApplication/StarVMCApplication.h"
#include "TDataSet.h"
#include "TGeoManager.h"
#include "TCernLib.h"
#include "StarRoot/TRSymMatrix.h"
#include "StarRoot/TRMatrix.h"
#include "StarRoot/TRVector.h"
void bfc (const Int_t Last=0, 
	  const Char_t *Chain="",
	  const Char_t *infile=0, 
	  const Char_t *outfile=0, 
	  const Char_t *TreeFile=0);
#else 
class TGeant3TGeo;
class StarVMCApplication;
class Gcflag_t;
class Ertrio_t;
#endif
TDataSet *CreateTable();
TGeant3TGeo* fgGeant3 = 0;
StarVMCApplication* fgStarVMCApplication = 0;
//________________________________________________________________________________
void LoadGeanE(const Char_t *tag = "y2007g") {
  gROOT->LoadMacro("bfc.C");
  TString Chain(Form("detDb,tpcDb,svtDb,ssdDb,VMCAppl,nodefault,dbSnapshot,%s",tag));
  bfc(-1,Chain.Data());
#ifdef __CINT__
  chain->Init();
  chain->Make();
#endif
  fgStarVMCApplication = new StarVMCApplication("StarVMC", "The STAR VMC application");
  fgGeant3 = new TGeant3TGeo("C++ Interface to Geant3");
  if (! gGeoManager) {
    gROOT->LoadMacro(Form("/afs/rhic.bnl.gov/star/packages/.DEV2/StarDb/VmcGeometry/Geometry.%s.C",tag));
    CreateTable();
  }
  if (! gGeoManager) return;
  fgStarVMCApplication->InitMC();
#if 0
  gMessMgr->Info() << "StVMCMaker::InitRun switch off physics" << endm;
  gMC->SetProcess("DCAY", 0);
  gMC->SetProcess("ANNI", 0);
  gMC->SetProcess("BREM", 0);
  gMC->SetProcess("COMP", 0);
  gMC->SetProcess("HADR", 0);
  gMC->SetProcess("MUNU", 0);
  gMC->SetProcess("PAIR", 0);
  gMC->SetProcess("PFIS", 0);
  gMC->SetProcess("PHOT", 0);
  gMC->SetProcess("RAYL", 0);
  gMC->SetProcess("LOSS", 4); // no fluctuations 
  //  gMC->SetProcess("LOSS 1"); // with delta electron above dcute
  gMC->SetProcess("DRAY", 0);
  gMC->SetProcess("MULS", 0);
  gMC->SetProcess("STRA", 0);
  gMC->SetCut("CUTGAM",	1e-3  );
  gMC->SetCut("CUTELE", 	1e-3  );
  gMC->SetCut("CUTHAD", 	.001  );
  gMC->SetCut("CUTNEU", 	.001  );
  gMC->SetCut("CUTMUO", 	.001  );
  gMC->SetCut("BCUTE", 	.001  );
  gMC->SetCut("BCUTM", 	.001  );
  gMC->SetCut("DCUTE", 	1e-3  );
  gMC->SetCut("DCUTM", 	.001  );
  gMC->SetCut("PPCUTM", 	.001  );
  gMC->SetCut("TOFMAX", 	50.e-6);
#endif
  fgGeant3->SetDEBU(1,100,1);
  fgGeant3->SetSWIT(1,2);
  fgGeant3->SetSWIT(2,2);
  fgGeant3->SetSWIT(4,0);
  Gcflag_t *cflag = (Gcflag_t *) fgGeant3->Gcflag();
  cflag->idebug = 1;
  Ertrio_t *ertrio = (Ertrio_t *)fgGeant3->Ertrio();
}
