#ifndef __CINT__
#include "Riostream.h"
#include "Rtypes.h"
#include "TROOT.h"
#include "TSystem.h"
#include "TH2.h"
#include "TH3.h"
#include "TF1.h"
#include "TProfile.h"
#include "TTree.h"
#include "TChain.h"
#include "TCanvas.h"
#include "TClassTable.h"
#include "TFileSet.h"
#include "TDataSetIter.h"
#include "StBFChain.h"
#include "StIOMaker.h"
void bfc (const Int_t Last, 
	  const Char_t *Chain,
	  const Char_t *infile, 
	  const Char_t *outfile, 
	  const Char_t *TreeFile);
#else
class StBFChain;
#endif
//________________________________________________________________________________
void MakeLcKppi(Int_t nevents=1, const Char_t *parn = "LcKppi",Int_t RunID = 1)
{
  if (gClassTable->GetID("TTable") < 0) {
    gSystem->Load("libTable");
  }
  gROOT->LoadMacro("bfc.C");
  TString RootFile(parn);
  RootFile += "_"; RootFile += RunID; 
  TString MainFile = RootFile;
  MainFile += ".root";
  RootFile += ".MuDst.root";
  //   TString Chain("AgML,TpcRS,bbcSim,btofsim,pxlFastSim,istSim,emcY2,eefs,TpxClu,y2014a,useXgeom,FieldOn,NoSsdIt"
  // 		",NoSvtIt,StiHftC,Idst,BAna,tpcDB,TpcHitMover,btofMatch,btofCalib,tags,emcY2,IdTruth,gstar"
  // 		",Corr4,OSpaceZ2,OGridLeak3D"
  // 		",StiCA,beamline,KFVertex,StiHftC,pxlFastSim,ssdfast,useXgeom,VFMCE,noRunco,noHistos,noTags");
  //		",muMc"); 
  TString Chain("MC.2016a,StiCA,-hitfilt,KFVertex,StiHftC,geantOut,VFMCE,noRunco,noHistos,noTags,");
  Chain += "vmc,CorrX,OSpaceZ2,OGridLeak3D,-useXgeom,VMCAlignment,sdt20160301,";
  Chain += parn;
  Chain += Form(",rung.%i",RunID);
  chain = bfc(0,Chain.Data(),0,RootFile.Data(),MainFile.Data());
  chain->EventLoop(nevents);
}
