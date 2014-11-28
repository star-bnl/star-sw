/////////////////////////////////////////////////////////////////
//
// RecoQA.C
//   Macro for providing a basic set of histograms for
//   the purpose of QA on reconstruction
//
//   Author: G. Van Buren, BNL
//           Jul. 16, 2008
//
//   Input: Any STAR reconstruction hist.root file
//     containing QA histograms. Also tries to determine
//     histogram file name if something close is given.
//
//   Output: CINT files to reconstruct canvases of QA files
//   Example output: st_physics_8120054_raw_1010030.hist_1.CC
//                   st_physics_8120054_raw_1010030.hist_2.CC
//     which can be viewed by executing:
//         root st_physics_8120054_raw_1010030.hist_1.CC
// 

void RecoQA(const char* inFile) {
  gROOT->LoadMacro("bfcread_hist_prefixes_add_to_ps.C");

  TString baseName = inFile;
  if (baseName.EndsWith(".daq"))
    baseName.ReplaceAll(".daq",".hist.root");
  else if (baseName.EndsWith(".event.root"))
    baseName.ReplaceAll(".event.root",".hist.root");
  else if (baseName.EndsWith(".MuDst.root"))
    baseName.ReplaceAll(".MuDst.root",".hist.root");

  TString histFile = baseName;

  baseName.Remove(0,baseName.Last('/')+1);

  TString outputName = baseName;
  outputName.ReplaceAll(".root",".CC");

  TString histList = "StRoot/St_QA_Maker/QAhlist_Reco.h";
  if (! gSystem->Which(".",histList.Data()))
    histList.Prepend(gSystem->ExpandPathName("$STAR/"));
  
  bfcread_hist_prefixes_add_to_ps(histFile.Data(),"EventQA",
    "bfcTree",outputName.Data(),baseName.Data(),histList.Data());
}


/////////////////////////////////////////////////////////////////
// $Id: RecoQA.C,v 1.1 2008/07/16 22:40:05 genevb Exp $
// $Log: RecoQA.C,v $
// Revision 1.1  2008/07/16 22:40:05  genevb
// Introduce RecoQA.C macro
//

