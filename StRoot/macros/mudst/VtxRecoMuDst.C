/////////////////////////////////////////////////////////////////////////////
// $Id: VtxRecoMuDst.C,v 1.4 2017/10/03 20:55:22 genevb Exp $
// Author: G. Van Buren (BNL)
//
// Description:
// Re-run vertex-finding on a MuDst
//
// options (attributes) are space-separated or comma-separated,
// and values are provided after a ':'
//
// Example options:
// beamline, beamline1D, beamline3D (otherwise no beamline)
// useBTOFmatchOnly
// VFstore:100
//
// Example list of options:
// "VFPPVnoCTB,beamline1D,VFstore:100"
//
/////////////////////////////////////////////////////////////////////////////
//
// To run this macro compiled, try something like:
// echo 'gROOT->LoadMacro("VtxRecoMuDst.C");loadLibs();gROOT->ProcessLine(".U VtxRecoMuDst.C");gROOT->LoadMacro("VtxRecoMuDst.C+");VtxRecoMuDst(1000,"MuDsts/st_*.MuDst.root","out.MuDst.root","VFPPVnoCTB,beamline1D,VFstore:100")' | root4star -b
//
/////////////////////////////////////////////////////////////////////////////


#if !defined(__CINT__) || defined(__MAKECINT__)
#include <algorithm>
#include <cfloat>
#include <iostream>
#include <string>

#include "TROOT.h"
#include "TSystem.h"
#include "TChain.h"
#include "TClonesArray.h"
#include "TFile.h"
#include "TTree.h"
#include "TString.h"
#include "TObjArray.h"

#include "StChain/StChain.h"
#include "St_db_Maker/St_db_Maker.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StGenericVertexMaker/StGenericVertexMaker.h"

#endif


void loadLibs()
{
   gROOT->Macro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
   gSystem->Load("libMinuit");
   gSystem->Load("StDbBroker");
   gSystem->Load("St_db_Maker");
   gSystem->Load("Sti");
   gSystem->Load("StEEmcUtil");
   gSystem->Load("StBTofUtil");
   gSystem->Load("StGenericVertexMaker");
}

int VtxRecoMuDst(unsigned int nEventsUser, char* inputFileName, char* outputFileName, const char* options="")
{
   loadLibs();

   StChain fullChain("fullChain");

   StMuDstMaker muDstMaker(0, 0, "", inputFileName, "st:MuDst.root", 1e9); // set up maker in read mode
   //                      0, 0                        this means read mode
   //                           dir                    read all files in this directory
   //                               file               bla.lis real all file in this list, if (file!="") dir is ignored
   //                                    filter        apply filter to filenames, multiple filters are separated by ':'
   //                                          10      maximum number of file to read

   // Specify (active) branches to read but first disable all branches
   muDstMaker.SetStatus("*",1);
   muDstMaker.SetStatus("PrimaryTracks",0);
   muDstMaker.SetStatus("PrimaryVertices",0);


   TChain& muDstChain = *muDstMaker.chain();
   unsigned int nEntries      = muDstChain.GetEntries();
   unsigned int nEventsToRead = nEventsUser > 0 ? min(nEventsUser, nEntries) : nEntries;
   cout << nEntries << " events in chain, " << nEventsToRead << " will be read." << endl;

   // Create new branch
   TClonesArray* verticesRefitted = new TClonesArray("StMuPrimaryVertex", 1000);
   TFile* outFile      = new TFile(outputFileName, "RECREATE");
   TTree* muDstTreeOut = muDstChain.CloneTree(0);
   muDstTreeOut->Branch("PrimaryVertices", &verticesRefitted, 65536, 99);

   St_db_Maker* st_db_maker = new St_db_Maker("db", "StarDb", "MySQL:StarDb", "$STAR/StarDb");

   StGenericVertexMaker* vertexMaker = new StGenericVertexMaker();
   vertexMaker->ToWhiteConst("vtxArray",verticesRefitted);
   vertexMaker->SetAttr("useMuDst",1);

   // Set additional options as maker attributes
   TString Options = options;
   TString optDelim = " ,";
   TObjArray* tokens = Options.Tokenize(optDelim);
   for (int tk=0; tk < tokens->GetEntries(); tk++) {
     TString& tok = ((TObjString*) (tokens->At(tk)))->String();
     Ssiz_t delim = tok.First(':');
     if (delim < 0) {
       vertexMaker->SetAttr(tok.Data(),1);
     } else {
       TString key(tok(0,delim));
       TString& val = tok.Remove(0,delim+1);
       if (val.IsDigit()) { vertexMaker->SetAttr(key.Data(),val.Atoi()); }
       else if (val.IsFloat()) { vertexMaker->SetAttr(key.Data(),val.Atof()); }
       else { vertexMaker->SetAttr(key.Data(),val.Data()); }
     }
   }
   vertexMaker->PrintAttr();


   // Main loop over events
   fullChain.Init();
   for (unsigned int iEvent = 0; iEvent < nEventsToRead; iEvent++)
   {
      if ( fullChain.Make() ) break;
      muDstTreeOut->Fill();
      fullChain.Clear();
   }

   outFile->Write();
   outFile->Close();

   delete st_db_maker;
   delete vertexMaker;
   delete outFile;
}

/////////////////////////////////////////////////////////////////////////////
//
// $Log: VtxRecoMuDst.C,v $
// Revision 1.4  2017/10/03 20:55:22  genevb
// Add StBTofUtil lib dependence
//
// Revision 1.3  2017/09/28 13:54:07  jeromel
// Fix from Leszek Adamczyk
//
// Revision 1.2  2017/08/24 19:38:19  genevb
// Correct the MuDst branches loaded - old primary tracks and vertices no longer kept
//
// Revision 1.1  2017/05/24 05:14:13  genevb
// Introduce new macro to re-find vertices in MuDsts
//
//
/////////////////////////////////////////////////////////////////////////////
