#include <stdlib.h>

#include "StHftPool/HftMatchedTree/HftMatchedTree.h"

#include "TSystem.h"
#include "TFile.h"
#include "TTree.h"
#include "TBranch.h"

#include "StEvent/StEvent.h"
#include "StBFChain/StBFChain.h"
#include "StHftPool/EventT/EventT.h"
#include "StIstDbMaker/StIstDb.h"
#include "StPxlDbMaker/StPxlDb.h"

ClassImp(HftMatchedTree)


HftMatchedTree::HftMatchedTree(const Char_t *name) : StMaker(name),
						     fTree(0),
						     fEvent(nullptr),
						     fMinNoHits(0),
						     fpCut(0.5)
{
}
//________________________________________________________________________________
HftMatchedTree::~HftMatchedTree(){}
//________________________________________________________________________________
Int_t HftMatchedTree::Init()
{
   if (Debug() >= 2)
      SetDebug(StMessMgr::kDebug);

   StBFChain *bfChain = (StBFChain *) StMaker::GetChain();

   if (!bfChain) return kStErr;

   // Authorize Trees up to 2 Terabytes (if the system can do it)
   TTree::SetMaxTreeSize(1000 * Long64_t(2000000000));
   
   TFile *f = GetTFile();
   assert(f);
   f->cd();
   fTree = new TTree("t", "TTree with HFT hits and tracks");
   fTree->SetAutoSave(1000000000);  // autosave when 1 Gbyte written
   
   SetTree();
   return kStOK;
}


Int_t HftMatchedTree::InitRun(Int_t runnumber)
{
   StPxlDb* stPxlDb = StPxlDb::instance();

   if (stPxlDb) {
   }
   else {
      LOG_ERROR << "InitRun : Dataset \"pxl_db\" not found" << endm;
      return kStErr;
   }

   TObjectSet *istDbDataSet = (TObjectSet*) GetDataSet("ist_db");
   StIstDb* stIstDb = StIstDb::instance();

   if (istDbDataSet) {
   }
   else {
      LOG_ERROR << "InitRun : Dataset \"ist_db\" not found" << endm;
      return kStErr;
   }
#if 0
   // Dump geometry matrix into root file
   TFile *file = new TFile("GeometryTables.root", "recreate");

   for (int i = 0; i < 24; i++) {
      for (int j = 0; j < 6; j++) {
         int id = 1000 + i * 6 + j + 1;
         TGeoHMatrix *comb = (TGeoHMatrix *) stIstDb->getRotations()->FindObject(Form("R%04i", id));
         comb->Write();
      }
   }

   for (int i = 0; i < 10; i++) {
      for (int j = 0; j < 4; j++) {
         for (int k = 0; k < 10; k++) {
            int id = i * 40 + j * 10 + k + 1;
            TGeoHMatrix *comb = (TGeoHMatrix *) stPxlDb->geoHMatrixSensorOnGlobal(i + 1, j + 1, k + 1);
            comb->SetName(Form("R%03i", id));
            comb->Write();
         }
      }
   }

   file->Close();
   delete file;
#endif
   return kStOK;
}




void HftMatchedTree::SetTree()
{
   fEvent = new EventT();
   TBranch *branch = fTree->Branch("e.", "EventT", &fEvent, 64000, 99);
   branch->SetAutoDelete(kFALSE);
}


Int_t HftMatchedTree::Make()
{
   // Fill the event with information from StEvent
   StEvent *stEvent = (StEvent*) GetInputDS("StEvent");

   if (!stEvent) {
      LOG_ERROR << "Make() - StEvent object not found. It is needed to fill HFT tree. Skipping..." << endm;
      return kStErr;
   }

   int retCode = fEvent->Build(stEvent, fMinNoHits, fpCut);

   if (retCode) {
      LOG_ERROR << "Make() - EventT was not built properly. Please investigate" << endm;
      return retCode;
   }

   fTree->Fill();
   return kStOK;
}


/** Clear the container for the next event. */
void HftMatchedTree::Clear(Option_t *opt)
{
   fEvent->Clear();
}
