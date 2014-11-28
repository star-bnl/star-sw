#include "SvtMatchedTree.h"
#include <stdlib.h>
#include "Riostream.h"
#include "TROOT.h"
#include "TSystem.h"
#include "TFile.h"
#include "TKey.h"
#include "TRandom.h"
#include "TTree.h"
#include "TBranch.h"
#include "TStopwatch.h"
#include "StThreeVectorF.hh"
#include "StMatrixF.hh"
#include "TH1.h"
#include "TH2.h"
#include "TProfile.h"
#include "TMath.h"
#include "TVector3.h"
#include "TProcessID.h"
#include "StEvent.h"
#include "StPrimaryVertex.h"
#include "StBFChain.h"
#include "TGeoMatrix.h"
#include "St_db_Maker/St_db_Maker.h"
#include "tables/St_svtWafersPosition_Table.h"
#include "tables/St_ssdWafersPosition_Table.h"
#include "tables/St_Survey_Table.h"
#include "StSvtPool/EventT/EventT.h"
#include "StSsdDbMaker/StSsdDbMaker.h"
#include "StSvtDbMaker/StSvtDbMaker.h"
//________________________________________________________________________________
SvtMatchedTree::SvtMatchedTree(const Char_t *name) : StMaker(name),fFile(0), fTree(0), fEvent(0) {
  SetMinNoHits();
  SetpCut();
  SetOut();
}
//________________________________________________________________________________
Int_t SvtMatchedTree::Init() {
  SetTree();
  return kStOK;
}
//________________________________________________________________________________
Int_t SvtMatchedTree::Finish() {
  fFile = fTree->GetCurrentFile(); //just in case we switched to a new file
  fFile->Write();
  fTree->Print();
  return kStOK;
}

//________________________________________________________________________________
void SvtMatchedTree::SetTree() {
  StBFChain *chain = (StBFChain *) StMaker::GetChain();
  if (! chain) return;
  // root.exe 
  Int_t split  = 99;       // by default, split Event in sub branches
  Int_t comp   = 1;       // by default file is compressed
  Int_t branchStyle = 1; //new style by default
  if (split < 0) {branchStyle = 0; split = -1-split;}
  Int_t bufsize;
  //Authorize Trees up to 2 Terabytes (if the system can do it)
  TTree::SetMaxTreeSize(1000*Long64_t(2000000000));
  TString FName(fOut);
  if (fMinNoHits > 0) FName += Form("_%i_%f2.1_",fMinNoHits,fpCut);
  FName += gSystem->BaseName(chain->GetFileIn().Data());
  FName.ReplaceAll("st_physics","");
  FName.ReplaceAll(".event","");
  FName.ReplaceAll(".daq",".root");
  fFile = new TFile(FName,"RECREATE","TTree with SVT + SSD hits and tracks");
  fFile->SetCompressionLevel(comp);
  // Create a ROOT Tree and one superbranch
  fTree = new TTree("T","TTree with SVT + SSD hits and tracks");
  fTree->SetAutoSave(1000000000);  // autosave when 1 Gbyte written
  bufsize = 64000;
  if (split)  bufsize /= 4;
  fEvent = new EventT();
  TTree::SetBranchStyle(branchStyle);
  TBranch *branch = fTree->Branch("EventT", &fEvent, bufsize,split);
  branch->SetAutoDelete(kFALSE);
}
//________________________________________________________________________________
Int_t SvtMatchedTree::Make() {
  if (! EventT::RotMatrices()) MakeListOfRotations();
  StEvent* pEvent = (StEvent*) GetInputDS("StEvent");
  if (pEvent && pEvent->primaryVertex() && !fEvent->Build(pEvent,fMinNoHits,fpCut)) fTree->Fill();  //fill the tree
  return kStOK;
}
//________________________________________________________________________________
void SvtMatchedTree::Print(Option_t *opt) const {
  if (! EventT::RotMatrices()) return;
  TIter next(EventT::RotMatrices());
  TGeoHMatrix *comb = 0;
  while ((comb = (TGeoHMatrix *) next())) {
    Int_t Id;
    sscanf(comb->GetName()+1,"%04i",&Id);
    Int_t Ladder = Id%100;
    Int_t Layer  = Id/1000; if (Layer > 7) Layer = 7;
    Int_t Wafer  = (Id - 1000*Layer)/100;
    cout << comb->GetName() << "\tLayer/Ladder/Wafer = " << Layer << "/" << Ladder << "/" << Wafer << endl;
    comb->Print();
    cout << "=================================" << endl;
  }
}
//________________________________________________________________________________
void SvtMatchedTree::MakeListOfRotations() {
  if (EventT::RotMatrices()) return;
  THashList *rotMHash = new THashList(100,0);
  rotMHash->SetOwner(kFALSE);
  EventT::SetRotMatrices(rotMHash);
  THashList *hash = 0;
  for (Int_t i = 0; i < 2; i++) {
    if (i == 0 && gStSvtDbMaker) hash = gStSvtDbMaker->GetRotations();
    if (i == 1 && gStSsdDbMaker) hash = gStSsdDbMaker->GetRotations();
    if (! hash) continue;
    TIter next(hash);
    TGeoHMatrix *comb;
    while ((comb = (TGeoHMatrix *) next())) rotMHash->Add(comb);
  }
#if 0  
  TDataSet *set = GetDataBase("Geometry/ssd");
  St_Survey *SsdOnGlobal = (St_Survey *) set->Find("SsdOnGlobal");
  if (! SsdOnGlobal)  {cout << "SsdOnGlobal has not been found"  << endl; return;}
  TGeoHMatrix GL, LS,SG,LA,WG;
  Survey_st *OnGlobal         = SsdOnGlobal->GetTable();        // SSD and SVT as whole 
  GL.SetRotation(&OnGlobal->r00);
  GL.SetTranslation(&OnGlobal->t0);
  
  // SSD 
  St_Survey *SsdSectorsOnGlobal = (St_Survey *) set->Find("SsdSectorsOnGlobal");
  if (! SsdSectorsOnGlobal)  {cout << "SsdSectorsOnGlobal has not been found"  << endl; return;}
  St_Survey *SsdLaddersOnSectors = (St_Survey *) set->Find("SsdLaddersOnSectors");// ladders in the SSD sector coordinate systems
  if (! SsdLaddersOnSectors) {cout << "SsdLaddersOnSectors has not been found" << endl; return;}
  St_Survey *SsdWafersOnLadders = (St_Survey *) set->Find("SsdWafersOnLadders");  // wafers in the SSD ladder coordinate systems
  if (! SsdWafersOnLadders)  {cout << "SsdWafersOnLadders has not been found"  << endl; return;}
  Survey_st *SectorsOnGlobal = SsdSectorsOnGlobal->GetTable();  // sectors in the SSD barrel coordinate system
  Survey_st *LaddersOnSectors = SsdLaddersOnSectors->GetTable();// ladders in the SSD sector coordinate systems
  Survey_st *WafersOnLadders = SsdWafersOnLadders->GetTable();  // wafers in the SSD ladder coordinate systems
  Int_t NoSectors = SsdSectorsOnGlobal->GetNRows();
  Int_t NoLadders = SsdLaddersOnSectors->GetNRows();
  Int_t NoWafers  = SsdWafersOnLadders->GetNRows();
  Int_t num = 0;
  for (Int_t i = 0; i < NoWafers; i++,WafersOnLadders++) {
    Int_t Id = WafersOnLadders->Id;
    TGeoHMatrix *comb = (TGeoHMatrix *) rotMHash->FindObject(Form("R%04i",Id));
    if (comb) continue;
    comb = new TGeoHMatrix(Form("R%04i",Id)); 
    Int_t layer  = Id/1000;
    if (layer > 7) layer = 7;
    Int_t ladder  = Id%100;
    TGeoHMatrix *WL = (TGeoHMatrix *) rotMHash->FindObject(Form("WL%04i",Id));
    if (! WL) {
      WL = new  TGeoHMatrix(Form("WL%04i",Id)); 
      WL->SetRotation(&WafersOnLadders->r00);
      WL->SetTranslation(&WafersOnLadders->t0); //cout << "WL\t"; WL.Print();
      rotMHash->Add(WL);
    }
    LaddersOnSectors = SsdLaddersOnSectors->GetTable();
    Int_t Ladder = 0;
    Int_t Sector = 0;
    for (Int_t l = 0; l < NoLadders; l++, LaddersOnSectors++) {
      //cout << "LaddersOnSectors Id\t" << LaddersOnSectors->Id << endl;
      Ladder = LaddersOnSectors->Id%100;
      if (Ladder == ladder) {
	Sector = LaddersOnSectors->Id/100;
	LS.SetRotation(&LaddersOnSectors->r00);
	LS.SetTranslation(&LaddersOnSectors->t0);
	//cout << "LS\t"; LS.Print();
	break;
      }
    }
    if (Sector <= 0 || Sector > 4) {cout << "Sector has not been defined" << endl; continue;}
    SectorsOnGlobal = SsdSectorsOnGlobal->GetTable();
    Int_t sector = 0;
    for (Int_t s = 0; s <NoSectors; s++, SectorsOnGlobal++) {
      //cout << "SectorsOnGlobal Id\t" << SectorsOnGlobal->Id << endl;
      if (SectorsOnGlobal->Id != Sector) continue;
      sector = Sector;
      SG.SetRotation(&SectorsOnGlobal->r00);
      SG.SetTranslation(&SectorsOnGlobal->t0); //cout << "Sector\t" << Sector << "\tSG\t"; SG.Print();
      break;
    }
    if (! sector) {cout << "Sector\t" << Sector << " has not been found" << endl; continue;}
    //    WG = SG * LS * WL * LA; //cout << "WG\t"; WG.Print();
    //    WG = SG * LS * WL * LA = SG * ( LS * WL * LA * WL**-1 ) *WL
    WG = GL * SG * LS * (*WL); //cout << "WG\t"; WG.Print();
    ssdWafersPosition_st row;
    row.id = Id;
    row.id_shape  = 2;
    row.ladder = ladder;
    row.layer  = layer;
    num++;
    row.num_chip  = (num-1)%16 + 1;
    //    TGeoHMatrix WGInv = WG.Inverse();
    //    Double_t *wgrot = WGInv.GetRotationMatrix();
    Double_t *r = WG.GetRotationMatrix();
    row.driftDirection[0] = r[0]; row.normalDirection[0] = r[1]; row.transverseDirection[0] = r[2];
    row.driftDirection[1] = r[3]; row.normalDirection[1] = r[4]; row.transverseDirection[1] = r[5];
    row.driftDirection[2] = r[6]; row.normalDirection[2] = r[7]; row.transverseDirection[2] = r[8];
    Double_t norm;
    TVector3 d(row.driftDirection); norm = 1/d.Mag(); d *= norm;
    TVector3 t(row.transverseDirection); norm = 1/t.Mag(); t *= norm;
    TVector3 n(row.normalDirection);
    TVector3 c = d.Cross(t);
    if (c.Dot(n) < 0) c *= -1;
    d.GetXYZ(row.driftDirection);
    t.GetXYZ(row.transverseDirection);
    c.GetXYZ(row.normalDirection);
    
    Double_t *wgtr = WG.GetTranslation();
    //    memcpy(row.driftDirection,wgrot, 9*sizeof(Double_t));
    memcpy(row.centerPosition,wgtr, 3*sizeof(Double_t));
    Double_t rot[9] = {
      row.driftDirection[0], row.transverseDirection[0], row.normalDirection[0],
      row.driftDirection[1], row.transverseDirection[1], row.normalDirection[1],
      row.driftDirection[2], row.transverseDirection[2], row.normalDirection[2]};
    Double_t tr[3] = {row.centerPosition[0],
		      row.centerPosition[1],
		      row.centerPosition[2]};
    comb->SetRotation(rot);
    comb->SetTranslation(tr);
    rotMHash->Add(comb);
  }
  // SVT  
  set = GetDataBase("Geometry/svt");
  St_Survey *WaferOnLadder = (St_Survey *) set->Find("WaferOnLadder");
  St_Survey *LadderOnSurvey = (St_Survey *) set->Find("LadderOnSurvey");
  St_Survey *LadderOnShell = (St_Survey *) set->Find("LadderOnShell");
  St_Survey *ShellOnGlobal = (St_Survey *) set->Find("ShellOnGlobal");
  Int_t NW = WaferOnLadder->GetNRows();
  Int_t NL = LadderOnSurvey->GetNRows();
  Survey_st *waferOnLadder = WaferOnLadder->GetTable();
  Survey_st *shellOnGlobal0 = ShellOnGlobal->GetTable(0);
  Survey_st *shellOnGlobal1 = ShellOnGlobal->GetTable(1);
  TGeoHMatrix LSU, LSH, SHG[2];
  SHG[0].SetRotation(&shellOnGlobal0->r00);
  SHG[0].SetTranslation(&shellOnGlobal0->t0);
  SHG[1].SetRotation(&shellOnGlobal1->r00);
  SHG[1].SetTranslation(&shellOnGlobal1->t0);
  
  for (Int_t i = 0; i < NW; i++, waferOnLadder++)    {
    Int_t id = waferOnLadder->Id;
    Int_t wbarrel  = id/1000;
    Int_t wwafer  = (id - 1000*wbarrel)/100;
    Int_t wladder = id%100;
    Int_t wlayer = 2*wbarrel + wladder%2 - 1;
    //    Id = 1000* layer + 100* wafer +  ladder;
    Int_t Id = 1000*wlayer + 100*wwafer + wladder;
    TGeoHMatrix *comb = (TGeoHMatrix *) rotMHash->FindObject(Form("R%04i",Id));
    if (comb) continue;
    comb = new TGeoHMatrix(Form("R%04i",Id)); 
    Int_t Found = 0;
    Survey_st *ladderOnSurvey = LadderOnSurvey->GetTable();
    Survey_st *ladderOnShell = LadderOnShell->GetTable();
    for ( Int_t j = 0; j < NL; j++, ladderOnSurvey++, ladderOnShell++)	{
      Int_t Idl =  ladderOnSurvey->Id;
      Int_t lladder = Idl%100;
      if( wladder !=  lladder )	continue;
      LSH.SetRotation(&ladderOnShell->r00);
      LSH.SetTranslation(&ladderOnShell->t0);
      Int_t Shell = 1;
      if( (wbarrel == 1 && wladder <= 4) || (wbarrel == 2 && wladder <= 6) ||  (wbarrel == 3 && wladder <= 8) ) Shell = 0;
      // 	shellOnGlobal *	ladderOnShell * ladderOnSurvey * waferOnLadder 
      TGeoHMatrix *WL = (TGeoHMatrix *) rotMHash->FindObject(Form("WL%04i",Id));
      if (! WL) {
	TGeoHMatrix wL;
	wL.SetRotation(&waferOnLadder->r00);
	wL.SetTranslation(&waferOnLadder->t0);
	WL = new  TGeoHMatrix(); 
	LSU.SetRotation(&ladderOnSurvey->r00);
	LSU.SetTranslation(&ladderOnSurvey->t0);
	*WL = LSU * wL;
	WL->SetName(Form("WL%04i",Id)); 
	rotMHash->Add(WL);
      }
      //	WG = GL * SHG * LSH * LSU * (*WL); //  WG.Print();
      WG = GL * SHG[Shell] * LSH * (*WL); //  WG.Print();
      Double_t *r = WG.GetRotationMatrix();
      Int_t fail = 0;
      for (int l = 0; l < 9; l++) {
	if (TMath::Abs(r[l]) >=  1.000001) fail++;
      }
      if (fail) {
	cout << "===============" << waferOnLadder->Id << "  "<< id << " " <<  Id <<endl;
	cout << "WG\t"; WG.Print();
      }
      svtWafersPosition_st row;
      row.driftDirection[0] = r[0]; row.normalDirection[0] = r[1]; row.transverseDirection[0] = r[2];
      row.driftDirection[1] = r[3]; row.normalDirection[1] = r[4]; row.transverseDirection[1] = r[5];
      row.driftDirection[2] = r[6]; row.normalDirection[2] = r[7]; row.transverseDirection[2] = r[8];
      Double_t norm;
      TVector3 d(row.driftDirection); norm = 1/d.Mag(); d *= norm;
      TVector3 t(row.transverseDirection); norm = 1/t.Mag(); t *= norm;
      TVector3 n(row.normalDirection);
      TVector3 c = d.Cross(t);
      if (c.Dot(n) < 0) c *= -1;
      d.GetXYZ(row.driftDirection);
      t.GetXYZ(row.transverseDirection);
      c.GetXYZ(row.normalDirection);
      
      row.ID = 100*wwafer + wladder + 1000*wlayer;
      Double_t *wgtr = WG.GetTranslation();
      memcpy(row.centerPosition,wgtr, 3*sizeof(Double_t));
      Double_t rot[9] = {
	row.driftDirection[0], row.transverseDirection[0], row.normalDirection[0],
	row.driftDirection[1], row.transverseDirection[1], row.normalDirection[1],
	row.driftDirection[2], row.transverseDirection[2], row.normalDirection[2]};
      Double_t tr[3] = {row.centerPosition[0],
			row.centerPosition[1],
			row.centerPosition[2]};
      comb->SetRotation(rot);
      comb->SetTranslation(tr);
      rotMHash->Add(comb);
      Found++;
      break;
    }
    assert(Found);
  }
#endif
  TIter next(rotMHash);
  TGeoHMatrix *comb;
  Int_t fail = 0;
  while ((comb = (TGeoHMatrix *) next())) {
    TString Name(comb->GetName());
    if (Name.BeginsWith("R")) {
      TGeoHMatrix *WL = (TGeoHMatrix *) rotMHash->FindObject(Form("WL%s",Name.Data()+1));
      if (! WL) {
	cout << Form("WL%s",Name.Data()+1) << " has not been found" << endl;
	fail++;
      }
    }
  }
  assert(! fail);
#if 0
  if (fFile) {
    TDirectory *g = 0;
    if (gDirectory != fFile) {
      g = gDirectory; 
      fFile->cd(); 
    }
    rotMHash->Write();
    if (g) g->cd();
  }
#endif
}
