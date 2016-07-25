class StBFChain;        
class StMessMgr;
#if !defined(__CINT__) || defined(__MAKECINT__)
#include "Stiostream.h"
#include "TSystem.h"
#include "TClassTable.h"
#include "TApplication.h"
#include "TInterpreter.h"
#include "StBFChain.h"
#include "StMessMgr.h"
#include "TROOT.h"
#include "StarRoot/TAttr.h"
#include "tables/St_g2t_vertex_Table.h"
#include "tables/St_g2t_track_Table.h"
#endif
#define UseLogger
StBFChain    *chain=0; 
TString defChain("y2005e,Test.default.ITTF");
void bfc(Int_t First, Int_t Last,const Char_t *Chain = defChain + ",Display",
	 const Char_t *infile=0, const Char_t *outfile=0, const Char_t *TreeFile=0);
void bfc(Int_t Last, const Char_t *Chain = defChain,
	 const Char_t *infile=0, const Char_t *outfile=0, const Char_t *TreeFile=0);
void bfc(Int_t First, const Char_t *Chain = "",	 const Char_t *infile=0);
//________________________________________________________________________________
void ctau(Int_t N = 1, const Char_t *file = "/star/institutions/ksu/margetis/D0/simul/rcf1272_4058_400evts_37.geant.root") {
  gROOT->LoadMacro("bfc.C");
  bfc(0,"in,gen_T,sim_T,nodefault",file); 
  // Event loop
  Int_t i = 0;
  Int_t istat = 0;
  Double_t M  = 1.865;
  Double_t ctauP = 124.4e-4;
  TFile *newF = new TFile("New_ctau.root","recreate");
  TH1 *tauF = new TH1D("tauF","tau",100,0,5);
  TH1 *DecF = new TH1D("DecF","tau",100,0,0.1);
  TH1 *DecXYF = new TH1D("DecXYF","tau",100,0,0.1);
 EventLoop: 
  istat = chain->MakeEvent();
  if (! istat) {
    St_g2t_vertex *vtx = (St_g2t_vertex *) chain->Find("bfc/.make/inputStream/.make/inputStream_Root/.data/bfcTree/geantBranch/g2t_vertex");
    St_g2t_track  *trk = (St_g2t_track *) chain->Find("bfc/.make/inputStream/.make/inputStream_Root/.data/bfcTree/geantBranch/g2t_track");
    if (vtx && trk) {
      //      vtx->Print(0,2);
      //      trk->Print(0,1);
      g2t_vertex_st *V0 = vtx->GetTable();
      g2t_vertex_st *V1 = V0+1;
      g2t_track_st  *T = trk->GetTable();
      if (V0 && V1 && T) {
	Double_t DX = V1->ge_x[0]-V0->ge_x[0];
	Double_t DY = V1->ge_x[1]-V0->ge_x[1];
	Double_t DZ = V1->ge_x[2]-V0->ge_x[2];
	Double_t DL = TMath::Sqrt(DX*DX + DY*DY + DZ*DZ);
	Double_t BG = T->ptot/M;
	Double_t t  = DL/(ctauP*BG);
	cout << "DL = " << DL << "\tBG = " << BG << "\tt = " << t << endl;
	tauF->Fill(t);
	DecF->Fill(DL);
	DecXYF->Fill(TMath::Sqrt(DX*DX + DY*DY));
      }
    }
    i++;
    if (i < N) goto EventLoop;
  }
  newF->Write();
}
