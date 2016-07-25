#include "Riostream.h"
#include "TH1.h"
#include "TFile.h"
#include "TGeoMatrix.h"
#include "TKey.h"
//#define DEBUG
#define PRP(x) cout << (#x) << " = "; if (x) x->Print();
#define PRM(x) cout << (#x) << " = "; x.Print();
#define PRT(x) cout << (#x); for (Int_t i = 0; i < 3; i++) cout << "\t" << x[i]; cout << endl;
void MoverS(const Char_t * marcelo = "20050101.1.ofl+sim.root",//"20041024.1.simu.root", 
	    const Char_t *spiros = "SurveyMatrices.root") {
  TFile *Marcelo = new TFile(marcelo);
  TFile *Spiros  = new TFile(spiros);
  TIter nextkey( Spiros->GetListOfKeys() );
  TGeoHMatrix C;
  Double_t rot[9] = {1, 0, 0, 0, 0, 1, 0, 1, 0};
  C.SetRotation(rot);
  TGeoHMatrix CI = C.Inverse();
  TKey *key;
  const Char_t *Names[6] = {"x","y","z","Phi_x","Phi_y","Phi_z"};
  TH1D *hists[2][6];
  for (Int_t k = 0; k < 2; k++) 
    for (Int_t i = 0; i < 6; i++) {
      hists[k][i] = new TH1D(Form("%sShell%i",Names[i],k),Names[i],100,0,0);
      hists[k][i]->SetLineColor(k+1);
    }
  while ((key = (TKey*) nextkey())) {
    TString name(key->GetName());
    if (! name.BeginsWith("W")) continue;
    Int_t Id = atoi(name.Data()+1); //  Id = ladder + 100*(wafer + 10*barel);
#ifdef DEBUG
    if (Id%1000 != 102) continue;
#endif
    cout << name << "\t" << Id 
	 << " ====================" << endl;
    TGeoHMatrix *W = (TGeoHMatrix *) Spiros->Get(name);
    Int_t shell  = 0;
    Int_t barrel  = Id/1000;
    Int_t wafer  = (Id - 1000*barrel)/100;
    Int_t ladder = Id%100;
    TGeoHMatrix *L = (TGeoHMatrix *) Spiros->Get(Form("L%i",1000*barrel+ladder));
    if (! L) {shell = 1; L = (TGeoHMatrix *) Spiros->Get(Form("L%i",10000+1000*barrel+ladder));}
    TGeoHMatrix *S = (TGeoHMatrix *) Spiros->Get(Form("S%i",Id));
    //  Id = ladder + 100*(wafer + 10*layer);
    Int_t layer = 2*barrel-1;
    TGeoHMatrix *M = (TGeoHMatrix *) Marcelo->Get(Form("M%i",ladder + 100*(wafer + 10*layer)));
    if (! M) {
      layer++;
      M = (TGeoHMatrix *) Marcelo->Get(Form("M%i",ladder + 100*(wafer + 10*layer)));
    }
    PRP(W);    PRP(L);    PRP(S);    PRP(M);
    if (!W || !L || !S || !M) {cout << "Missing matrix" << endl; continue;}
    TGeoHMatrix MR = (*M) * CI; PRM(MR);
    //    TGeoHMatrix MR = CI * (*M); PRM(MR);
    //    TGeoHMatrix WC = (*W) * C;
    TGeoHMatrix LW = (*L) * (*W);
    //    TGeoHMatrix LW = (*L) * (*W);
    TGeoHMatrix SLW = (*S) * LW;    PRM(SLW);
    const TGeoHMatrix &SLWInv = SLW.Inverse(); PRM(SLWInv);
    //    TGeoHMatrix Shell   = (*M) * SLWInv;
    TGeoHMatrix Shell   = MR * SLWInv; PRM(Shell);
    Double_t *tran = SLW.GetTranslation(); PRT(tran);
    Double_t *rinv = Shell.GetRotationMatrix();
    TGeoHMatrix R; R.SetRotation(rinv); PRM(R);
    Double_t tranc[3];
    R.LocalToMaster(tran,tranc); PRT(tranc);
    const Double_t *tr1 = SLWInv.GetTranslation(); PRT(tr1);
    //    const Double_t *trM = M->GetTranslation(); PRT(trM);
    const Double_t *trM = MR.GetTranslation(); PRT(trM);
    Double_t tr[3] = {trM[0]-tranc[0],trM[1]-tranc[1],trM[2]-tranc[2]}; PRT(tr);
    //    Double_t *trr = Shell.GetTranslation(); PRT(trr);
    //    Shell.SetTranslation(tr); PRM(Shell);
    Double_t *rr = Shell.GetRotationMatrix();
    for (Int_t i = 0; i < 3; i++) hists[shell][i]->Fill(tr[i]);
    hists[shell][3]->Fill(rr[5]);
    hists[shell][4]->Fill(rr[2]);
    hists[shell][5]->Fill(rr[1]);
  }
}
