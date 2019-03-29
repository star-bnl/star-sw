/*  Tpc Super Sector Position based on MuTpcG.C
    root.exe lDb.C MakeTpcSuperSector.C+
    > MakeSuperSectorPositionB()
*/
#if !defined(__CINT__)
// code that should be seen ONLY by the compiler
#else
#if !defined(__CINT__) || defined(__MAKECINT__)
// code that should be seen by the compiler AND rootcint
#else
// code that should always be seen
#endif
#endif
//________________________________________________________________________________
#if !defined(__CINT__) || defined(__MAKECINT__)
#include "Riostream.h"
#include "TROOT.h"
#include "TSystem.h"
#include "TMath.h"
#include "TH1.h"
#include "TStyle.h"
#include "TGraph.h"
#include "TNtuple.h"
#include "TCanvas.h"
#include "TString.h"
#include "TLegend.h"
#include "TFile.h"
#include "TNamed.h"
#include "SuperSectorPar.h"
#include "THStack.h"
#include "StChain.h"
#include "tables/St_Survey_Table.h"
#include "TGeoMatrix.h"
#include "StTpcDb/StTpcDb.h"
#include "StEvent/StEnumerations.h"
#include "StDetectorDbMaker/StTpcSurveyC.h"
#endif
TCanvas *c1 = 0;
static THStack *hs[6];
static TLegend *leg[6];
static TGeoHMatrix RAv[3], RAvI[3];
static TGeoHMatrix dR[24], dRC[24];
static TGeoHMatrix Rideal[24], RidealI[24];
static const Double_t scale = 1.0;
//________________________________________________________________________________
void RemoveOverAllShift(SurveyPass_t &Pass) {
  /* Euler angles: a = phi, b = theta, g = psi
                  (  1 -(a+g)  0)   
     R(a,b,g) =   (a+g      1 -b) = 
                  (  0      b  1)
   ( cospsi*cosphi - costhe*sinphi*sinpsi;   -sinpsi*cosphi - costhe*sinphi*cospsi;    sinthe*sinphi;)
   ( cospsi*sinphi + costhe*cosphi*sinpsi;   -sinpsi*sinphi + costhe*cosphi*cospsi;   -sinthe*cosphi;) =
   (                        sinpsi*sinthe;                           cospsi*sinthe;           costhe;) 

   (       1; -psi-phi;    0;
   ( phi+psi;        1; -the;) =  psi = 0
   (       0;      the;    1;) 


   (   1;  -(a+g);   0;)
   ( a+g;      1;   -b;) =
   (   0;;     b;    1;) 

   Tait–Bryan angles:      dR.RotateX(a); dR.RotateY(b); dR.Rotate(g);

                            ( 1  -g  b dx)       (nx)              (   nx +g*ny -b*nz)        (x)           (  x - g*y )
      dR(a,b,g,dx,dy,dz) =  ( g   1 -a dy); n =  (ny);   dR x n =  (-g*nx +  ny +a*nz);   X = (y); dR x X = (dy)
                            (-b   a  1 dz)       (nz)		   ( b*nx -a*ny +  nz)        (z)           (dz)
		            ( 0   0  0  1)       ( 0) 		   ( 0               )        (1)           ( 1)

			    Euler <=> Tait–Bryan : a+g <=> g; b <=> a; 0 <=> b;
Euler Rotation in Tait-Bryan angles

   (   1;  -g;   0;)
   (   g;   1;  -a;) =
   (   0;;  a;   1;) 
=========              T = Sup2TPC; dRi = I, Rold 
    dRAv = <dTPCreal * dTPCideal^-1> = <T * dRreal * T^-1> = <T *dRr * T^-1> = dRAv; 
    Rtpc * dRAv^-1 = T * dRr * dRAv^-1
    T^-1 * dRAv * T= dRr * dRi^-1 
========
    A    = <T * dR * C * T^-1>; dR * C = 
    D    =  T * dR;  D * A^-1 = T * dR * A^-1 = T * dR *  
   */
  Int_t date = Pass.date;
  Int_t time = Pass.time;
  StChain *chain = (StChain *) StChain::GetChain();
  StMaker *db = chain->Maker("db");
  if (! db) return;
  db->SetDebug(1);
  db->SetDateTime(date,time);
  static Bool_t InitDone = kFALSE;
  if (! InitDone) {
    chain->Init();
    InitDone = kTRUE;
  }
  chain->MakeEvent();
  // Calculate average shift and rotation for each half of TPC and overall TPC
  Double_t phi[24] = {0};
  Double_t theta[24] = {0};
  Double_t psi[24] = {0};
  Double_t tra[24][3] = {0};
  for (Int_t i = 0; i < 24; i++) {
    /*
      dR.RotateX(a); dR.RotateY(b); dR.Rotate(g);

                            ( 1  -g  b dx)       (nx)              (   nx +g*ny -b*nz)        (x)           (  x - g*y )
      dR(a,b,g,dx,dy,dz) =  ( g   1 -a dy); n =  (ny);   dR x n =  (-g*nx +  ny +a*nz);   X = (y); dR x X = (dy)
                            (-b   a  1 dz)       (nz)		   ( b*nx -a*ny +  nz)        (z)           (dz)
		            ( 0   0  0  1)       ( 0) 		   ( 0               )        (1)           ( 1)
     */
    StBeamDirection part = east;
    if (i < 12) part = west;
    Double_t xyz[3];
    xyz[0] = 1e-4*Pass.Data[i].x*scale;
    xyz[1] = 1e-4*Pass.Data[i].y*scale;
    xyz[2] = 1e-4*Pass.Data[i].z*scale;
    //#define __SWAP_SIGNS__
#ifdef __SWAP_SIGNS__
    Pass.Data[i].alpha *= -1.;
    Pass.Data[i].beta  *= -1.;
    Pass.Data[i].gamma *= -1.;
#endif
    //#define __ROTATION__
#ifdef __ROTATION__
    dR[i].RotateX( TMath::RadToDeg()*Pass.Data[i].alpha*1e-3*scale);
#if 0
    dR[i].RotateY( TMath::RadToDeg()*Pass.Data[i].beta*1e-3*scale); 
#endif
    dR[i].RotateZ( TMath::RadToDeg()*Pass.Data[i].gamma*1e-3*scale);  // swap sign 03/13/19
#endif /* __ROTATION__ */
    dR[i].SetTranslation(xyz);
    Rideal[i]     = StTpcDb::instance()->SupS2Glob(i+1); cout << "Ideal:"; Rideal[i].Print();
    RidealI[i]    = Rideal[i].Inverse(); cout << "Inverse:"; RidealI[i].Print();
    TGeoHMatrix Rreal      = StTpcDb::instance()->SupS2Glob(i+1)*dR[i]; cout << "Real:"; Rreal.Print();
    TGeoHMatrix R = Rreal * Rideal[i].Inverse(); cout <<"Diff:"; R.Print();
    const Double_t *xyzR = R.GetTranslation();
    for (Int_t j = 0; j < 3; j++) tra[i][j] = xyzR[j];
#if 0
    TGeoRotation RR(R);
    RR.GetAngles(phi[i],theta[i],psi[i]); 
#else
    Double_t *m = R.GetRotationMatrix();
    theta[i] = TMath::ACos(m[8])*TMath::RadToDeg();
    phi[i]   = TMath::ATan2(-m[8]*m[1],m[0])*TMath::RadToDeg();
    psi[i]   = 0.; // convention, phi+psi matters
#endif
    cout << " sec " << i+1 << " phi " << phi[i] << " theta " << theta[i] << " psi " << psi[i] << endl;
  }
  Double_t dphi[3] = {0}, dtheta[3] = {0}, dpsi[3] = {0}, dxyz[3][3] = {0};
  for (Int_t l = 0; l < 3; l++) {
    Int_t i1 = 0, i2 = 24;
    if      (l == 0) {i2 = 12;}
    else if (l == 1) {i1 = 12;}
    Int_t N = i2 - i1;
    for (Int_t i = i1; i < i2; i++) {
      dphi[l]   += phi[i]/N;
      dtheta[l] += theta[i]/N;
      dpsi[l]   += psi[i]/N;
      for (Int_t j = 0; j < 3; j++)dxyz[l][j] += tra[i][j]/N;
    }
    TGeoRotation RR;
    RR.SetAngles(dphi[l], dtheta[l], dpsi[l]);
    RAv[l].SetRotation(RR.GetRotationMatrix());
    RAv[l].SetTranslation(dxyz[l]);
    if (l == east) cout << "east:";
    else if (l == west) cout << "west:";
    else                cout << "Tpc:";
    RAv[l].Print();
    RAvI[l] = RAv[l].Inverse(); cout << "Inverse:"; RAvI[l].Print("");
  }
  for (Int_t i = 0; i < 24; i++) {
    StBeamDirection part = east;
    if (i < 12) part = west;
    //    cout << "dR" << i << ":"; dR[i].Print("");
    /*
      RAV = <Rideal*dR*Rideal^1>; dRAv = Rideal
      RA  = Rideal*dR
      RA*dRAV^-1   = Rideal*dR*<Rideal*dR*Rideal^1>^-1 = Rideal*dR *(
      T*dR * T^-1 * RAv^-1 * T

      g = T * l;
      A = g * T^-1 = T * l * T^-1
      A = g * C^-1; g = A * C = 

      
     */
#ifdef __ACCOUNT_OFFSET__
    TGeoHMatrix A = RidealI[i] * RAvI[part] * Rideal[i]; cout << "A\t:"; A.Print();
    dRC[i] = dR[i]*A ; //cout << "dRC:"; dRC[i].Print("");
    cout << "sec = " << i+1 << " before:"; Pass.Data[i].Print();
#else
    dRC[i] = dR[i];
#endif
    Pass.Data[i].x = dRC[i].GetTranslation()[0]*1e4/scale;
    Pass.Data[i].y = dRC[i].GetTranslation()[1]*1e4/scale;
    Pass.Data[i].z = dRC[i].GetTranslation()[2]*1e4/scale;
    Pass.Data[i].alpha = -dRC[i].GetRotationMatrix()[5]*1e3/scale;
    //    Pass.Data[i].beta  = 0;
    Pass.Data[i].gamma = -dRC[i].GetRotationMatrix()[1]*1e3/scale;
    cout << "sec = " << i+1 << " after :"; Pass.Data[i].Print();
    cout << "================================================================================" << endl;
  }
}
//________________________________________________________________________________
void MakeSuperSectorPositionB(){
  const Char_t *plots[3] = {"dXS","dYS","dZS"};
  for (Int_t j = 1; j <= 24; j++) {
    cout << Form("%2i ",j); Passes[NP-1].Data[j-1].Print();
  }
  //  St_Survey *TpcSuperSectorPositionB = (St_Survey *) chain->GetDataBase("Geometry/tpc/TpcSuperSectorPositionB");
  St_Survey *TpcSuperSectorPositionB = (St_Survey *) StTpcSuperSectorPosition::instance()->Table(); //chain->GetDataBase("Geometry/tpc/TpcSuperSectorPositionB");
  if (! TpcSuperSectorPositionB)  {cout << "TpcSuperSectorPositionB has not been found"  << endl; return;}
  TpcSuperSectorPositionB->Print(0,24);
  TGeoHMatrix GL;
  Survey_st *TpcSuperSectorOld         = TpcSuperSectorPositionB->GetTable();        
  Survey_st row[24];
  /* 
     Rav = R * dR => R^-1 * Rav =  dR
     
   */
  for (Int_t i = 0; i < 24; i++) {
    StBeamDirection part = east;
    if (i < 12) part = west;
    row[i] = TpcSuperSectorOld[i];
    GL.SetRotation(&TpcSuperSectorOld[i].r00);
    GL.SetTranslation(&TpcSuperSectorOld[i].t0); cout << "s: " << i+1 << " GL\t"; GL.Print();
    Double_t dxyz[3], drot[3];
    dxyz[0] = 1e-4*Passes[NP-1].Data[i].Dx;
    dxyz[1] = 1e-4*Passes[NP-1].Data[i].Dy;
    dxyz[2] = 1e-4*Passes[NP-1].Data[i].Dz;
    drot[0] = Passes[NP-1].Data[i].Dalpha*1e-3;
    drot[1] = Passes[NP-1].Data[i].Dbeta*1e-3; 
    drot[2] = Passes[NP-1].Data[i].Dgamma*1e-3;
    memcpy(&row[i].sigmaRotX, drot, 3*sizeof(Double_t));
    memcpy(&row[i].sigmaTrX, dxyz, 3*sizeof(Double_t));
    cout << "Additional rotation for Super Sector\t"; dR[i].Print();
    cout << "dR" << i << ":"; dR[i].Print("");
    cout << "dRC" << i << ":"; dRC[i].Print("");
    //    TGeoHMatrix dRC = dR[i]*RAvI[part]; cout << "dRC:"; dRC.Print("");
    //    TGeoHMatrix GLnew = StTpcDb::instance()->Flip().Inverse()*dR[i]*StTpcDb::instance()->Flip()*GL; GLnew.Print(); // Flip 03/14/19
    //    TGeoHMatrix GLnew = GL*dR[i]; GLnew.Print(); // used till 03/13/19 and after 03/15/19
    TGeoHMatrix GLnew = GL*dRC[i]; GLnew.Print(); // used till 03/13/19 and after 03/15/19
    //    TGeoHMatrix GLnew = dR[i] * GL; GLnew.Print();
    Double_t *R = GLnew.GetRotationMatrix();
    memcpy(&row[i].r00, R, 9*sizeof(Double_t));
    Double_t *tr = GLnew.GetTranslation();
    memcpy(&row[i].t0, tr, 3*sizeof(Double_t));
  }
  Int_t date = Passes[NP-1].date;
  Int_t time = Passes[NP-1].time;
  TString fOut =  Form("TpcSuperSectorPositionB.%8i.%06i.C", date, time);
  ofstream out;
  cout << "Create " << fOut.Data() << endl;
  out.open(fOut.Data());
  out << "TDataSet *CreateTable() {" << endl;
  out << "  if (!gROOT->GetClass(\"St_Survey\")) return 0;" << endl;
  out << "  Survey_st row[24] = {" << endl; 
  out << "    //             -gamma     beta    gamma            -alpha    -beta    alpha                x0       y0       z0" << endl;
  for (Int_t i = 0; i < 24; i++) {
    out << "    {" << Form("%2i",i+1) << ","; 
    Double_t *r = &(row[i].r00);
    //    cout << " ";
    for (Int_t j =  0; j <  9; j++) out << Form("%8.5f,",r[j]);
    //    cout << " ";
    for (Int_t j =  9; j < 12; j++) out << Form("%8.4f,",r[j]); 
    cout << " ";
    for (Int_t j = 12; j < 18; j++) out << Form("%8.5f,",r[j]);
    out << "\"" << Passes[NP-1].PassName << "\"}";
    if (i != 23) out << ",";
    out << endl;
  }
  cout << endl;
  out << "  };" << endl;
  out << "  St_Survey *tableSet = new St_Survey(\"TpcSuperSectorPositionB\",24);" << endl; 
  out << "  for (Int_t i = 0; i < 24; i++) tableSet->AddAt(&row[i].Id);" << endl;
  out << "  return (TDataSet *)tableSet;" << endl;
  out << "}" << endl;
  out.close(); 
}
//________________________________________________________________________________
void MakeTpcSuperSector(const Char_t *opt="") {
  gStyle->SetMarkerStyle(20);
  gStyle->SetOptStat(0);
  cout << "NP \t" << NP << endl;
  Int_t NH = NP;
  if (NH == 2) NH++; // make average if we have only FF + RF
  TH1D ***dath = new TH1D**[NH]; 
  for (Int_t p = 0; p < NH; p++) {dath[p] = new TH1D*[6]; memset(dath[p],0, 6*sizeof(TH1D*));}
  const Char_t *names[6] = {" #Deltax"," #Deltay"," #Deltaz"," #Delta #alpha"," #Delta #beta"," #Delta #gamma"};
  const Char_t *nameK[6] = {"Dx","Dy","Dz","Da",     "Db",    "Dg"};
  TString Opt(opt);
  for (Int_t i = 0; i < 6; i++) {
    hs[i] = new THStack(nameK[i],names[i]);
#if 0
    if (i < 3) hs[i]->SetYTitle(Form("%s (#mum)",names[i]));
    else       hs[i]->SetYTitle(Form("%s (mrad)",names[i]));
    hs[i]->SetXTitle("sector");
#endif
    if (! i)     leg[i] = new TLegend(0.10,0.65,0.30,0.90);
    else         leg[i] = 0;
  }
  TString same("e");
  TH1::SetDefaultSumw2(kTRUE);
  for (Int_t k = 0; k < NP; k++) {
    RemoveOverAllShift(Passes[k]);
    for (Int_t i = 0; i < 6; i++) {
      Int_t color = k+1;
      TString Name;
      TString Title;
      Double_t ymin =  1e10;
      Double_t ymax = -1e10;
      if (k < NP) {
	if (i == 0 && k < NP) Passes[k].Print();
	Name = Form("%s%s",Passes[k].PassName,nameK[i]);
	if (Opt != "" && ! Name.Contains(Opt,TString::kIgnoreCase)) continue;
	Title = Form("Alignment fit for  %s %s",names[i],Passes[k].PassName);
      } else { // Average
	Name = Form("%s%s%s",Passes[0].PassName,Passes[1].PassName,nameK[i]);
	if (Opt != "" && ! Name.Contains(Opt,TString::kIgnoreCase)) continue;
	Title = Form("Alignment fit for %s sum %s %s",names[i],Passes[0].PassName,Passes[1].PassName);
	
      }
      //      cout << Name.Data() << "\t" << Title.Data() << "\ti\t" << i << "\tk\t" << k << endl;
      dath[k][i] = (TH1D *) gDirectory->Get(Name);
      if (dath[k][i]) delete dath[k][i];
      
      dath[k][i] = new TH1D(Name,Title, 24, 0.5, 24.5);
      //      cout << "Create: " << dath[k][i]->GetName() << "\t" << dath[k][i]->GetTitle() << endl;
      dath[k][i]->SetMarkerColor(color);
      dath[k][i]->SetLineColor(color);
      dath[k][i]->SetXTitle("sector");
      if (i < 3) dath[k][i]->SetYTitle(Form("%s (#mum)",names[i]));
      else       dath[k][i]->SetYTitle(Form("%s (mrad)",names[i]));
      for (Int_t l = 0; l < 24; l++) {
	Int_t secs;
	Double_t val, err;
	if (k < NP) {
	  Double_t *X = &Passes[k].Data[l].x;
	  secs = Passes[k].Data[l].sector;
	  if (X[2*i+1] >= 0 /* && X[2*i+1] < 99 */) {
	    val = X[2*i];
	    err = X[2*i+1];
	  } else {continue;}
	} else {
	  Double_t *X0 = &Passes[0].Data[l].x;
	  Double_t *X1 = &Passes[1].Data[l].x;
	  secs = Passes[0].Data[l].sector;
	  if (X0[2*i+1] >= 0 /* && X0[2*i+1] < 99 */ &&
	      X1[2*i+1] >= 0 /* && X1[2*i+1] < 99 */) {
	    val = 0.5*(X0[2*i] + X1[2*i]);
	    dath[k][i]->SetBinContent(secs,val);
	    err = TMath::Sqrt(X0[2*i+1]*X0[2*i+1]+X1[2*i+1]*X1[2*i+1])/2;
	  } else {continue;}
	} 
	if (err < 0.001) err = 0.001;
	dath[k][i]->SetBinContent(secs,val);
	dath[k][i]->SetBinError(secs,err);
	if (ymin > val - err) ymin = val - err;
	if (ymax < val + err) ymax = val + err;
      }
      hs[i]->Add(dath[k][i]);
      if (leg[i]) {
	if (k < NP) leg[i]->AddEntry(dath[k][i],Passes[k].PassName);
	else        leg[i]->AddEntry(dath[k][i],"sum");
      }
    }
  }
  c1 = new TCanvas("IO","Tpc Super Sector alignment parameters",1200,800);
  c1->Divide(3,2);
  for (Int_t i = 0; i < 6; i++) {
    c1->cd(i+1);
    if (! hs[i]) continue;
    TString same("e");
    Double_t ymax = hs[i]->GetMaximum("nostack");
    Double_t ymin = hs[i]->GetMinimum("nostack");
    TList *list = hs[i]->GetHists();
    TIter next(list);
    TH1 *h = 0;
    while ((h = (TH1*) next())) {
      h->GetYaxis()->SetTitleOffset(1.4);
      if (same == "e") {
	if (ymax > 0)     h->SetMaximum(1.1*ymax);
	else              h->SetMaximum(0.9*ymax);
	if (ymin < 0)     h->SetMinimum(1.1*ymin);
	else              h->SetMinimum(0.9*ymin);
      }
      TString hName(h->GetName());
      if (hName.BeginsWith("db",TString::kIgnoreCase)) h->Draw("same");
      else                                             h->Draw(same);
      same = "same";
    }
    if (leg[i]) leg[i]->Draw();
  }
  c1->Update();
  MakeSuperSectorPositionB();
}
