#include "StJetFinderKonst.h"
#include <assert.h>
#include <TDataSetIter.h>
#include <TArrayI.h>
#include <TArrayD.h>
#include <TH1.h>  
#include <TH2.h>
#include <TFile.h>
#include <TVector2.h>
#include <TVector3.h>
#include <TMath.h>
#include <TCanvas.h>
#include <TPad.h>
#include "TObject.h"
#include "Stiostream.h"

extern "C" void ktjetp_(int &, float &, float &, float &, float &);
extern "C" void ktjet_(int &, float *, int *, int &, float &);

ClassImp(StJetFinderKonst)

StJetFinderKonst::StJetFinderKonst()
{// default value of parameters
   mC1 = 0;
   mDebug      = 0;
   mConeRadius = 0.7;
   mEtSeed     = 1.0;
   mMinJetEt   = 5.0;
   mMinCellEt  = 0.5;
   mPtMax      = 50.; 
  
   // may be get from geometry
   mNbinEta    = 40;
   mEtaMin     = -1.;
   mEtaMax     = +1.;

   mNbinPhi    = 120;
   mPhiMin     = -TMath::Pi();
   mPhiMax     =  TMath::Pi();

   file1 = new TFile("JetHist.root","recreate");
   mHstat = new TH1F("mHstat", "Statistics for jet finder ", 20, 0.5, 20.5);
   mHlego = new TH2F("mHlego", "Calo grid", mNbinEta,mEtaMin,mEtaMax,  mNbinPhi,mPhiMin,mPhiMax);
   mHjetEt  = new TH1F("mHjetEt", "E_{T} of jet", 100, 0, 30.);
   mHjetEta = new TH1F("mHjetEta", "#eta of jet", 100, -2., 2.0);
   mHjetPhi = new TH1F("mHjetPhi", "#phi of jet", 60, -TMath::Pi(), TMath::Pi());
   mHjetPart = new TH1F("mHjetPart", "N particles in jet", 25, 0.5, 25.5);
   mHPtPartM = new TH1F("mHPtPartM", "Ratio Max Part. to all Jet",100,0.,1.1);
   mHNpartR  = new TH2F("mHNpartR","Npart---Ratio",25,0.5,25.5,100,0.,1.1);
   mHPhiEta = new TH2F("mHPhiEta","Phi---Eta",100,-TMath::Pi(),TMath::Pi(),100,-1.5,1.5); 
   mHDz = new TH1F("mHDz","Jet Fragmentation Function",100,0.,1.); 
   mHEjet = new TH1F("mHEjet","Energy of Jet",100,0.,mPtMax);
// if dijet events
   mHdiffEt  = new TH1F("mHdiffEt","E_{T1} - E_{T2}", 40, -20., 20.);
   mHdiffEta = new TH1F("mHdiffEta","#eta_{1} - #eta_{2}", 40, -2., 2.);
   mHdiffPhi = new TH1F("mHdiffPhi","#phi_{1} - #phi_{2}", 60, -TMath::Pi(), TMath::Pi());
}

StJetFinderKonst::~StJetFinderKonst()
{
}

Int_t StJetFinderKonst::FindCone(Int_t ntrk, Float_t *pt, Float_t *eta, Float_t *phi, Float_t *ms,
    StProtoJet* jets, StProtoJet** tracks)
{ // see subroutine PYCELL in PYTHIA
  // Provides a simple way of jet finding in eta-phi-ET coordinates,
  // as used for calorimeters at hadron colliders.
  //  cout << endl;
  //  cout << "ntrk= " << ntrk << "  pt= " << pt[0] << endl;

  mHstat->Fill(1.);
  if (!ntrk) {
     mHstat->Fill(11.);
     return -1; // no particles table;
  }
  TVector3 vjet;
  TVector3 vpart;
  for(Int_t i=0; i<ntrk; i++) {npintb[i]=-1; ninjet[i]=-1;}
  Int_t I_max=0;
  Double_t Pt_max;
  NJets=0;  
  while(NJets<5 && I_max>=0) { 
    I_max=-1;
    Pt_max=0.;
    for(Int_t i=0; i<ntrk; i++) {
      if(eta[i] < mEtaMin || eta[i] > mEtaMax) continue;
      if(pt[i] > mPtMax) continue;
      if(ninjet[i]>=0) continue;
      if(npintb[i]>=0) continue;
      if(pt[i] > Pt_max && pt[i] > mMinCellEt) {
        I_max = i;
        Pt_max = pt[i];
      }
    }    
    float mass;
//    const float mPion = 0.0194798;
//    const float mKaon = 0.243717;
//    const float mProton = 0.880354;
    if (I_max<0) break;
    npintb[I_max] = I_max;
    vjet.SetXYZ(0.,0.,0.);
    Double_t eta_i=0., phi_i=0.;
    NpartJ[NJets]=0;
    vjet.SetX(pt[I_max]*cos(phi[I_max]));
    vjet.SetY(pt[I_max]*sin(phi[I_max]));
    vjet.SetZ(pt[I_max]*sinh(eta[I_max]));
    mass = ms[I_max];
//    mass = 0.;
//    if(Pion[I_max] > 950) mass = mPion;
//    else
//      if(Kaon[I_max] > 950) mass = mKaon;
//      else
//        if(Proton[I_max] > 950) mass = mProton;              
    EJet[NJets]=::sqrt(vjet*vjet+mass);
    eta_i   = vjet.Eta();
    phi_i   = vjet.Phi();
    PtPartM[NJets]=::sqrt(vjet(0)*vjet(0)+vjet(1)*vjet(1));
    NpartJ[NJets]++;
    ninjet[I_max] = NJets;        
      //      cout << "eta_i = " << eta_i << "  phi_i = " << phi_i << endl; 
    Double_t eta_p, phi_p, deta=0, dphi=0., radius=0;
    for(Int_t i=0; i<ntrk; i++) {
      if(eta[i] < mEtaMin || eta[i] > mEtaMax) continue;
      if(i==I_max) continue;
      if(ninjet[i] >= 0) continue; // particle already in jet
      if(pt[i] > mPtMax) continue;
      vpart.SetX(pt[i]*cos(phi[i]));
      vpart.SetY(pt[i]*sin(phi[i]));
      vpart.SetZ(pt[i]*sinh(eta[i]));
      eta_p   = vpart.Eta();
      phi_p   = vpart.Phi();
      deta = eta_i - eta_p;
      if(TMath::Abs(deta) > mConeRadius) continue;
      dphi = TVector2::Phi_mpi_pi(phi_i - phi_p);
      if(TMath::Abs(dphi) > mConeRadius) continue;
      radius = TMath::Sqrt(deta*deta + dphi*dphi);
      if(radius > mConeRadius) continue;
      mass = ms[I_max];
//      mass = 0.;
//      if(Pion[I_max] > 950) mass = mPion;
//      else
//        if(Kaon[I_max] > 950) mass = mKaon;
//        else
//          if(Proton[I_max] > 950) mass = mProton;      
      EJet[NJets]+=::sqrt(vpart*vpart+mass);
      vjet+=vpart;
      eta_i   = vjet.Eta();
      phi_i   = vjet.Phi();
      NpartJ[NJets]++;
      ninjet[i] = NJets;            // attach particle to jet
    }
    PtJ[NJets] = ::sqrt(vjet(0)*vjet(0)+vjet(1)*vjet(1));
    if(PtJ[NJets] > mMinJetEt) { // o'k - this is a jet
      EtaJ[NJets] = vjet.Eta();
      PhiJ[NJets] = vjet.Phi();
      Int_t npart_dz;
      npart_dz = 0;
      for(Int_t i=0; i<ntrk; i++) {
        if(ninjet[i] == NJets) {
          vpart.SetX(pt[i]*cos(phi[i]));
          vpart.SetY(pt[i]*sin(phi[i]));
          vpart.SetZ(pt[i]*sinh(eta[i]));
          Double_t angle = vjet.Angle(vpart);
          Double_t Pl= ::sqrt(vpart*vpart)*cos(angle);
          Dz[NJets][npart_dz]=Pl/EJet[NJets];
	  npart_dz++;
        }
      }
      NJets++;
    }
    else { // particles are free from this jet  
      for(Int_t j=0; j<ntrk; j++) {
        if(ninjet[j] == NJets) ninjet[j] = -1;
      }
    }
  }  // end cycle for jet seed
  //  fillControlHists();
  // Added by Thomas Henry:  Add track particles to each jet 
  // according to ninjet info:
  for(Int_t j = 0; j < ntrk; j++)
  {
    if((ninjet[j] > -1) && (ninjet[j] < MAXJETS_KJF))
    {
      jets[ninjet[j]].merge(*(tracks[j]));
    }
  } 
  // End Added by Thomas Henry 09/02
  return NJets;
}


Int_t StJetFinderKonst::FindKt(Int_t ntrk, Float_t *pt, Float_t *eta, Float_t *phi, Float_t *ms,
    StProtoJet* jets, StProtoJet** tracks)
{ // kt - jet finding algorithm

  for(Int_t i=0; i<ntrk; i++) { ninjet[i]=-1;}
  mHstat->Fill(1.);
  if (ntrk <= 1) {
     mHstat->Fill(11.);
     return -1; // no particles table;
  }
  Int_t I_max=-1;
  Float_t Pt_max;
  NJets=0;  
  Pt_max=0.;
  for(Int_t i=0; i<ntrk; i++) {
    if(eta[i] < mEtaMin || eta[i] > mEtaMax) continue;
    if(pt[i] > mPtMax) continue;
    if(pt[i] > Pt_max && pt[i] > mMinCellEt) {
      I_max = i;
      Pt_max = pt[i];
    }
  }    
  float mass;
//  const float mPion = 0.0194798;
//  const float mKaon = 0.243717;
//  const float mProton = 0.880354;
  Int_t nprt_eta;
  if(I_max >= 0) {
    nprt_eta = 0;
    for(Int_t i=0; i<ntrk; i++) {
      if(eta[i] < mEtaMin || eta[i] > mEtaMax) continue;
      if(pt[i] > mPtMax) continue;
      nprt_eta++;
      mass = ms[i];
//      mass = 0.;
//      if(Pion[i] > 950) mass = mPion;
//      else
//        if(Kaon[i] > 950) mass = mKaon;
//        else
//          if(Proton[i] > 950) mass = mProton;              
      //      cout << " mass=" << mass << "  Pion= " << Pion[i] << "  Kaon= " << Kaon[i] << 
      //	"  Proton = " << Proton[i] << endl;
      float px = pt[i]*cos(phi[i]);
      float py = pt[i]*sin(phi[i]);
      float pz = pt[i]*sinh(eta[i]);
      float e = ::sqrt(px*px+py*py+pz*pz+mass);
      ktjetp_(nprt_eta, px, py, pz, e);    
    }
    int NJets_kt;
    double PJet[1000][4];
    int Jetj[1000];
    int nsub;
    float rad = (float) mConeRadius;  // Added by Thomas Henry 
    //if(rad > 1.0) rad = 1.0;  // Added by Thomas Henry - otherwise seg fault
    ktjet_(NJets_kt, (float *)PJet, Jetj, nsub, rad);
    // Code Added by Thomas Henry:  Jetj[i] is the 1-based
    // index of the jet which
    // contains the ith particle.  This is the
    // information we need to extract from this routine:
    for(int i = 0; i < ntrk; i++)
      ninjet[i] = Jetj[i]-1;
    // Added by Thomas Henry:  Add track particles to each jet 
    // according to ninjet info:
    int maxJetIndex = -1;
    for(Int_t j = 0; j < ntrk; j++)
    {
      if((ninjet[j] > -1) && (ninjet[j] < MAXJETS_KJF))
      {
        jets[ninjet[j]].merge(*(tracks[j]));
        if(ninjet[j] > maxJetIndex)
          maxJetIndex = ninjet[j];
      }
    }
    return maxJetIndex+1;
    // End Added by Thomas Henry 09/02
    TVector3 vjet;
    TVector3 vpart;
    float eta_p, phi_p, deta, dphi, radius;
    vjet.SetXYZ(PJet[NJets_kt-1][0],PJet[NJets_kt-1][1],PJet[NJets_kt-1][2]);            
    PtJ[NJets] = ::sqrt(vjet(0)*vjet(0)+vjet(1)*vjet(1));
    if(NJets_kt) {
      for(int i=NJets_kt-1; i>=0; i--) {
        vjet.SetXYZ(PJet[i][0],PJet[i][1],PJet[i][2]);            
        PtJ[NJets] = ::sqrt(vjet(0)*vjet(0)+vjet(1)*vjet(1));
        if(PtJ[NJets] > mMinJetEt && !nsub) {
          EtaJ[NJets] = vjet.Eta();
          PhiJ[NJets] = vjet.Phi();
          EJet[NJets] = PJet[NJets_kt-1][3];
          if(NJets >= 1) {
            Int_t I_max=-1;
            Pt_max=0.;
            for(Int_t j=0; j<ntrk; j++) {
              if(eta[j] < mEtaMin || eta[j] > mEtaMax) continue;
              if(pt[j] > mPtMax) continue;
              vpart.SetX(pt[j]*cos(phi[j]));
              vpart.SetY(pt[j]*sin(phi[j]));
              vpart.SetZ(pt[j]*sinh(eta[j]));
              eta_p   = vpart.Eta();
              phi_p   = vpart.Phi();
              deta = EtaJ[NJets] - eta_p;
              if(TMath::Abs(deta) >  mConeRadius) continue;
              dphi = TVector2::Phi_mpi_pi(PhiJ[NJets] - phi_p);
              if(TMath::Abs(dphi) > mConeRadius) continue;
              radius = TMath::Sqrt(deta*deta + dphi*dphi);
              if(radius > mConeRadius) continue;
              if(pt[j] > Pt_max && pt[j] > mMinCellEt) {
                I_max = j;
                Pt_max = pt[j];
              }
            }
          }
          if(I_max == -1) continue;          
          nprt_eta = 0;
          NpartJ[NJets] = 0;
          for(Int_t j=0; j<ntrk; j++) {
            if(eta[j] < mEtaMin || eta[j] > mEtaMax) continue;
            if(pt[j] > mPtMax) continue;
            if(Jetj[nprt_eta]-1 == i) { 
              vpart.SetX(pt[j]*cos(phi[j]));
              vpart.SetY(pt[j]*sin(phi[j]));
              vpart.SetZ(pt[j]*sinh(eta[j]));
              Double_t angle = vjet.Angle(vpart);
              Double_t Pl= ::sqrt(vpart*vpart)*cos(angle);
              Dz[NJets][NpartJ[NJets]]=Pl/EJet[NJets];
              NpartJ[NJets]++;
            }              
            nprt_eta++;
          }
          PtPartM[NJets] = Pt_max;
          NJets++; 
        }
        else continue;
      }
    }
  }
  return NJets;
}
void StJetFinderKonst::print()
{
  printf("    Cone Jet Finder Algorithm");
  printf("    cone            %4.2f\n", mConeRadius);
  printf("    seed            %4.2f\n", mEtSeed);
  printf("    minimal Et      %4.2f\n", mMinJetEt);
  printf("    minimal cell Et %4.2f\n --\n", mMinCellEt);
  printf("    Calo grid \n");
  printf("    eta bin %3i : %7.5f < eta < %7.5f\n", mNbinEta, mEtaMin, mEtaMax);
  printf("    phi bin %3i : %7.5f < phi < %7.5f\n", mNbinPhi, mPhiMin, mPhiMax);
}

void StJetFinderKonst::printStatistics()
{
  printf(" StJetFinder::printStatistics() \n");
  printf(" ============================== \n");
  printf(" # entries        %8i\n", Int_t((*mHstat)[1]));
  printf(" no jets          %8i\n", Int_t((*mHstat)[2]));
  for(Int_t i=1; i<=5; i++) 
  printf(" %2i jet(s)       %8i\n", i, Int_t((*mHstat)[2+i])); 
  printf("--\n");
  printf(" no particle table          %8i\n", Int_t((*mHstat)[11]));
  printf(" no particles after pt cut  %8i\n", Int_t((*mHstat)[12]));
}

void StJetFinderKonst::fillControlHists()
{
  if(NJets>2) NJets=2;
  for(Int_t i=0; i<NJets; i++){
     mHjetEt->Fill(PtJ[i]);
     mHjetEta->Fill(EtaJ[i]);
     mHjetPhi->Fill(PhiJ[i]);
     mHjetPart->Fill(NpartJ[i]);
     mHPtPartM->Fill(PtPartM[i]/PtJ[i]);
     mHPhiEta->Fill(PhiJ[i],EtaJ[i]);
     mHNpartR->Fill(NpartJ[i],PtPartM[i]/PtJ[i]);
     mHEjet->Fill(EJet[i]);
     for(Int_t j=0; j<NpartJ[i]; j++) 
       mHDz->Fill(Dz[i][j],100.);
     //               cout << "Npart = " << NpartJ[i] << "  Ptp = " << PtPartM[i] <<
     //     	    "  PtJet = " <<  PtJ[i] << "   eta " <<  EtaJ[i] << "  Phi = " <<
     //                 PhiJ[i] << endl;
     //               cout << endl;
     if(i==1) {
       mHdiffEta->Fill(EtaJ[0] - EtaJ[1]);
       mHdiffPhi->Fill(TVector2::Phi_mpi_pi(PhiJ[0] - PhiJ[1]));
       if (TMath::Abs(EtaJ[0])<0.5 && TMath::Abs(EtaJ[1])<0.5) { // both jets in EMC
          mHdiffEt->Fill(PtJ[0]   - PtJ[1]);
       }
     }
  }
}

void StJetFinderKonst::drawControlHists()
{
  if (mC1 == 0) mC1 = new TCanvas("jetFinder::mC1", "Jet Finder : control histogramms",0,25,600,800);
  mC1->Clear();
  mC1->Divide(2,3);

  mC1->cd(1); drawHist(mHjetEt,3);
  gPad->SetLogy(1);
  mC1->cd(2); drawHist(mHdiffEt,3);
  mC1->cd(3); drawHist(mHjetEta,3);
  mC1->cd(4); drawHist(mHdiffEta,3);
  mC1->cd(5); drawHist(mHjetPhi,3);
  mC1->cd(6); drawHist(mHdiffPhi,3);
  mC1->Update();
  file1->Write();
}

void StJetFinderKonst::drawHist(TH1* hid, Int_t lineWidth, Int_t lineColor, Int_t lineStyle, char* opt)
{ // for convinience only
   if (hid) {
      if(lineWidth) hid->SetLineWidth(lineWidth);
      if (lineColor) hid->SetLineColor(lineColor);
      if (lineStyle) hid->SetLineStyle(lineStyle); // 1=solid, 2=dash, 3=dot-dot, 4=dash-dot
      hid->Draw(opt);
   } else {
     printf("StJetFinder::drawHist(TH1* hid=0, Int_t lineWidth=2, Int_t lineColor=1, Int_t lineStyle=1, char* opt=\"\")\n");
   }
}
