#include "SpinIUHistos.h"
#include "TMath.h"
#include "TH1F.h"
#include "TH2F.h"
#include "TString.h" 
#include "TMath.h"
#include "TDirectory.h"
#include <iostream> 

ClassImp(SpinIUHistos);

// ----------------------------------------------------------------------------
SpinIUHistos::SpinIUHistos(const Char_t *name, const Char_t *title):TDirectory(name,title,"SpinIUHistos")
{

  //Double_t pt_bins[] = { 0.0, 3.4, 4.4, 5.4, 6.4, 8.4, 10.4, 12.4, 16.4, 32.4 };
  //Int_t    npt_bins  = sizeof(pt_bins)/sizeof(Double_t)-1;

  Int_t    nmass = 120;
  Double_t min   = 0.;
  Double_t max   = 1.2;

  mMin = 0.08;
  mMax = 0.18;

  hMass = new TH1F(TString("hMass")  +name,  "diphoton invariant mass",              nmass,min,max );
  //hPT   = new TH2F(TString("hPT")    +name,  "Diphoton transverse momentum vs mass", nmass,min,max,npt_bins,pt_bins);
  hPT   = new TH2F(TString("hPT")    +name,  "Diphoton transverse momentum vs mass", nmass,min,max,50,0.0, 25.0);
  hZgg  = new TH2F(TString("hZgg")   +name,  "Diphoton energy sharing vs mass",      nmass,min,max,50,0.,1.);
  hZvert= new TH2F(TString("hZvert") +name,  "Event z-vertex vs mass",               nmass,min,max,150,-150.,150.);
  hEta  = new TH2F(TString("hEta")   +name,  "Reconstructed #eta of #pi^{0} candidate vs mass",nmass,min,max,180,1.0,2.5);
  hEEmcEta  = new TH2F(TString("hEEmcEta")   +name,  "Detector #eta of #pi^{0} candidate vs mass",nmass,min,max,180,1.0,2.5);
  hPhi  = new TH2F(TString("hPhi")   +name,  "Detector #Phi of #pi^{0} candidate vs mass",nmass,min,max,360,-180.,180.);
  hRGeo=new TH2F(TString("hRGeo")+name, "Reconstructed pi0 track; Recon phi / deg;ReconEta",360,-180.,180,20,1.,2.);
  //hPiPosition= new TH2F(TString("hYX")  +name,  "#pi^{0} position at z=280cm",          120,-240.,240.,120,-240.,240.);
  hYX[0]= new TH2F(TString("hYX_0")  +name,  "#pi^{0} position at z=280cm",          120,-240.,240.,120,-240.,240.);
  hYX[1]= new TH2F(TString("hYX_1")  +name,  "higher energy gamma at z=280 cm",      240,-240.,240.,240,-240.,240.);
  hYX[2]= new TH2F(TString("hYX_2")  +name,  "lower  energy gamma at z=280 cm",      240,-240.,240.,240,-240.,240.);

  hE1E2 = new TH2F(TString("hE1E2")  +name,  "E1 vs E2",                             100,0.,50.,100,0.,50.);
  
  hPhiggVsEnergy = new TH2F(TString("hPhiggVsEnergy")+name,"#phi_{#gamma #gamma} vs energy",100,0.,50.,100,0.,0.2);

  hEpi  = new TH2F(TString("hEpi")   +name,  "Reconstructed energy of #pi^{0} candidate vs mass",nmass,min,max,60,0.0,60.);
  hEsmd=new TH2F(TString("hEsmd")+name,"E_{smd} / E_{#pi^{0}} vs E_{#pi^{0}}", 60,0.,30.,60,0.,0.12); 
  hEpre1=new TH2F(TString("hEpre1")+name,"E_{pre1} [MeV] / E_{#pi^{0}} [GeV] vs E_{#pi^{0}}", 60,0.,30.,60,0.,6.); 
  hEpre2=new TH2F(TString("hEpre2")+name,"E_{pre2} [MeV] / E_{#pi^{0}} [GeV] vs E_{#pi^{0}}", 60,0.,30.,60,0.,12.); 
  hEpost=new TH2F(TString("hEpost")+name,"E_{post} [MeV] / E_{#pi^{0}} [GeV] vs E_{#pi^{0}}", 60,0.,30.,60,0.,1.2); 

  hEpre12=new TH2F(TString("hEpre12")+name,"E_{pre2} vs E_{pre1} [MeV]",75,0.,150.,75,0.,150.);

}

// ----------------------------------------------------------------------------
void SpinIUHistos::Fill( StEEmcIUPair &pair )
{

  Float_t mass = pair.mass();
  hMass  -> Fill( mass );
  hPT    -> Fill( mass, pair.pt() );
  hZgg   -> Fill( mass, pair.zgg() );
  hZvert -> Fill( mass, pair.vertex().Z() );
  hEta   -> Fill( mass, pair.momentum().Eta() );
  hEpi->Fill(mass,pair.energy());
  //calculate detector Eta
  float Rxy=TMath::Sqrt(pair.vertex().x()*pair.vertex().x()+pair.vertex().y()*pair.vertex().y());
  float hHeight=pair.pt()*(270.0-pair.vertex().Z())/pair.pz()+Rxy;
  float etatheta=TMath::ATan(hHeight/270.0);
  //printf("accept pz=%f\n",pair.pz());
  float mideta=TMath::Tan(etatheta/2.0);
  float eemceta=-TMath::Log(mideta);
  hEEmcEta->Fill(mass,eemceta);
  hPhi->Fill(mass,pair.momentum().Phi()*180./3.14159265);
  hRGeo->Fill(pair.momentum().Phi()*180./3.14159265,eemceta);
  Float_t esmd=0.;

  esmd += pair.point(0).cluster(0).energy();
  esmd += pair.point(0).cluster(1).energy();
  esmd += pair.point(1).cluster(0).energy();
  esmd += pair.point(1).cluster(1).energy(); 
 
  Float_t epre1 = 0.;

  epre1 += pair.point(0).energy(1);
  epre1 += pair.point(1).energy(1); 

  Float_t epre2 = 0.;

  epre2 += pair.point(0).energy(2);
  epre2 += pair.point(1).energy(2);

  Float_t epost = 0.;

  epost += pair.point(0).energy(3);
  epost += pair.point(1).energy(3); 

  //if ( mass >= mMin && mass < mMax ) 
  //{
  Float_t e1 = pair.point(0).energy();
  Float_t e2 = pair.point(1).energy();
  Float_t epi0=e1+e2; 
  hEsmd->Fill( epi0, esmd/epi0 ); 
  hEpre1->Fill( epi0, epre1*1000./epi0 ); 
  hEpre2->Fill( epi0, epre2*1000./epi0 ); 
  hEpost->Fill( epi0, epost*1000./epi0 ); 
  hEpre12->Fill( epre1*1000, epre2*1000 );
  TVector3 p1 = pair.point(0).position();
  TVector3 p2 = pair.point(1).position();
  TVector3 pp = (e1*p1 + e2*p2) * ( 1/(e1+e2) );
  hYX[0] -> Fill( pp.X(), pp.Y() );
  hYX[1] -> Fill( p1.X(), p1.Y() );
  hYX[2] -> Fill( p2.X(), p2.Y() );
  hE1E2  -> Fill( e2, e1 ); 
  hPhiggVsEnergy -> Fill( pair.phigg(), pair.energy() );
      //}

  

}
// ----------------------------------------------------------------------------
void SpinIUHistos::Clear(Option_t *opts)
{

  hMass->Reset();
  hPT->Reset();
  hZgg->Reset();
  hZvert->Reset();
  hEta->Reset();
  hEEmcEta->Reset();
  hPhi->Reset();
  hRGeo->Reset();
  hPhiggVsEnergy->Reset();
  for ( Int_t i=0;i<3;i++ ) hYX[i]->Reset();
  hE1E2->Reset();
  hEpi->Reset();
  hEsmd->Reset();
  hEpre1->Reset();
  hEpre2->Reset();
  hEpost->Reset();
  hEpre12->Reset();
}
