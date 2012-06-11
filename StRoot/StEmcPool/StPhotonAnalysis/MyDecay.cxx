#include <Riostream.h>
#include <TCanvas.h>
#include <TRandom.h>
#include <TGenPhaseSpace.h>
#include "TDatabasePDG.h"
#include <TParticle.h>
#include <TDecayChannel.h>
#include <assert.h>

#include "MyDecay.h"
using namespace std;

ClassImp(MyDecay)

MyDecay::MyDecay(const char *file)
{
  mFileName=TString(file);
  isPP05=kFALSE;
  isDAU=kFALSE;
  cout<<"construct"<<endl;
}
MyDecay::~MyDecay()
{
  cout<<"destruct"<<endl;
}

TObjArray *MyDecay::decay(TParticle *part) 
{
  Float_t frac=gRandom->Rndm();
  TParticlePDG *part_pdg=part->GetPDG();
  Int_t i_dec=0;
  Double_t sum_br=0;
  while(i_dec<part_pdg->NDecayChannels()) 
    {
      sum_br+=part_pdg->DecayChannel(i_dec)->BranchingRatio();
      if(frac<sum_br) break;
      i_dec++;
    }
  if(i_dec==part_pdg->NDecayChannels()) return 0;

  TDecayChannel *decay=part_pdg->DecayChannel(i_dec);
  Int_t n_part=decay->NDaughters();

  Double_t *mass_arr=new Double_t[n_part];
  for (Int_t i=0; i<n_part; i++) 
    {
      mass_arr[i]=TDatabasePDG::Instance()->GetParticle(decay->DaughterPdgCode(i))->Mass();
    }

  TGenPhaseSpace *gs=new TGenPhaseSpace();//del
  Double_t mass=part_pdg->Mass();
  TLorentzVector v;
  part->Momentum(v);
  v.SetE(sqrt(mass*mass+v.P()*v.P()));
  gs->SetDecay(v,n_part,mass_arr);
  delete [] mass_arr;
  Float_t max_wt=gs->GetWtMax();
  while(gRandom->Rndm()*max_wt>gs->Generate());

  TObjArray *dec_part=new TObjArray(n_part);
  for(Int_t i_part=0;i_part<n_part;i_part++)
    {
      TParticle *dght=new TParticle(decay->DaughterPdgCode(i_part),0,0,0,0,0,0,0,0,0,0,0,0,0);
      dght->SetMomentum(*gs->GetDecay(i_part));
      dec_part->Add(dght);      
    }
  gs->Delete();
  
  return dec_part;
}

void MyDecay::doDecay(Int_t ndecays)
{
  if(!isPP05&&!isDAU){
    cout<<"label not set"<<endl;
    return;
  }
  //limits:
  Double_t ptmin=.0;
  Double_t ptmax=30.;
  Double_t ptminbin=.0;
  Double_t ptmaxbin=15.;
  Int_t ptbins=60;
  Double_t phimin=-TMath::Pi();
  Double_t phimax=TMath::Pi();
  Double_t etamin=-1.;
  Double_t etamax=2.;

  Double_t rapmin=0.0;
  Double_t rapmax=1.0;

  //create pi0:
  TParticle *part=new TParticle(111,0,0,0,0,0,0,0,0,0,0,0,0,0);
  TLorentzVector *v=new TLorentzVector();
  //create eta:
  TParticle *part2=new TParticle(221,0,0,0,0,0,0,0,0,0,0,0,0,0);
  TLorentzVector *v2=new TLorentzVector();
  //create omega:
  TParticle *part3=new TParticle(223,0,0,0,0,0,0,0,0,0,0,0,0,0);
  TLorentzVector *v3=new TLorentzVector();

  //output hists:
  TH1F *pion=new TH1F("pion","# versus pT",ptbins,ptminbin,ptmaxbin);
  TH1F *etameson=new TH1F("etameson","# versus pT",ptbins,ptminbin,ptmaxbin);
  TH1F *omega=new TH1F("omega","# versus pT",ptbins,ptminbin,ptmaxbin);
  TH1F *gamma=new TH1F("gamma","# versus pT",ptbins,ptminbin,ptmaxbin);
  TH1F *gamma2=new TH1F("gamma2","# versus pT",ptbins,ptminbin,ptmaxbin);
  TH1F *gamma3=new TH1F("gamma3","# versus pT",ptbins,ptminbin,ptmaxbin);

  for(Int_t i=0;i<ndecays;i++) 
    {
      if(i%10000==0) cout<<i<<endl;

      //set part:
      Float_t pT=ptmin+gRandom->Rndm()*(ptmax-ptmin);
      Float_t phi=phimin+gRandom->Rndm()*(phimax-phimin);
      Float_t eta=etamin+gRandom->Rndm()*(etamax-etamin);
      Float_t mass=part->GetMass();
      Float_t mass2=part2->GetMass();
      Float_t mass3=part3->GetMass();
      v->SetPtEtaPhiM(pT,eta,phi,mass);
      v2->SetPtEtaPhiM(pT,eta,phi,mass2);
      v3->SetPtEtaPhiM(pT,eta,phi,mass3);
      part->SetMomentum(*v);
      part2->SetMomentum(*v2);
      part3->SetMomentum(*v3);

      Float_t WEIGHT=ptweight(pT)*phiweight(phi)*etaweight(eta);
      Float_t WEIGHT2=ptweightETA(pT)*phiweight(phi)*etaweight(eta);      
      Float_t WEIGHT3=ptweightOMEGA(pT)*phiweight(phi)*etaweight(eta);

      if(v->PseudoRapidity()>rapmin && v->PseudoRapidity()<rapmax) pion->Fill(v->Pt(),WEIGHT);
      if(v2->PseudoRapidity()>rapmin && v2->PseudoRapidity()<rapmax) etameson->Fill(v2->Pt(),WEIGHT2);
      if(v3->PseudoRapidity()>rapmin && v3->PseudoRapidity()<rapmax) omega->Fill(v3->Pt(),WEIGHT3);

      //gen. decay:
      TObjArray *dec_dght=decay(part);
      TObjArray *dec_dght2=decay(part2);
      TObjArray *dec_dght3=decay(part3);
      
      //get pion daughters:
      Int_t nD=dec_dght->GetEntries();
      for(Int_t j=0;j<nD;j++)
	{
	  if(nD==2)
	    {
	      TParticle *d=(TParticle*)dec_dght->At(j);
	      if(d->GetPdgCode()==22 && d->Eta()>rapmin &&  d->Eta()<rapmax) 
		{
		  gamma->Fill(d->Pt(),WEIGHT);
		}
	    }
	  else if(nD==3)
	    {
	      TParticle *d=(TParticle*)dec_dght->At(j);
	      if(d->GetPdgCode()==22 && d->Eta()>rapmin &&  d->Eta()<rapmax) 
		{
		  gamma->Fill(d->Pt(),WEIGHT);
		}
	    }
	}      
      //delete array:
      dec_dght->Delete();
      delete dec_dght;

      //get eta daughters:
      Int_t nD2=dec_dght2->GetEntries();
      for(Int_t j=0;j<nD2;j++)
        {
          if(nD2==2)
            {
              TParticle *d=(TParticle*)dec_dght2->At(j);
              if(d->GetPdgCode()==22 && d->Eta()>rapmin &&  d->Eta()<rapmax)
                {
                  gamma2->Fill(d->Pt(),WEIGHT2);
                }
            }
          else if(nD2==3)
            {
              TParticle *d=(TParticle*)dec_dght2->At(j);
              if(d->GetPdgCode()==22 && d->Eta()>rapmin &&  d->Eta()<rapmax)
                {
                  gamma2->Fill(d->Pt(),WEIGHT2);
                }
            }
          //there is e+e-pi+pi- for eta!!
        }
      //delete array:
      dec_dght2->Delete();
      delete dec_dght2;

      //get omega daughters:
      Int_t nD3=dec_dght3->GetEntries();
      for(Int_t j=0;j<nD3;j++)
        {
          if(nD3==2)
            {
              TParticle *d=(TParticle*)dec_dght3->At(j);
              if(d->GetPdgCode()==22 && d->Eta()>rapmin &&  d->Eta()<rapmax)
                {
                  gamma3->Fill(d->Pt(),WEIGHT3);
                }
            }
          else if(nD3==3)
            {
              TParticle *d=(TParticle*)dec_dght3->At(j);
              if(d->GetPdgCode()==22 && d->Eta()>rapmin &&  d->Eta()<rapmax)
                {
                  gamma3->Fill(d->Pt(),WEIGHT3);
                }
            }
        }
      //delete array:
      dec_dght3->Delete();
      delete dec_dght3;

    }


  TCanvas *c=new TCanvas("c","c",600,900);
  c->Divide(2,3);
  c->cd(1);
  gPad->SetLogy();
  gamma->SetTitle("pion(daughters) vs pT");
  gamma->Draw();
  gamma->SetLineStyle(2);
  pion->Draw("same");
  c->cd(2);
  gPad->SetLogy();
  gamma2->SetTitle("eta(daughters) vs pT");
  gamma2->Draw();
  gamma2->SetLineStyle(2);
  etameson->Draw("same");
  c->cd(3);
  gPad->SetLogy();
  gamma3->SetTitle("omega782(daughters) vs pT");
  gamma3->Draw();
  gamma3->SetLineStyle(2);
  omega->Draw("same");
  c->cd(4);
  gPad->SetLogy();
  pion->SetTitle("pions/etas/omegas vs pT");
  pion->Draw();
  pion->SetLineColor(2);
  etameson->Draw("same");
  etameson->SetLineColor(4);
  omega->Draw("same");
  omega->SetLineColor(3);
  c->cd(5);
  gPad->SetLogy();
  TH1F *f=new TH1F(*gamma);
  f->SetLineStyle(1);
  TH1F *f2=new TH1F(*gamma2);
  f2->SetLineStyle(1);
  TH1F *f3=new TH1F(*gamma3);
  f3->SetLineStyle(1);
  TH1F *fsum=new TH1F(*f);
  fsum->Add(f2);
  fsum->Add(f3);
  TH1F *g=new TH1F(*pion);
  f->Divide(g);
  f2->Divide(g);
  f3->Divide(g);
  fsum->Divide(g);
  fsum->Draw();
  fsum->SetTitle("bg photons per pion");
  f->Draw("same");
  f->SetLineColor(2);
  f2->Draw("same");
  f2->SetLineColor(4);
  f3->Draw("same");
  f3->SetLineColor(3);
  fsum->SetMinimum(.001);
  c->cd(6);
  TH1F *etatopi=new TH1F(*etameson);
  TH1F *omegatopi=new TH1F(*omega);
  etatopi->Divide(pion);
  omegatopi->Divide(pion);
  omegatopi->SetTitle("ratio of #eta/#omega to #pi");
  omegatopi->Draw();
  etatopi->Draw("same");
  c->cd(0);
  c->SaveAs((mFileName+".ps").Data());
  c->SaveAs((mFileName+".root").Data());

  TFile *dec_out=new TFile((mFileName+"Sum.root").Data(),"recreate");
  fsum->Write();
  f->Write("gamma_pion");
  f2->Write("gamma_eta");
  f3->Write("gamma_omega");
  dec_out->Close();
}

Double_t MyDecay::ptweight(Double_t x)
{
  float weight=1.;
  if(x<0.2) x=0.2;
  if(isPP05){
    weight=300.*x*pow(1.+x,-9.0);//error: +/- 1.0%
  }
  if(isDAU){
    weight=8.24601e-02*x*pow(1.+x,-9.01897e+00);//error: +/- 1.0%
  }
  return weight;
}
Double_t MyDecay::ptweightETA(Double_t x)
{
  Float_t ME=0.548;//eta meson mass
  Float_t MP=0.135;//pi0 mass
  Float_t weight=fETAMTSCALE * x/sqrt(x*x + ME*ME - MP*MP) * ptweight(sqrt(x*x + ME*ME - MP*MP));
  return weight;
}
Double_t MyDecay::ptweightOMEGA(Double_t x)
{
  Float_t MO=0.782;//omega mass
  Float_t MP=0.135;//pi0 mass
  Float_t weight=1. * x/sqrt(x*x + MO*MO - MP*MP) * ptweight(sqrt(x*x + MO*MO - MP*MP));
  return weight;
}
Double_t MyDecay::etaweight(Double_t /*x*/)
{
  return 1.;
}  
Double_t MyDecay::phiweight(Double_t /*x*/)
{
  return 1.;
} 
