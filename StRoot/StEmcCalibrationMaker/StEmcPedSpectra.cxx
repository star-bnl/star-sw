/**********************************************************************
* StEmcPedSpectra
* Author: Alexandre A. P. Suaide 
*
* This is responsible for emc equalization
***********************************************************************/
#include "StEmcPedSpectra.h"
#include <iostream.h>
#include <math.h>
#include "emc_def.h"
#include "TCanvas.h"
#include "TGraph.h"
#include "StEmcUtil/StEmcGeom.h"
#include "StEvent/StEvent.h"
#include "TF1.h"

ClassImp(StEmcPedSpectra);
//_____________________________________________________________________________
StEmcPedSpectra::StEmcPedSpectra(const char* cdet,Int_t nbis, Float_t bin0, Float_t bin1):StEmcSpectra(cdet,nbis,bin0,bin1)
{  
	mPedMode = 0;
  mPed = new TH2F("mPed","mPed",18000,1,18001,5,0,5);
  mPed->Reset();
}
//_____________________________________________________________________________
StEmcPedSpectra::~StEmcPedSpectra()
{ 
 if (mPed) delete mPed;
}
//_____________________________________________________________________________
Bool_t StEmcPedSpectra::Fill(TH1F* hits,StEvent* event)
{  
	cout <<"***** Filling pedestals ...\n";  
  StEmcSpectra::Fill();
	Int_t NTracks = GetNTracks(event);
  if(NTracks>GetMaxMultiplicity()) return kFALSE;  
  for(Int_t j=0;j<GetNBin();j++)
  {
    Int_t did=j+1;
    Int_t ibin = hits->FindBin((Float_t)did);
		Float_t y = hits->GetBinContent(ibin);
    if(GetStatus(did)==1)  FillSpectra(did,y);
  }    
  Float_t x,y,z;
  GetOccupancy(GetMinHits(),&x,&y,&z);  
  cout   <<"PEDESTAL: Avg Nevents/bin = "<<x<<" +- "<<y<<"\n";
  cout   <<"PEDESTAL: fraction of bins nevents > minimum = "<<z<<"\n";
  if(z<GetMinOcc()) return kFALSE;  // minimum occupancy not reached yed...
  return kTRUE;

}
//_____________________________________________________________________________
Bool_t StEmcPedSpectra::CalculatePedestals()
{  
	cout <<"***** Calculating pedestals ...\n"; 
  mPed->Reset();
  Float_t left = 3;
  Float_t right= 2;
  
  Int_t ngood=0,nped=0,nrms=0,nchi=0;
  
  TF1 *func = new TF1("ped","([0]/sqrt(6.28*[2]))*exp(-0.5*((x-[1])/[2])*((x-[1])/[2]))");
  for(Int_t id = 1;id<=GetNBin();id++) if(GetStatus(id)==1)
  {
    //cout <<"----------------------------------------------------------------\n";
    TH1D *h = GetSpectra(id);
    Int_t ibin = h->GetMaximumBin();
    Float_t avg = (Float_t)h->GetBinCenter(ibin);
    Float_t max = (Float_t)h->GetMaximum();
    Float_t rms = 1.5;
    max/=sqrt(6.28*rms);
    func->SetParameter(0,max);
    func->SetParameter(1,avg);
    func->SetParameter(2,rms);
    func->SetRange(avg-left*rms,avg+right*rms);
    Int_t ndg = (Int_t)((left+right)*rms-3.0);
    //cout <<"id = "<<id<<"  ped = "<<avg<<"  rms = "<<rms<<endl; 
    h->Fit(func,"RQN");
    max = func->GetParameter(0);
    avg = func->GetParameter(1);
    rms = func->GetParameter(2);
    Float_t chi = func->GetChisquare()/(Float_t)ndg;
    cout <<"id = "<<id<<"  ped = "<<avg<<"  rms = "<<rms<<"  chi = "<<chi<<endl; 
    
    Int_t status = 1; // data present
    if(avg<0) {status+= 2; nped++;}// negative pedestal
    if(rms<0) {status+= 4; nrms++;}// negative rms
    if(chi>8) {status+= 8; nchi++;}// large chi square
    if(status==1) ngood++;
    mPed->Fill((Float_t)id,0.0,avg);
    mPed->Fill((Float_t)id,1.0,rms);
    mPed->Fill((Float_t)id,2.0,chi);
    mPed->Fill((Float_t)id,3.0,(Float_t)status);    
  }
  cout <<"nGood = "<<ngood<<"  neg Ped = "<<nped<<"  neg rms = "<<nrms<<"  large chi = "<<nchi<<endl;
  delete func;
  return kTRUE;
}
