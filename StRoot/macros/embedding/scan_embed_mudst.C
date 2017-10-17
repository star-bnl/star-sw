//Original Version done by Cristina May 2007
//Modification extra histograms by Xianglei
//Extra 3D Histograms Dca : ETA: pt and Nfit: Eta: pt are being added

#ifndef __CINT__
#include "TROOT.h"
#include "TSystem.h"
#include <iostream.h>
#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "TFile.h"
#include "TTree.h"
#include "TChain.h"
#include "TTreeHelper.h"
#endif

void scan_embed_mudst(
			
		const TString inputMuDst = "/star/data06/embed/andrewar/hiroshi/MuDst/*.MuDst.root",
		
		const Char_t* production = "P08ic",
	
		const Char_t* date = "041909",
	
		)
{
  gSystem->Load("StarRoot");
  
  // -------------------- A n a l y s i s  C u t s  ---------------------------------
  
  const float VZCut   = 30.; // Cut for Z Vertex 
  const float yCut    = 1;  // Cut for pseudoRapidity. we do not need this cuts for MC tracks and matched tracks
  const float NFitCut = 25.; //Cut for Number of Fit Points
  const int  NCommCut = 10;  // Cut for Number of common hits
  
  //------------------Ranges of the histograms  ---------------------------------------
  
  const int  nchNch     = 200;   const float maxNch   = 1000;
  const int  nchPt      = 80;    const float maxPt    = 8.0;
  const int  nchNhits   = 50;    const float minNhits = 0.;    const float maxNhits = 50.;   
  const int  nchDca     = 100;   const float maxDca   = 3.;
  const float maxDedx   = 20.;
  const float NSigmaCut = 2.0;
  float mass2 = -1.0;
  
   //-------------------------MC---------------------------------
  

  const Char_t* outputFileName = Form("../MuDst/%s_MuDst_%s.root", production,  date);
  TString oFile(outputFileName);

 
  TString filePathR(inputMuDst);
  cout << endl;
  cout << endl;
  cout << endl;
  cout << " Input MuDst (real data) : " << filePathR.Data() <<endl;

  TTreeHelper THmu("MuDst");
  THmu.AddFile(filePathR);

  TH1D* hMultR=new TH1D("hMultR","Multiplicity",1000,0,1000); 

  TH1D* nChTotalR = new TH1D("nChTotalR", "nCharged Distribution", 1200, 0, maxNch);

  TH2F* dedxR = new TH2F("dedxR", "de/dx vs P", 400, 0.0, 2., 400, 0., maxDedx);
  TH1D* hDcax = new TH1D("hDcax", "Distance of Closest Approach x", 1000, 0, maxDca);
  TH1D* hDcay = new TH1D("hDcay", "Distance of Closest Approach y", 1000, 0, maxDca);
  TH1D* hDcaz = new TH1D("hDcaz", "Distance of Closest Approach z", 1000, 0, maxDca);
  TH3D* hDcaR = new TH3D("hDcaR","Nch:Pt:Dca",nchNch,0,maxNch,nchPt,0,maxPt,nchDca,0, maxDca);
  TH3D* hNfitR = new TH3D("hNfitR","Nch:Pt:Nhits",nchNch,0,maxNch,nchPt,0,maxPt,nchNhits,minNhits,maxNhits);


  TH1D* hNSigmaPi = new TH1D("hNSigmaPi","Sigma Pion",1000,-5000,5000);
  TH1D* hNSigmaK = new TH1D("hNSigmaK","Sigma Kaon",1000,-5000,5000);
  TH1D* hNSigmaP = new TH1D("hNSigmaP","Sigma Proton",1000,-5000,5000);
  //Real DCA:Eta: pt and Nfit : Eta : pt
  
  TH3D* hPtEtaDcaR  = new TH3D("hPtEtaDcaR","Pt:Eta:Dca", nchPt,0,maxPt,200,-1.8,1.8,  nchDca,0, maxDca);// for Real
  TH3D* hPtEtaNfitR = new TH3D("hPtEtaNfitR","Pt:Eta:Nfit",nchPt,0,maxPt,200,-1.8,1.8,nchNhits,0,maxNhits);// for Real
  hPtEtaDcaR->Sumw2();
  hPtEtaNfitR->Sumw2();



  const UShort_t *&nNeg= THmu("MuEvent.mRefMultNeg");
  const UShort_t *&nPos= THmu("MuEvent.mRefMultPos");

  const Float_t *&VXR =  THmu("MuEvent.mEventSummary.mPrimaryVertexPos.mX1");
  const Float_t *&VYR =  THmu("MuEvent.mEventSummary.mPrimaryVertexPos.mX2");
  const Float_t *&VZR =  THmu("MuEvent.mEventSummary.mPrimaryVertexPos.mX3");

  const Int_t  &ntrkR =  THmu("GlobalTracks");
  const UChar_t *&NFitR = THmu("GlobalTracks.mNHitsFit");
  const Float_t *&dEdxR = THmu("GlobalTracks.mdEdx");
  const Float_t *&PtR    = THmu("GlobalTracks.mPt");
  const Float_t *&EtaR  = THmu("GlobalTracks.mEta");

  const Float_t *&dcax = THmu("GlobalTracks.mDCAGlobal.mX1");
  const Float_t *&dcay = THmu("GlobalTracks.mDCAGlobal.mX2");
  const Float_t *&dcaz = THmu("GlobalTracks.mDCAGlobal.mX3");
  const Int_t *&vtxid = THmu("GlobalTracks.mVertexIndex");

  const UChar_t *&NHitsR    = THmu("GlobalTracks.mNHits");
  const Int_t  *&NSigmaPion = THmu("GlobalTracks.mNSigmaPion");
  const Int_t  *&NSigmaKaon = THmu("GlobalTracks.mNSigmaKaon");
  const Int_t  *&NSigmaProton = THmu("GlobalTracks.mNSigmaProton");


  /* if (id ==2 ||id == 8 || id == 9) 
  {const Int_t *&NSigma = THmu("GlobalTracks.mNSigmaPion"); }

  if (id == 11|| id ==12) 
  {const Int_t *&NSigma = THmu("GlobalTracks.mNSigmaKaon"); }

  if (id == 14|| id ==15)
  {const Int_t *&NSigma = THmu("GlobalTracks.mNSigmaProton"); }
  */

  int evReal = 0;
  int evRealGood = 0;

  while(THmu.Next() )// && ev ==0) 
  {    
    hMultR->Fill(*nPos + *nNeg); 
    evReal++;  

    if(fabs(*VZR)> VZCut)  continue;

    evRealGood++;  

    if( evReal % 100 == 0 ){
      cout << Form("MuDst:         (Good Event)/(Event number)  = %5d/%5d, VZ = %1.4f, Number of primary tracks = %10d",
          evRealGood, evReal, VZR, (Int_t)ntrkR)
        << endl;
    }

    for (Int_t itr=0; itr<ntrkR; itr++) { 
      float dca = sqrt(dcax[itr]*dcax[itr]+dcay[itr]*dcay[itr]+dcaz[itr]*dcaz[itr]);

      if (fabs(EtaR[itr]) > yCut) continue;

      float p  = PtR[itr]*cosh(EtaR[itr]);

      hDcax->Fill(dcax[itr]);
      hDcay->Fill(dcay[itr]);
      hDcaz->Fill(dcaz[itr]);

      hNSigmaPi->Fill(NSigmaPion[itr]);
      hNSigmaK->Fill(NSigmaKaon[itr]);
      hNSigmaP->Fill(NSigmaProton[itr]);

      //filling DCA  
      //if(NHitsR[itr]>= NFitCut && fabs(1.*NSigma[itr]/1000.0) < NSigmaCut)
      if(NFitR[itr]>= NFitCut && vtxid[itr]==0 && fabs(NSigmaPion[itr]/1000.0) < NSigmaCut)
	{
	  hDcaR->Fill((*nNeg +*nPos), PtR[itr],dca);    
	  hPtEtaDcaR->Fill( PtR[itr],EtaR[itr],dca);  
	}
      
      //filling Nfit 
      //if (dca < maxDca && fabs(NSigma[itr]/1000.0) < NSigmaCut && NHitsR[itr]>=10)
      if (dca < maxDca && NFitR[itr]>=10 && vtxid[itr]==0 && fabs(NSigmaPion[itr]/1000.0) < NSigmaCut)
	{
	  hNfitR ->Fill((*nNeg +*nPos), PtR[itr], NFitR[itr]);
	  hPtEtaNfitR ->Fill(PtR[itr], EtaR[itr], NFitR[itr]);
	}

      //to fill de/dx vs P
      //if(dca< maxDca && NFitR[itr]>= NFitCut && vtxid[itr]==0)
      if(dca< maxDca && NFitR[itr]>= NFitCut)
      //if(dca< maxDca && NHitsR[itr]>= NFitCut && fabs(EtaR[itr])< yCut)
        dedxR->Fill(p,dEdxR[itr]*1e6);

    }//close for loop

  }//close while loop


  //-------Writing-----------

  cout<< "Creating output file .... "<<oFile; flush(cout);  

  TFile *fout=new TFile(oFile,"recreate"); 
  fout->cd();

 
  //-----REAL

  hMultR->Write();
  nChTotalR->Write();
  dedxR->Write();
  hDcaR->Write();
  hNfitR-> Write();
 
  hPtEtaNfitR->Write();
  hPtEtaDcaR->Write();
  
  fout->GetList()->Sort();

  fout->Close();
  cout<<" done"<<endl; flush(cout);
}
