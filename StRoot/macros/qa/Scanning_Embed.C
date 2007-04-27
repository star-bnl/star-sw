///////////////////////////////////////////////////////////////////////////////////////////////
// $Id: Scanning_Embed.C,v 1.1 2007/04/27 15:00:30 cristina Exp $
// owner: Cristina
//
// What it does: scans files from MonteCarlo Embeddded Data and Ral ,fills the 3D
// histogram (nch,pt,dca), 3D histogram (nch,pt,nfit) and 2D histogram of dEdx vs P
// also fills the histograms to check some global variables such as z vertex, x-y vertex 
// and Transverese momentum, Eta, phi  and creates a root file as the output
//
// When running this macro, just include tag(id) for the particle that you are interested,
// check the cuts that you want to apply and the ranges for the histograms that you need.
// This macro uses Tree helper : "StMiniMcTre" and MuDst
/////////////////////////////////////////////////////////////////////////////////////////////////

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

void scan_embed_mc(Int_t id=9){

// -------------------- A n a l y s i s   C u t s   ---------------------------------

  float VZCut   = 25; // Cut for Z Vertex 
  float yCut    = 0.5; // Cut for Rapidity
  float NFitCut = 25; //Cut for Number of Fit Points
  int   NCommCut= 10; // Cut for Number of common hits
  
  //------------------Ranges of the histograms  ---------------------------------------
 
  int  nchNch  = 200;	float maxNch=1000;

  int  nchPt   = 80;    float maxPt   = 15.0;

  int  nchNhits= 50;	float minNhits = 0.;	float maxNhits = 50.;	

  int  nchDca  = 100;	float maxDca  = 3.;

  float maxDedx = 20.;
 
  float mass2;

	if (id == 8)  { TString tag = "PiPlus";  mass2 = 0.019;}
	if (id == 9)  { TString tag = "PiMinus"; mass2 = 0.019;}
	if (id == 11) { TString tag = "KPlus";   mass2 = 0.245;}
	if (id == 12) { TString tag = "KMinus";  mass2 = 0.245;}
	if (id == 14) { TString tag = "Proton";  mass2 = 0.880;}
	if (id == 15) { TString tag = "Pbar";    mass2 = 0.880;}
	if (id == 50) { TString tag = "Phi";     mass2 = 1.020;}
	if (id == 2)  { TString tag = "Eplus";   mass2 = 0.511;}
	
//location of files that you are scanning---------------------------------------

	TString filePathMC;

	filePath="~/Phi_101/Mini_Mc/*root";

	cout <<filePath<<endl;

//oFile is where the output will be saved---------------------------------------------

	TString oFile;

	oFile="~/outputs/Phi/out_scan_embed_mc_"+tag+"1.root";

//------------------------------------------

	TTreeHelper TH("StMiniMcTree"); 
	TH.AddFile(filePath);

//Definition of Histograms


       TH1D* hMult=new TH1D("hMult","Multiplicity",1000,0,maxNch); 
 
       TH1D* nChTotal = new TH1D("nChTotal", "nCharged Distribution", 1200, 0, maxNch);
       TH2F* dedx = new TH2F("dedx", "de/dx vs P", 500, 0.0, maxPt, 400, 0., maxDedx);
       TH3D* hDca = new TH3D("hDca3","Nch:Pt:Dca",nchNch,0,maxNch,nchPt,0,maxPt,nchDca,0, maxDca);
       TH3D* hNfit = new TH3D("hNfit3","Nch:Pt:Nhits",nchNch,0,maxNch,nchPt,0,maxPt,nchNhits,0,maxNhits);

//Defining the histograms to Check some Global Variables

       TH1D* vz = new TH1D("vz","Vertex Z",200,0.-50.,50.0);
       TH2D* v_xy =  new TH2D("vxy", "Vertez_X_Y", 200, -3.0, 3., 400, -3.0,  3.);
       
       TH1D* dvz = new TH1D("dvz","Delta Vertex Z MC - Reco",50,0.-5.,5.0);
       TH1D* dvx = new TH1D("dvx","Delat Vertex X MC -  Reco",50,0.-5.,5.0);
       TH1D* dvy = new TH1D("dvy","Delta Vertex Y Mc - Reco",50,0.-5.,5.0);

       //Histograms for Embedded Tracks

       TH1D* hPtMc  =new TH1D("PtMc","Pt_Mc",400,0,20);
       TH1D* hEtaMc =new TH1D("EtaMc","Eta_Mc",200,-1.8,1.8);
       TH1D* hPhiMc =new TH1D("PhiMc","Phi_Mc",200,-5.0,5.0);
       

       //Histograms for Matched tracks

       TH1D* hPtPrMatched = new TH1D("PtPrMatched","Pt_Pr",400,0,20);
       TH1D* hEtaPrMatched = new TH1D("EtaPrMatched","EtaPr",200,-1.8,1.8);
       TH1D* hPhiPrMatched = new TH1D("PhiPrMatched","PhiPr",200,-5.0,5.0);

       //Histogram for Energy Loss

       TH2D* hPtM_E_Pr =new TH2D("hPtM_E_Pr",title,100,0,10,200,-0.1,0.1);// for Primary Tracks
       TH2D* hPtM_E_Gl =new TH2D("hPtM_E_Gl",title,100,0,10,200,-0.1,0.1);// for Global Tracks

// Definition of variables-----------------------------------------

	const Int_t   &nch = TH("mNUncorrectedPrimaries"); 
	const Float_t &VX  = TH("mVertexX");
        const Float_t &VY  = TH("mVertexY");
        const Float_t &VZ  = TH("mVertexZ");

	const Float_t &VXmc  = TH("mMcVertexX");
        const Float_t &VYmc  = TH("mMcVertexY");
        const Float_t &VZmc  = TH("mMcVertexZ");


	// Branch : McTracks // branch is related with the embedded tracks

	const Int_t   &ntrk1 = TH ("mMcTracks"); 

	const Float_t *&PtMc   =  TH("mMcTracks.mPtMc");
	const Float_t *&PzMc   =  TH("mMcTracks.mPzMc");
	const Float_t *&NHits  =  TH("mMcTracks.mNHitMc");
	const Float_t *&EtaMc  = TH("mMcTracks.mEtaMc");
	const Float_t *&PhiMc  = TH("mMcTracks.mPhiMc");

	// Branch : Matched Pairs //

	//Extension Pr ( Pr  stands for Primary and it's related with all possible reconstructed tracks)
	//Ext  Mc ( Mc  stands for MonteCarlo and it's related with reconstructed and matched with embedded tracks)

	const Int_t   &ntrk  = TH("mMatchedPairs"); 
 
 	const Float_t *&PtPrMatched  = TH("mMatchedPairs.mPtPr");
	const Float_t *&PzPrMatched = TH("mMatchedPairs.mPzPr");
	const Float_t *&EtaPrMatched = TH("mMatchedPairs.mEtaPr");
	const Float_t *&PhiPrMatched= TH("mMatchedPairs.mPhiPr");
	
	const Float_t *&PtMcMatched  = TH("mMatchedPairs.mPtMc");
	const Float_t *&PzMcMatched = TH("mMatchedPairs.mPzMc");
	const Float_t *&EtaMcMatched = TH("mMatchedPairs.mEtaMc");
	const Float_t *&PhiMcMatched= TH("mMatchedPairs.mPhiMc");

	const Float_t *&PtGlMatched= TH("mMatchedPairs.mPtGl");
	const Float_t *&PzGlMatched= TH("mMatchedPairs.mPzGl");
	const Float_t *&EtaGlMatched = TH("mMatchedPairs.mEtaMGl");
	const Float_t *&PhiGlMatched = TH("mMatchedPairs.mPhiMGl");


	const Float_t *&dEdx = TH("mMatchedPairs.mDedx");
	const Float_t *&Dca   = TH("mMatchedPairs.mDcaGl");
	const Short_t *&NFit  = TH("mMatchedPairs.mFitPts");
	const Short_t *&NComm = TH("mMatchedPairs.mNCommonHit");
	
	const Short_t *&GidMatched = TH("mMatchedPairs.mGeantId");
	
	//========================================================
	// Branch :  MatGlobalPairs 
	/*
	const Int_t   &ntrk  = TH("mMatGlobPairs");  

 	const Float_t *&PtGlGlobal  = TH("mMatGlobPairs.mPtGl");
	const Float_t *&PzGlGlobal  = TH("mMatGlobPairs.mPzGl");
	const Float_t *&EtaGlGlobal = TH("mMatGlobPairs.mEtaGl");
	const Float_t *&PhiGlGlobal = TH("mMatGlobPairs.mPhiGl");

	const Float_t *&PtPrGlobal= TH("mMatGlobPairs.mPtPr");
	const Float_t *&PzPrGlobal= TH("mMatGlobPairs.mPzPr );
	const Float_t *&EtaPrGlobal = TH("mMatGlobPairs.mEtaPr");
	const Float_t *&PhiPrGlobal = TH("mMatGlobPairs.mPhiPr"

	const Float_t *&dEdx = TH("mMatGlobPairs.mDedx");
	const Float_t *&Dca   = TH("mMatGlobPairs.mDcaGl");
	const Short_t *&NFit  = TH("mMatGlobPairs.mFitPts");
	const Short_t *&NComm = TH("mMatGlobPairs.mNCommonHit");

	const Short_t *&GidGlobal = TH("mMatGlobPairs.mGeantId");
	*/
	//=======================================================
	/*
	// Branch : Ghost Pairs  (Real Tracks with no matches)

 	const Int_t   &ntrk  = TH("mGhostPairs");  
 
  	const Float_t *&PtGlGlobal  = TH("mGhostPairs.mPtGl");
  	const Float_t *&PzGlGlobal  = TH("mGhostPairs.mPzGl");
 	const Float_t *&EtaGlGlobal = TH("mGhostPairs.mEtaGl");
	const Float_t *&PhiGlGlobal = TH("mGhostPairs.mPhiGl");

 	const Float_t *&PtPrGlobal= TH("mGhostPairs.mPtPr");
 	const Float_t *&PzPrGlobal= TH("mGhostPairs.mPzPr" );
 	const Float_t *&EtaPrGlobal = TH("mGhostPairs.mEtaPr");
 	const Float_t *&PhiPrGlobal = TH("MmGhostPairs.mPhiPr"

 	const Float_t *&dEdx = TH("mGhostPairs.mDedx");
	const Float_t *&Dca   = TH("mGhostPairs.mDcaGl");
  	const Short_t *&NFit  = TH("mGhostPairs.mFitPts");
	const Short_t *&NComm = TH("mGhostPairs.mNCommonHit");

 	const Short_t *&GidGhost = TH("mGhostPairs.mGeantId");
	*/
//-----------------------------------------------------------------

int ev = 0;
while(TH.Next())// && ev ==0) 
{    
  //ev++;
               vz->Fill(VZ);// 1 vertex per event
                            //  first fill vz and then apply the cuts

	       vxy->Fill(VX,VY);

	       dvz->Fill(VZ-VZmc);
	       dvx->Fill(VX-VXmc);
	       dvy->Fill(VY-VYmc);

		if(fabs(VZ)> VZCut)  continue;
		ev++;

for (Int_t itr=0; itr<ntrk; itr++)
 { 
 //to fill the histograms related with all possible reconstructed tracks
   hMult ->Fill();
 
   	if ( GidMatched[itr]!=11 && GidMatched[itr]!=12) continue; //use this line just for Phi

	if (fabs(EtaPrMatched[itr]) > yCut) continue;

	//Defining Momentum

	  float p = sqrt(PtPrMatched[itr]*PtPrMatched[itr]+PzPrMatched[itr]*PzPrMatched[itr]);
	
	
	if (NFit[itr] >= NFitCut  && NComm[itr] > NCommCut)
		hDca->Fill(nch, PtPrMatched[itr],Dca[itr]);
       
	if (Dca[itr] < maxDca && (1.*(NFit[itr])/NComm[itr]) < 2.5)
		hNfit ->Fill(nch, PtPrMatched[itr], NFit[itr]); 
	
	if( Dca[itr] < maxDca  && NFit[itr] >= NFitCut  && fabs(EtaPrMatched[itr])< yCut)
		dedx->Fill(p,dEdx[itr]*1e6);
		
	//to fill the histograms related with the global variables

	if(NFit[itr] >= NFitCut  && NComm[itr] > NCommCut && Dca[itr] < maxDca )
	  {
	    v_xy->Fill(VX[itr], VY[itr]);
	    hPtPrMatched->Fill(PtPrMatched[itr]);
	    hEtaPrMatched->Fill(EtaPrMatched[itr]);
	    hPhiPrMatched->Fill(PhiPrMatched[itr]);
	  }
  }//close for

// To Fill histograms of embedded tracks

 for (int itr1=0; itr1 < ntrk1; itr1++)
   {
     if (fabs(EtaM[itr]) > yCut) continue;

	hPtMc->Fill(PtMc[itr1]);
        hEtaMc->Fill(EtaMc[itr]);
	hPhiMc->Fill(PhiMc[itr]);
   }

}//close while


//--------------------------SCANNING REAL DATA------------------------------

//Using TTree Helper MuDst

TTreeHelper THmu("MuDst");

THmu.AddFile(filePathR);


TH1D* nChTotalR = new TH1D("nChTotalR", "nCharged Distribution", 1200, 0, 1200);
TH2F* dedxR = new TH2F("dedxR", "de/dx vs P", 500, 0.0, 4., 400, 0., 20);//start at 0
TH1D* hDcax=new TH1D("hDcax", "Distance of Closest Approach x", 1000, 0, 5);
TH1D* hDcay=new TH1D("hDcay", "Distance of Closest Approach y", 1000, 0, 5);
TH1D* hDcaz=new TH1D("hDcaz", "Distance of Closest Approach z", 1000, 0, 5);
TH3D* hDcaR=new TH3D("hDcaR","Nch:Pt:Dca",nchNch,0,maxNch,nchPt,0,maxPt,nchDca,0, maxDca);
TH3D* hNfitR = new TH3D("hNfitR","Nch:Pt:Nhits",nchNch,0,maxNch,nchPt,0,maxPt,nchNhits,minNhits,maxNhits);
TH1D* hNSigmaPi=new TH1D("hNSigmaPi","Sigma Pion",1000,0,1000);
TH1D* hNSigmaK=new TH1D("hNSigmaK","Sigma Kaon",1000,0,1000);
TH1D* hNSigmaP=new TH1D("hNSigmaP","Sigma Proton",1000,0,1000);


	const UShort_t *&nNeg= THmu("MuEvent.mRefMultNeg");
	const UShort_t *&nPos= THmu("MuEvent.mRefMultPos");

	const Float_t *&VX1 =  THmu("MuEvent.mEventSummary.mPrimaryVertexPos.mX1");
	const Float_t *&VY1 =  THmu("MuEvent.mEventSummary.mPrimaryVertexPos.mX2");
	const Float_t *&VZ1 =  THmu("MuEvent.mEventSummary.mPrimaryVertexPos.mX3");

	const Int_t  &ntrk1 =  THmu("PrimaryTracks");

	const Float_t *&dEdxR = THmu("PrimaryTracks.mdEdx");
	const Float_t *&Pt1   = THmu("PrimaryTracks.mPt");
	const Float_t *&Eta1  = THmu("PrimaryTracks.mEta");

	const Float_t *&dcax1 = THmu("PrimaryTracks.mDCAGlobal.mX1");
	const Float_t *&dcay1 = THmu("PrimaryTracks.mDCAGlobal.mX2");
	const Float_t *&dcaz1 = THmu("PrimaryTracks.mDCAGlobal.mX3");

	const UChar_t *&NHits1 = THmu("PrimaryTracks.mNHits");
	const Int_t *&NSigmaPion = THmu("PrimaryTracks.mNSigmaPion");
	const Int_t *&NSigmaKaon = THmu("PrimaryTracks.mNSigmaKaon");
	const Int_t *&NSigmaProton = THmu("PrimaryTracks.mNSigmaProton");


	if (id == 8 || id == 9) 
		{const Int_t *&NSigma = THmu("PrimaryTracks.mNSigmaPion"); }

	if (id == 11|| id ==12) 
		{const Int_t *&NSigma = THmu("PrimaryTracks.mNSigmaKaon"); }

	if (id == 14|| id ==15)
		{const Int_t *&NSigma = THmu("PrimaryTracks.mNSigmaProton"); }

//const Int_t  &nchR = *nNeg + *nPos; 
//THmu.Print();
//return;

int ev = 0;
while(THmu.Next() )// && ev ==0) 
{    
hMult->Fill(*nPos + *nNeg); 

for (Int_t itr=0; itr<ntrk1; itr++)
 { 
      ev++;   

//cout<<*VZ1<<endl;

	if(fabs(*VZ1)>25.)  continue;//cut in vertex
	
	float dca = sqrt(dcax1[itr]*dcax1[itr]+dcay1[itr]*dcay1[itr]+dcaz1[itr]*dcaz1[itr]);
	float p_r = Pt1[itr]*cosh(Eta1[itr]);
	
	hDcax->Fill(dcax1[itr]);
	hDcay->Fill(dcay1[itr]);
	hDcaz->Fill(dcaz1[itr]);
		
	hNSigmaPi->Fill(NSigmaPion[itr]);
	hNSigmaK->Fill(NSigmaKaon[itr]);
	hNSigmaP->Fill(NSigmaProton[itr]);

	//to fill DCA histogram

	if (fabs(Eta1[itr]) >0.5) continue;
	
	if(NHits1[itr]>= 25)
		hDcaR->Fill((*nNeg +*nPos), Pt1[itr],dca);	
	

	//to fill Nfit points histogram
	
	if (dca < 3)
		hNfitR ->Fill(*nNeg +*nPos, Pt1[itr], NHits1[itr]);
	

	//to fill de/dx vs P
	
	if(dca< 3. && NHits1[itr]>= 25 && fabs(Eta1[itr])< 0.5 && NSigma[itr]/1000 < 2.)
		dedxR->Fill(p_r,dEdxR[itr]*1e6);
     	
}
}//close MuDst


cout<< "Creating output file .... "<<oFile; flush(cout);  
  
 TFile *fout=new TFile(oFile,"recreate"); 
 fout->cd();

 //writting all the histograms

 nChTotal->Write();
 dedx->Write("dEdx");
 hNfit->Write();
 hDca-> Write();

 //Global Variables

 v_xy->Write();
 vz->Write();

 dvx->Write();
 dvy->Write();
 dvz->Write();

 hPtPrMatched->Write();
 hEtaPrMatched->Write();
 hPhiPrMatched->Write();

 hEtaMc->Write();
 hPtMc->Write();
 hPhiMc->Write();
  
 fout->Close();
 cout<<" done"<<endl; flush(cout);
}
