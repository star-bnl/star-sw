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

void scan_embed_mc(
		const Int_t id = 8,
		
		const TString inputMiniMc ="/eliza3/starprod/embedding/P08id/dAu/*/*/*.minimc.root", //test 
		
		const Char_t* particle= "Phi",
		const Char_t* production = "P08id",
		const Int_t eid = 90117,
		const Char_t* date = "040709",
	
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
  
  if (id == 2)  { TString tag = "Eplus";  mass2 = 0.511; Char_t *Daugther = "Eplus";}
  if (id == 8)  { TString tag = "PiPlus";  mass2 = 0.019; Char_t *Daugther = "PiPlus";}
  if (id == 9)  { TString tag = "PiMinus"; mass2 = 0.019; Char_t *Daugther = "PiMius";}
  if (id == 11) { TString tag = "KPlus";  mass2 = 0.245; Char_t *Daugther = "KPlus";}
  if (id == 12) { TString tag = "KMinus";  mass2 = 0.245; Char_t *Daugther = "KMinus";}
  if (id == 14) { TString tag = "Proton";  mass2 = 0.880; Char_t *Daugther = "Proton";}
  if (id == 15) { TString tag = "Pbar";    mass2 = 0.880; Char_t *Daugther = "Pbar";}
  if (id == 50) { TString tag = "Phi";    mass2 = 1.020; }
  
  
  if( mass2 < 0 ) {
    cout << "Negative mass2 for pid = " << id << endl;
    return;
  }
  
  //-------------------------MC---------------------------------
  
  TString filePathMc(inputMiniMc);
  
  cout << endl;
  cout << endl;
  cout << "Input MiniMc file : " << filePathMc.Data() <<endl;
  cout << "#---------------------------------------------------------------------------" << endl;
  cout << " Event selection" << endl;
  cout << " |VZ| < " << VZCut << endl;
  cout << "#---------------------------------------------------------------------------" << endl;
  cout << " Track selection (mMatchedPairs)" << endl;
  cout << "  For DCA          : mMatchedPairs.mFitPts >= " << NFitCut << " && mMatchedPairs.mNCommonHit > " << NCommCut << endl;
  cout << "  For NFit         : mMatchedPairs.mDcaGl < " << maxDca << " && mMatchedPairs.mFitPts/mMatchedPairs.mNCommonHit < 2.5 && mMatchedPairs.mFitPts >= 10" << endl;
  cout << "  For dE/dx        : mMatchedPairs.mDcaGl < " << maxDca << " && mMatchedPairs.mFitPts >= " << NFitCut << " && |mMatchedPairs.mEtaPr| < " << yCut << endl;
  cout << "  For pt, eta, phi : |mMatchedPairs.mDcaGl| < " << maxDca << endl;
  cout << "#---------------------------------------------------------------------------" << endl;
  cout << " Track selection (mGhostPairs)" << endl;
  cout << "  For dE/dx        : mGhostPairs.mDcaGl < " << maxDca << " && mGhostPairs.mFitPts >= " << NFitCut << " && |mGhostPairs.mEtaPr| < " << yCut << endl;
  cout << "#---------------------------------------------------------------------------" << endl;
  cout << " Track selection (mMcTracks)" << endl;
  cout << "  For pt, eta, phi : |mMatchedPairs.mEtaPr| > " << yCut << endl;
  cout << endl;

  const Char_t* outputFileName = Form("./%s_%s_%s_mc_%s.root", production, particle, Daugther, date);
  TString oFile(outputFileName);

  TTreeHelper TH("StMiniMcTree"); 
  TH.AddFile(filePathMc);

  //All reconstructed Histograms  

  TH1D* hMult=new TH1D("hMult","Multiplicity",1000,0,maxNch); 

  TH1D* nChTotal = new TH1D("nChTotal", "nCharged Distribution", 1200, 0, maxNch);
  TH2F* dedx = new TH2F("dedx", "de/dx vs P", 400, 0.0, 2., 400, 0., maxDedx);
  TH3D* hDca = new TH3D("hDca","Nch:Pt:Dca",nchNch,0,maxNch,nchPt,0,maxPt,nchDca,0, maxDca);
  TH3D* hNfit = new TH3D("hNfit","Nch:Pt:Nhits",nchNch,0,maxNch,nchPt,0,maxPt,nchNhits,0,maxNhits);

  // Checking some Global Variables

  TH1D* vz = new TH1D("vz","Vertex Z",200,0.-50.,50.0);
  TH2D* v_xy =  new TH2D("vxy", "Vertez_X_Y", 200, -3.0, 3., 400, -3.0,  3.);

  TH1D* dvz = new TH1D("dvz","Delta Vertex Z MC - Reco",50,0.-5.,5.0);
  TH1D* dvx = new TH1D("dvx","Delta Vertex X MC -  Reco",50,0.-5.,5.0);
  TH1D* dvy = new TH1D("dvy","Delta Vertex Y MC - Reco",50,0.-5.,5.0);

  TH1D* hPtM = new TH1D("PtM","Pt_Pr",400,0,20);
  TH1D* hEtaM = new TH1D("EtaM","EtaPr",200,-1.8,1.8);
  TH1D* hPhiM = new TH1D("PhiM","PhiPr",200,-5.0,5.0);
  TH1D* hYM = new TH1D("YM","YPr",100,-1.5,1.5);
  TH3D* hPtYPhiM = new TH3D("PtYPhiM","Pt:Y:Phi",nchPt,0,maxPt,100,-1.5,1.5,200,-5.0,5.0);
  TH1D* hFSectM = new TH1D("FSectM","First Sector",30,0,30.);
  TH1D* hLSectM = new TH1D("LSectM","Last Sector",30,0,30.);

  TH1D* hPtMc =new TH1D("PtMc","Pt_Mc",400,0,20);
  TH1D* hEtaMc=new TH1D("EtaMc","Eta_Mc",200,-1.8,1.8);
  TH1D* hYMc=new TH1D("YMc","Y_Mc",100,-1.5,1.5);
  TH1D* hPhiMc=new TH1D("PhiMc","Phi_Mc",200,-5.0,5.0);
  TH3D* hPtYPhiMc = new TH3D("PtYPhiMc","Pt:Y:Phi",nchPt,0,maxPt,100,-1.5,1.5,200,-5.0,5.0);

  hPtMc->Sumw2();
  hPtM->Sumw2();

  //Energy loss
  TString title = Form("ptM-ptMc :ptM ( |vtx|<30, %.1f< |yMc|<%.1f, nFitM>=25, nComm>=10)", 1., 1.);
  TH2D* hPtM_E = new  TH2D ("hPtM_E", title.Data(), 50, 0, 10, 50, -0.05, 0.05);
  TString title = Form("YM-YMc :YM ( |vtx|<30, %.1f< |yMc|<%.1f, nFitM>=25, nComm>=10)", 1., 1.);
  TH2D* hYM_E = new  TH2D ("hYM_E", title.Data(), 50, -1.2, 1.2, 50, -0.05, 0.05);
  TString title = Form("PhiM-PhiMc :PhiM ( |vtx|<30, %.1f< |yMc|<%.1f, nFitM>=25, nComm>=10)", 1., 1.);
  TH2D* hPhiM_E = new  TH2D ("hPhiM_E", title.Data(), 50, -3.2, 3.2, 50, -0.05, 0.05);

  //Ghost ==Real 
  TH2F* dedxG = new TH2F("dedxG","dE/dx vs p", 400,0., 2., 400, 0.,maxDedx);//for MIPS
  TH3D* hDcaG = new TH3D("hDcaG","Nch:Pt:Dca",nchNch,0,maxNch,nchPt,0,maxPt,nchDca,0, maxDca);
  TH3D* hNfitG = new TH3D("hNfitG","Nch:Pt:Nhits",nchNch,0,maxNch,nchPt,0,maxPt,nchNhits,0,maxNhits);
  TH1D* hPhiG = new TH1D("PhiG","PhiG",200,-5.0,5.0);
  TH1D* hFSectG = new TH1D("FSectG","First Sector",30,0,30.);
  TH1D* hLSectG = new TH1D("LSectG","Last Sector",30,0,30.);

  //3 D histograms for DCa ETA  and Pt, also NFit Eta and Pt

  TH3D* hPtEtaDcaM  = new TH3D("hPtEtaDcaM","Pt:Eta:DcaM", nchPt,0, maxPt,200,-1.8,1.8, nchDca,0, maxDca);// for MonteCarlo
  TH3D* hPtEtaDcaG  = new TH3D("hPtEtaDcaG","Pt:Eta:DcaG", nchPt,0, maxPt,200,-1.8,1.8, nchDca,0, maxDca);// for Global
  hPtEtaDcaM->Sumw2();
  hPtEtaDcaG->Sumw2();

  TH3D* hPtEtaNfitM  = new TH3D("hPtEtaNfitM","Pt:Eta:NfitM",nchPt,0,maxPt,200,-1.8,1.8,nchNhits,0,maxNhits);// for MonteCarlo
  TH3D* hPtEtaNfitG  = new TH3D("hPtEtaNfitG","Pt:Eta:NfitG",nchPt,0,maxPt,200,-1.8,1.8,nchNhits,0,maxNhits);// for Global
  hPtEtaNfitM->Sumw2();
  hPtEtaNfitG->Sumw2();

  //Variables
  
  const Int_t  &nch = TH("mNUncorrectedPrimaries"); 
  const Float_t &VX  = TH("mVertexX");
  const Float_t &VY  = TH("mVertexY");
  const Float_t &VZ  = TH("mVertexZ");

  const Float_t &VXmc  = TH("mMcVertexX");
  const Float_t &VYmc  = TH("mMcVertexY");
  const Float_t &VZmc  = TH("mMcVertexZ");
  
  //Mc Tracks 
  
  const Int_t  &ntrk1 = TH ("mMcTracks"); 
  
  const Float_t *&PtMc  = TH("mMcTracks.mPtMc");
  const Float_t *&PzMc  = TH("mMcTracks.mPzMc");
  const Float_t *&NHits  = TH("mMcTracks.mNHitMc");
  const Float_t *&EtaMc  = TH("mMcTracks.mEtaMc");
  const Float_t *&PhiMc  = TH("mMcTracks.mPhiMc");

  // Matched Pairs 
  /*
  const Int_t  &ntrk  = TH("mMatchedPairs"); //Extension Pr - all possible reconstructed tracks
  
  const Float_t *&PtM  = TH("mMatchedPairs.mPtPr");
  const Float_t *&PzM  = TH("mMatchedPairs.mPzPr");
  const Float_t *&EtaM = TH("mMatchedPairs.mEtaPr");
  const Float_t *&PhiM = TH("mMatchedPairs.mPhiPr");
  const Short_t *&FSectM = TH("mMatchedPairs.mFirstSector");
  const Short_t *&LSectM = TH("mMatchedPairs.mLastSector");


  //Ext  Mc -  reconstructed and matched with embedded tracks

  const Float_t *&PtMmc= TH("mMatchedPairs.mPtMc");
  const Float_t *&PzMmc= TH("mMatchedPairs.mPzMc");
  const Float_t *&EtaMmc = TH("mMatchedPairs.mEtaMc");
  const Float_t *&PhiMmc = TH("mMatchedPairs.mPhiMc");
  
  const Float_t *&dEdx  = TH("mMatchedPairs.mDedx");
  const Float_t *&Dca  = TH("mMatchedPairs.mDcaGl");
  const Short_t *&NFit  = TH("mMatchedPairs.mFitPts");
  const Short_t *&NComm = TH("mMatchedPairs.mNCommonHit");

  const Short_t *&Gid = TH("mMatchedPairs.mGeantId");

  */
  //========================================================
  //to scan meson Phi, use MatGlobalPairs instead of Matchd pairs
  
     const Int_t  &ntrk  = TH("mMatGlobPairs");  

     const Float_t *&PtM  = TH("mMatGlobPairs.mPtGl");
     const Float_t *&PzM  = TH("mMatGlobPairs.mPzGl");
     const Float_t *&EtaM = TH("mMatGlobPairs.mEtaGl");
     const Float_t *&PhiM = TH("mMatGlobPairs.mPhiGl");

     const Short_t *&FSectM = TH("mMatGlobPairs.mFirstSector");
     const Short_t *&LSectM = TH("mMatGlobPairs.mLastSector");
  

     const Float_t *&PtMmc= TH("mMatGlobPairs.mPtMc");
     const Float_t *&PzMmc= TH("mMatGlobPairs.mPzMc");
     const Float_t *&EtaMmc = TH("mMatGlobPairs.mEtaMc");
     const Float_t *&PhiMmc = TH("mMatGlobPairs.mPhiMc");

     const Float_t *&dEdx = TH("mMatGlobPairs.mDedx");
     const Float_t *&Dca  = TH("mMatGlobPairs.mDcaGl");
     const Short_t *&NFit  = TH("mMatGlobPairs.mFitPts");
     const Short_t *&NComm = TH("mMatGlobPairs.mNCommonHit");

     const Short_t *&Gid = TH("mMatGlobPairs.mGeantId");

  //=======================================================
   

  //Ghost Tracks

  const Int_t    &ntrkG  = TH("mGhostPairs");  

  const Float_t  *&PtG  = TH("mGhostPairs.mPtPr");
  const Float_t  *&PzG  = TH("mGhostPairs.mPzPr");
  const Float_t  *&dEdxG= TH("mGhostPairs.mDedx");
  const Float_t  *&DcaG = TH("mGhostPairs.mDcaGl");
  const Float_t  *&EtaG = TH("mGhostPairs.mEtaPr");
  const Float_t  *&PhiG = TH("mGhostPairs.mPhiPr");
  const Short_t  *&NFitG = TH("mGhostPairs.mFitPts");
  const Short_t *&FSectG = TH("mGhostPairs.mFirstSector");
  const Short_t *&LSectG = TH("mGhostPairs.mLastSector");

  int ev = 0;
  int evGood = 0;
  while(TH.Next())// && ev ==0) 
  {    
    //checking vertex position

    vz->Fill(VZ);// 1 vertex per event, first fill vz and then apply the cuts
    
    vxy->Fill(VX,VY);//this is the 2d ok

    dvx->Fill(VX-VXmc);//delta VX
    dvy->Fill(VY-VYmc);
    dvz->Fill(VZ-VZmc);//delta VZ
    
    ev++;
    
    if(fabs(VZ)> VZCut)  continue;
    evGood++;
    
    // cout << Form("MiniMcTree:    (Good Event)/(Event number)  = %5d/%5d, VZ = %1.4f, Number of embedded tracks = %10d, Matched tracks = %10d", evGood, ev, VZ, ntrk1, ntrk) << endl;
    
    for (Int_t itr=0; itr<ntrk; itr++) 
      { 

	if ( Gid[itr]!=14 && Gid[itr]!=15) continue; //use this line  for Phi meson decaying in KK (14, 15)
      	if (fabs(EtaM[itr]) > yCut) continue;
	
	//Defining Momentum
	
	float p = sqrt(PtM[itr]*PtM[itr]+PzM[itr]*PzM[itr]);
	
	if (NFit[itr] >= NFitCut  && NComm[itr] > NCommCut)
	  {
	   
	    hDca->Fill(nch, PtM[itr],Dca[itr]);
	    hPtEtaDcaM ->Fill( PtM[itr], EtaM[itr],  Dca[itr]);
	    //of course x ->Pt, y->Eta, z->DCA
	  }
	
	
	//if (Dca[itr] < maxDca && (1.*(NFit[itr])/NComm[itr])< 2.5 && NFit[itr]>=10)
	if (Dca[itr] < maxDca && NFit[itr]>=10)
	  {
	    hNfit ->Fill(nch, PtM[itr], NFit[itr]); 
	    hPtEtaNfitM ->Fill( PtM[itr], EtaM[itr], NFit[itr]);
	    //of course x ->Pt, y->Eta, z->NFIT

	  }
	
	if( Dca[itr] < maxDca  && NFit[itr] >= NFitCut && NComm[itr] > NCommCut)
	  dedx->Fill(p,dEdx[itr]*1e6);
	
	// filling histograms for global variables and energy loss
	
	if (Dca[itr]<0. || Dca[itr]>maxDca) continue;
	if(NFit[itr] >= NFitCut  && NComm[itr] > NCommCut )
	  {
	    hPtM->Fill(PtM[itr]);
	    hEtaM->Fill(EtaM[itr]);
	    hFSectM->Fill(FSectM[itr]);
	    hLSectM->Fill(LSectM[itr]);
	    Double_t mass0 = 0.13957;
	    Double_t P0 = sqrt(PtM[itr]*PtM[itr]+PzM[itr]*PzM[itr]+mass0*mass0);
	    Double_t P0mc = sqrt(PtMmc[itr]*PtMmc[itr]+PzMmc[itr]*PzMmc[itr]+mass0*mass0);
	    Double_t rap = 0.5*log((P0+PzM[itr])/(P0-PzM[itr]));
	    Double_t rapmc = 0.5*log((P0mc+PzMmc[itr])/(P0mc-PzMmc[itr]));
	    hYM->Fill(rap);
	    hPhiM->Fill(PhiM[itr]);
	    hPtM_E->Fill(PtM[itr], PtM[itr]-PtMmc[itr]);
	    hYM_E->Fill(rap, rap-rapmc);
	    hPhiM_E->Fill(PhiM[itr], PhiM[itr]-PhiMmc[itr]);
	    hPtYPhiM->Fill(PtM[itr],rap,PhiM[itr]);
	  }
	
      }//closing for loop
    
    //filling Ghosts tracks
    
    for (Int_t itrG=0; itrG<ntrkG; itrG++)
      { 
	if (fabs(EtaG[itrG]) > yCut) continue;
	float pg = sqrt(PtG[itrG]*PtG[itrG]+PzG[itrG]*PzG[itrG]);
	
	if (NFitG[itrG] >= NFitCut)
	  {
	    hDcaG      ->Fill(nch, PtG[itrG],DcaG[itrG]);	  
	    hPtEtaDcaG ->Fill(PtG[itr], EtaG[itr], DcaG[itr]);
	    //of course x-> Pt, y->Eta, z->DCA
	  }
	
	if (DcaG[itrG] < maxDca && NFitG[itrG] >= 10)
	  {
	    hNfitG     ->Fill(nch, PtG[itrG], NFitG[itrG]); 
	    hPtEtaNfitG->Fill( PtG[itr], EtaG[itr], NFitG[itr]);
	    //of course x-> Pt, y->Eta, z->Nfit
	  }
	
	if( DcaG[itrG] < maxDca && NFitG[itrG] >= NFitCut){
	  dedxG->Fill(pg,dEdxG[itrG]*1e6);
	  hPhiG->Fill(PhiG[itrG]);
	  hFSectG->Fill(FSectG[itrG]);
	  hLSectG->Fill(LSectG[itrG]);
	}
      }
    
    //filling Mc tracks(embedded)
    
    for (int itr1=0; itr1 < ntrk1; itr1++)
      {
	//if (fabs(EtaMc[itr1]) > yCut) continue;
	
	hPtMc->Fill(PtMc[itr1]);
	hEtaMc->Fill(EtaMc[itr1]);
	Double_t mass0 = 0.13957;
	Double_t P0 = sqrt(PtMc[itr1]*PtMc[itr1]+PzMc[itr1]*PzMc[itr1]+mass0*mass0);
	hYMc->Fill(0.5*log((P0+PzMc[itr1])/(P0-PzMc[itr1])));
	hPhiMc->Fill(PhiMc[itr1]);
	hPtYPhiMc->Fill(PtMc[itr1],0.5*log((P0+PzMc[itr1])/(P0-PzMc[itr1])),PhiMc[itr1]);
      }
    
  }//closing while loop
 

  //-------Writing-----------

  cout<< "Creating output file .... "<<oFile; flush(cout);  

  TFile *fout=new TFile(oFile,"recreate"); 
  fout->cd();

  // -- MC
  hMult->Write();
  nChTotal->Write();
  dedx->Write();
  dedxG->Write();
  hNfit->Write();
  hNfitG->Write();
  hDca-> Write();

  //Ghost
  hDcaG-> Write();
  hPhiG->Write();
  hFSectG->Write();
  hLSectG->Write();
  
  hPtEtaNfitG->Write();
  hPtEtaDcaG ->Write();
 
  //Global Variables
  v_xy->Write();
  vz->Write();

  dvx->Write();
  dvy->Write();
  dvz->Write();

  //---Matched
  hPtM->Write();
  hEtaM->Write();
  hFSectM->Write();
  hLSectM->Write();
  hYM->Write();
  hPhiM->Write();
  hPtM_E->Write();
  hYM_E->Write();
  hPhiM_E->Write();
  hPtYPhiM->Write();
  
  hPtEtaDcaM->Write();
  hPtEtaNfitM-> Write();
  
  //---Embedded
  hEtaMc->Write();
  hYMc->Write();
  hPtMc->Write();
  hPhiMc->Write();
  hPtYPhiMc->Write();

   fout->GetList()->Sort();

  fout->Close();
  cout<<" done"<<endl; flush(cout);
}
