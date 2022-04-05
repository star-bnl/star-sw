///////estimate efficiencies of disks using straight line
#include "StFgtGenPlotter.h"
#include "StRoot/StEvent/StFgtCollection.h"
#include "StRoot/StEvent/StFgtHitCollection.h"
#include "StRoot/StEvent/StFgtHit.h"
#include "StRoot/StFgtUtil/geometry/StFgtGeom.h"
#include "StRoot/StEvent/StEvent.h"
#include "StRoot/StEvent/StEventInfo.h"
#include "StRoot/StFgtUtil/geometry/StFgtGeom.h"
#include <TH2D.h>
#include <TROOT.h>
#include <TStyle.h>
#include <TCanvas.h>
#include <utility>
#include <TArc.h>
#include <TLine.h>
#include <set>


#include "StRoot/StFgtUtil/geometry/StFgtGeom.h"

#include "StRoot/StEvent/StEvent.h"
#include "StRoot/StEvent/StFgtCollection.h"

#define MAX_DIST 0.5
#define MIN_NUM_POINTS 4

/** grab the cluster container, fill histograms

*/
Int_t StFgtGenPlotter::Make()
{
  cout <<"gen plot  make " <<endl;
  Int_t ierr = kStOk;
  cout <<"general make " <<endl;
  StFgtGeneralBase::Make();
  cout <<" making cluster plotter " <<endl;
  Int_t realEvtNr=0;

  Float_t x;
  Float_t y;
  Int_t prvGeoId=-1;
  for(int iDx=0;iDx<kFgtNumDiscs;iDx++)
    {
      
	for(int iQ=0;iQ<2;iQ++)
	{
	  vector<generalStrip> &stripVec=(pStrips[iDx*2+iQ]);
	  vector<generalStrip>::iterator stripIter;
	  cout <<"got strips" <<endl;
	  Int_t seed1R=0;
	  Int_t seed2R=0;
	  Int_t seed3R=0;
	  Int_t seedDeadR=0;
	  
	  Int_t seed1P=0;
	  Int_t seed2P=0;
	  Int_t seed3P=0;
	  Int_t seedDeadP=0;
	  
	  for(stripIter=stripVec.begin();stripIter != stripVec.end();stripIter++)
	    {
	      Short_t tDisc, tQuad,tStrip;
	      Char_t tLayer;
	      Int_t geoId=stripIter->geoId;
	  	  cout <<" looking at strip with geo : " << stripIter->geoId<<endl;
	  //get strip id from the geo id
	      StFgtGeom::decodeGeoId(geoId,tDisc,tQuad,tLayer,tStrip);     
	      //	cout <<"seed is : " << stripIter->seedType <<endl;
	      if(stripIter->seedType==kFgtSeedType1)
		{
		  if(tLayer=='R')
		    seed1R++;
		  else
		    seed1P++;
		}
	      if(stripIter->seedType==kFgtSeedType2)
		{
		  if(tLayer=='R')
		    seed2R++;
		  else
		    seed2P++;
		  
		}
	      if(stripIter->seedType==kFgtSeedType3)
		{
		  if(tLayer=='R')
		    seed3R++;
		  else
		    seed3P++;
		}
	      
	      
	      if(stripIter->seedType==kFgtDeadStrip)
		{
		  if(tLayer=='R')
		    seedDeadR++;
		  else
		    seedDeadP++;
		}
	      
	    }
	cout <<"filling seed etc .. " <<endl;
	cout << " seed1R: " << seed1R <<" 2R: " << seed2R <<" dead: " << seedDeadR << endl;
	cout << " seed1R: " << seed3R <<" 2R: " << seed3P <<" dead: " << seed3P << endl;
	seedsPerDiscR[iDx*2+iQ]->Fill(kFgtSeedType1,seed1R);
	seedsPerDiscR[iDx*2+iQ]->Fill(kFgtSeedType2,seed2R);
	seedsPerDiscR[iDx*2+iQ]->Fill(kFgtSeedType3,seed3R);
	seedsPerDiscR[iDx*2+iQ]->Fill(kFgtDeadStrip,seedDeadR);
	seedsPerDiscP[iDx*2+iQ]->Fill(kFgtSeedType1,seed1P);
	seedsPerDiscP[iDx*2+iQ]->Fill(kFgtSeedType2,seed2P);
	seedsPerDiscP[iDx*2+iQ]->Fill(kFgtSeedType3,seed3P);
	seedsPerDiscP[iDx*2+iQ]->Fill(kFgtDeadStrip,seedDeadP);
	cout <<"done .." <<endl;
	}
      
      vector<float> vPhi[4];
      vector<float> vR[4];

      vector<float> vPhiCharge[4];
      vector<float> vRCharge[4];

      vector<float> vPhiClusSize[4];
      vector<float> vRClusSize[4];

      cout <<"trying to get clusters in disc " << iDx << endl;

      /*     const TArrayI& geoIdArray = clusterPtr->getStripGeoIdArray();
	     const TArrayF& weightArray = clusterPtr->getStripWeightArray();

	     Int_t n = geoIdArray.GetSize();
	     if( n != weightArray.GetSize() ){
	     // something weird happened
	     };

	     for( Int_t i=0; i<n; ++i ){
	     Int_t geoId = geoIdArray[i];
	     Float_t w = weightArray[i];

	     // do what you want here
	     };
      */

      vector<generalCluster> &hitVec=*(pClusters[iDx]);
      cout <<"got collection, looking at " << hitVec.size() << " hits ..  " <<endl;
      vector<generalCluster>::iterator hitIter;
      Int_t multQ1R=0;
      Int_t multQ2R=0;
      Int_t multQ1P=0;
      Int_t multQ2P=0;
      cout <<"hit iter " <<endl;
      for(hitIter=hitVec.begin();hitIter != hitVec.end();hitIter++)
	{
	  Int_t iq=hitIter->quad;
	  Float_t phi=hitIter->posPhi;
	  Float_t r=hitIter->posR;
	  Int_t geoId=hitIter->centralStripGeoId;
	  Float_t charge=hitIter->clusterCharge;
	  Int_t clusSize=hitIter->clusterSize;

	  Int_t numStrips=hitIter->clusterSize;
	  ///check if at least one of the strips has a valid seed)
	  Short_t quad, disc, strip;
	  Char_t layer; 
	  Bool_t containsSeed=true; // just take all clusters
	       
	  if(hitIter->layer=='R')
	    {
	      if(iq==0)
		multQ1R++;
	      else
		multQ2R++;
	      cout <<"push " <<endl;
	      vR[iq].push_back(r);
	      vRCharge[iq].push_back(charge);
	      vRClusSize[iq].push_back(clusSize);
	    }
	  else
	    {
	      if(iq==0)
		multQ1P++;
	      else
		multQ2P++;
	      cout <<"pushP " <<endl;
	      vPhi[iq].push_back(phi);
	      vPhiCharge[iq].push_back(charge);
	      vPhiClusSize[iq].push_back(clusSize);
	    }
	  cout <<"push done" <<endl;
	  Short_t tDisc, tQuad,tStrip;
	  Char_t tLayer;
	  //get strip id from the geo id
	  StFgtGeom::decodeGeoId(geoId,tDisc,tQuad,tLayer,tStrip);     
	  cout <<" filling" <<endl;
	  hCChargePosSpacePhi[iDx*kFgtNumQuads+iq]->Fill(phi,charge);
	  hCChargePosSpaceR[iDx*kFgtNumQuads+iq]->Fill(r,charge);
	  hClusSizePhi[iDx*kFgtNumQuads+iq]->Fill(phi,numStrips);
	  hClusSizePhi[iDx*kFgtNumQuads+iq]->Fill(phi,numStrips);
	  hClusSizeR[iDx*kFgtNumQuads+iq]->Fill(r,numStrips);
	  hCChargeElecSpace[iDx*kFgtNumQuads+iq]->Fill(tStrip,charge);
	  hClusSizeElecSpace[iDx*kFgtNumQuads+iq]->Fill(tStrip,numStrips);
	}
      cout <<"done " <<endl;
      multPerDiscR[iDx*2]->Fill(multQ1R);
      multPerDiscR[iDx*2+1]->Fill(multQ2R);
      multPerDiscP[iDx*2]->Fill(multQ1P);
      multPerDiscP[iDx*2+1]->Fill(multQ2P);

      cout <<"going on " <<endl;
      for(int iQ=0;iQ<4;iQ++)
	{
	  int rCount=0;
	  for(vector<float>::iterator itR=vR[iQ].begin();itR!=vR[iQ].end();itR++)
	    {
	      float chargeR=(vRCharge[iQ])[rCount];
	      float clusSizeR=(vRClusSize[iQ])[rCount];
	      rCount++;
	      int phiCount=0;
	      for(vector<float>::iterator itP=vPhi[iQ].begin();itP!=vPhi[iQ].end();itP++)
		{
		  float chargePhi=(vPhiCharge[iQ])[phiCount];
		  float clusSizePhi=(vPhiClusSize[iQ])[phiCount];
		  //		     cout <<" chargePhi: " << chargePhi <<" clussize: " << clusSizePhi << " counter: " << phiCount <<endl;
		  phiCount++;
		  x=(*itR)*cos(*itP);
		  y=(*itR)*sin(*itP);
		  radioPlots[iDx]->Fill(x,y);
		  radioRatio[iDx]->Fill(x,y);
		  radioChargeR[iDx]->Fill(x,y,chargeR);
		  radioChargePhi[iDx]->Fill(x,y,chargePhi);
		  radioClusSizeR[iDx]->Fill(x,y,clusSizeR);
		  radioClusSizePhi[iDx]->Fill(x,y,clusSizePhi);
		}
	    }

	  for(vector<float>::iterator itR=vRCharge[iQ].begin();itR!=vRCharge[iQ].end();itR++)
	    {
	      for(vector<float>::iterator itP=vPhiCharge[iQ].begin();itP!=vPhiCharge[iQ].end();itP++)
		{
		  corrPlots[iDx*kFgtNumQuads+iQ]->Fill(*itR,*itP);
		}
	    }
	     
	  vPhi[iQ].clear();
	  vR[iQ].clear();
	  vPhiCharge[iQ].clear();
	  vRCharge[iQ].clear();
	  vPhiClusSize[iQ].clear();
	  vRClusSize[iQ].clear();
	}
	 
    }

 
  runningEvtNr++; 
  return ierr;

};
 
StFgtGenPlotter::StFgtGenPlotter( const Char_t* name): StFgtGeneralBase( name ),runningEvtNr(0),hitCounter(0),hitCounterR(0)
{


};

StFgtGenPlotter::~StFgtGenPlotter()
{

  //delete histogram arrays
};


Int_t StFgtGenPlotter::Finish(){
  cout <<"genPlotter finish" <<endl;
  gStyle->SetPalette(1);
  cout <<"cluster plotter finish funciton " <<endl;
  Int_t ierr = kStOk;
  TCanvas* cChargePhi=new TCanvas("chargePhi","chargePhi",850,1100);
  TCanvas* cChargePhiBig=new TCanvas("chargePhiBig","chargePhiBig",850,1100);
  cChargePhi->SetLogz();
  cChargePhi->Divide(kFgtNumDiscs,kFgtNumQuads);
  TCanvas* cChargeR=new TCanvas("chargeR","chargeR",850,1100);
  TCanvas* cChargeRBig=new TCanvas("chargeRBig","chargeRBig",850,1100);
  cChargeR->SetLogz();
  cChargeR->Divide(kFgtNumDiscs,kFgtNumQuads);
  TCanvas* cClusSizePhi=new TCanvas("clusSizePhi","clusSizePhi",850,1100);
  TCanvas* cClusSizePhiBig=new TCanvas("clusSizePhiBig","clusSizePhiBig",850,1100);
  cClusSizePhi->Divide(kFgtNumDiscs,kFgtNumQuads);
  TCanvas* cClusSizeR=new TCanvas("clusSizeR","clusSizeR",850,1100);
  TCanvas* cClusSizeRBig=new TCanvas("clusSizeRBig","clusSizeRBig",850,1100);
  cClusSizeR->Divide(kFgtNumDiscs,kFgtNumQuads);
  TCanvas* cChargeElecSpace=new TCanvas("cChargeElecSpace","cChargeEledId",850,1100);
  TCanvas* cChargeElecSpaceBig=new TCanvas("cChargeElecSpaceBig","cChargeEledIdBig",850,1100);

  TCanvas* cDiscQ=new TCanvas("cDiscQ","cDiscQ",850,1100);
  //  cDiscQ->Divide(kFgtNumDiscs,2);

  cChargeElecSpace->SetLogz();
  cChargeElecSpace->Divide(kFgtNumDiscs,kFgtNumQuads);
  TCanvas* cClusSizeElecSpace=new TCanvas("clusSizeElecSpace","clusSizeEledId",850,1100);
  TCanvas* cClusSizeElecSpaceBig=new TCanvas("clusSizeElecSpaceBig","clusSizeEledIdBig",850,1100);
  cClusSizeElecSpace->Divide(kFgtNumDiscs,kFgtNumQuads);

  TCanvas* cRadio=new TCanvas("radioPlots","radioPlot",1000,1500);
  cRadio->Divide(2,3); //6 discs

  TCanvas* cRadioChargeR=new TCanvas("radioPlotsChargeR","radioPlotChargeR",1000,1500);
  cRadioChargeR->Divide(2,3); //6 discs
  TCanvas* cRadioChargePhi=new TCanvas("radioPlotsChargePhi","radioPlotChargePhi",1000,1500);
  cRadioChargePhi->Divide(2,3); //6 discs

  TCanvas* cRadioClusSizeR=new TCanvas("radioPlotsClusSizeR","radioPlotClusSizeR",1000,1500);
  cRadioClusSizeR->Divide(2,3); //6 discs
  TCanvas* cRadioClusSizePhi=new TCanvas("radioPlotsClusSizePhi","radioPlotClusSizePhi",1000,1500);
  cRadioClusSizePhi->Divide(2,3); //6 discs

  TCanvas* cRadioRatio=new TCanvas("radioRatio","radioRatio",1000,1500);
  cRadioRatio->Divide(2,3); //6 discs

  TCanvas* cCorr=new TCanvas("correlationPlots","correlationPlot",1000,1500);
  TCanvas* cCorrBig=new TCanvas("correlationPlotsBig","correlationPlotBig",1000,1500);
  cCorr->Divide(kFgtNumDiscs,kFgtNumQuads);

  for(Int_t iD=0;iD<kFgtNumDiscs;iD++)
    {
      for(int nx=0;nx<radioRatio[iD]->GetNbinsX();nx++)
	{
	  for(int ny=0;ny<radioRatio[iD]->GetNbinsY();ny++)
	    {
	      Double_t denom=radioRatio[iD]->GetBinContent(nx,ny);
	      if(denom>0)
		{
		  radioChargeR[iD]->SetBinContent(nx,ny,radioChargeR[iD]->GetBinContent(nx,ny)/denom);
		  //		  cout <<" radioChargePhi content is :" << radioChargePhi[iD]->GetBinContent(nx,ny) <<" dividing by " << denom <<endl;
		  radioChargePhi[iD]->SetBinContent(nx,ny,radioChargePhi[iD]->GetBinContent(nx,ny)/denom);


		  radioClusSizeR[iD]->SetBinContent(nx,ny,radioClusSizeR[iD]->GetBinContent(nx,ny)/denom);
		  radioClusSizePhi[iD]->SetBinContent(nx,ny,radioClusSizePhi[iD]->GetBinContent(nx,ny)/denom);
		}
	      else
		{
		  radioChargeR[iD]->SetBinContent(nx,ny,0.0);
		  radioChargePhi[iD]->SetBinContent(nx,ny,0.0);
		  radioClusSizeR[iD]->SetBinContent(nx,ny,0.0);
		  radioClusSizePhi[iD]->SetBinContent(nx,ny,0.0);
		}
	      ///to get to the ratio of clusters per event
	      //	      cout <<"radioratio: " << radioRatio[iD]->GetBinContent(nx,ny) <<" div by : " << runningEvtNr << " is: " << (radioRatio[iD]->GetBinContent(nx,ny))/(double)runningEvtNr <<endl;
	      radioRatio[iD]->SetBinContent(nx,ny,((double)(radioRatio[iD]->GetBinContent(nx,ny)))/(double)runningEvtNr);
	    }
	}

      char buffer[200];
      for(Int_t iQ=0;iQ<2;iQ++)
	{
	  cDiscQ->cd();
	  multPerDiscR[iD*2+iQ]->Draw();
	  sprintf(buffer,"multPerDiscR_%d_%d.png",iD,iQ);
	  cDiscQ->SaveAs(buffer);

	  //	  cDiscQ->cd(iD*2+iQ+1);
	  multPerDiscP[iD*2+iQ]->Draw();
	  sprintf(buffer,"multPerDiscP_%d_%d.png",iD,iQ);
	  cDiscQ->SaveAs(buffer);

	  //	  cDiscQ->cd(iD*2+iQ+1);
	  seedsPerDiscR[iD*2+iQ]->Draw();
	  sprintf(buffer,"seedPerDiscR_%d_%d.png",iD,iQ);
	  cDiscQ->SaveAs(buffer);

	  //	  cDiscQ->cd(iD*2+iQ+1);
	  seedsPerDiscP[iD*2+iQ]->Draw();
	  sprintf(buffer,"seedPerDiscP_%d_%d.png",iD,iQ);
	  cDiscQ->SaveAs(buffer);
	}

      for(Int_t iQ=0;iQ<kFgtNumQuads;iQ++)
	{
	  cout <<"drawing disc " << iD <<" quad " << iQ <<endl;
	  cChargePhi->cd(iD*kFgtNumQuads+iQ+1)->SetLogz();
	  hCChargePosSpacePhi[iD*kFgtNumQuads+iQ]->Draw("colz");
	  cChargePhiBig->cd()->SetLogz();
	  hCChargePosSpacePhi[iD*kFgtNumQuads+iQ]->Draw("colz");
	  cChargePhiBig->Print("cChargePhiBig.pdf");

	  cChargeR->cd(iD*kFgtNumQuads+iQ+1)->SetLogz();
	  hCChargePosSpaceR[iD*kFgtNumQuads+iQ]->Draw("colz");

	  cChargeRBig->cd()->SetLogz();
	  hCChargePosSpaceR[iD*kFgtNumQuads+iQ]->Draw("colz");
	  cChargeRBig->Print("cChargeRBig.pdf");


	  cClusSizePhi->cd(iD*kFgtNumQuads+iQ+1)->SetLogz();
	  hClusSizePhi[iD*kFgtNumQuads+iQ]->Draw("colz");

	  cClusSizePhiBig->cd()->SetLogz();
	  hClusSizePhi[iD*kFgtNumQuads+iQ]->Draw("colz");
	  cClusSizePhiBig->Print("cClusSizePhiBig.pdf");

	  cClusSizeR->cd(iD*kFgtNumQuads+iQ+1)->SetLogz();
	  hClusSizeR[iD*kFgtNumQuads+iQ]->Draw("colz");

	  cClusSizeRBig->cd()->SetLogz();
	  hClusSizeR[iD*kFgtNumQuads+iQ]->Draw("colz");
	  cClusSizeRBig->Print("cClusSizeRBig.pdf");


	  cChargeElecSpace->cd(iD*kFgtNumQuads+iQ+1)->SetLogz();
	  hCChargeElecSpace[iD*kFgtNumQuads+iQ]->Draw("colz");

	  cChargeElecSpaceBig->cd()->SetLogz();
	  hCChargeElecSpace[iD*kFgtNumQuads+iQ]->Draw("colz");
	  cChargeElecSpaceBig->Print("cChargeElecSpaceBig.pdf");


	  cClusSizeElecSpace->cd(iD*kFgtNumQuads+iQ+1)->SetLogz();
	  hClusSizeElecSpace[iD*kFgtNumQuads+iQ]->Draw("colz");


	  cClusSizeElecSpace->cd()->SetLogz();
	  hClusSizeElecSpace[iD*kFgtNumQuads+iQ]->Draw("colz");
	  cClusSizeElecSpaceBig->Print("cClusSizeElecCooBig.pdf");


	  cCorr->cd(iD*kFgtNumQuads+iQ+1)->SetLogz();
	  corrPlots[iD*kFgtNumQuads+iQ]->Draw("colz");

	  cCorrBig->cd()->SetLogz();
	  corrPlots[iD*kFgtNumQuads+iQ]->Draw("colz");
	  cCorrBig->Print("r_phi_correlationsBig.pdf");

	}
      cRadio->cd(iD+1)->SetLogz();
      radioPlots[iD]->Draw("colz");

      //      cRadioChargeR->cd(iD+1)->SetLogz();
      cRadioChargeR->cd(iD+1);
      radioChargeR[iD]->Draw("colz");

      //      cRadioChargePhi->cd(iD+1)->SetLogz();
      cRadioChargePhi->cd(iD+1);
      radioChargePhi[iD]->Draw("colz");

      //      cRadioClusSizeR->cd(iD+1)->SetLogz();
      cRadioClusSizeR->cd(iD+1);
      radioClusSizeR[iD]->Draw("colz");

      //      cRadioClusSizePhi->cd(iD+1)->SetLogz();
      cRadioClusSizePhi->cd(iD+1);
      radioClusSizePhi[iD]->Draw("colz");

      //cRadioRatio->cd(iD+1)->SetLogz();
      cRadioRatio->cd(iD+1);
      radioRatio[iD]->Draw("colz");

    }


  cout <<"saving .." <<endl;
  cChargePhi->SaveAs("cChargePhi.pdf");
  cChargePhi->SaveAs("cChargePhi.png");

  cChargeR->SaveAs("cChargeR.pdf");
  cChargeR->SaveAs("cChargeR.png");

  cClusSizePhi->SaveAs("cClusSizePhi.pdf");
  cClusSizePhi->SaveAs("cClusSizePhi.png");

  cClusSizeR->SaveAs("cClusSizeR.pdf");
  cClusSizeR->SaveAs("cClusSizeR.png");


  cChargeElecSpace->SaveAs("cClusSizeElecSpace.pdf");
  cChargeElecSpace->SaveAs("cClusSizeElecSpace.png");

  cClusSizeElecSpace->SaveAs("cClusSizeElecSpace.pdf");
  cClusSizeElecSpace->SaveAs("cClusSizeElecSpace.png");

  cRadio->SaveAs("radioPlots.png");
  cRadio->SaveAs("radioPlots.pdf");


  cRadioChargeR->SaveAs("radioChargeR.png");
  cRadioChargePhi->SaveAs("radioChargePhi.png");

  cRadioClusSizeR->SaveAs("radioClusSizeR.png");
  cRadioClusSizePhi->SaveAs("radioClusSizePhi.png");

  cRadioRatio->SaveAs("radioRatio.png");


  cCorr->SaveAs("corrPlots.png");
  cCorr->SaveAs("corrPlots.pdf");

  myRootFile->Write();
  myRootFile->Close();


  return ierr;
};


/**
   construct histograms

*/
Int_t StFgtGenPlotter::Init(){
  cout <<"gen plotter init" <<endl;
  Int_t ierr=kStOk;

  myRootFile=new TFile("clusterPlotter.root","RECREATE");
  //  outTxtFile=new ofstream;
  //  outTxtFile->open("clusters.txt");

  outTxtFileP=new ofstream;
  outTxtFileP->open("clustersP.txt");
  outTxtFileR=new ofstream;
  outTxtFileR->open("clustersR.txt");

  multPerDiscR=new TH1I*[kFgtNumDiscs*4];
  multPerDiscP=new TH1I*[kFgtNumDiscs*5];

  seedsPerDiscR=new TH1I*[kFgtNumDiscs*4];
  seedsPerDiscP=new TH1I*[kFgtNumDiscs*4];



  hClusterCharge=new TH1D("clusterCharge","clusterCharge",100, 0, 1000);
  char buffer[100];
  hCChargePosSpacePhi=new TH2D*[kFgtNumDiscs*kFgtNumQuads];
  hCChargePosSpaceR=new TH2D*[kFgtNumDiscs*kFgtNumQuads];
  hClusSizePhi=new TH2D*[kFgtNumDiscs*kFgtNumQuads];
  hClusSizeR=new TH2D*[kFgtNumDiscs*kFgtNumQuads];
  hCChargeElecSpace=new TH2D*[kFgtNumDiscs*kFgtNumQuads];
  hClusSizeElecSpace=new TH2D*[kFgtNumDiscs*kFgtNumQuads];

  radioPlots=new TH2D*[kFgtNumDiscs];
  radioRatio=new TH2D*[kFgtNumDiscs];

  radioChargeR=new TH2D*[kFgtNumDiscs];
  radioChargePhi=new TH2D*[kFgtNumDiscs];

  radioClusSizeR=new TH2D*[kFgtNumDiscs];
  radioClusSizePhi=new TH2D*[kFgtNumDiscs];
  cout <<"1" <<endl;

  corrPlots=new TH2D*[kFgtNumDiscs*kFgtNumQuads];


  for(int iD=0;iD<kFgtNumDiscs;iD++)
    {
      for(int iQ=0;iQ<4;iQ++)
	{
	  sprintf(buffer,"clusterMult%d_QuadR%d_phi",iD,iQ);
	  multPerDiscR[iD*2+iQ]=new TH1I(buffer,buffer,10,0,10);
	  multPerDiscR[iD*2+iQ]->SetFillColor(kYellow);
	  sprintf(buffer,"clusterMult%d_QuadP%d_phi",iD,iQ);
	  multPerDiscP[iD*2+iQ]=new TH1I(buffer,buffer,10,0,10);
	  multPerDiscP[iD*2+iQ]->SetFillColor(kYellow);
	  sprintf(buffer,"seeds%d_QuadR%d_phi",iD,iQ);
	  seedsPerDiscR[iD*2+iQ]=new TH1I(buffer,buffer,10,0,10);
	  seedsPerDiscR[iD*2+iQ]->SetFillColor(kYellow);
	  sprintf(buffer,"seeds%d_QuadP%d_phi",iD,iQ);
	  seedsPerDiscP[iD*2+iQ]=new TH1I(buffer,buffer,10,0,10);
	  seedsPerDiscP[iD*2+iQ]->SetFillColor(kYellow);
	}
      cout <<"2" <<endl;
      for(int iQ=0;iQ<kFgtNumQuads;iQ++)
	{
	  sprintf(buffer,"clusterChargeDisk%d_Quad%d_phi",iD,iQ);
	  hCChargePosSpacePhi[iD*kFgtNumQuads+iQ]=new TH2D(buffer,buffer,200,-4,4, 200, 0, 2000);
	  sprintf(buffer,"clusterChargeDisk%d_Quad%d_R",iD,iQ);
	  hCChargePosSpaceR[iD*kFgtNumQuads+iQ]=new TH2D(buffer,buffer,200,10,35, 200, 0, 2000);
	  sprintf(buffer,"clusterSizeDisk%d_Quad%d_phi",iD,iQ);
	  hClusSizePhi[iD*kFgtNumQuads+iQ]=new TH2D(buffer,buffer,1000,-4,4, 20, 0, 20);
	  //	  hClusSizePhi[iD*kFgtNumQuads+iQ]->SetFillColor(kYellow);
	  sprintf(buffer,"clusterSizeDisk%d_Quad%d_R",iD,iQ);
	  hClusSizeR[iD*kFgtNumQuads+iQ]=new TH2D(buffer,buffer,1000,10,35, 20, 0, 20);
	  //	  hClusSizeR[iD*kFgtNumQuads+iQ]->SetFillColor(kYellow);
	  sprintf(buffer,"clusterSizeDisk%d_Quad%d_ElecSpace",iD,iQ);
	  hClusSizeElecSpace[iD*kFgtNumQuads+iQ]=new TH2D(buffer,buffer,100,0,1000, 20, 0, 20);
	  sprintf(buffer,"clusterChargeDisk%d_Quad%d_ElecSpace",iD,iQ);
	  hCChargeElecSpace[iD*kFgtNumQuads+iQ]=new TH2D(buffer,buffer,100,0,1000, 100, 0, 2000);
	  sprintf(buffer,"radioDisk%d_Quad_%d",iD,iQ);
	  radioPlots[iD]=new TH2D(buffer,buffer,100,-50,50,100,-50,50);

	  sprintf(buffer,"radioRatio_%d_Quad_%d",iD,iQ);
	  radioRatio[iD]=new TH2D(buffer,buffer,30,-50,50,30,-50,50);

	  sprintf(buffer,"radioChargeR_%d_Quad_%d",iD,iQ);
	  radioChargeR[iD]=new TH2D(buffer,buffer,30,-50,50,30,-50,50);
	  sprintf(buffer,"radioChargePhi_%d_Quad_%d",iD,iQ);
	  radioChargePhi[iD]=new TH2D(buffer,buffer,30,-50,50,30,-50,50);

	  sprintf(buffer,"radioClusSizeR_%d_Quad_%d",iD,iQ);
	  radioClusSizeR[iD]=new TH2D(buffer,buffer,30,-50,50,30,-50,50);
	  sprintf(buffer,"radioClusSizePhi_%d_Quad_%d",iD,iQ);
	  radioClusSizePhi[iD]=new TH2D(buffer,buffer,30,-50,50,30,-50,50);


	  sprintf(buffer,"r_phi_ChargeCorr%d_Quad_%d",iD,iQ);


	  corrPlots[iD*kFgtNumQuads+iQ]=new TH2D(buffer,buffer,100,0,2000,100,0,2000);
	}
    }
  cout <<"4" <<endl;
  return ierr;
};
ClassImp(StFgtGenPlotter);
