/////
#include "StFgtClusterPlotter.h"
#include "StRoot/StEvent/StFgtCollection.h"
#include "StRoot/StEvent/StFgtHitCollection.h"
#include "StRoot/StEvent/StFgtHit.h"
#include "StRoot/StFgtUtil/geometry/StFgtGeom.h"
#include "StRoot/StEvent/StEvent.h"
#include "StRoot/StEvent/StEventInfo.h"
#include <TH2D.h>
#include <TROOT.h>
#include <TStyle.h>
#include <TCanvas.h>

#include "StRoot/StFgtUtil/geometry/StFgtGeom.h"

#include "StRoot/StEvent/StEvent.h"
#include "StRoot/StEvent/StFgtCollection.h"


/**
check if at least one hit in the cluster fulfills a pulse shape criterion
*/
/*Bool_t checkPulse(StFgtHit* pClus)
{
  int N=7;
  stripWeightMap_t strips=pClus->getStripWeightMap();
  for(stripWeightMap_t::iterator it=strips.begin();it!=strips.end();it++)
    {
      Float_t peakAdc=leadEdgeBin=-9999;
      Float_t sumAdc=0;
    }
  for(int i=0;i<N;i++)
    {
      sumAdc+=it->getAdc(i);
      if(leadEdgeBin<0 && it->getAdc(i)>...
    }

}*/


/** grab the cluster container, fill histograms

*/
Int_t StFgtClusterPlotter::Make()
{
  cout <<" making cluster plotter " <<endl;
  Int_t ierr = kStOk;
  StEvent* eventPtr = 0;
  eventPtr = (StEvent*)GetInputDS("StEvent");

  if( !eventPtr ) {
    LOG_ERROR << "Error getting pointer to StEvent from '" << ClassName() << "'" << endm;
    ierr = kStErr;
  };

   mFgtCollectionPtr = 0;

   if( eventPtr ) {
      mFgtCollectionPtr=eventPtr->fgtCollection();
   };

   if( !mFgtCollectionPtr) {
      LOG_ERROR << "Error getting pointer to StFgtCollection from '" << ClassName() << "'" << endm;
      ierr = kStErr;
   };
   Int_t realEvtNr=-1;
   if(eventPtr)
     {
       if(eventPtr->info())
	 realEvtNr=eventPtr->info()->id();
     }

   (*outTxtFileP) <<endl<<endl<<" ------new event: " << realEvtNr << "----------------------" << " running nr: " << runningEvtNr << "------" << endl;
   (*outTxtFileR) <<endl<<endl<<" ------new event: " << realEvtNr << "----------------------" << " running nr: " << runningEvtNr << "------" << endl;

   cout <<"in plotter make " << endl;
   Float_t x;
   Float_t y;
   Int_t prvGeoId=-1;
   for(int iDx=0;iDx<kFgtNumDiscs;iDx++)
   {
     vector<float> vPhi[4];
     vector<float> vR[4];
     
     vector<float> vPhiCharge[4];
     vector<float> vRCharge[4];
     
     StFgtStripCollection& strips=*(mFgtCollectionPtr->getStripCollection( iDx ));
     
     for( StSPtrVecFgtStripIterator it=strips.getStripVec().begin();it!=strips.getStripVec().end();++it)
       {
	 /*	     	     	     cout <<"maxAdc: " << (*it)->getMaxAdc() <<endl;
	   cout <<"hits : ";
	   for(int i=0;i<7;i++)
	       { cout <<" " << (*it)->getAdc(i);}
	       cout <<endl;*/
	 
	 Short_t quad, disc, strip;
	 Char_t layer; 
	 
	 Bool_t stripDead=false;
	 
	 StFgtGeom::decodeGeoId((*it)->getGeoId(),disc, quad, layer, strip);
	 if(layer=='R')
	   outTxtFile=outTxtFileR;
	 else
	   outTxtFile=outTxtFileP;
	 
	 if(((*it)->getGeoId()-prvGeoId)>2 && prvGeoId>=0)
	   {
	     (*outTxtFile) <<endl<<endl<<endl;;
	   }
	 
	 prvGeoId=(*it)->getGeoId();
	 switch((*it)->getClusterSeedType())
	   {
	   case kFgtSeedType1:
	     (*outTxtFile) <<"#";
	     break;
	   case kFgtSeedType2:
	     (*outTxtFile) <<"#";
		 break;
	   case kFgtSeedType3:
		 (*outTxtFile) <<"#";
		 break;
	       case kFgtClusterPart:
		 (*outTxtFile) <<"*";
		 break;
	       case kFgtClusterEndUp:
		 (*outTxtFile) <<"*";
		 break;
	       case kFgtClusterEndDown:
		 (*outTxtFile) <<"*";
		 break;
	       case kFgtDeadStrip:
		 if(((it-1))>=strips.getStripVec().begin() && (it+1)<strips.getStripVec().end())
		   {
		     if((*(it-1))->getClusterSeedType()>kFgtDeadStrip&& (*(it+1))->getClusterSeedType()>kFgtDeadStrip)
		       (*outTxtFile) <<"*";
		     else
		       (*outTxtFile) <<"x";
		   }
		 else
		   (*outTxtFile) <<"x";
		 break;

	       default:
		 (*outTxtFile) <<"-";
	       }
	     if((*it)->getClusterSeedType()==kFgtDeadStrip) 
	       stripDead=true;
	     else
	       stripDead=false;
	     for( Int_t timebin = 0; timebin < kFgtNumTimeBins-2; ++timebin )
	       {
		 if(stripDead)
		   (*outTxtFile) << setw(4) << " ?  "<< "    ";
		 else
		   {
		     float numSig=(*it)->getAdc(timebin)/(*it)->getPedErr();
		     if(numSig >1)
		       (*outTxtFile) << setw(4) << (*it)->getAdc(timebin)<< "    ";
		     else
		       (*outTxtFile) << setw(4) << " .  "<< "    ";
		   }
	       }
	     (*outTxtFile) << " : charge: " << (*it)->getCharge()<<" +- " << (*it)->getChargeUncert() <<" location: "<<StFgtGeom::encodeGeoName(disc,quad,layer,strip);
	     (*outTxtFile) <<"/"<<(*it)->getGeoId();
	     (*outTxtFile) << " ped: " << (*it)->getPed() <<" +- " << (*it)->getPedErr();
	     (*outTxtFile) << " run evtNr " << runningEvtNr;
	     //		     (*outTxtFile)  <<" t0: " << (*it)->getFitParamT0() <<" fit chi2/ndf: " << (*it)->getFitChi2();
	     if((*it)->getClusterSeedType()==kFgtSeedType1)
	       {
		 (*outTxtFile) << " ---> seed w/ 3 high strips";
	       }
	     if((*it)->getClusterSeedType()==kFgtSeedType2)
	       (*outTxtFile) << " ---> seed w/ 2 high strips";
	     if((*it)->getClusterSeedType()==kFgtSeedType3)
	       (*outTxtFile) << " ---> seed w/ 1 high strip";

	     if((*it)->getClusterSeedType()==kFgtClusterSeedInSeaOfNoise)
	       (*outTxtFile) <<" ---> seed in too much noise";
	     
	     if((*it)->getClusterSeedType()==kFgtClusterPart)
	       (*outTxtFile) << " ---> Part of cluster";
	     if((*it)->getClusterSeedType()==kFgtDeadStrip) 
	       (*outTxtFile) <<" ---> Strip is marked dead";
	     if((*it)->getClusterSeedType()==kFgtClusterEndUp) 
	       (*outTxtFile) <<" ---> End of a cluster";
	     if((*it)->getClusterSeedType()==kFgtClusterEndDown) 
	       (*outTxtFile) <<" ---> Beginning of a cluster";
	     (*outTxtFile) <<endl;
	   }

     cout <<"trying to get clusters in disc " << iDx << endl;
     StFgtHitCollection* clusterCol=mFgtCollectionPtr->getHitCollection(iDx);
     
     if(clusterCol)
       {

	 const StSPtrVecFgtHit &hitVec=clusterCol->getHitVec();
	 cout <<"got collection, looking at " << hitVec.size() << " hits ..  " <<endl;
	 StSPtrVecFgtHitConstIterator hitIter;
	 for(hitIter=hitVec.begin();hitIter != hitVec.end();hitIter++)
	   {
	     Int_t iq=(*hitIter)->getQuad();
	     Float_t phi=(*hitIter)->getPositionPhi();
	     Float_t r=(*hitIter)->getPositionR();
	     Int_t geoId=(*hitIter)->getCentralStripGeoId();
	     Float_t charge=(*hitIter)->charge();
	     Float_t chargeErr=(*hitIter)->getChargeUncert();
	     Int_t numStrips=(*hitIter)->getStripWeightMap().size();
	     ///check if at least one of the strips has a valid seed)
	     Short_t quad, disc, strip;
	     Char_t layer; 

	     Bool_t containsSeed=false;

	     for(stripWeightMap_t::iterator it=(*hitIter)->getStripWeightMap().begin();it!=(*hitIter)->getStripWeightMap().end();it++)
	       {
		 //		 if(it->first->getClusterSeedType()==kFgtSeedType1 || it->first->getClusterSeedType()==kFgtSeedType2  || it->first->getClusterSeedType()==kFgtSeedType3) //require 2 or 3 strips
		 //		 if(it->first->getClusterSeedType()==kFgtSeedType1 || it->first->getClusterSeedType()==kFgtSeedType2) //require  3 strips
		 if(it->first->getClusterSeedType()==kFgtSeedType1 || it->first->getClusterSeedType()==kFgtSeedType2 ||it->first->getClusterSeedType()==kFgtSeedType3) //require  1,2,3 strips
		   {
		     containsSeed=true;
		     ///use as charge the seed strip, seems to lead to better correlation:
		     charge=it->first->getCharge();
		   }
	       }
	     if(containsSeed)
	       {
		 //		 (*outTxtFile) <<" looking at Cluster for geoId: " << (*hitIter)->getCentralStripGeoId() << " layer: " << (*hitIter)->getLayer() <<endl;
		 for(stripWeightMap_t::iterator it=(*hitIter)->getStripWeightMap().begin();it!=(*hitIter)->getStripWeightMap().end();it++)
		   {
		     StFgtGeom::decodeGeoId(it->first->getGeoId(),disc, quad, layer, strip);
		     for( Int_t timebin = 0; timebin < kFgtNumTimeBins-2; ++timebin )
		       {
			 float numSig=it->first->getAdc(timebin)/it->first->getPedErr();
			 //			 if(numSig >1)
			   //			   (*outTxtFile) << setw(4) << it->first->getAdc(timebin)<< "    ";
			 //			 else
			 //			   (*outTxtFile) << setw(4) << " .  "<< "    ";
		       }
		     
		     //		     (*outTxtFile) << " ::: charge: " << it->first->getCharge()<<" +- " << it->first->getChargeUncert() <<" location: "<<StFgtGeom::encodeGeoName(disc,quad,layer,strip);
		     //		     (*outTxtFile) << " ped: " << it->first->getPed() <<" +- " << it->first->getPedErr();
		     //		     (*outTxtFile) << " running evtNo " << runningEvtNr;
		     //		     (*outTxtFile)  <<" t0: " << it->first->getFitParamT0() <<" fit chi2/ndf: " << it->first->getFitChi2();
		     if(it->first->getClusterSeedType()==kFgtSeedType1)
		   {
		     //		     (*outTxtFile) << " ---> seed with 3 high strips";
		   }
		 /*		 if(it->first->getClusterSeedType()==kFgtSeedType2)
		   (*outTxtFile) << " ---> seed with 2 high strips";
		 if(it->first->getClusterSeedType()==kFgtSeedType3)
		   (*outTxtFile) << " ---> seed with 1 high strip";

		 if(it->first->getClusterSeedType()==kFgtClusterPart)
		 (*outTxtFile) << " ---> part of cluster";
		 (*outTxtFile) <<endl;*/
		   }
		 //		 (*outTxtFile) <<" -----------------------cluster charge: " << charge<<" -----------------------" <<endl; 
		 //	     	     (*outTxtFile) <<" -----------------------end of cluster -----------------------" <<endl; 

		     if(!containsSeed)
		       {
			 //			 (*outTxtFile) << " no seed pulse " << endl;
			 continue;///go to next cluster
		       }

		     //		     (*outTxtFile) <<" found seed" <<endl;
	       
	     if((*hitIter)->getLayer()=='R')
	       {
		 vR[iq].push_back((*hitIter)->getPositionR());
		 vRCharge[iq].push_back(charge);
	       }
	     else
	       {
		 vPhi[iq].push_back((*hitIter)->getPositionPhi());
		 vPhiCharge[iq].push_back(charge);
	       }

	     Short_t tDisc, tQuad,tStrip;
	     Char_t tLayer;
	     //get strip id from the geo id
	     StFgtGeom::decodeGeoId(geoId,tDisc,tQuad,tLayer,tStrip);     
	     //	     cout <<"filling charge with phi: " << phi << " r : " << r << " charge: " << charge <<" numStrips: " << numStrips<< " strip nr: " << tStrip<<endl;
	     //	     cout<<"disc nr: " << iDx <<" quad: " << iq << " numquads: " << kFgtNumQuads <<endl;
	     hCChargePosSpacePhi[iDx*kFgtNumQuads+iq]->Fill(phi,charge);
	     hCChargePosSpaceR[iDx*kFgtNumQuads+iq]->Fill(r,charge);
	     hClusSizePhi[iDx*kFgtNumQuads+iq]->Fill(phi,numStrips);
	     hClusSizePhi[iDx*kFgtNumQuads+iq]->Fill(phi,numStrips);
	     hClusSizeR[iDx*kFgtNumQuads+iq]->Fill(r,numStrips);
	     hCChargeElecSpace[iDx*kFgtNumQuads+iq]->Fill(tStrip,charge);
	     hClusSizeElecSpace[iDx*kFgtNumQuads+iq]->Fill(tStrip,numStrips);
	       }
	   }

	 for(int iQ=0;iQ<4;iQ++)
	   {
	 
	     for(vector<float>::iterator itR=vR[iQ].begin();itR!=vR[iQ].end();itR++)
	       {
		 for(vector<float>::iterator itP=vPhi[iQ].begin();itP!=vPhi[iQ].end();itP++)
		   {
		     x=(*itR)*cos(*itP);
		     y=(*itR)*sin(*itP);
		     radioPlots[iDx]->Fill(x,y);
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
	   }
	 
       }
   }
 
   runningEvtNr++; 
 return ierr;
};
 
 StFgtClusterPlotter::StFgtClusterPlotter( const Char_t* name): StMaker( name ),runningEvtNr(0)
{


};

StFgtClusterPlotter::~StFgtClusterPlotter()
{
  //delete histogram arrays
};


Int_t StFgtClusterPlotter::Finish(){
  outTxtFile->close();
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
  cChargeElecSpace->SetLogz();
  cChargeElecSpace->Divide(kFgtNumDiscs,kFgtNumQuads);
  TCanvas* cClusSizeElecSpace=new TCanvas("clusSizeElecSpace","clusSizeEledId",850,1100);
  TCanvas* cClusSizeElecSpaceBig=new TCanvas("clusSizeElecSpaceBig","clusSizeEledIdBig",850,1100);
  cClusSizeElecSpace->Divide(kFgtNumDiscs,kFgtNumQuads);

  TCanvas* cRadio=new TCanvas("radioPlots","radioPlot",1000,1500);
  cRadio->Divide(2,3); //6 discs

  TCanvas* cCorr=new TCanvas("correlationPlots","correlationPlot",1000,1500);
  TCanvas* cCorrBig=new TCanvas("correlationPlotsBig","correlationPlotBig",1000,1500);
  cCorr->Divide(kFgtNumDiscs,kFgtNumQuads);

  for(Int_t iD=0;iD<kFgtNumDiscs;iD++)
   {
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

  cCorr->SaveAs("corrPlots.png");
  cCorr->SaveAs("corrPlots.pdf");

  myRootFile->Write();
  myRootFile->Close();

   return ierr;
};


/**
construct histograms

*/
Int_t StFgtClusterPlotter::Init(){

  myRootFile=new TFile("clusterPlotter.root","RECREATE");
  //  outTxtFile=new ofstream;
  //  outTxtFile->open("clusters.txt");

  outTxtFileP=new ofstream;
  outTxtFileP->open("clustersP.txt");
  outTxtFileR=new ofstream;
  outTxtFileR->open("clustersR.txt");


   Int_t ierr = kStOk;
   hClusterCharge=new TH1D("clusterCharge","clusterCharge",100, 0, 1000);
   char buffer[100];
   hCChargePosSpacePhi=new TH2D*[kFgtNumDiscs*kFgtNumQuads];
   hCChargePosSpaceR=new TH2D*[kFgtNumDiscs*kFgtNumQuads];
   hClusSizePhi=new TH2D*[kFgtNumDiscs*kFgtNumQuads];
   hClusSizeR=new TH2D*[kFgtNumDiscs*kFgtNumQuads];
   hCChargeElecSpace=new TH2D*[kFgtNumDiscs*kFgtNumQuads];
   hClusSizeElecSpace=new TH2D*[kFgtNumDiscs*kFgtNumQuads];

   radioPlots=new TH2D*[kFgtNumDiscs];
   corrPlots=new TH2D*[kFgtNumDiscs*kFgtNumQuads];

   trkRadioPlots=new TH2D*[kFgtNumDiscs];
   trkPhiProj=new TH1D*[kFgtNumDiscs];



   for(int iD=0;iD<kFgtNumDiscs;iD++)
     {
       sprintf(buffer,"radioDisk%d_",iD);
       radioPlots[iD]=new TH2D(buffer,buffer,100,-50,50,100,-50,50);
       sprintf(buffer,"trkRadioDisk%d_",iD);
       trkRadioPlots[iD]=new TH2D(buffer,buffer,100,-50,50,100,-50,50);
       sprintf(buffer,"trkPhiDisk%d_",iD);
       trkPhiProj[iD]=new TH1D(buffer,buffer,100,-3.5,3.5);

       for(int iQ=0;iQ<kFgtNumQuads;iQ++)
	 {
	   sprintf(buffer,"clusterChargeDisk%d_Quad%d_phi",iD,iQ);
	   hCChargePosSpacePhi[iD*kFgtNumQuads+iQ]=new TH2D(buffer,buffer,200,-4,4, 200, 0, 2000);
	   sprintf(buffer,"clusterChargeDisk%d_Quad%d_R",iD,iQ);
	   hCChargePosSpaceR[iD*kFgtNumQuads+iQ]=new TH2D(buffer,buffer,200,10,35, 200, 0, 2000);
	   sprintf(buffer,"clusterSizeDisk%d_Quad%d_phi",iD,iQ);
	   hClusSizePhi[iD*kFgtNumQuads+iQ]=new TH2D(buffer,buffer,1000,-4,4, 20, 0, 20);
	   sprintf(buffer,"clusterSizeDisk%d_Quad%d_R",iD,iQ);
	   hClusSizeR[iD*kFgtNumQuads+iQ]=new TH2D(buffer,buffer,1000,10,35, 20, 0, 20);
	   sprintf(buffer,"clusterSizeDisk%d_Quad%d_ElecSpace",iD,iQ);
	   hClusSizeElecSpace[iD*kFgtNumQuads+iQ]=new TH2D(buffer,buffer,100,0,1000, 20, 0, 20);
	   sprintf(buffer,"clusterChargeDisk%d_Quad%d_ElecSpace",iD,iQ);
	   hCChargeElecSpace[iD*kFgtNumQuads+iQ]=new TH2D(buffer,buffer,100,0,1000, 100, 0, 2000);
	   sprintf(buffer,"radioDisk%d_Quad_%d",iD,iQ);

	   sprintf(buffer,"r_phi_ChargeCorr%d_Quad_%d",iD,iQ);
	   corrPlots[iD*kFgtNumQuads+iQ]=new TH2D(buffer,buffer,100,0,2000,100,0,2000);
	 }
     }
   return ierr;
};
ClassImp(StFgtClusterPlotter);
