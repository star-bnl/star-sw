///////estimate efficiencies of disks using straight line
#include "StFgtAVEfficiencyMaker.h"
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
//Double_t StFgtAVEfficiencyMaker::getRPhiRatio()
//{

//};
Double_t getRPhiRatio(StSPtrVecFgtHitConstIterator hitIterBegin, StSPtrVecFgtHitConstIterator hitIterEnd)
{
  Short_t quad, disc, strip;
  Char_t layer; 
  Bool_t stripDead=false;
  Int_t numR=0;
  Int_t numPhi=0;
  
  StSPtrVecFgtHitConstIterator hitIter=hitIterBegin;
  for(;hitIter!=hitIterEnd;hitIter++)
    {
      StFgtGeom::decodeGeoId((*hitIter)->getCentralStripGeoId(),disc, quad, layer, strip);
      if(layer=='R')
	numR++;
      else
	numPhi++;
    }

  if(numR+numPhi>0)
    return (numR-numPhi)/((Double_t)(numR+numPhi));
  else
    return -1;
};

/** grab the cluster container, fill histograms

*/
Int_t StFgtAVEfficiencyMaker::Make()
{
  //  cout <<" in eff make " <<endl;
  StEvent* eventPtr = 0;
  eventPtr = (StEvent*)GetInputDS("StEvent");

  Int_t ierr = StFgtQaMaker::Make();
  Float_t x;
  Float_t y;
  Int_t prvGeoId=-1;

  StFgtHitCollection* clusterColD1=mFgtCollectionPtr->getHitCollection(0);
  StFgtHitCollection* clusterColD6=mFgtCollectionPtr->getHitCollection(5);

  //look at the r phi ratio for each disk
  for(int i=0;i<6;i++)
    {
      StFgtHitCollection* tmpClusterCol=mFgtCollectionPtr->getHitCollection(i);
      Double_t ratio=getRPhiRatio(tmpClusterCol->getHitVec().begin(),tmpClusterCol->getHitVec().end());
      rPhiRatioPlots[i]->Fill(ratio);
      //      cout << "ratio for disk: " << i << " is " << ratio <<" disk has: " << tmpClusterCol->getHitVec().size() << "hits" <<endl;
    }



  const StSPtrVecFgtHit &hitVecD1=clusterColD1->getHitVec();

  const StSPtrVecFgtHit &hitVecD6=clusterColD6->getHitVec();
  Double_t D1Pos=StFgtGeom::getDiscZ(0);
  Double_t D6Pos=StFgtGeom::getDiscZ(5);
  Double_t zArm=D6Pos-D1Pos;
  StSPtrVecFgtHitConstIterator hitIterD1,hitIterD6, hitIterD1R, hitIterD6R, hitIter, hitIter2;
  for(hitIterD1=hitVecD1.begin();hitIterD1 != hitVecD1.end();hitIterD1++)
    {
      Short_t quad, disc, strip;
      Char_t layer; 
      Bool_t stripDead=false;
      StFgtGeom::decodeGeoId((*hitIterD1)->getCentralStripGeoId(),disc, quad, layer, strip);
      /*      if(layer=='P')
	cout <<"found phi d1 " <<endl;
      else
      cout <<"found r d1 " <<endl;*/
    }

  for(hitIterD1=hitVecD6.begin();hitIterD1 != hitVecD6.end();hitIterD1++)
    {
      Short_t quad, disc, strip;
      Char_t layer; 
      Bool_t stripDead=false;
      StFgtGeom::decodeGeoId((*hitIterD1)->getCentralStripGeoId(),disc, quad, layer, strip);
      /*      if(layer=='P')
	cout <<"found phi d6 " <<endl;
      else
      cout <<"found r d6 " <<endl;*/
    }


  //  cout <<"looking at " << hitVecD1.size() << " hits in D1 and " << hitVecD6.size() << " in D6 " <<endl;
  for(hitIterD1=hitVecD1.begin();hitIterD1 != hitVecD1.end();hitIterD1++)
    {
      Short_t quad, disc, strip;
      Char_t layer; 
      Bool_t stripDead=false;
      StFgtGeom::decodeGeoId((*hitIterD1)->getCentralStripGeoId(),disc, quad, layer, strip);
      ////use only hits in the half disk that is the same in all disks!!!
      if(quad>2)
	continue;

      //do 1D 'fit' with r strips and the (x,y) thing
      Int_t geoIdD1=(*hitIterD1)->getCentralStripGeoId();
      StFgtGeom::decodeGeoId(geoIdD1,disc, quad, layer, strip);
      if(layer!='P')
	continue;

      Float_t phiD1=(*hitIterD1)->getPositionPhi();
      for(hitIterD1R=hitVecD1.begin();hitIterD1R != hitVecD1.end();hitIterD1R++)
	{
	  Int_t geoIdR=(*hitIterD1R)->getCentralStripGeoId();
	  StFgtGeom::decodeGeoId(geoIdR,disc, quad, layer, strip);
	  if(layer!='R')
	    continue;


	  Float_t rD1=(*hitIterD1R)->getPositionR();
	  Float_t xD1=rD1*cos(phiD1);
	  Float_t yD1=rD1*sin(phiD1);
	  for(hitIterD6=hitVecD6.begin();hitIterD6 != hitVecD6.end();hitIterD6++)
	    {
	      Int_t geoIdD6=(*hitIterD6)->getCentralStripGeoId();
	      StFgtGeom::decodeGeoId(geoIdD6,disc, quad, layer, strip);
	      if(layer!='P')
		continue;
	      Float_t phiD6=(*hitIterD6)->getPositionPhi();
	      for(hitIterD6R=hitVecD6.begin();hitIterD6R != hitVecD6.end();hitIterD6R++)
		{
		  Int_t geoIdR=(*hitIterD6R)->getCentralStripGeoId();
		  StFgtGeom::decodeGeoId(geoIdR,disc, quad, layer, strip);
		  if(layer!='R')
		    continue;

		  Float_t rD6=(*hitIterD6R)->getPositionR();
		  Double_t xD6=rD6*cos(phiD6);
		  Double_t yD6=rD6*sin(phiD6);

		  ///for each combination in d1,d6
		  vector< pair<Int_t,Double_t> > v_x;
		  vector< pair<Int_t,Double_t> > v_y;
		  vector< pair<Int_t,Double_t> > v_r;

		  vector< pair<Int_t,Double_t> > v_xFail;
		  vector< pair<Int_t,Double_t> > v_yFail;
		  vector< pair<Int_t,Double_t> > v_rFail;

		  int iFound=0;
		  int iFoundR=0;

		  for(int iD=1;iD<5;iD++)
		    {
		      Bool_t found=false;
		      Bool_t foundR=false;
		      //check for hit
		      Double_t diskZ=StFgtGeom::getDiscZ(iD);
		      //expected

		      Double_t xPosExp=xD1+(xD6-xD1)*(diskZ-D1Pos)/zArm;
		      Double_t yPosExp=yD1+(yD6-yD1)*(diskZ-D1Pos)/zArm;
		      Double_t rPosExp=rD1+(rD6-rD1)*(diskZ-D1Pos)/zArm;
		      //		      cout <<"x1: " << xD1 << " y1: " << yD1 <<" x6: " << xD6 <<" y6: " << yD6 << " zpos: " << diskZ <<" arm: " << zArm<<endl;
		      //		      cout <<"expect hit at : " << xPosExp <<" / " << yPosExp <<" r: " << rPosExp <<endl;
		      StFgtHitCollection* clusterCol=mFgtCollectionPtr->getHitCollection(iD);
		      const StSPtrVecFgtHit &hitVec=clusterCol->getHitVec();
	      
		      for(hitIter=hitVec.begin();hitIter!=hitVec.end();hitIter++)
			{
			  //do 1D 'fit' with r strips and the (x,y) thing
			  Int_t geoIdPhi=(*hitIter)->getCentralStripGeoId();
			  StFgtGeom::decodeGeoId(geoIdPhi,disc, quad, layer, strip);
			  if(layer!='P')
			    continue;
			  Float_t phi=(*hitIter)->getPositionPhi();
			  for(hitIter2=hitVec.begin();hitIter2!=hitVec.end();hitIter2++)
			    {
			      Int_t geoIdR=(*hitIter2)->getCentralStripGeoId();
			      StFgtGeom::decodeGeoId(geoIdR,disc, quad, layer, strip);
			      if(layer!='R')
				continue;
			      Float_t r=(*hitIter2)->getPositionR();
			      //			      cout <<"checking with r:" << r <<endl;
			      if(fabs(r-rPosExp)<1)
				{
				  foundR=true;
				  //				  cout <<"found r: " << r  <<endl;
				  v_r.push_back(pair<Int_t,Double_t> (iD,r));
				}

			      x=r*cos(phi);
			      y=r*sin(phi);
			      //			      cout <<"checking with x: " << x << " y: " << y <<endl;
			      if(fabs(x-xPosExp) < 1 && fabs(y-yPosExp)<1) //found hit
				{
				  found=true;
				  cout <<"found! " <<" pushing back: iD: " << iD << "x: " << x << " y "<< y  <<endl;
				  v_x.push_back(pair<Int_t,Double_t>(iD,x));
				  v_y.push_back(pair<Int_t,Double_t>(iD,y));
				}
			    }
			}
		      //only one per disk
		      if(found)
			iFound++;
		      else
			{
			  //			  cout <<"failed to find, pushing back " << xPosExp << " y: " << yPosExp <<endl;
			  v_xFail.push_back(pair<Int_t,Double_t>(iD,xPosExp));
			  v_yFail.push_back(pair<Int_t,Double_t>(iD,yPosExp));
			}
		      if(foundR)
			iFoundR++;
		      else
			{
			  //			  cout <<"failed to find r " << rPosExp<<endl;
			  v_rFail.push_back(pair<Int_t, Double_t>(iD,rPosExp));
			}

		    }

		  //		  cout << " Ifound: " << iFound <<endl;
		  if(iFound>=2) //at least one hit plus seed
		    {
		      if(v_x.size()>iFound)
			{
			  cout<<"more hits than disks hit!!!" <<endl;
			}
		      else
			{
			  for(int i=0;i<v_x.size();i++)
			    {
			      Int_t disk=v_x[i].first;
			      Double_t x=v_x[i].second;
			      Double_t y=v_y[i].second;
			      cout <<"filling disk: " << disk <<" with: " << x <<" / " <<y <<endl;
			      radioPlotsEff[disk]->Fill(x,y);
			    }
			  for(int i=0;i<v_xFail.size();i++)
			    {
			      Int_t disk=v_xFail[i].first;
			      Double_t x=v_xFail[i].second;
			      Double_t y=v_yFail[i].second;
			      //			      cout <<"filling disk fail: " << disk <<" with: " << x <<" / " <<y <<endl;
			      radioPlotsNonEff[disk]->Fill(x,y);
			    }
			  hitCounter++;
			}
		    }

		  if(iFoundR>=1) //at least one hit plus seed
		    {
		      if(v_r.size()>iFound)
			{
			  //			  cout<<"more r hits than disks hit!!!" <<endl;
			}
		      else
			{
			  for(int i=0;i<v_r.size();i++)
			    {
			      Int_t disk=v_r[i].first;
			      Double_t r=v_r[i].second;
			      //			      cout <<"filling  r disk: " << disk <<" with: " << r <<endl;
			      rEff[disk]->Fill(r);
			    }
			  for(int i=0;i<v_rFail.size();i++)
			    {
			      Int_t disk=v_rFail[i].first;
			      Double_t r=v_rFail[i].second;
			      //			      cout <<"filling r disk fail: " << disk <<" with: " << r <<endl;
			      rNonEff[disk]->Fill(r);
			    }
			  hitCounterR++;
			}
		    }


		  //start over
		  iFound=0;
		  iFoundR=0;
		  v_x.clear();
		  v_y.clear();
		  v_r.clear();
		  v_xFail.clear();
		  v_yFail.clear();
		  v_rFail.clear();
	 
		}
	    }
	}
    }
return ierr;

};
 
StFgtAVEfficiencyMaker::StFgtAVEfficiencyMaker( const Char_t* name):runningEvtNr(0),hitCounter(0),hitCounterR(0)
{
  cout <<"AVE constructor!!" <<endl;
  StFgtQaMaker( name, 0,0, "qName" );

};

StFgtAVEfficiencyMaker::~StFgtAVEfficiencyMaker()
{

  //delete histogram arrays
};


Int_t StFgtAVEfficiencyMaker::Finish(){
  gStyle->SetPalette(1);
  cout <<"cluster plotter finish funciton " <<endl;
  Int_t ierr = kStOk;

  TCanvas* cRadio=new TCanvas("radioPlots","radioPlot",1000,1500);
  TCanvas* cRadioHits=new TCanvas("radioPlotsHits","radioPlotHits",1000,1500);
  TCanvas* cRadioNonHits=new TCanvas("radioPlotsNonHits","radioPlotNonHits",1000,1500);
  cRadio->Divide(2,3); //6 discs
  cRadioHits->Divide(2,3); //6 discs
  cRadioNonHits->Divide(2,3); //6 discs
  TCanvas* cRPRatio=new TCanvas("rPhiRatio","rPhiRatios",1000,1500);
  cRPRatio->Divide(2,3); //6 discs

  TCanvas* cREff=new TCanvas("crEff","crEff",1000,1500);

  cREff->Divide(2,3); //6 discs

  for(Int_t iD=0;iD<kFgtNumDiscs;iD++)
    {
      //      cRadio->cd(iD+1)->SetLogz();
      cRadioHits->cd(iD+1);
      radioPlotsEff[iD]->Draw("colz");
      cRadioNonHits->cd(iD+1);
      radioPlotsNonEff[iD]->Draw("colz");

    }
  cRadioHits->SaveAs("radioPlotsHits.png");
  cRadioNonHits->SaveAs("radioPlotsNonHits.png");

  cout <<"saving .." <<endl;

  for(Int_t iD=0;iD<kFgtNumDiscs;iD++)
    {
      cRadio->cd(iD+1);

      TH2D* tmpAllCounts=(TH2D*)radioPlotsEff[iD]->Clone("tmp");
      radioPlotsEff[iD]->Add(radioPlotsNonEff[iD]);//all counts
      //      radioPlotsEff[iD]->Add(radioPlotsNonEff[iD],-1); //subtract non eff
      for(int nx=0;nx<radioPlotsEff[iD]->GetNbinsX();nx++)
	{
	  for(int ny=0;ny<radioPlotsEff[iD]->GetNbinsY();ny++)
	     {
	       Double_t denom=radioPlotsEff[iD]->GetBinContent(nx,ny);
	       if(denom>0)
		 radioPlotsEff[iD]->SetBinContent(nx,ny,tmpAllCounts->GetBinContent(nx,ny)/denom);
	       else
		 radioPlotsEff[iD]->SetBinContent(nx,ny,0.0);
	     }
	}
      //      radioPlotsEff[iD]->Divide(tmpAllCounts);
      radioPlotsEff[iD]->Draw("colz");
      cRPRatio->cd(iD+1);
      rPhiRatioPlots[iD]->Draw();
      cREff->cd(iD+1);

      TH1D* tmpR=(TH1D*)rEff[iD]->Clone("tmpR");
      rEff[iD]->Add(rNonEff[iD]);
      for(int nx=0;nx<rEff[iD]->GetNbinsX();nx++)
	{
	  Double_t denom=rEff[iD]->GetBinContent(nx);
	  if(denom>0)
	    rEff[iD]->SetBinContent(nx,tmpR->GetBinContent(nx)/denom);
	  else
	    rEff[iD]->SetBinContent(nx,0.0);
	}
      rEff[iD]->Draw();
    }
  cRadio->SaveAs("radioPlotsEff.png");
  cRadio->SaveAs("radioPlotsEff.pdf");


  cREff->SaveAs("rEff.png");
  cREff->SaveAs("rEff.pdf");

  cRPRatio->SaveAs("rpRatio.png");
  cRPRatio->SaveAs("rpRatio.pdf");

  cout <<"returning after finish" <<endl;
  return ierr;
};


/**
   construct histograms

*/
Int_t StFgtAVEfficiencyMaker::Init(){
  cout <<"AVE!!" <<endl;
  myRootFile=new TFile("clusterEff.root","RECREATE");
  //  outTxtFile=new ofstream;
  //  outTxtFile->open("clusters.txt");


   Int_t ierr = kStOk;

   char buffer[100];

   radioPlotsEff=new TH2D*[kFgtNumDiscs];
   radioPlotsNonEff=new TH2D*[kFgtNumDiscs];
   rPhiRatioPlots=new TH1D*[kFgtNumDiscs];
   rEff=new TH1D*[kFgtNumDiscs];
   rNonEff=new TH1D*[kFgtNumDiscs];

   for(int iD=0;iD<kFgtNumDiscs;iD++)
     {
       cout <<"id: " << iD <<endl;

       sprintf(buffer,"radioDiskEff_%d",iD);
       radioPlotsEff[iD]=new TH2D(buffer,buffer,20,-50,50,20,-50,50);

       cout <<"1" <<endl;
       sprintf(buffer,"rEff_%d",iD);
       rEff[iD]=new TH1D(buffer,buffer,100,0,50);
       sprintf(buffer,"rNonEff_%d",iD);
       rNonEff[iD]=new TH1D(buffer,buffer,100,0,50);
       cout <<"2" <<endl;
       for(int nx=0;nx<radioPlotsEff[iD]->GetNbinsX();nx++)
	 {
	   for(int ny=0;ny<radioPlotsEff[iD]->GetNbinsY();ny++)
	     {
	       //	       radioPlotsEff[iD]->SetBinContent(nx,ny,0.1);//so that there is no divide by zero
	     }
	 }
       cout <<"3" <<endl;
       sprintf(buffer,"radioDiskNonEff_%d",iD);
       radioPlotsNonEff[iD]=new TH2D(buffer,buffer,20,-50,50,20,-50,50);
       for(int nx=0;nx<rEff[iD]->GetNbinsX();nx++)
	 {
	   //	   rEff[iD]->SetBinContent(nx,0.1);
	 }
       cout <<"4" <<endl;
       sprintf(buffer,"rPhiRatio_%d",iD);
       rPhiRatioPlots[iD]=new TH1D(buffer,buffer,100,-2,10);
     }
   return ierr;
};
ClassImp(StFgtAVEfficiencyMaker);
