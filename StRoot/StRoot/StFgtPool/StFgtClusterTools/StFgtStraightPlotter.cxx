///////estimate efficiencies of disks using straight line
#include "StFgtStraightPlotter.h"
#include "StFgtGeneralBase.h"
#include "StRoot/StEvent/StFgtCollection.h"
#include "StRoot/StEvent/StFgtHitCollection.h"
#include "StRoot/StEvent/StFgtHit.h"
#include "StRoot/StFgtUtil/geometry/StFgtGeom.h"
#include "StRoot/StEvent/StEvent.h"
#include "StRoot/StEvent/StEventInfo.h"
#include "StRoot/StFgtUtil/geometry/StFgtGeom.h"
#include "TMath.h"

#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuPrimaryVertex.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StarClassLibrary/StThreeVectorF.hh"
#include <cstdlib>

#include <TH2D.h>
#include <TROOT.h>
#include <TStyle.h>
#include <TCanvas.h>
#include <utility>
#include <TArc.h>
#include <TLine.h>
#include <set>

//#define PRINT_1D
//max num clusters any disk is allowed to have

#include "StFgtCosmicAlignment.h"

#define MAX_DIST_STRIP_R 0.7
#define MAX_DIST_STRIP_PHI 0.03
#include "StRoot/StFgtUtil/geometry/StFgtGeom.h"
//#define LEN_CONDITION
#define PULSE_CONDITION
#define DO_PRINT


#include "StRoot/StEvent/StEvent.h"
#include "StRoot/StEvent/StFgtCollection.h"
//disk for which I want to calculate efficieny


#define MAX_CHARGE_RATIO
#define MIN_CHARGE_RATIO

//#define DISK_EFF 2
#define QUAD_EFF 1
#define MY_PI 3.14159265359



#define MAX_DIST2_EFF 1.0
#define MAX_DIST2 1.0

#define MIN_NUM_POINTS 2
#define DISK_DIM 40

#define NUM_EFF_BIN 30

Bool_t StFgtStraightPlotter::arePointsMatched(vector<generalCluster>::iterator  c1, vector<generalCluster>::iterator  c2)
{
  Float_t tUncert1;
  Float_t tCharge1;

  Float_t tUncert2;
  Float_t tCharge2;

  if(c1->clusterCharge<20 || c2->clusterCharge<20)
    return false;
  if(c1->clusterCharge<c2->clusterCharge)
    {
      tCharge1=c2->clusterCharge;
      tUncert1=c2->clusterChargeUncert;
      tCharge2=c1->clusterCharge;
      tUncert2=c1->clusterChargeUncert;
    }
  else
    {
      tCharge1=c1->clusterCharge;
      tUncert1=c1->clusterChargeUncert;
      tCharge2=c2->clusterCharge;
      tUncert2=c2->clusterChargeUncert;
    }


  if(((tCharge1/tCharge2) <chargeMatchCut) ||((tCharge1-tUncert1)/(tCharge2+tUncert2))<chargeMatchCut)
    return true;
  else 
    return false;
}


Bool_t StFgtStraightPlotter::validPulse(generalStrip& strip)
{
  for(int i=0;i<4;i++)
    {
      Float_t adc1=strip.adc[i];
      Float_t adc2=strip.adc[i+1];
      Float_t adc3=strip.adc[i+2];
      Float_t cut=5*strip.pedErr;
      if(adc1>cut && adc2 >cut && adc3 > cut)
	{
	  if(adc1 <adc2 && adc2 < adc3)
	    return true;
	}
    }
  return false;
}

Bool_t StFgtStraightPlotter::fitTheStrip(generalStrip* pStrip, generalStrip* pStripOtherLayer,float* amp, float* t0, float* chi2Ndf, int iD, int iq, int apvBin, Char_t layer)
{
  if(fitCounter>2000)
    return true;
  fitCounter++;
  char buffer[100];
  sprintf(buffer,"d%d_quad%d",iD,iq);
  pulsePictureFile->cd();
  //  cout <<"trying to cd to " << buffer <<endl;
  pulsePictureFile->cd(buffer);
  //  cout <<"done " <<endl;
  if(layer=='R')
    sprintf(buffer,"apv%d_R",apvBin);
  else
    sprintf(buffer,"apv%d_P",apvBin);
  //  cout <<"changing to " << buffer <<endl;
  //  cout <<" and now to " << buffer <<endl;
  gDirectory->cd(buffer);
  //  cout  << "done cd 2 " << endl;
  Int_t minAdcCount=100000;
  Int_t maxAdcCount=0;
  for(Int_t tb=0;tb<7;tb++)
    {
      mHistPtr->SetBinContent(tb+1,0);
      mHistPtr->SetBinError(tb+1,10000);
      mHistPtr->SetBinContent(tb+1, pStrip->adc[tb]);
      if(pStrip->adc[tb]<minAdcCount)
	minAdcCount=pStrip->adc[tb]-pStrip->pedErr;
      if(pStrip->adc[tb]>maxAdcCount)
	maxAdcCount=pStrip->adc[tb]+pStrip->pedErr;
      mHistPtr->SetBinError(tb+1,pStrip->pedErr);
    }

  //  mHistPtr->Fit(mPulseShapePtr);
  (*amp)=mPulseShapePtr->GetParameter(0);
  (*t0)=mPulseShapePtr->GetParameter(4);
  (*chi2Ndf)=mPulseShapePtr->GetChisquare()/mPulseShapePtr->GetNDF();
  sprintf(buffer,"pulse histo_D%d_Q%d_APV%d_ev%d",iD,iq,apvBin,evtNr);
  //  cout <<"histo name: "<< buffer <<endl;
  TH1F* tmpPulseHisto=(TH1F*)mHistPtr->Clone(buffer);
  tmpPulseHisto->Write();

  if(pStripOtherLayer!=0)
    {
      mCanvas->cd();

      sprintf(buffer,"tmpCnvsD%d_Q%d_APV%d_ev%d",iD,iq,apvBin,evtNr);
      TCanvas* tmpCnv=(TCanvas*)mCanvas->Clone(buffer);

      tmpCnv->SetTitle(buffer);
      tmpCnv->SetName(buffer);

      tmpCnv->cd();

      for(Int_t tb=0;tb<7;tb++)
	{
	  mHistPtr2->SetBinContent(tb+1,0);
	  mHistPtr2->SetBinError(tb+1,10000);
	  mHistPtr2->SetBinContent(tb+1, pStripOtherLayer->adc[tb]);
	  mHistPtr2->SetBinError(tb+1,pStripOtherLayer->pedErr);
	  if(pStripOtherLayer->adc[tb]<minAdcCount)
	    minAdcCount=pStripOtherLayer->adc[tb]-pStripOtherLayer->pedErr;
	  if(pStripOtherLayer->adc[tb]>maxAdcCount)
	    maxAdcCount=pStripOtherLayer->adc[tb]+pStripOtherLayer->pedErr;
	}

      tmpPulseHisto->GetYaxis()->SetRangeUser(minAdcCount-10,maxAdcCount+10);
      mHistPtr2->GetYaxis()->SetRangeUser(minAdcCount-10,maxAdcCount+10);
      tmpPulseHisto->Draw();
      mHistPtr2->Draw("SAME");
      tmpCnv->Write();
    }

  pulsePictureFile->cd();

  myRootFile->cd();

  return true;
}


template<class T> void StFgtStraightPlotter::createPlots(T*** pH, int numH, const char* nameBase, int numBin, int first, int last)
{

  char buffer[200];
  char bufferNB[200];
  sprintf(bufferNB,"%s_%s",nameBase,mFileName);
  (*pH)=new T*[numH];
  
  if (numH==22)
    {
      for(int iD=0;iD<numH;iD++)
        {  
	  sprintf(buffer, "%s_APV%d", bufferNB, iD);
	  (*pH)[iD]=new T(buffer,buffer,numBin, first, last);
	}
    }
  
  if ((numH == kFgtNumDiscs*4)&&(numH!=22))
    //if ((numH == kFgtNumDiscs*4))
    {
      for(int iD=0;iD<kFgtNumDiscs;iD++)
        {
          for(int iQ=0;iQ<4;iQ++)
            {
              sprintf(buffer,"%s_disc%d_quad%d",bufferNB,iD+1,iQ);
              (*pH)[iD*4+iQ]=new T(buffer,buffer,numBin,first,last);
            }
        }
    }
  else   
    {
      for(int iD=0;iD<kFgtNumDiscs;iD++)
        {

	  if (numH==kFgtNumDiscs)
	    {
	      sprintf(buffer, "%s_disc%d", bufferNB, iD+1);
	      (*pH)[iD]=new T(buffer, buffer,numBin, first, last);
	    }
	  else
	    {
	      for(int binAPVi=0;binAPVi<40;binAPVi++)
		{
		  int iQ = -1;
		  if((binAPVi>= 0) && (binAPVi<= 9)) iQ=0;
		  if((binAPVi>=10) && (binAPVi<=19)) iQ=1;
		  if((binAPVi>=20) && (binAPVi<=29)) iQ=2;
		  if((binAPVi>=30) && (binAPVi<=39)) iQ=3;
		  sprintf(buffer,"%s_disc%d_quad%d_apvBIN%d",bufferNB,iD+1,iQ,binAPVi);
		  (*pH)[iD*40+binAPVi]=new T(buffer,buffer,numBin,first,last);
		}
	    }
	}
    }
}


template void StFgtStraightPlotter::createPlots(TH1I*** pH, int numH,const char* nameBase, int numBin, int first, int last);
template void StFgtStraightPlotter::createPlots(TH1F*** pH, int numH, const char* nameBase, int numBin, int first, int last);
template void StFgtStraightPlotter::createPlots(TH1D*** pH, int numH, const char* nameBase, int numBin, int first, int last);

void StFgtStraightPlotter::doNormalize(TH2D** hEff, TH2D** hNonEff)
{
  for(Int_t iD=0;iD<kFgtNumDiscs;iD++)
    {
      TH2D* tmpAllCounts=(TH2D*)hEff[iD]->Clone("tmp");
      hEff[iD]->Add(hNonEff[iD]);//all counts
      for(int nx=1;nx<hEff[iD]->GetNbinsX()+1;nx++)
	{
	  for(int ny=1;ny<hEff[iD]->GetNbinsY()+1;ny++)
	    {
	      Double_t denom=hEff[iD]->GetBinContent(nx,ny);
	      if(denom>0 && (tmpAllCounts->GetBinContent(nx,ny)/denom)<=1.0)
		{
		  hEff[iD]->SetBinContent(nx,ny,tmpAllCounts->GetBinContent(nx,ny)/denom);
		}
	      else
		{
		  hEff[iD]->SetBinContent(nx,ny,0.0);
		}
	    }
	}
      delete tmpAllCounts;
    }
}



void StFgtStraightPlotter::saveSigs(Double_t* sigR, Double_t* sigP, Double_t r, Double_t phi, Int_t maxR, Int_t maxPhi, Int_t discId, Int_t quad)
{
  Char_t buffer[200];
  int uniqueId=rand() % 1000;
  sprintf(buffer,"Sig_Disc%d_quad%d_Phi_Evt_%d_R_%f_Phi_%f_%d_%s",discId,quad,evtNr,r,phi,uniqueId,mFileName);

  TH2D* histoP=new TH2D(buffer,buffer,7,0,6,maxPhi,0,maxPhi-1);
  for(int i=0;i<maxPhi;i++)
    {
      for(int j=0;j<7;j++)
	{
	  histoP->SetBinContent(j+1,i+1,sigP[i*7+j]);
	  //	  cout <<"P: setting bin : i: " << i << " j: " << j << " index: "<< i*7+j << " sig: " << sigP[i*7+j]<<endl;
	}
    }
  v_hClusP.push_back(histoP);
  sprintf(buffer,"Sig_Disc%d_quad%d_R_Evt_%d_R_%f_Phi_%f_%d_%s",discId,quad,evtNr,r,phi,uniqueId,mFileName);
  //  cout <<"creating histoR: "<< buffer<<endl;
  TH2D* histoR=new TH2D(buffer,buffer,7,0,6,maxR,0,maxR-1);
  for(int i=0;i<maxR;i++)
    {
      for(int j=0;j<7;j++)
	{
	  histoR->SetBinContent(j+1,i+1,sigR[i*7+j]);
	  //	  cout <<"R: setting bin : i: " << i << " j: " << j << " index: "<< i*7+j << " sig: " << sigR[i*7+j]<<endl;
	}
    }
  v_hClusR.push_back(histoR);
}

pair<double,double> StFgtStraightPlotter::getDca(  vector<AVTrack>::iterator it)
{

  float oldDist=100000;
  float zStep=1.0;
  float optZ=-200;
  float dist=0;
  for(float z=-100;z<100;z=z+zStep)
    {
      float x=it->mx*z+it->ax;
      float y=it->my*z+it->ay;
      dist=x*x+y*y;
      if(dist>oldDist)
	{
	  ///we go away
	  dist=oldDist;
	  optZ=z-zStep;//last z was better
	  break;
	}
      else
	oldDist=dist;
    }
  return pair<double,double>(optZ,sqrt(dist));
}


Double_t StFgtStraightPlotter::findClosestStrip(Char_t layer, double ord, Int_t iD, Int_t iQ)
{
  if(iD<0 || iD >5)
    {
      return -99999;
    }
  vector<generalCluster> &hitVec=*(pClusters[iD]);
  Double_t dist=99999;
  for(vector<generalCluster>::iterator it=hitVec.begin();it!=hitVec.end();it++)
    {
      if(useChargeMatch && !it->hasMatch)
	continue;
      if(it->layer!=layer)
	continue;
      Double_t mDist=9999;
      if(it->layer!='R')
	{
	  mDist=fabs(it->posPhi-ord);
	}
      else
	mDist=fabs(it->posR-ord);
      
      if(mDist<dist)
	dist=mDist;
    }
  return dist;
}


///careful here if called for every disk. The function now checks for chargeCorr, cluster size histos if it is you are 
///looking at m_effDisk and only fills then. Assuming that for the other discs you use the points from the found tracks
void StFgtStraightPlotter::fillStripHistos(Float_t r, Float_t phi, Int_t iD, Int_t iq)
{

  //  cout <<"filling strip histos " <<endl;
  bool partOfClusterP=false;
  bool partOfClusterR=false;
  Double_t clusterChargeR=-9999;
  Double_t clusterChargeP=-9999;

  Double_t mClusterSizeP=-9999;
  Double_t mClusterSizeR=-9999;

  Double_t maxRCharge=-9999;
  Double_t maxPhiCharge=-9999;
  Double_t maxRChargeUncert=-9999;
  Double_t maxPhiChargeUncert=-9999;
  Int_t maxRInd=-1;
  Int_t maxPInd=-1;
  Int_t maxRTb=-1;
  Int_t maxPTb=-1;
  Int_t maxRAdc=-1;
  Int_t maxPAdc=-1;
  Double_t maxSigAdcR=-1;
  Double_t maxSigAdcP=-1;
  Int_t numFSigP=0;
  Int_t numFSigR=0;
  Int_t firstFSigTbP=0;
  Int_t firstFSigTbR=0;
  
  Float_t secondToLastRatioP=0;
  Float_t secondToLastRatioR=0;
  
  Float_t firstTbSigR=-1;
  Float_t firstTbSigP=-1;
  
  
  Double_t ordinate, lowerSpan, upperSpan;//, prvOrdinate;
  Short_t disc, quadrant,strip;
  Char_t layer;
  //These variables will store the APV number at which the relevant stuff happens for the overall quad.                                                                                                    
  Int_t APVmaxRCharge=-9999;
  Int_t APVmaxPhiCharge=-9999;
  //Int_t APVmaxRChargeUncert=-9999;
  //Int_t APVmaxPhiChargeUncert=-9999;
  Int_t APVmaxRInd=-1;
  Int_t APVmaxPInd=-1;
  Int_t APVmaxRTb=-1;
  Int_t APVmaxPTb=-1;
  Int_t APVmaxRAdc=-1;
  Int_t APVmaxPAdc=-1;
  Int_t APVmaxSigAdcR=-1;
  Int_t APVmaxSigAdcP=-1;
  Int_t APVnumFSigP=0;
  Int_t APVnumFSigR=0;
  Int_t APVfirstFSigTbP=0;
  Int_t APVfirstFSigTbR=0;
  
  Int_t APVsecondToLastRatioP=0;
  Int_t APVsecondToLastRatioR=0;
  
  Int_t APVfirstTbSigR=-1;
  Int_t APVfirstTbSigP=-1; 
  
  
  
  for(unsigned int i=0;i<  pStrips[iD*4+iq].size();i++)
    {
      Int_t geoId=pStrips[iD*4+iq][i].geoId;
      generalStrip& pStrip=pStrips[iD*4+iq][i];
      
      Int_t rdo, arm, apv, chan;
      mDb->getElecCoordFromGeoId(geoId, rdo,arm,apv,chan);
      //using binAPV for simplicity
      Int_t binAPV = (iq*10)+(apv%12);
      
      StFgtGeom::getPhysicalCoordinate(geoId,disc,quadrant,layer,ordinate,lowerSpan,upperSpan);
      StFgtGeom::decodeGeoId(geoId,disc, quadrant, layer, strip);
      //char buffer[100];
      //      if(layer=='P' && disc==iD && iq==quadrant)
      //	cout <<"looking for " << phi << " have: " << ordinate <<" diff: " << fabs(ordinate-phi) <<endl;
      if(disc==iD && iq==quadrant && ((layer =='R' && fabs(ordinate-r)<0.7) || (layer=='P' && fabs(ordinate-phi)<0.03) || (layer=='P' && fabs(ordinate-phi+2*MY_PI)<0.03 ) || (layer=='P' && fabs(ordinate-phi-2*MY_PI)<0.03)|| (layer=='P' && fabs(ordinate-phi+MY_PI)<0.03 ) || (layer=='P' && fabs(ordinate-phi-MY_PI)<0.03)))
	{
	  if(layer=='P')
	    {
	      if(pStrip.charge>maxPhiCharge)
		{
		  if((pStrip.seedType>=kFgtSeedType1 && pStrip.seedType<=kFgtSeedTypeMax)|| pStrip.seedType==kFgtClusterPart || pStrip.seedType==kFgtClusterEndUp ||pStrip.seedType==kFgtClusterEndDown)
		    {
		      partOfClusterP=true;
		      pair<Double_t, Double_t> cluSize=findCluChargeSize(iD,'P',ordinate);
		      clusterChargeP=cluSize.first;
		      mClusterSizeP=cluSize.second;
		    }
		  else
		    {
		      partOfClusterP=false;
		      clusterChargeP=-9999;
		    }
		  maxPhiCharge=pStrip.charge;
		  maxPhiChargeUncert=pStrip.chargeUncert;
		  maxPInd=i;
		  maxPAdc=-9999;
		  maxSigAdcP=-9999;
		  numFSigP=0;
		  firstFSigTbP=-1;
		  firstTbSigP=-1;
		  APVmaxPhiCharge=-1;
                  APVmaxPInd=-1;
                  APVmaxPAdc=-1;
                  APVmaxSigAdcP=-1;
                  APVnumFSigP=-1;
                  APVfirstFSigTbP=-1;
                  APVfirstTbSigP=-1;
		  
		  if(pStrip.adc[6]>0)
		    {
		      secondToLastRatioP=pStrip.adc[5]/(float)pStrip.adc[6];
		      APVsecondToLastRatioP=binAPV;
		    }
		  if(pStrip.pedErr>0)
		    {
		      firstTbSigP=pStrip.adc[0]/(float)pStrip.pedErr;
		      APVfirstTbSigP=binAPV;
		    }
		  for(int iAdc=0;iAdc<7;iAdc++)
		    {
		      if(pStrip.adc[iAdc]>5*pStrip.pedErr)
			{
			  numFSigP++;
			  APVnumFSigP=binAPV;
			  if(firstFSigTbP<0)
			    {
			      firstFSigTbP=iAdc;
			      APVfirstFSigTbP=binAPV;
			    }
			}
		      if(pStrip.adc[iAdc]>maxPAdc)
			{
			  maxPAdc=pStrip.adc[iAdc];
			  APVmaxPAdc=binAPV;
			  maxPTb=iAdc;
			  APVmaxPTb=binAPV;
			  if(pStrip.pedErr>0)
			    {
			      maxSigAdcP=(Double_t)maxPAdc/pStrip.pedErr;
			      APVmaxSigAdcP=binAPV;
			    }
			}
		    }
		}
	    }
	  else
	    {
	      if(pStrip.charge>maxRCharge)
		{
		  if((pStrip.seedType>=kFgtSeedType1 && pStrip.seedType<=kFgtSeedTypeMax)|| pStrip.seedType==kFgtClusterPart || pStrip.seedType==kFgtClusterEndUp ||pStrip.seedType==kFgtClusterEndDown)
		    {
		      partOfClusterR=true;
		      pair<Double_t, Double_t> cluSize=findCluChargeSize(iD,'R',ordinate);
		      clusterChargeR=cluSize.first;
		      mClusterSizeR=cluSize.second;
		    }
		  else
		    {
		      clusterChargeR=-9999;
		      partOfClusterR=false;
		    }
		  
		  maxRCharge=pStrip.charge;
		  maxRInd=i;
		  maxRChargeUncert=pStrip.chargeUncert;
		  maxRAdc=-9999;
		  maxSigAdcR=-9999;
		  numFSigR=0;
		  firstFSigTbR=-1;
		  firstTbSigR=-1;
		  APVmaxRCharge=-1;
                  APVmaxRInd=-1;
                  APVmaxRAdc=-1;
                  APVmaxSigAdcR=-1;
                  APVnumFSigR=-1;
                  APVfirstFSigTbR=-1;
                  APVfirstTbSigR=-1;
		  
		  if(pStrip.adc[6]>0)
		    {
		      secondToLastRatioR=pStrip.adc[5]/(float)pStrip.adc[6];
		      APVsecondToLastRatioR=binAPV;
		    }
		  if(pStrip.pedErr>0)
		    {
		      firstTbSigR=pStrip.adc[0]/(float)pStrip.pedErr;
		      APVfirstTbSigR=binAPV;
		    }
		  for(int iAdc=0;iAdc<7;iAdc++)
		    {
		      //			  cout <<"adc: "<< pStrip.adc[iAdc] <<endl;
		      if(pStrip.adc[iAdc]>5*pStrip.pedErr)
			{
			  numFSigR++;
			  APVnumFSigR=binAPV;
			  if(firstFSigTbR<0)
			    {
			      firstFSigTbR=iAdc;
			      APVfirstFSigTbR=binAPV;
			    }
			}
		      if(pStrip.adc[iAdc]>maxRAdc)
			{
			  maxRAdc=pStrip.adc[iAdc];
			  APVmaxRAdc=binAPV;
			  maxRTb=iAdc;
			  APVmaxRTb=binAPV;
			  if(pStrip.pedErr>0)
			    {
			      maxSigAdcR=(Double_t)maxRAdc/pStrip.pedErr;
			      APVmaxSigAdcR=binAPV;
			    }
			}
		    }
		  //		      cout <<"numfSigma: " << numFSigR <<endl;
		}
	    }
	}
    }

      Float_t InnerRad=19;
  //fill histos
  if(maxPhiCharge>200)
    {
      firstTbSigCloseClusterP[iD*4+iq]->Fill(firstTbSigP);
      maxAdcCloseClusterP[iD*4+iq]->Fill(maxPAdc);
      
      maxTbCloseClusterP[iD*4+iq]->Fill(maxPTb);
      numFSigCloseClusterP[iD*4+iq]->Fill(numFSigP);
      numFirstHighCloseClusterP[iD*4+iq]->Fill(firstFSigTbP);
      //	  maxAdcCloseClusterP[iD*4+iq]->Fill(maxPAdc);
      maxSigCloseClusterP[iD*4+iq]->Fill(maxSigAdcP);
      //	  cout <<"firstFSigtbP: " << firstFSigTbP <<endl;
      //	  cout <<"filling ratio with : " << secondToLastRatioP <<endl;
      secondToLastRatioCloseClusterP[iD*4+iq]->Fill(secondToLastRatioP);
      ////with radius cut!!!!!!!!!!!!!



      if(r>InnerRad&& APVfirstTbSigP>-1 && APVmaxPAdc>-1 && APVmaxPTb>-1 && APVnumFSigP>-1 && APVfirstFSigTbP>-1 && APVmaxSigAdcP>-1 && APVsecondToLastRatioP>-1)
        {
          APVfirstTbSigCloseClusterP[iD*40+APVfirstTbSigP]->Fill(firstTbSigP);
          APVmaxAdcCloseClusterP[iD*40+APVmaxPAdc]->Fill(maxPAdc);
          APVmaxTbCloseClusterP[iD*40+APVmaxPTb]->Fill(maxPTb);
          APVnumFSigCloseClusterP[iD*40+APVnumFSigP]->Fill(numFSigP);
          APVnumFirstHighCloseClusterP[iD*40+APVfirstFSigTbP]->Fill(firstFSigTbP);
          APVmaxSigCloseClusterP[iD*40+APVmaxSigAdcP]->Fill(maxSigAdcP);
          APVsecondToLastRatioCloseClusterP[iD*40+APVsecondToLastRatioP]->Fill(secondToLastRatioP);
        }

      if(iD==2 && iq==1 && (float)pStrips[iD*4+iq][maxPInd].pedErr>0)
	{
	  pulseCounterP++;
	  for(int iB=0;iB<7;iB++)
	    {
	      exPulseMaxAdcNormP->SetBinContent(iB+1,exPulseMaxAdcNormP->GetBinContent(iB+1)+pStrips[iD*4+iq][maxPInd].adc[iB]/(float)maxPAdc);
	      exPulseSigP->SetBinContent(iB+1,exPulseSigP->GetBinContent(iB+1)+pStrips[iD*4+iq][maxPInd].adc[iB]/(float)pStrips[iD*4+iq][maxPInd].pedErr);
	    }
	}
    }
  
  if(maxRCharge>200)
    {
      firstTbSigCloseClusterR[iD*4+iq]->Fill(firstTbSigR);
      maxTbCloseClusterR[iD*4+iq]->Fill(maxRTb);
      maxAdcCloseClusterR[iD*4+iq]->Fill(maxRAdc);
      numFSigCloseClusterR[iD*4+iq]->Fill(numFSigR);
      numFirstHighCloseClusterR[iD*4+iq]->Fill(firstFSigTbR);
      //	  cout <<"firstFSigtbR: " << firstFSigTbR <<endl;
      maxSigCloseClusterR[iD*4+iq]->Fill(maxSigAdcR);
      //	  cout <<"filling ratio R with : " << secondToLastRatioR <<endl;
      secondToLastRatioCloseClusterR[iD*4+iq]->Fill(secondToLastRatioR);

      if(r> InnerRad && APVfirstTbSigR>-1 && APVmaxRAdc>-1 && APVmaxRTb>-1 && APVnumFSigR>-1 && APVfirstFSigTbR>-1 && APVmaxSigAdcR>-1 && APVsecondToLastRatioR>-1)
        {
          APVfirstTbSigCloseClusterR[iD*40+APVfirstTbSigR]->Fill(firstTbSigR);
          APVmaxTbCloseClusterR[iD*40+APVmaxRTb]->Fill(maxRTb);
          APVmaxAdcCloseClusterR[iD*40+APVmaxRAdc]->Fill(maxRAdc);
          APVnumFSigCloseClusterR[iD*40+APVnumFSigR]->Fill(numFSigR);
          APVnumFirstHighCloseClusterR[iD*40+APVfirstFSigTbR]->Fill(firstFSigTbR);
          APVmaxSigCloseClusterR[iD*40+APVmaxSigAdcR]->Fill(maxSigAdcR);
          APVsecondToLastRatioCloseClusterR[iD*40+APVsecondToLastRatioR]->Fill(secondToLastRatioR);
        }

      if(iD==2 && iq==1 && (float)pStrips[iD*4+iq][maxRInd].pedErr>0)
	{
	  pulseCounterR++;
	  for(int iB=0;iB<7;iB++)
	    {
	      exPulseMaxAdcNormR->SetBinContent(iB+1,exPulseMaxAdcNormR->GetBinContent(iB+1)+pStrips[iD*4+iq][maxRInd].adc[iB]/(float)maxRAdc);
	      exPulseSigR->SetBinContent(iB+1,exPulseSigR->GetBinContent(iB+1)+pStrips[iD*4+iq][maxRInd].adc[iB]/(float)pStrips[iD*4+iq][maxRInd].pedErr);
	    }
	}
    }
  
  
  if(maxRCharge> 200 && maxPhiCharge>200 && iD==m_effDisk)// && (float)pStrips[iD*4+iq][maxRInd].charge)
    {
      StFgtGeom::getPhysicalCoordinate((float)pStrips[iD*4+iq][maxRInd].geoId,disc,quadrant,layer,ordinate,lowerSpan,upperSpan);
      //	    if(ordinate>20)
      {
	chargeCorrMaxStrip->Fill(maxRCharge,maxPhiCharge);
	chargeCorrMaxAdc->Fill(maxRAdc,maxPAdc);
	

      }
    }
  //	  if(iD==2 && iq==1 && (float)pStrips[iD*4+iq][maxRInd].pedErr>0)
  
  
  
  if(partOfClusterR&& partOfClusterP && iD==m_effDisk)
    {
      chargeCorrInEffDisk->Fill(clusterChargeR,clusterChargeP);
      StFgtGeom::getPhysicalCoordinate((float)pStrips[iD*4+iq][maxRInd].geoId,disc,quadrant,layer,ordinate,lowerSpan,upperSpan);
      //	    if(ordinate>20)

    }
  if(partOfClusterR&& partOfClusterP)
    {///this is what is plotted lateron
      chargeCorr[iD*4+iq]->Fill(clusterChargeR,clusterChargeP);
      //      cout <<"disk: " << iD << " quad: " << iq <<endl;
      //      cout <<"looking at cluster (part of track?) at r: " << r <<" phi: " << phi <<" size r: " << mClusterSizeR <<" phi: " << mClusterSizeP;
      //      cout << "extr charge r: " <<clusterChargeR <<" charge phi: " << clusterChargeP <<endl;

  //basically only here we fill with the estimated cluster, for the other disks we fill with the cluster on the track
  //	   if(r>20)
      {
	clusterSizeR[iD*4+iq]->Fill(mClusterSizeR);
	clusterSizeP[iD*4+iq]->Fill(mClusterSizeP);
      } 
    } 
  
  if(partOfClusterP)
    {

      //      cout <<" part of p cluster " << endl;
      float intPCharge=(float)pStrips[iD*4+iq][maxPInd].charge+(float)pStrips[iD*4+iq][maxPInd-1].charge+(float)pStrips[iD*4+iq][maxPInd+1].charge;
      float chi2Ndf;
      float amp;
      float t0;
      ///do the fitting...
      if(partOfClusterR)
	fitTheStrip(&(pStrips[iD*4+iq][maxPInd]),&(pStrips[iD*4+iq][maxRInd]),&amp,&t0,&chi2Ndf,iD,iq,APVmaxPAdc,'P');
      else
	fitTheStrip(&(pStrips[iD*4+iq][maxPInd]),0,&amp,&t0,&chi2Ndf,iD,iq,APVmaxPAdc,'P');

      APVfitChi2P[iD*40+APVmaxPAdc]->Fill(chi2Ndf);

      firstTbSigTrackClusterP[iD*4+iq]->Fill(firstTbSigP);
      maxAdcTrackClusterP[iD*4+iq]->Fill(maxPAdc);
      maxSigTrackClusterP[iD*4+iq]->Fill(maxSigAdcP);
      maxTbTrackClusterP[iD*4+iq]->Fill(maxPTb);
      numFSigTrackClusterP[iD*4+iq]->Fill(numFSigP);
      numFirstHighTrackClusterP[iD*4+iq]->Fill(firstFSigTbP);
      secondToLastRatioTrackClusterP[iD*4+iq]->Fill(secondToLastRatioP);
      
      if(iD==2 && iq==1 && (float)pStrips[iD*4+iq][maxPInd].pedErr>0)
	{
	  pulseCounterTP++;
	  for(int iB=0;iB<7;iB++)
	    {
	      exPulseMaxAdcNormTrackP->SetBinContent(iB+1,exPulseMaxAdcNormTrackP->GetBinContent(iB+1)+pStrips[iD*4+iq][maxPInd].adc[iB]/(float)maxPAdc);
	      
	      exPulseSigTrackP->SetBinContent(iB+1,exPulseSigTrackP->GetBinContent(iB+1)+pStrips[iD*4+iq][maxPInd].adc[iB]/(float)pStrips[iD*4+iq][maxPInd].pedErr);
	      
	    }
	}
      
    }
  if(partOfClusterR)
    {

      //      cout <<" part of R cluster " << endl;
      float intRCharge=(float)pStrips[iD*4+iq][maxRInd].charge+(float)pStrips[iD*4+iq][maxRInd-1].charge+(float)pStrips[iD*4+iq][maxRInd+1].charge;
      float chi2Ndf;
      float amp;
      float t0;
      ///do the fitting...
      if(partOfClusterP)
	fitTheStrip(&(pStrips[iD*4+iq][maxRInd]),&pStrips[iD*4+iq][maxPInd],&amp,&t0,&chi2Ndf, iD, iq, APVmaxRAdc,'R');
      else
	fitTheStrip(&(pStrips[iD*4+iq][maxRInd]),0,&amp,&t0,&chi2Ndf, iD, iq, APVmaxRAdc,'R');
      APVfitChi2R[iD*40+APVmaxRAdc]->Fill(chi2Ndf);


      firstTbSigTrackClusterR[iD*4+iq]->Fill(firstTbSigR);
      maxTbTrackClusterR[iD*4+iq]->Fill(maxRTb);
      maxAdcTrackClusterR[iD*4+iq]->Fill(maxRAdc);
      maxSigTrackClusterR[iD*4+iq]->Fill(maxSigAdcR);
      numFSigTrackClusterR[iD*4+iq]->Fill(numFSigR);
      numFirstHighTrackClusterR[iD*4+iq]->Fill(firstFSigTbR);
      secondToLastRatioTrackClusterR[iD*4+iq]->Fill(secondToLastRatioR);
      
      if(iD==2 && iq==1 && (float)pStrips[iD*4+iq][maxRInd].pedErr>0)
	{
	  pulseCounterTR++;
	  for(int iB=0;iB<7;iB++)
	    {
	      exPulseMaxAdcNormTrackR->SetBinContent(iB+1,exPulseMaxAdcNormTrackR->GetBinContent(iB+1)+pStrips[iD*4+iq][maxRInd].adc[iB]/(float)maxRAdc);
	      exPulseSigTrackR->SetBinContent(iB+1,exPulseSigTrackR->GetBinContent(iB+1)+pStrips[iD*4+iq][maxRInd].adc[iB]/(float)pStrips[iD*4+iq][maxRInd].pedErr);
	    }
	}
    }
  
}
//is this a good hit in the loose term. We can implement different criteria, so either pulse or some charge sum
//Bool_t StFgtStraightPlotter::isGoodHit(


//is there something where we expect it? Again, here phi is relative to the quadrant!!
Bool_t StFgtStraightPlotter::isSomewhatEff(Float_t r, Float_t phi, Int_t iD, Int_t iq)
{
  Double_t maxRCharge=-9999;
  Double_t maxPhiCharge=-9999;
  Double_t maxRChargeUncert=-9999;
  Double_t maxPhiChargeUncert=-9999;
  Int_t maxRInd=-1;
  Int_t maxPInd=-1;
  Bool_t validPhiPulse=false;
  Bool_t validRPulse=false;
  for(unsigned int i=0;i<  pStrips[iD*4+iq].size();i++)
    {
      Int_t geoId=pStrips[iD*4+iq][i].geoId;
      generalStrip& pStrip=pStrips[iD*4+iq][i];
      Short_t disc, quadrant,strip;
      Char_t layer;
      Double_t ordinate, lowerSpan, upperSpan;//, prvOrdinate;
      StFgtGeom::getPhysicalCoordinate(geoId,disc,quadrant,layer,ordinate,lowerSpan,upperSpan);
      StFgtGeom::decodeGeoId(geoId,disc, quadrant, layer, strip);
      //      if(layer=='P' && disc==iD && iq==quadrant)
      //	cout <<"looking for " << phi << " have: " << ordinate <<" diff: " << fabs(ordinate-phi) <<endl;
      if(disc==iD && iq==quadrant && ((layer =='R' && fabs(ordinate-r)<0.7) || (layer=='P' && fabs(ordinate-phi)<0.04) || (layer=='P' && fabs(ordinate-phi+2*MY_PI)<0.04 ) || (layer=='P' && fabs(ordinate-phi-2*MY_PI)<0.04)|| (layer=='P' && fabs(ordinate-phi+MY_PI)<0.04 ) || (layer=='P' && fabs(ordinate-phi-MY_PI)<0.04)))
	{
	  if(layer=='P')
	    {
	      if(validPulse(pStrip))
		validPhiPulse=true;

	      if(pStrip.charge>maxPhiCharge)
		{
		  maxPhiCharge=pStrip.charge;
		  maxPhiChargeUncert=pStrip.chargeUncert;
		  maxPInd=i;
		}
	    }
	  else
	    {
	      if(validPulse(pStrip))
		validRPulse=true;

	      if(pStrip.charge>maxRCharge)
		{
		  maxRCharge=pStrip.charge;
		  maxRInd=i;
		  maxRChargeUncert=pStrip.chargeUncert;
		}
	    }
	}
    }
#ifdef PULSE_CONDITION
  if(validPhiPulse && validRPulse)
    return true;
#endif


  if(maxRInd>=0 && maxPInd>=0)
    {
#ifdef LEN_CONDITION
      if(maxRCharge>1000 && maxPhiCharge>1000)
	return true;
#endif

      /////add neighbouring strips
      if(maxRInd>0)
	maxRCharge+=pStrips[iD*4+iq][maxRInd-1].charge;
      if(maxRInd< (int)(pStrips[iD*4+iq].size()-1))
	maxRCharge+=pStrips[iD*4+iq][maxRInd+1].charge;
      if(maxPInd>0)
	maxPhiCharge+=pStrips[iD*4+iq][maxPInd-1].charge;
      if(maxPInd< (int)(pStrips[iD*4+iq].size()-1))
	maxPhiCharge+=pStrips[iD*4+iq][maxPInd+1].charge;
      ///////////////////////////////////

      //	  cout <<"charge: "<< maxRCharge<< " 3* uncert " << 3*maxRChargeUncert <<" maxPhiCharge " << maxPhiCharge<<" 3*phi "<< 3*maxPhiChargeUncert <<endl;
      if(maxRCharge>3*maxRChargeUncert && maxPhiCharge>3*maxPhiChargeUncert)
	{
	  //if we don't care about charge ratio
	  //  return true;
	  if(maxRCharge>maxPhiCharge)
	    {
	      if(maxRCharge/maxPhiCharge<4)
		return true;
	      else
		return false;
	    }
	  else
	    {
	      if(maxPhiCharge/maxPhiCharge<4)
		return true;
	      else
		return false;
	    }
	}
	  
    }
  return false;
}

//if required only return clusters with matches...
pair<Double_t,Double_t> StFgtStraightPlotter::findCluChargeSize(Int_t iD,Char_t layer, Double_t ordinate)
{
  if(iD<0 || iD >5)
    {
      return pair<Double_t,Double_t>(-99999,-99999);
    }
  vector<generalCluster> &hitVec=*(pClusters[iD]);
  Double_t charge=-99999;
  Double_t cluSize=-9999;
  Double_t minDist=99999;
  for(vector<generalCluster>::iterator it=hitVec.begin();it!=hitVec.end();it++)
    {
      if(it->layer!=layer)
	continue;
      Float_t ord=-9999;
      if(layer=='R')
	{
	  ord=it->posR;
	}
      else
	{
	  ord=it->posPhi;
	  if(fabs(ordinate-(ord+MY_PI))<fabs(ord-ordinate))
	    ord=ord+MY_PI;
	  if(fabs(ordinate-(ord-MY_PI))<fabs(ord-ordinate))
	    ord=ord-MY_PI;
	}

      if((fabs(ordinate-ord)<minDist) && (!useChargeMatch || it->hasMatch))
	{
	  charge=it->clusterCharge;
	  cluSize=it->clusterSize;
	  minDist=fabs(ordinate-ord);
	}
    }
  return pair<Double_t,Double_t>(charge,cluSize);
}

Double_t StFgtStraightPlotter::findClosestPoint(float mx, float bx, float my, float by, double xE, double yE, Int_t iD)
{
  //  cout <<"expecting point at " << xE <<", " <<yE <<endl;
  if(iD<0 || iD >5)
    {
      return 99999;
    }
  vector<generalCluster> &hitVec=*(pClusters[iD]);
  Double_t dist2=99999;
  for(vector<generalCluster>::iterator it=hitVec.begin();it!=hitVec.end();it++)
    {
      for(vector<generalCluster>::iterator it2=hitVec.begin();it2!=hitVec.end();it2++)
	{
	  //
	  if(useChargeMatch && !arePointsMatched(it,it2))
	    continue;

	  if(it->layer==it2->layer)
	    continue;
	  Float_t r=it->posR;
	  Float_t phi=it2->posPhi;
	  if(it->layer!='R')
	    {
	      phi=it->posPhi;
	      r=it2->posR;
	    }

	  Float_t x=r*cos(phi);
	  Float_t y=r*sin(phi);
	  //	  cout <<"we have " << x <<", " << y <<endl;
	  Double_t mDist=(x-xE)*(x-xE)+(y-yE)*(y-yE);
	  if(mDist<dist2)
	    {
	      dist2=mDist;
	      //recalculate distance with proper alignment
	      float tmpX, tmpY,tmpZ,tmpP,tmpR;
	      if(isCosmic)
		{
		  getAlign(iD,phi,r,tmpX,tmpY,tmpZ,tmpP,tmpR);
		

		  Double_t xExpUpdate=mx*tmpZ+bx;
		  Double_t yExpUpdate=my*tmpZ+by;
		  //	      cout<<"tmpx: " << tmpX <<" old: " << x <<" xE old: " << xE << " updated: " << xExpUpdate;
		  //	      cout<<"tmpy: " << tmpY <<" old: " << y <<" yE old: " << yE << " updated: " << yExpUpdate<<endl;
		  mDist=(tmpX-xExpUpdate)*(tmpX-xExpUpdate)+(tmpY-yExpUpdate)*(tmpY-yExpUpdate);
		}
	      dist2=mDist;
	      ///Double_t xExp=mx*StFgtGeom::getDiscZ(i)+bx;
	      //	    Double_t yExp=my*StFgtGeom::getDiscZ(i)+by;


	      //	      (*outTxtFile) <<"point found, x: " << x <<" y: " << y << " dist: " << dist2 <<endl;
	    }
	}
    }
  //  (*outTxtFile) <<"returning : " << dist2<<endl;
  return dist2;
}


///this is too naive..., assumes non-rotated quads
Short_t StFgtStraightPlotter::getQuadFromCoo(Double_t x, Double_t y)
{
  cout <<"do not use this function!!!" <<endl;
  if(x>0 && y>0)
    return 0;
  if(x>0 && y<0)
    return 1;
  if(x<0 && y<0)
    return 2;
  if(x<0 && y>0)
    return 3;

  return -9999;
}


Bool_t StFgtStraightPlotter::printArea1D(Int_t iD,Int_t iq, Int_t centerGeoId)
{
  
  for(unsigned int i=0;i<pStrips[iD*4+iq].size();i++)
    {
      Int_t geoId=pStrips[iD*4+iq][i].geoId;
      generalStrip& pStrip=pStrips[iD*4+iq][i];
      Short_t disc, quadrant,strip;
      Char_t layer;
      Double_t ordinate, lowerSpan, upperSpan;//, prvOrdinate;
      StFgtGeom::getPhysicalCoordinate(geoId,disc,quadrant,layer,ordinate,lowerSpan,upperSpan);
      StFgtGeom::decodeGeoId(geoId,disc, quadrant, layer, strip);
      char buffer[100];
      switch(pStrip.seedType)
	{
	case kFgtSeedTypeNo:
	  sprintf(buffer,"No Seed");
	  break;
	case kFgtSeedType1:
	  sprintf(buffer,"Seed1");
	  break;
	case kFgtSeedType2:
	  sprintf(buffer,"Seed2");
	  break;
	case kFgtSeedType3:
	  sprintf(buffer,"Seed3");
	  break;
	case kFgtSeedType4:
	  sprintf(buffer,"Seed4");
	  break;
	case kFgtSeedType5:
	  sprintf(buffer,"Seed5");
	  break;
	case kFgtClusterPart:
	  sprintf(buffer,"PartOfCluster");
	  break;
	case kFgtClusterEndUp:
	  sprintf(buffer,"EoC");
	  break;
	case kFgtClusterEndDown:
	  sprintf(buffer,"BoC");
	  break;
	case kFgtDeadStrip:
	  sprintf(buffer,"DeadStrip");
	  break;
	case kFgtClusterTooBig:
	  sprintf(buffer,"cluster too big");
	  break;
	case kFgtClusterSeedInSeaOfNoise:
	  sprintf(buffer,"seed in noise");
	  break;
	default:
	  sprintf(buffer,"somethingWrong: %d", pStrip.seedType);
	}

      //     if(layer=='P' && disc==iD && iq==quadrant)
      //	cout <<"looking for " << phi << " have: " << ordinate <<" diff: " << fabs(ordinate-phi) <<endl;
      if(abs(geoId-centerGeoId)<8)
	{
	  //	  cout <<" found!!!" << endl;
	  (*outTxtFile) <<StFgtGeom::encodeGeoName(iD,iq,layer,strip)<<"geo: " << geoId<< " ord: " << ordinate <<" layer: " <<layer<<" ped: " << pStrip.ped <<" pedErr: " << pStrip.pedErr <<" seedType: " <<buffer<<" ";
	  for(int iT=0;iT<7;iT++)
	    {
	      if(pStrip.adc[iT]<pStrip.pedErr)
		(*outTxtFile) << setw(4) << " .  "<< " ";
	      else
		(*outTxtFile) <<  setw(4) <<pStrip.adc[iT] <<" ";
	    }
	  (*outTxtFile) <<endl;
	}
    }
  return kStOk;

}

//print the strips around the place where we expect hit
Bool_t StFgtStraightPlotter::printArea(Float_t r, Float_t phi, Int_t iD, Int_t iq)
{
  //just print the first 1000 clusters

  //  cout <<" looking for r: " << r <<" phi: " << phi<< " to print " <<endl;
  if(printCounter>1000)
    return true;
  printCounter++;
  //first r: 
  Double_t signalsP[900];
  Double_t signalsR[900];

  Int_t counterR=0;
  Int_t counterP=0;
  //  cout <<" looking for strips.., we have: " << pStrips[iD*4+iq].size() << " in iD: " << iD << " iq: " << iq <<endl;
  for(unsigned int i=0;i<pStrips[iD*4+iq].size();i++)
    {
      Int_t geoId=pStrips[iD*4+iq][i].geoId;
      generalStrip& pStrip=pStrips[iD*4+iq][i];
      Short_t disc, quadrant,strip;
      Char_t layer;
      Double_t ordinate, lowerSpan, upperSpan;//, prvOrdinate;
      StFgtGeom::getPhysicalCoordinate(geoId,disc,quadrant,layer,ordinate,lowerSpan,upperSpan);
      StFgtGeom::decodeGeoId(geoId,disc, quadrant, layer, strip);
      char buffer[100];
      switch(pStrip.seedType)
	{
	case kFgtSeedTypeNo:
	  sprintf(buffer,"No Seed");
	  break;
	case kFgtSeedType1:
	  sprintf(buffer,"Seed1");
	  break;
	case kFgtSeedType2:
	  sprintf(buffer,"Seed2");
	  break;
	case kFgtSeedType3:
	  sprintf(buffer,"Seed3");
	  break;
	case kFgtSeedType4:
	  sprintf(buffer,"Seed4");
	  break;	
	case kFgtSeedType5:
	  sprintf(buffer,"Seed5");
	  break;
	case kFgtClusterPart:
	  sprintf(buffer,"PartOfCluster");
	  break;
	case kFgtClusterEndUp:
	  sprintf(buffer,"EoC");
	  break;
	case kFgtClusterEndDown:
	  sprintf(buffer,"BoC");
	  break;
	case kFgtDeadStrip:
	  sprintf(buffer,"DeadStrip");
	  break;
	case kFgtClusterTooBig:
	  sprintf(buffer,"cluster too big");
	  break;
	case kFgtClusterSeedInSeaOfNoise:
	  sprintf(buffer,"seed in noise");
	  break;
	default:
	  sprintf(buffer,"somethingWrong: %d", pStrip.seedType);
	}

      //          if(layer=='P' && disc==iD && iq==quadrant)
      //	    cout <<"looking for " << phi << " have: " << ordinate <<" diff: " << fabs(ordinate-phi) <<endl;
      //          if(layer=='R' && disc==iD && iq==quadrant)
      //	    cout <<"looking for r=" << r << " have: " << ordinate <<" diff: " << fabs(ordinate-r) <<endl;
      if(disc==iD && iq==quadrant && ((layer =='R' && fabs(ordinate-r)<1.0) || (layer=='P' && fabs(ordinate-phi)<0.04) || (layer=='P' && fabs(ordinate-phi+2*MY_PI)<0.04 ) || (layer=='P' && fabs(ordinate-phi-2*MY_PI)<0.04)|| (layer=='P' && fabs(ordinate-phi+MY_PI)<0.04 ) || (layer=='P' && fabs(ordinate-phi-MY_PI)<0.04)))
	{
	  //	  cout <<" found!!!" << endl;
	  (*outTxtFile) <<"Id: " << geoId<<" "<<StFgtGeom::encodeGeoName(iD,iq,layer,strip)<<" ord: " << ordinate <<" layer: " <<layer<<" ped: " << pStrip.ped <<" pedErr: " << pStrip.pedErr <<" seedType: " <<buffer<<" ";
	  for(int iT=0;iT<7;iT++)
	    {
	      if(pStrip.adc[iT]<pStrip.pedErr)
		(*outTxtFile) << setw(4) << " .  "<< " ";
	      else
		(*outTxtFile) <<  setw(4) <<pStrip.adc[iT] <<" ";


	      if(layer=='P'&& counterP<40)
		{
		  signalsP[counterP*7+iT]=pStrip.adc[iT];
		  //		  cout <<"first p: setting bin : i: " << counterP << " j: " << iT << " index: "<< counterP*7+iT << " sig: " << signalsP[counterP*7+iT]<<endl;
		  if(iT==6)
		    counterP++;
		}
	      if(layer=='R'&& counterR<40)
		{
		  signalsR[counterR*7+iT]=pStrip.adc[iT];
		  //		  cout <<"first R: setting bin : i: " << counterR << " j: " << iT << " index: "<< counterR*7+iT << " sig: " << signalsR[counterP*7+iT]<<endl;
		  if(iT==6)
		    counterR++;
		}
	    }
	  (*outTxtFile) <<endl;
	}
    }

  saveSigs(signalsR,signalsP,r,phi,counterR,counterP, iD, iq);
  return kStOk;
}



//print the strips around the place where we expect hit
pair<Double_t,Double_t> StFgtStraightPlotter::getChargeRatio(Float_t r, Float_t phi, Int_t iD, Int_t iq)
{
  //first r: 
  Double_t maxRCharge=-9999;
  Double_t maxPhiCharge=-9999;
  Int_t maxRInd=-1;
  Int_t maxPInd=-1;
  for(unsigned int i=0;i<  pStrips[iD*4+iq].size();i++)
    {
      Int_t geoId=pStrips[iD*4+iq][i].geoId;
      generalStrip& pStrip=pStrips[iD*4+iq][i];
      Short_t disc, quadrant,strip;
      Char_t layer;
      Double_t ordinate, lowerSpan, upperSpan;//, prvOrdinate;
      StFgtGeom::getPhysicalCoordinate(geoId,disc,quadrant,layer,ordinate,lowerSpan,upperSpan);
      StFgtGeom::decodeGeoId(geoId,disc, quadrant, layer, strip);
      //      if(layer=='P' && disc==iD && iq==quadrant)
      //	cout <<"looking for " << phi << " have: " << ordinate <<" diff: " << fabs(ordinate-phi) <<endl;
      if(disc==iD && iq==quadrant && ((layer =='R' && fabs(ordinate-r)<0.7) || (layer=='P' && fabs(ordinate-phi)<0.03) || (layer=='P' && fabs(ordinate-phi+2*MY_PI)<0.03 ) || (layer=='P' && fabs(ordinate-phi-2*MY_PI)<0.03)|| (layer=='P' && fabs(ordinate-phi+MY_PI)<0.03 ) || (layer=='P' && fabs(ordinate-phi-MY_PI)<0.03)))
	{
	  if(layer=='P')
	    {
	      if(pStrip.charge>maxPhiCharge)
		{
		  maxPhiCharge=pStrip.charge;
		  maxPInd=i;
		}
	    }
	  else
	    {
	      if(pStrip.charge>maxRCharge)
		{
		  maxRCharge=pStrip.charge;
		  maxRInd=i;
		}
	    }
	}
    }
  if(maxRInd>=0 && maxPInd>=0)
    {
      if(maxRCharge>1000 && maxPhiCharge>1000)
	{
	  ////might pick up phi strips... oh wlll
	  if(maxRInd>0)
	    maxRCharge+=pStrips[iD*4+iq][maxRInd-1].charge;
	  if(maxRInd< (int)(pStrips[iD*4+iq].size()-1))
	    maxRCharge+=pStrips[iD*4+iq][maxRInd+1].charge;
	  if(maxPInd>0)
	    maxPhiCharge+=pStrips[iD*4+iq][maxPInd-1].charge;
	  if(maxPInd< (int)(pStrips[iD*4+iq].size()-1))
	    maxPhiCharge+=pStrips[iD*4+iq][maxPInd+1].charge;

	  return pair<Double_t,Double_t>(maxRCharge,maxPhiCharge);
	}

    }
  return pair<Double_t,Double_t>(-9999,-9999);
}


Double_t StFgtStraightPlotter::getRPhiRatio(vector<generalCluster>::iterator hitIterBegin, vector<generalCluster>::iterator hitIterEnd)
{
  Short_t quad;
  Char_t layer; 
  Int_t numR=0;
  Int_t numPhi=0;
  vector<generalCluster>::iterator hitIter=hitIterBegin;
  for(;hitIter!=hitIterEnd;hitIter++)
    {
      quad=hitIter->quad;
      layer=hitIter->layer;

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
Int_t StFgtStraightPlotter::Make()
{
  evtNr++;
  Int_t numTracksInCurrEv=0;
  StFgtGeneralBase *fgtGenMkr = static_cast<StFgtGeneralBase * >( GetMaker("fgtGenBase"));
  pClusters=fgtGenMkr->getClusters();
  pStrips=fgtGenMkr->getStrips();
  Int_t tmpCluCount[6*4*2];
  memset(tmpCluCount,0,6*4*2*sizeof(Int_t));
  for(int iD=0;iD<6;iD++)
    {
      vector<generalCluster> &hitVec=*(pClusters[iD]);
      for( vector<generalCluster>::iterator hitIter=hitVec.begin();hitIter != hitVec.end();hitIter++)
	{

	  Int_t iq=hitIter->quad;
	  if(hitIter->layer=='R')
	    {
	      tmpCluCount[iD*4+iq]++;
	      h_clusterChargeR[iD]->Fill(hitIter->clusterCharge);
	    }
	  else
	    {
		tmpCluCount[6*4+iD*4+iq]++;
		h_clusterChargePhi[iD]->Fill(hitIter->clusterCharge);
	    }
	    
	}
    }
  for(int iD=0;iD<6;iD++)
    {
      for(int iq=0;iq<4;iq++)
	{
	  numClustersR[iD*4+iq]->Fill(tmpCluCount[iD*4+iq]);
	  numClustersPhi[iD*4+iq]->Fill(tmpCluCount[6*4+iD*4+iq]);
	}
    }

  //  cout <<"ave make " <<endl;
  Int_t ierr = kStOk;
  (*outTxtFile) <<"----------------------------- Event Nr: " << evtNr<<" -----------------" <<endl;

  StFgtStraightTrackMaker *fgtSTracker = static_cast<StFgtStraightTrackMaker * >( GetMaker(mTrackerName));

  vector<AVTrack>& tracks=fgtSTracker->getTracks();
  //    cout <<"plotter: we have " << tracks.size() << "tracks " <<endl;
  //  intNumTracks+=tracks.size();
  for(vector<AVTrack>::iterator it=tracks.begin();it!=tracks.end();it++)
    {
      //	  cout <<"plotter chi2: "<< it->chi2 <<" vertex: " << it->trkZ <<endl;
      if(it->chi2>maxDistChi || fabs(it->trkZ)> vertexCut || it->dca> dcaCut )
	{
	  continue;
	}
      //only add tracks that pass our cut criteria....
      intNumTracks++; 
      numTracksInCurrEv++;
      numPointsPerTrack->Fill(it->points->size());
      for(int i=0;i<it->points->size();i++)
	{
	  Int_t iD=(*(it->points))[i].dID;
	  Int_t iq=(*(it->points))[i].quadID;
	  if(iD<6 && iD>=0 && iq<4 && iq>=0)
	    {
	      //	      cout <<"disk :  " << iD << " iq: " << iq <<endl;
	      Float_t rCharge=(*(it->points))[i].rCharge;
	      Float_t phiCharge=(*(it->points))[i].phiCharge;
	      Float_t rPos=(*(it->points))[i].r;
	      Float_t phiPos=(*(it->points))[i].phi;
	      //r clusters are at constant r, so we plot against phi and vice versa
	      if(phiPos<0)
		phiPos+=2*TMath::Pi();
	      chargeTrackClusterRvsP[iD*4+iq]->Fill(phiPos,(*(it->points))[i].rCharge);
	      chargeTrackClusterPvsR[iD*4+iq]->Fill(rPos,(*(it->points))[i].phiCharge);
	      chargeTrackClusterR[iD*4+iq]->Fill((*(it->points))[i].rCharge);
	      chargeTrackClusterP[iD*4+iq]->Fill((*(it->points))[i].phiCharge);
	      numTrackClusterR[iD*4+iq]->Fill(rPos);
	      numTrackClusterP[iD*4+iq]->Fill(phiPos);
	      if(fabs((rCharge-phiCharge)/(rCharge+phiCharge))<0.1)
		{
		  //num track is not needed beause we don't do it position depedent
		  numTrackSymCutClusterR[iD*4+iq]->Fill(rPos);
		  numTrackSymCutClusterP[iD*4+iq]->Fill(phiPos);
		  //---
		  chargeTrackSymCutClusterR[iD*4+iq]->Fill((*(it->points))[i].rCharge);
		  chargeTrackSymCutClusterP[iD*4+iq]->Fill((*(it->points))[i].phiCharge);
		}
	      radioPlotsTrackHits[iD]->Fill((*(it->points))[i].x,(*(it->points))[i].y);
	      radioPlotsClusChargeR[iD]->Fill((*(it->points))[i].x,(*(it->points))[i].y,(*(it->points))[i].rCharge);
	      radioPlotsClusChargeP[iD]->Fill((*(it->points))[i].x,(*(it->points))[i].y,(*(it->points))[i].phiCharge);
	      radioPlotsClusSizeR[iD]->Fill((*(it->points))[i].x,(*(it->points))[i].y,(*(it->points))[i].rSize);
	      radioPlotsClusSizeP[iD]->Fill((*(it->points))[i].x,(*(it->points))[i].y,(*(it->points))[i].phiSize);
	      chargeCorrTracks[iD*4+iq]->Fill((*(it->points))[i].rCharge,(*(it->points))[i].phiCharge);


	      trkPhiProj[iD]->Fill((*(it->points))[i].phi);

	      if((*(it->points))[i].r>19)
		{
		  chargeCorrTracksRCut[iD*4+iq]->Fill((*(it->points))[i].rCharge,(*(it->points))[i].phiCharge);
		}

	      //	      cout <<" r charge: " << (*(it->points))[i].rCharge <<  " phi : " << (*(it->points))[i].phiCharge <<endl;
	      //	      cout << "point on track: R hit: " << (*(it->points))[i].fgtHitR->getPositionR()<< " geoId: " <<  (*(it->points))[i].fgtHitR->getCentralStripGeoId() <<" size: " <<(*(it->points))[i].fgtHitR->getNstrip()<< endl;
	      //	      cout << "point on track: Phi hit: " << (*(it->points))[i].fgtHitPhi->getPositionPhi()<< " geoId: " <<  (*(it->points))[i].fgtHitPhi->getCentralStripGeoId()<<" size: " << (*(it->points))[i].fgtHitPhi->getNstrip() <<endl;

	    }
	}

      Double_t mx=it->mx;
      Double_t my=it->my;
      Double_t bx=it->ax;
      Double_t by=it->ay;

      //      cout <<"looking at track with mx: " << it->mx <<" ax: " << it->ax <<" my: " << it->my <<" ay: " << it->ay <<endl;
      for(int i=0;i<6;i++)
	{
	  //	  cout <<"checking on disk " << i <<endl;
	  Double_t xExp=mx*StFgtGeneralBase::getLocDiscZ(i)+bx;
	  Double_t yExp=my*StFgtGeneralBase::getLocDiscZ(i)+by;
	  //	    cout <<"expecting x: " << xExp << ", " << yExp<<endl;
	  Int_t quad=-1;
	  //x=r*cos(phi)
	  //y=r*sin(phi)
	  Double_t r=sqrt(xExp*xExp+yExp*yExp);
	  //if both, x and y, are negative we get the wrong angle
	  Double_t phi=atan(yExp/xExp);
	  if(xExp <0 && yExp <0)
	    phi-=TMath::Pi();
	  //	    cout <<"phi: "<<phi << " from x: " << xExp <<" y: " << yExp<<endl;
	  if(phi<-MY_PI)
	    phi+=2*MY_PI;
	  if(phi>MY_PI)
	    phi-=2*MY_PI;
	  //	    if(phi<-MY_PI)
	  //	      phi+=2*MY_PI;
	    
	  //	    quad=getQuadFromCoo(xExp,yExp);
	  quad=StFgtGeom::getQuad(phi);
	  //	    cout <<"got quad : " << quad << " for phi: " << phi <<endl;
	  //convert to phi in quad.., so we have to subtract that axis...

	  phi-=StFgtGeom::phiQuadXaxis(quad);
	    
	  if(phi>TMath::Pi())
	    phi-=(2*TMath::Pi());
	  if(phi<((-1)*TMath::Pi()))
	    phi+=(2*TMath::Pi());
	    
	  if(phi<0)
	    phi+=MY_PI;

	  fillStripHistos(r,phi,i,quad);
	  if(isSomewhatEff(r,phi,i,quad))
	    {
	      //		cout <<" somewhat eff: " << xExp <<" y: " << yExp <<" disk: " << i <<endl;
	      radioPlotsEffLoose[i]->Fill(xExp,yExp);
	    }
	  else
	    {
	      //		    cout <<"not somewhat eff: " << xExp <<" y: " << yExp <<endl;
	      radioPlotsNonEffLoose[i]->Fill(xExp,yExp);
	    }
	    
	  if(i==m_effDisk && quad==QUAD_EFF)
	    {
	      //do the r/phi thing
	      if(findClosestStrip('R',r,i,quad)<MAX_DIST_STRIP_R)
		radioPlotsEffR[i]->Fill(xExp,yExp);
	      else
		radioPlotsNonEffR[i]->Fill(xExp,yExp);
	      //correct phi again, so it is absolute in space, like the quads
	      if(findClosestStrip('P',phi+StFgtGeom::phiQuadXaxis(quad),i,quad)<MAX_DIST_STRIP_PHI)
		{
		  //		    cout <<" found phi " <<endl;
		  radioPlotsEffPhi[i]->Fill(xExp,yExp);
		}
	      else
		{
		  //		    cout << " not found, dist is:" <<findClosestStrip('P',phi,i,quad) <<endl;
		  radioPlotsNonEffPhi[i]->Fill(xExp,yExp);
		}
	    }
	    
	  Double_t closestPoint=findClosestPoint(mx,bx,my,by,xExp,yExp,i);
	  //	  cout <<" closest point is " << closestPoint <<" away " <<endl;


	  //		 (*outTxtFile) <<"closest point t " << xExp <<" , " << yExp << " is : " << closestPoint << " away " << endl;

	  if(findClosestPoint(mx,bx,my,by,xExp,yExp,i)<MAX_DIST2_EFF)
	    {
	      //	      cout <<"found point on eff disk, x: " << xExp <<" y: " << yExp <<", dist: " << closestPoint <<endl;
	      radioPlotsEff[i]->Fill(xExp,yExp);
	      if(i==m_effDisk)
		//		    if(i==1)
		{
		  hResidua->Fill(sqrt(closestPoint));
		  hResiduaX->Fill(xExp,sqrt(closestPoint));
		  hResiduaY->Fill(yExp,sqrt(closestPoint));
		  hResiduaR->Fill(r,sqrt(closestPoint));
		  hResiduaP->Fill(phi,sqrt(closestPoint));
		}

	      (*outTxtFile) <<"***** found hit in disk " <<i << " quad: " << quad << " at " << xExp<<", " << yExp<<" r: " << r <<" phi: " <<phi << endl;

#ifdef DO_PRINT
	      //		if(i==m_effDisk)
	      printArea(r,phi,i,quad);
#endif
	      //		    chargeCorrInEffDisk->Fill(rPhiRatio.first,rPhiRatio.second);
	    }
	  else
	    {
	      //		    cout <<"non eff disk, x: " << xExp <<" y: " << yExp <<endl;
	      radioPlotsNonEff[i]->Fill(xExp,yExp);
	      //		    if(i==m_effDisk)
	      (*outTxtFile) <<"expected (but haven't found)  point on disk " << i <<", x: " << xExp <<" y: " << yExp << " r: " << r  <<" phi: " << phi << " quad:: " << quad << endl;
	      ////		cout <<" expect hit at disc " << iD <<" quad: " << iq  << " r: " <<  r <<" phi: "<< phi <<endl;
#ifdef DO_PRINT
	      //    if(i==m_effDisk)
	      printArea(r,phi,i,quad);
#endif
	    }
	  pair<Double_t,Double_t> rPhiRatio=getChargeRatio(r,phi,i,quad);

	  if(rPhiRatio.first>0 && rPhiRatio.second>0)
	    {
	      double asym=fabs((Double_t)1-rPhiRatio.first/rPhiRatio.second);
	      double ratio=rPhiRatio.first/rPhiRatio.second;

	      //		      cout <<" filling with : r charge: " << rPhiRatio.first <<" , " << rPhiRatio.second <<" ratio: " << ratio <<" asym: " << asym<<endl;
	      if(asym<2 && ratio <2)
		{
		  hChargeAsym->Fill(asym);
		  hChargeRatio->Fill(ratio);
		  chargeRatioInEffDisk->Fill(xExp,yExp,ratio);
		  chargeAsymInEffDisk->Fill(xExp,yExp,asym);
		}
	    }
	}
    }
  int counter=0;
  for(vector<AVTrack>::iterator it=tracks.begin();it!=tracks.end();it++)
    {
      //      cout <<" looking at track " << counter <<endl;
      counter++;
      //      cout <<"This track has parameters: ";
      //      cout <<" mx: " << it->mx <<" my: " << it->my <<" bx: " << it->ax << " by: " << it->ay << " chi2: " << it->chi2 <<endl;
      Double_t vertZ = (  -( it->mx*it->ax + it->my*it->ay )/(it->mx*it->mx+it->my*it->my));

      pair<double,double> dca=getDca(it);
      //      cout <<"dca: " << dca.second <<" z vertex: " << vertZ <<" or " << dca.first <<endl;
      if(it->chi2<maxDistChi && fabs(vertZ)< vertexCut )
	{
	  hIpZ->Fill(dca.first);
	  hIp->Fill(dca.first,dca.second);
	  hTrkZ->Fill(vertZ);
	  hMx->Fill(it->mx);	  
	  hMy->Fill(it->my);
	  hBx->Fill(it->ax);	  
	  hBy->Fill(it->ay);
	  hChi2->Fill(it->chi2);
	  if(it->vtxRank>0)
	    {
	      tpcFgtZVertexCorr->Fill(dca.first,it->ipZEv);
	      tpcFgtZVertexCorr2->Fill(vertZ,it->ipZEv);
	      tpcFgtZVtxDiff->Fill(dca.first-it->ipZEv);
	      tpcFgtZVtxDiff->Fill(vertZ-it->ipZEv);


	    }
	  tpcFgtZVertexCorr3->Fill(vertZ,dca.first);

	  hIpDca->Fill(dca.second);
	}
    }
  numTracks->Fill(numTracksInCurrEv);
  return ierr;

};
 
StFgtStraightPlotter::StFgtStraightPlotter( const Char_t* name, const Char_t* trackerName): StMaker( name ),intNumTracks(0),useChargeMatch(false),runningEvtNr(0),hitCounter(0),hitCounterR(0),printCounter(0),fitCounter(0)
{

  sprintf(mTrackerName,"%s",trackerName);

  StFgtDbMaker *fgtDbMkr = static_cast< StFgtDbMaker* >( GetMakerInheritsFrom( "StFgtDbMaker" ) );
  if( !fgtDbMkr ){
    LOG_FATAL << "StFgtDb not provided and error finding StFgtDbMaker" << endm;
  }
  mDb = fgtDbMkr->getDbTables();
  if( !mDb ){
    LOG_FATAL << "StFgtDb not provided and error retrieving pointer from StFgtDbMaker '"
	      << fgtDbMkr->GetName() << endm;
  }
  //count from 0--> third disk
  m_effDisk=2;
  sprintf(mFileBase,"%s",".");
  pulseCondition=true;
  lenCondition=false;
  maxDistStrip_R=0.7;
  maxDistStrip_Phi=0.03;
  maxDist2Eff=1.0;
  doPrint=false;
  StFgtGeneralBase *fgtGenMkr = static_cast<StFgtGeneralBase * >( GetMaker("fgtGenBase"));
  isCosmic=fgtGenMkr->isCosmic();

  if(!isCosmic)
    vertexCut=50;
  else
    vertexCut=100000000;

     dcaCut=3;
  //no cut 
  //   dcaCut=100;

  maxDistChi=1.0;
  //  cout <<"AVE constructor!!" <<endl;



};

StFgtStraightPlotter::~StFgtStraightPlotter()
{

  //delete histogram arrays
};
void StFgtStraightPlotter::SetFileBase(const Char_t* filebase, const Char_t* filename )
{
  cout <<"setting file base to " << filebase <<endl;
  sprintf(mFileBase,"%s",filebase);
  sprintf(mFileName,"%s",filename);
}

Int_t StFgtStraightPlotter::Finish(){


  outTxtFile->close();
  gStyle->SetPalette(1);
  Int_t ierr = kStOk;
  //  cout <<"we found " << intNumTracks << " tracks " <<endl;
  ///////////////////////////track collection//does this make sense? Not if m_tracks gets erased for every event...
  StFgtStraightTrackMaker *fgtSTracker = static_cast<StFgtStraightTrackMaker * >( GetMaker("fgtStraightTracker"));
  vector<AVTrack>& tracks=fgtSTracker->getTracks();

  ///  cout <<"canvases etc.. " << endl;
  //////////////////// //////////////////////////////
  char buffer[300];
  sprintf(buffer,"radioPlots_%s",mFileName);
  TCanvas* cRadio=new TCanvas(buffer,buffer,1000,1500);
  sprintf(buffer,"radioPlotsLoose_%s",mFileName);
  TCanvas* cRadioLoose=new TCanvas(buffer,buffer,1000,1500);
  sprintf(buffer,"radioPlotsR_%s",mFileName);
  TCanvas* cRadioR=new TCanvas(buffer,buffer,1000,1500);
  sprintf(buffer,"radioPlotsPhi_%s",mFileName);
  TCanvas* cRadioPhi=new TCanvas(buffer,buffer,1000,1500);
  sprintf(buffer,"radioPlotsHits_%s",mFileName);
  TCanvas* cRadioHits=new TCanvas(buffer,buffer,1000,1500);
  sprintf(buffer,"radioPlotsNonHits_%s",mFileName);
  TCanvas* cRadioNonHits=new TCanvas(buffer,buffer,1000,1500);
  //  cout <<"divide "<<endl;
  cRadio->Divide(2,3); //6 discs
  cRadioR->Divide(2,3); //6 discs
  cRadioPhi->Divide(2,3); //6 discs
  cRadioLoose->Divide(2,3); //6 discs
  cRadioHits->Divide(2,3); //6 discs
  cRadioNonHits->Divide(2,3); //6 discs
  sprintf(buffer,"rPhiRatios_%s",mFileName);
  TCanvas* cRPRatio=new TCanvas(buffer,buffer,1000,1500);
  cRPRatio->Divide(2,3); //6 discs
  sprintf(buffer,"crEff_%s",mFileName);
  TCanvas* cREff=new TCanvas(buffer,buffer,1000,1500);

  cREff->Divide(2,3); //6 discs
  //  cout <<"drawing hits " <<endl;
  for(Int_t iD=0;iD<kFgtNumDiscs;iD++)
    {
      //      cRadio->cd(iD+1)->SetLogz();
      cRadioHits->cd(iD+1);
      radioPlotsEff[iD]->Draw("colz");
      cRadioNonHits->cd(iD+1);
      radioPlotsNonEff[iD]->Draw("colz");
    }

  sprintf(buffer,"%s/clusterPics%s.root",mFileBase,mFileName);
   cout <<"setting cluster pic file to : " << buffer <<endl;
  TFile *fClu = new TFile(buffer,"recreate");
  cout <<" 1 " << endl;
  fClu->cd();
  cout <<" 2 " << endl;
  for(unsigned int i=0;i<v_hClusP.size();i++)
    {
      //      (v_hClusP[i])->Write();
    }
  cout <<" 3 " << endl;
  for(unsigned int i=0;i<v_hClusR.size();i++)
    {
      //      (v_hClusR[i])->Write();
    }
  cout <<" 4 " << endl;
  fClu->Write();
  cout <<" 5 " << endl;
  fClu->Close();
  cout <<"about to open signal shapes file... "<<endl;
  sprintf(buffer,"%s/signalShapes%s.root",mFileBase,mFileName);
  cout <<"setting signal shapes file to : " << buffer <<endl;
  TFile *f1 = new TFile(buffer,"recreate");
  f1->cd();
  cout <<"writing hip..."<<endl;
  TCanvas ctmp;
  hIp->Draw();
  hIp->Write();
  f1->Write();
  for(int iD=0;iD<6;iD++)
    {
      radioPlotsTrackHits[iD]->Write();
      radioPlotsClusSizeR[iD]->Write();
      radioPlotsClusSizeP[iD]->Write();
      radioPlotsClusChargeR[iD]->Write();
      radioPlotsClusChargeP[iD]->Write();
      trkPhiProj[iD]->Write();
    }

  //  ctmp.SaveAs("tmp.png");

  //normalize
  for(int iB=1;iB<8;iB++)
    {
      exPulseMaxAdcNormP->SetBinContent(iB,exPulseMaxAdcNormP->GetBinContent(iB)/(float)pulseCounterP);
      exPulseSigP->SetBinContent(iB,exPulseSigP->GetBinContent(iB)/(float)pulseCounterP);
  
      exPulseMaxAdcNormR->SetBinContent(iB,exPulseMaxAdcNormR->GetBinContent(iB)/(float)pulseCounterR);
      exPulseSigR->SetBinContent(iB,exPulseSigR->GetBinContent(iB)/(float)pulseCounterR);
  
      exPulseMaxAdcNormTrackP->SetBinContent(iB,exPulseMaxAdcNormTrackP->GetBinContent(iB)/(float)pulseCounterTP);
      exPulseSigTrackP->SetBinContent(iB,exPulseSigTrackP->GetBinContent(iB)/(float)pulseCounterTP);
      
      exPulseMaxAdcNormTrackR->SetBinContent(iB,exPulseMaxAdcNormTrackR->GetBinContent(iB)/(float)pulseCounterTR);
      exPulseSigTrackR->SetBinContent(iB,exPulseSigTrackR->GetBinContent(iB)/(float)pulseCounterTR);
    }

  chargeCorrSum3->Write();
  chargeCorrMaxStrip->Write();
  chargeCorrMaxAdc->Write();

  exPulseMaxAdcNormP->Write();
  exPulseSigP->Write();
  
  exPulseMaxAdcNormR->Write();
  exPulseSigR->Write();
  
  exPulseMaxAdcNormTrackP->Write();
  exPulseSigTrackP->Write();
  exPulseMaxAdcNormTrackR->Write();
  exPulseSigTrackR->Write();
  numTracks->Write();
  numPointsPerTrack->Write();
  
  for(int xx=0; xx<22; xx++){

    disk1QuadA[xx]->Write();
  }

  
  for(int iD=0;iD<kFgtNumDiscs;iD++)
    {

      //Added for writing geoId, R, phi cluster histrograms
      
      clusterGeoId[iD]->Write();
      clustersR[iD]->Write();
      clustersP[iD]->Write();
     

      for(int iq=0;iq<4;iq++)
	{
	  numClustersR[iD*4+iq]->Write();
	  numClustersPhi[iD*4+iq]->Write();
	  numTrackHits[iD*4+iq]->Write();


	  maxTbCloseClusterP[iD*4+iq]->Write();
	  maxTbCloseClusterR[iD*4+iq]->Write();
	  maxAdcCloseClusterP[iD*4+iq]->Write();
	  maxSigTrackClusterP[iD*4+iq]->Write();
	  numFSigCloseClusterP[iD*4+iq]->Write();
	  numFirstHighCloseClusterP[iD*4+iq]->Write();
	  maxAdcCloseClusterP[iD*4+iq]->Write();
	  maxSigCloseClusterP[iD*4+iq]->Write();
	  secondToLastRatioCloseClusterP[iD*4+iq]->Write();
	  maxAdcCloseClusterR[iD*4+iq]->Write();
	  maxSigCloseClusterR[iD*4+iq]->Write();
	  numFSigCloseClusterR[iD*4+iq]->Write();
	  numFirstHighCloseClusterR[iD*4+iq]->Write();
	  maxAdcCloseClusterR[iD*4+iq]->Write();
	  maxSigCloseClusterR[iD*4+iq]->Write();
	  secondToLastRatioCloseClusterR[iD*4+iq]->Write();
	  firstTbSigCloseClusterR[iD*4+iq]->Write();
	  firstTbSigCloseClusterP[iD*4+iq]->Write();

	  chargeTrackClusterR[iD*4+iq]->Write();
	  chargeTrackClusterP[iD*4+iq]->Write();
	  chargeTrackClusterRvsP[iD*4+iq]->Write();
	  chargeTrackClusterPvsR[iD*4+iq]->Write();
	  numTrackClusterR[iD*4+iq]->Write();
	  numTrackClusterP[iD*4+iq]->Write();
	  numTrackSymCutClusterR[iD*4+iq]->Write();
	  numTrackSymCutClusterP[iD*4+iq]->Write();
	  chargeCorrTracks[iD*4+iq]->Write();
	  chargeCorrTracksRCut[iD*4+iq]->Write();
	  chargeTrackSymCutClusterR[iD*4+iq]->Write();
	  chargeTrackSymCutClusterP[iD*4+iq]->Write();
	  maxTbTrackClusterP[iD*4+iq]->Write();
	  maxTbTrackClusterR[iD*4+iq]->Write();
	  maxAdcTrackClusterP[iD*4+iq]->Write();
	  maxSigTrackClusterP[iD*4+iq]->Write();
	  numFSigTrackClusterP[iD*4+iq]->Write();
	  numFirstHighTrackClusterP[iD*4+iq]->Write();
	  maxAdcTrackClusterP[iD*4+iq]->Write();
	  maxSigTrackClusterP[iD*4+iq]->Write();
	  secondToLastRatioTrackClusterP[iD*4+iq]->Write();
	  maxAdcTrackClusterR[iD*4+iq]->Write();
	  maxSigTrackClusterR[iD*4+iq]->Write();
	  numFSigTrackClusterR[iD*4+iq]->Write();
	  numFirstHighTrackClusterR[iD*4+iq]->Write();
	  maxAdcTrackClusterR[iD*4+iq]->Write();
	  maxSigTrackClusterR[iD*4+iq]->Write();
	  secondToLastRatioTrackClusterR[iD*4+iq]->Write();
	  firstTbSigTrackClusterR[iD*4+iq]->Write();
	  firstTbSigTrackClusterP[iD*4+iq]->Write();

	}
    }

  for(int iD=0;iD<6;iD++)
    {
      for(int binAPVi=0;binAPVi<40;binAPVi++)
	{
	  APVfitChi2P[iD*40+binAPVi]->Write();
	  APVfitChi2R[iD*40+binAPVi]->Write();
	  APVfitAmpP[iD*40+binAPVi]->Write();
	  APVfitAmpR[iD*40+binAPVi]->Write();
	  APVfitT0P[iD*40+binAPVi]->Write();
	  APVfitT0R[iD*40+binAPVi]->Write();

          APVfirstTbSigCloseClusterP[iD*40+binAPVi]->Write();
          APVmaxAdcCloseClusterP[iD*40+binAPVi]->Write();
          APVmaxTbCloseClusterP[iD*40+binAPVi]->Write();
          APVnumFSigCloseClusterP[iD*40+binAPVi]->Write();
          APVnumFirstHighCloseClusterP[iD*40+binAPVi]->Write();
          APVmaxSigCloseClusterP[iD*40+binAPVi]->Write();
          APVsecondToLastRatioCloseClusterP[iD*40+binAPVi]->Write();
          APVfirstTbSigCloseClusterR[iD*40+binAPVi]->Write();
          APVmaxAdcCloseClusterR[iD*40+binAPVi]->Write();
          APVmaxTbCloseClusterR[iD*40+binAPVi]->Write();
          APVnumFSigCloseClusterR[iD*40+binAPVi]->Write();
          APVnumFirstHighCloseClusterR[iD*40+binAPVi]->Write();
          APVmaxSigCloseClusterR[iD*40+binAPVi]->Write();
          APVsecondToLastRatioCloseClusterR[iD*40+binAPVi]->Write();
        }
    }


  cout <<"writen and closed " << endl;


  ///---->   cRadioHits->SaveAs("radioPlotsHits.png");
  ///---->  cRadioNonHits->SaveAs("radioPlotsNonHits.png");
  

  sprintf(buffer,"clusterSizeR_%s",mFileName);
  TCanvas* cClusterSizeR=new TCanvas(buffer,buffer,1000,1500);

  cClusterSizeR->Divide(2,3);
  sprintf(buffer,"clusterSizePhi_%s",mFileName);
  TCanvas* cClusterSizePhi=new TCanvas(buffer,buffer,1000,1500);
  cClusterSizePhi->Divide(2,3);
  sprintf(buffer,"chargeCorr_%s",mFileName);
  TCanvas* cChargeCorr=new TCanvas(buffer,buffer,1000,1500);
  cChargeCorr->Divide(3,4);
  sprintf(buffer,"clusterChargePhi_%s",mFileName);
  TCanvas* cClusterChargePhi=new TCanvas(buffer,buffer,1000,1500);
  cClusterChargePhi->Divide(2,3);
  sprintf(buffer,"clusterChargeR_%s",mFileName);
  TCanvas* cClusterChargeR=new TCanvas(buffer,buffer,1000,1500);
  cClusterChargeR->Divide(2,3);

  //  TCanvas cIPProj;
  //  hIp->Draw("colz");

  ///---->  cIPProj.SaveAs("ipProj.png");
  hBx->Draw();
  hBx->Write();
  ///---->  cIPProj.SaveAs("hBx.png");
  hBy->Draw();
  hBy->Write();
  ///---->  cIPProj.SaveAs("hBy.png");
  hMx->Draw();
  hMx->Write();
  ///---->  cIPProj.SaveAs("hMx.png");
  hMy->Draw();
  hMy->Write();
  ///---->  cIPProj.SaveAs("hMy.png");

  
  hIpZ->Draw();
  hIpZ->Write();
  ///---->  cIPProj.SaveAs("ipZ.png");


  hIpDca->Draw();
  hIpDca->Write();
  ///---->  cIPProj.SaveAs("ipDca.png");

  hTrkZ->Draw();
  hTrkZ->Write();
  ///---->  cIPProj.SaveAs("hTrkZ.png");

  hResidua->Draw();
  hResidua->Write();
  hResiduaX->Draw();
  hResiduaX->Write();

  hResiduaY->Draw();
  hResiduaY->Write();
  hResiduaR->Draw();
  hResiduaR->Write();
  hResiduaP->Draw();
  hResiduaP->Write();
  ///---->  cIPProj.SaveAs("hResidua.png");

  hChi2->Draw();
  hChi2->Write();
  ///---->  cIPProj.SaveAs("chi2Dist.png");

  tpcFgtZVertexCorr->Draw("colz");
  tpcFgtZVertexCorr->Write();
  ///---->  cIPProj.SaveAs("tpcFgtCorr.png");

  tpcFgtZVertexCorr2->Draw("colz");
  tpcFgtZVertexCorr2->Write();

  tpcFgtZVtxDiff->Write();
  tpcFgtZVtxDiff2->Write();
  ///---->  cIPProj.SaveAs("tpcFgtCorr2.png");
  tpcFgtZVertexCorr3->Draw("colz");
  tpcFgtZVertexCorr3->Write();
  ///---->  cIPProj.SaveAs("trackTrackZCorr.png");


  for(Int_t iD=0;iD<kFgtNumDiscs;iD++)
    {
      cClusterSizeR->cd(iD+1);
      h_clusterSizeR[iD]->Draw();
      cClusterSizePhi->cd(iD+1);
      h_clusterSizePhi[iD]->Draw();
      for(int iq=0;iq<4;iq++)
	{
	  cChargeCorr->cd(iD*4+iq+1);
	  chargeCorr[iD*4+iq]->Draw("colz");
	  chargeCorr[iD*4+iq]->Write();
	  clusterSizeR[iD*4+iq]->Write();
	  clusterSizeP[iD*4+iq]->Write();
	}
      cClusterChargeR->cd(iD+1);
      h_clusterChargeR[iD]->Draw();
      cClusterChargePhi->cd(iD+1);
      h_clusterChargePhi[iD]->Draw();
      h_clusterChargeR[iD]->Write();
      h_clusterChargePhi[iD]->Write();

    }
  f1->Write();

  ///---->  cClusterSizeR->SaveAs("clusterSizeR.png");
  ///---->  cClusterSizePhi->SaveAs("clusterSizePhi.png");
  ///---->  cChargeCorr->SaveAs("chargeCorrelation.png");
  ///---->  cClusterChargeR->SaveAs("clusterChargeR.png");
  ///---->  cClusterChargePhi->SaveAs("clusterChargePhi.png");

  cout <<"saving .." <<endl;
  doNormalize(radioPlotsEffR, radioPlotsNonEffR);
  doNormalize(radioPlotsEffPhi, radioPlotsNonEffPhi);


  for(Int_t iD=0;iD<kFgtNumDiscs;iD++)
    {
      sprintf(buffer,"allCountsLooseDisk_%d",iD+1);
      TH2D* tmpAllCountsLoose=(TH2D*)radioPlotsEffLoose[iD]->Clone(buffer);
      tmpAllCountsLoose->Write();
      sprintf(buffer,"allClusterCountsDisk_%d",iD+1);
      TH2D* tmpAllClusterCounts=(TH2D*)radioPlotsEff[iD]->Clone(buffer);
      tmpAllClusterCounts->Write();
    }

  doNormalize(radioPlotsEffLoose, radioPlotsNonEffLoose);


  for(Int_t iD=0;iD<kFgtNumDiscs;iD++)
    {
      cRadio->cd(iD+1);
      sprintf(buffer,"allCountsDisk_%d",iD+1);
      TH2D* tmpAllCounts=(TH2D*)radioPlotsEff[iD]->Clone(buffer);
      tmpAllCounts->Write();
      radioPlotsEff[iD]->Add(radioPlotsNonEff[iD]);//all counts
      //      radioPlotsEff[iD]->Add(radioPlotsNonEff[iD],-1); //subtract non eff

      ////argh, overflow bins!!
      for(int nx=1;nx<radioPlotsEff[iD]->GetNbinsX()+1;nx++)
	{
	  for(int ny=1;ny<radioPlotsEff[iD]->GetNbinsY()+1;ny++)
	    {
	      Double_t denom=radioPlotsEff[iD]->GetBinContent(nx,ny);
	      if(denom>0 && (tmpAllCounts->GetBinContent(nx,ny)/denom)<=1.0)
		{
		  //		  cout <<" efficiency bin nx: " << nx << " ny: " << ny << ", num counts " << tmpAllCounts->GetBinContent(nx,ny) << " / " << denom << " : " << tmpAllCounts->GetBinContent(nx,ny)/denom <<endl;
		  radioPlotsEff[iD]->SetBinContent(nx,ny,tmpAllCounts->GetBinContent(nx,ny)/denom);
		  if(iD==m_effDisk)
		    {
		      //		      cout <<"chargeRatio is: " << chargeRatioInEffDisk->GetBinContent(nx,ny)<<endl;
		      chargeRatioInEffDisk->SetBinContent(nx,ny,chargeRatioInEffDisk->GetBinContent(nx,ny)/denom);
		      chargeAsymInEffDisk->SetBinContent(nx,ny,chargeAsymInEffDisk->GetBinContent(nx,ny)/denom);
		    }
		}
	      else
		{
		  //		  cout <<" efficiency bin nx: " << nx << " ny: " << ny << ", num counts " << tmpAllCounts->GetBinContent(nx,ny) << " / " << denom << " : 0.0" <<endl;
		  radioPlotsEff[iD]->SetBinContent(nx,ny,0.0);
		}
	    }
	}
      //      radioPlotsEff[iD]->Divide(tmpAllCounts);
      radioPlotsEff[iD]->SetMaximum(1.0);
      radioPlotsEff[iD]->Draw("colz");

      radioPlotsEffR[iD]->SetMaximum(1.0);
      radioPlotsEffPhi[iD]->SetMaximum(1.0);
      radioPlotsEffLoose[iD]->SetMaximum(1.0);
      cRadioR->cd(iD+1);
      radioPlotsEffR[iD]->Draw("colz");
      radioPlotsEffR[iD]->Write();
      cRadioPhi->cd(iD+1);
      radioPlotsEffPhi[iD]->Draw("colz");
      radioPlotsEffPhi[iD]->Write();
      cRadioLoose->cd(iD+1);
      radioPlotsEffLoose[iD]->Draw("colz");   
      radioPlotsEffLoose[iD]->Write();
      radioPlotsNonEffLoose[iD]->Write();
      radioPlotsNonEff[iD]->Write();
      cRPRatio->cd(iD+1);
      rPhiRatioPlots[iD]->Draw();
      cREff->cd(iD+1);
      f1->Write();
      
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

  ///---->  cRadio->SaveAs("radioPlotsEff.png");
  ///---->  cRadio->SaveAs("radioPlotsEff.pdf");

  ///---->  cRadioR->SaveAs("radioPlotsR.png");
  ///---->  cRadioPhi->SaveAs("radioPlotsPhi.png");
  ///---->  cRadioLoose->SaveAs("radioPlotsLoose.png");

  cRadio->cd(0);
  chargeRatioInEffDisk->Draw("colz");
  ///---->  cRadio->SaveAs("chargeRatioInEffDisk.png");
  chargeAsymInEffDisk->Draw("colz");
  ///---->  cRadio->SaveAs("chargeAsymInEffDisk.png");
  chargeCorrInEffDisk->Draw("colz");
  chargeCorrInEffDisk->Write();
  ///---->  cRadio->SaveAs("chargeCorrInEffDisk.png");
  hChargeAsym->Draw();
  ///---->  cRadio->SaveAs("chargeAsym.png");
  hChargeRatio->Draw();
  ///---->  cRadio->SaveAs("chargeRatio.png");


  ///---->  cREff->SaveAs("rEff.png");
  ///---->  cREff->SaveAs("rEff.pdf");

  ///---->  cRPRatio->SaveAs("rpRatio.png");
  ///---->  cRPRatio->SaveAs("rpRatio.pdf"
  f1->Write();
  ////this has to be the last thing!!!! Otherwise the histos become invalid and the code seg faults...
  f1->Close();
  myRootFile->Write();
  myRootFile->Close();

  pulsePictureFile->Write();
  cout <<"returning after finish" <<endl;
  return ierr;
};



// construct histograms


Int_t StFgtStraightPlotter::Init(){
  cout <<"init file name is " << mFileName<<endl;
  outTxtFile=new ofstream;
  outTxtFile->open("clusExpectations.txt");
  cluNotFoundTxt=new ofstream;
  cluNotFoundTxt->open("clusNotFound.txt");

  Char_t buffer[500];
  sprintf(buffer,"tmpCnvs%s",mFileName);
  mCanvas=new TCanvas(buffer,buffer);
  cout <<"made canvas with name " << buffer <<" pointer: " << mCanvas <<endl;
  sprintf(buffer,"%s/clusterEff%s.root",mFileBase,mFileName);

  myRootFile=new TFile(buffer,"RECREATE");
  sprintf(buffer,"%s/pulsePic%s.root",mFileBase,mFileName);

  pulsePictureFile=new TFile(buffer,"RECREATE");
  cout <<"making dirs.." <<endl;
  for(int i=0;i<6;i++){
    for(int iq=0;iq<4;iq++){
      sprintf(buffer,"d%d_quad%d",i,iq);
      pulsePictureFile->mkdir(buffer);
      pulsePictureFile->cd(buffer);
      for(int iA=0;iA<24;iA++)
	{
	  sprintf(buffer,"apv%d_R",iA);
	  gDirectory->mkdir(buffer);
	  sprintf(buffer,"apv%d_P",iA);
	  gDirectory->mkdir(buffer);
	}
      pulsePictureFile->cd();

    }}
  cout <<"done " << endl;
  myRootFile->cd();
  //  outTxtFile=new ofstream;
  //  outTxtFile->open("clusters.txt");
  sprintf(buffer,"pulseShape_%s",mFileName);
  int numTb=7;
  mPulseShapePtr=new TF1(buffer,"[0]*(x>[4])*(x-[4])**[1]*exp(-[2]*(x-[4]))+[3]",0,numTb);
  mPulseShapePtr->SetParName( 0, "C" );
  mPulseShapePtr->SetParName( 1, "a" );
  mPulseShapePtr->SetParName( 2, "b" );
  mPulseShapePtr->SetParName( 3, "ped" );
  mPulseShapePtr->SetParName( 4, "t0" );

  sprintf(buffer,"tempFitHist_%s",mFileName);
  mHistPtr=new TH1F(buffer,buffer,numTb,0,numTb);
  sprintf(buffer,"tempFitHist2_%s",mFileName);
  mHistPtr2=new TH1F(buffer,buffer,numTb,0,numTb);
  mHistPtr2->SetLineColor(kBlue);
  mHistPtr2->SetFillColor(kBlue);
  mHistPtr2->SetMarkerColor(kBlue);
  cout <<"producing my canvas... "<<endl;






  Int_t ierr = kStOk;
  sprintf(buffer,"chargeRatioInEffDisk_%s",mFileName);
  chargeRatioInEffDisk=new TH2D(buffer,buffer,NUM_EFF_BIN,-DISK_DIM,DISK_DIM,NUM_EFF_BIN,-DISK_DIM,DISK_DIM);
  chargeRatioInEffDisk->SetMaximum(2.0);
  sprintf(buffer,"chargeAsymInEffDisk_%s",mFileName);
  chargeAsymInEffDisk=new TH2D(buffer,buffer,NUM_EFF_BIN,-DISK_DIM,DISK_DIM,NUM_EFF_BIN,-DISK_DIM,DISK_DIM);
  chargeAsymInEffDisk->SetMaximum(1.0);
  sprintf(buffer,"chargeCorrInEffDisk_%s",mFileName);
  chargeCorrInEffDisk=new TH2D(buffer,buffer,500,0,50000,500,0,50000);
  sprintf(buffer,"chargeAsym_%s",mFileName);
  hChargeAsym=new TH1D(buffer,buffer,100,0,50);
  sprintf(buffer,"chargeRatio_%s",mFileName);
  hChargeRatio=new TH1D(buffer,buffer,100,0,50);

  sprintf(buffer,"chargeCorrSum3_%s",mFileName);
  chargeCorrSum3=new TH2D(buffer,buffer,500,0,50000,500,0,50000);
  sprintf(buffer,"chargeCorrMaxStrip_%s",mFileName);
  chargeCorrMaxStrip=new TH2D(buffer,buffer,500,0,20000,500,0,20000);
  sprintf(buffer,"chargeCorrMaxAdc_%s",mFileName);
  chargeCorrMaxAdc=new TH2D(buffer,buffer,500,0,5000,500,0,5000);
  chargeCorrTracks=new TH2D*[kFgtNumDiscs*4];
  chargeCorrTracksRCut=new TH2D*[kFgtNumDiscs*4];

  trkPhiProj=new TH1D*[kFgtNumDiscs];

  radioPlotsTrackHits=new TH2D*[kFgtNumDiscs];
  radioPlotsClusChargeR=new TH2D*[kFgtNumDiscs];
  radioPlotsClusSizeR=new TH2D*[kFgtNumDiscs];
  radioPlotsClusChargeP=new TH2D*[kFgtNumDiscs];
  radioPlotsClusSizeP=new TH2D*[kFgtNumDiscs];

  radioPlotsEff=new TH2D*[kFgtNumDiscs];
  radioPlotsNonEff=new TH2D*[kFgtNumDiscs];
  radioPlotsEffR=new TH2D*[kFgtNumDiscs];
  radioPlotsNonEffR=new TH2D*[kFgtNumDiscs];
  radioPlotsEffPhi=new TH2D*[kFgtNumDiscs];
  radioPlotsEffLoose=new TH2D*[kFgtNumDiscs];
  radioPlotsNonEffPhi=new TH2D*[kFgtNumDiscs];
  radioPlotsNonEffLoose=new TH2D*[kFgtNumDiscs];
  rPhiRatioPlots=new TH1D*[kFgtNumDiscs];

  sprintf(buffer,"pulseMaxAdcNormP_%s",mFileName);
  exPulseMaxAdcNormP=new TH1F(buffer,buffer,7,0,6);
  sprintf(buffer,"pulseSigNormP_%s",mFileName);
  exPulseSigP=new TH1F(buffer,buffer,7,0,6);
  sprintf(buffer,"pulseMaxAdcNormR_%s",mFileName);
  exPulseMaxAdcNormR=new TH1F(buffer,buffer,7,0,6);
  sprintf(buffer,"pulseSigNormR_%s",mFileName);
  exPulseSigR=new TH1F(buffer,buffer,7,0,6);

  sprintf(buffer,"pulseMaxAdcNormTrackP_%s",mFileName);
  exPulseMaxAdcNormTrackP=new TH1F(buffer,buffer,7,0,6);
  sprintf(buffer,"pulseSigNormTrackP_%s",mFileName);
  exPulseSigTrackP=new TH1F(buffer,buffer,7,0,6);

  sprintf(buffer,"pulseMaxAdcNormTrackR_%s",mFileName);
  exPulseMaxAdcNormTrackR=new TH1F(buffer,buffer,7,0,6);
  sprintf(buffer,"pulseSigNormTrackR_%s",mFileName);
  exPulseSigTrackR=new TH1F(buffer,buffer,7,0,6);


  pulseCounterP=0;
  pulseCounterR=0;

  pulseCounterTP=0;
  pulseCounterTR=0;




  createPlots(&numClustersR,kFgtNumDiscs*4,"numClustersR",101,0,100);
  createPlots(&numClustersPhi,kFgtNumDiscs*4,"numClustersPhi",101,0,100);
  createPlots(&numTrackHits,kFgtNumDiscs*4,"numTrackHits",101,0,100);
  sprintf(buffer,"numTracksPerEvent_%s",mFileName);
  numTracks=new TH1I(buffer,buffer,10,0,9);
  sprintf(buffer,"numPointsPerTrack_%s",mFileName);
  numPointsPerTrack=new TH1I(buffer,buffer,7,0,6);

  createPlots(&firstTbSigCloseClusterR,kFgtNumDiscs*4,"firstTbSigCloseClusterR",100,0,20);
  createPlots(&firstTbSigCloseClusterP,kFgtNumDiscs*4,"firstTbSigCloseClusterP",100,0,20);
  createPlots(&firstTbSigTrackClusterR,kFgtNumDiscs*4,"firstTbSigTrackClusterR",100,0,20);
  createPlots(&firstTbSigTrackClusterP,kFgtNumDiscs*4,"firstTbSigTrackClusterP",100,0,20);

  createPlots(&chargeTrackSymCutClusterR,kFgtNumDiscs*4,"chargeTrackSymCutClusterR",100,0,10000);
  createPlots(&chargeTrackSymCutClusterP,kFgtNumDiscs*4,"chargeTrackSymCutClusterP",100,0,10000);

  createPlots(&numTrackSymCutClusterR,kFgtNumDiscs*4,"numTrackSymCutClusterR",100,0,10000);
  createPlots(&numTrackSymCutClusterP,kFgtNumDiscs*4,"numTrackSymCutClusterP",100,0,10000);

  createPlots(&numTrackClusterR,kFgtNumDiscs*4,"numTrackClusterR",100,10,40);
  createPlots(&numTrackClusterP,kFgtNumDiscs*4,"numTrackClusterP",100,0,7);

  createPlots(&chargeTrackClusterR,kFgtNumDiscs*4,"chargeTrackClusterR",100,0,10000);
  createPlots(&chargeTrackClusterP,kFgtNumDiscs*4,"chargeTrackClusterP",100,0,10000);

  createPlots(&chargeTrackClusterRvsP,kFgtNumDiscs*4,"chargeTrackClusterRvsP",100,0,7);
  createPlots(&chargeTrackClusterPvsR,kFgtNumDiscs*4,"chargeTrackClusterPvsR",100,10,40);

  createPlots(&maxAdcTrackClusterR,kFgtNumDiscs*4,"maxAdcTrackClusterR",100,0,5000);
  createPlots(&maxAdcCloseClusterR,kFgtNumDiscs*4,"maxAdcCloseClusterR",100,0,5000);
  createPlots(&maxSigTrackClusterR,kFgtNumDiscs*4,"maxSigTrackClusterR",100,1,200);
  createPlots(&maxSigCloseClusterR,kFgtNumDiscs*4,"maxSigCloseClusterR",100,1,200);
  createPlots(&numFSigTrackClusterR,kFgtNumDiscs*4,"numFSigTrackClusterR",9,1,8);
  createPlots(&maxTbCloseClusterR,kFgtNumDiscs*4,"maxTbCloseClusterR",8,0,7);

  createPlots(&maxTbCloseClusterP,kFgtNumDiscs*4,"maxTbCloseClusterP",8,0,7);

  createPlots(&maxTbTrackClusterR,kFgtNumDiscs*4,"maxTbTrackClusterR",8,0,7);

  createPlots(&maxTbTrackClusterP,kFgtNumDiscs*4,"maxTbTrackClusterP",9,0,8);
  createPlots(&numFSigCloseClusterR,kFgtNumDiscs*4,"numFSigCloseClusterR",9,0,8);

  createPlots(&numFirstHighTrackClusterR,kFgtNumDiscs*4,"numFirstHighTrackClusterR",8,0,7);
  createPlots(&numFirstHighCloseClusterR,kFgtNumDiscs*4,"numFirstHighCloseClusterR",8,0,7);

  createPlots(&maxAdcTrackClusterP,kFgtNumDiscs*4,"maxAdcTrackClusterP",100,0,5000);
  createPlots(&maxAdcCloseClusterP,kFgtNumDiscs*4,"maxAdcCloseClusterP",100,0,5000);
  createPlots(&maxSigTrackClusterP,kFgtNumDiscs*4,"maxSigTrackClusterP",100,1,200);
  createPlots(&maxSigCloseClusterP,kFgtNumDiscs*4,"maxSigCloseClusterP",100,1,200);
  createPlots(&numFSigTrackClusterP,kFgtNumDiscs*4,"numFSigTrackClusterP",9,0,8);
  createPlots(&numFSigCloseClusterP,kFgtNumDiscs*4,"numFSigCloseClusterP",9,0,8);
  createPlots(&numFirstHighTrackClusterP,kFgtNumDiscs*4,"numFirstHighTrackClusterP",8,0,7);
  createPlots(&numFirstHighCloseClusterP,kFgtNumDiscs*4,"numFirstHighCloseClusterP",8,0,7);
  createPlots(&secondToLastRatioCloseClusterP,kFgtNumDiscs*4,"secondToLastRatioClosClusterP",100,0,5);

  createPlots(&secondToLastRatioCloseClusterR,kFgtNumDiscs*4,"secondToLastRatioClosClusterR",100,0,5);

  createPlots(&secondToLastRatioTrackClusterP,kFgtNumDiscs*4,"secondToLastRatioTrackClusterP",100,0,5);

  createPlots(&secondToLastRatioTrackClusterR,kFgtNumDiscs*4,"secondToLastRatioTrackClusterR",100,0,5);



  createPlots(&APVfitChi2P,kFgtNumDiscs*40,"APVfitChi2P",100,0,30);
  createPlots(&APVfitChi2R,kFgtNumDiscs*40,"APVfitChi2R",100,0,30);
  createPlots(&APVfitAmpP,kFgtNumDiscs*40,"APVfitAmpP",100,0,30);
  createPlots(&APVfitAmpR,kFgtNumDiscs*40,"APVfitAmpR",100,0,30);
  createPlots(&APVfitT0P,kFgtNumDiscs*40,"APVfitT0P",100,0,30);
  createPlots(&APVfitT0R,kFgtNumDiscs*40,"APVfitT0R",100,0,30);

  createPlots(&APVfirstTbSigCloseClusterP,kFgtNumDiscs*40,"APVfirstTbSigCloseClusterP",100,0,20);
  createPlots(&APVfirstTbSigCloseClusterR,kFgtNumDiscs*40,"APVfirstTbSigCloseClusterR",100,0,20);
  createPlots(&APVmaxAdcCloseClusterP,kFgtNumDiscs*40,"APVmaxAdcCloseClusterP",100,0,5000);
  createPlots(&APVmaxAdcCloseClusterR,kFgtNumDiscs*40,"APVmaxAdcCloseClusterR",100,0,5000);
  createPlots(&APVmaxTbCloseClusterP,kFgtNumDiscs*40,"APVmaxTbCloseClusterP",8,0,7);
  createPlots(&APVmaxTbCloseClusterR,kFgtNumDiscs*40,"APVmaxTbCloseClusterR",8,0,7);
  createPlots(&APVnumFSigCloseClusterP,kFgtNumDiscs*40,"APVnumFSigCloseClusterP",9,0,8);
  createPlots(&APVnumFSigCloseClusterR,kFgtNumDiscs*40,"APVnumFSigCloseClusterR",9,0,8);
  createPlots(&APVnumFirstHighCloseClusterP,kFgtNumDiscs*40,"APVnumFirstHighCloseClusterP",8,0,7);
  createPlots(&APVnumFirstHighCloseClusterR,kFgtNumDiscs*40,"APVnumFirstHighCloseClusterR",8,0,7);
  createPlots(&APVmaxSigCloseClusterP,kFgtNumDiscs*40,"APVmaxSigCloseClusterP",100,1,200);
  createPlots(&APVmaxSigCloseClusterR,kFgtNumDiscs*40,"APVmaxSigCloseClusterR",100,1,200);
  createPlots(&APVsecondToLastRatioCloseClusterP,kFgtNumDiscs*40,"APVsecondToLastRatioCloseClusterP",100,0,5);
  createPlots(&APVsecondToLastRatioCloseClusterR,kFgtNumDiscs*40,"APVsecondToLastRatioCloseClusterR",100,0,5);
  

  //Joes plots
  createPlots(&clusterGeoId, kFgtNumDiscs,"clusterGeoId",32000,0,32000);
  createPlots(&clustersR, kFgtNumDiscs,"clustersR", 500, 0, 50);
  createPlots(&clustersP, kFgtNumDiscs,"clustersP", 100, -3.14159, 3.14159);
  //createPlots(&disk1QuadA, 22,"disk1QuadA",128,0,128);
  for (Int_t iii=0;iii< 22;iii++)
    { 
      char buffer[100];
      sprintf(buffer, "%s_APV%d", "disk1QuadA_%s",iii,mFileName);
      disk1QuadA[iii]=new TH1I(buffer,buffer,128,0,128);
    }

  rEff=new TH1D*[kFgtNumDiscs];
  rNonEff=new TH1D*[kFgtNumDiscs];
  //    cout <<"ave2" << endl;

  clusterSizeP=new TH1D*[kFgtNumDiscs*4];
  clusterSizeR=new TH1D*[kFgtNumDiscs*4];
  chargeCorr=new TH2D*[kFgtNumDiscs*4];

  h_clusterSizeR=new TH1D*[kFgtNumDiscs];
  h_clusterSizePhi=new TH1D*[kFgtNumDiscs];

  h_clusterChargeR=new TH1D*[kFgtNumDiscs];
  h_clusterChargePhi=new TH1D*[kFgtNumDiscs];

  sprintf(buffer,"ProjToIP_%s",mFileName);
  hIp=new TH2D(buffer,buffer,50,-100,100,50,0,10);
  sprintf(buffer,"hBx_%s",mFileName);
  hBx=new TH1D(buffer,buffer,50,-100,100);
  sprintf(buffer,"hBy_%s",mFileName);
  hBy=new TH1D(buffer,buffer,50,-100,100);
  sprintf(buffer,"hMx_%s",mFileName);
  hMx=new TH1D(buffer,buffer,50,-100,100);
  sprintf(buffer,"hMy_%s",mFileName);
  hMy=new TH1D(buffer,buffer,50,-0.1,0.1);
  sprintf(buffer,"IP_Z_%s",mFileName);
  hIpZ=new TH1D(buffer,buffer,50,-100,100);

  sprintf(buffer,"ipDCA_%s",mFileName);
  hIpDca=new TH1D(buffer,buffer,50,-100,100);
  sprintf(buffer,"z_Vtx_From_trk_fit_%s",mFileName);
  hTrkZ=new TH1D(buffer,buffer,50,-100,100);
  sprintf(buffer,"residua_%s",mFileName);
  hResidua=new TH1D(buffer,buffer,100,0,2);
  sprintf(buffer,"residuaX_%s",mFileName);
  hResiduaX=new TH2D(buffer,buffer,100,-10,10,200,0,1.0);
  sprintf(buffer,"residuaY_%s",mFileName);
  hResiduaY=new TH2D(buffer,buffer,100,-40,-20,200,0,1.0);
  sprintf(buffer,"residuaR_%s",mFileName);
  hResiduaR=new TH2D(buffer,buffer,100,12,32,200,0,1.0);
  sprintf(buffer,"residuaP_%s",mFileName);
  hResiduaP=new TH2D(buffer,buffer,100,0,0.8,200,0,1.0);

  sprintf(buffer,"chi2_%s",mFileName);
  hChi2=new TH1D(buffer,buffer,50,0,2);
  sprintf(buffer,"tpc_fgt_corr_%s",mFileName);
  tpcFgtZVertexCorr=new TH2D(buffer,buffer,100,-120,120,100,-120,120);
  sprintf(buffer,"tpc_fgt_corr2_%s",mFileName);
  tpcFgtZVertexCorr2=new TH2D(buffer,buffer,100,-120,120,100,-120,120);
  sprintf(buffer,"tpc_fgt_corr3_%s",mFileName);
  tpcFgtZVertexCorr3=new TH2D(buffer,buffer,50,-50,50,50,-50,50);

  sprintf(buffer,"tpc_fgt_diff%s",mFileName);
  tpcFgtZVtxDiff=new TH1D(buffer,buffer,1000,-50,50);
  sprintf(buffer,"tpc_fgt_diff2%s",mFileName);
  tpcFgtZVtxDiff2=new TH1D(buffer, buffer,1000,-50,50);

  for(int iD=0;iD<kFgtNumDiscs;iD++)
    {

      for(int iq=0;iq<4;iq++)
	{
	  sprintf(buffer,"chargeCorrTracks_disk_%d_quad_%d_%s",iD,iq,mFileName);
	  chargeCorrTracks[iD*4+iq]=new TH2D(buffer,buffer,200,0,10000,200,0,10000);
	  sprintf(buffer,"chargeCorrTracksRCut_disk_%d_quad_%d_%s",iD,iq,mFileName);
	  chargeCorrTracksRCut[iD*4+iq]=new TH2D(buffer,buffer,200,0,10000,200,0,10000);
	}

      sprintf(buffer,"radioTrackHits_%d_%s",iD,mFileName);
      radioPlotsTrackHits[iD]=new TH2D(buffer,buffer,NUM_EFF_BIN,-DISK_DIM,DISK_DIM,NUM_EFF_BIN,-DISK_DIM,DISK_DIM);

      sprintf(buffer,"trkPhiProj_%d_%s",iD,mFileName);
      trkPhiProj[iD]=new TH1D(buffer,buffer,100,-3.5,3.5);

      sprintf(buffer,"radioClusterChargeR_%d_%s",iD,mFileName);
      radioPlotsClusChargeR[iD]=new TH2D(buffer,buffer,NUM_EFF_BIN,-DISK_DIM,DISK_DIM,NUM_EFF_BIN,-DISK_DIM,DISK_DIM);

      sprintf(buffer,"radioClusterSizeR_%d_%s",iD,mFileName);
      radioPlotsClusSizeR[iD]=new TH2D(buffer,buffer,NUM_EFF_BIN,-DISK_DIM,DISK_DIM,NUM_EFF_BIN,-DISK_DIM,DISK_DIM);

      sprintf(buffer,"radioClusterChargeP_%d_%s",iD,mFileName);
      radioPlotsClusChargeP[iD]=new TH2D(buffer,buffer,NUM_EFF_BIN,-DISK_DIM,DISK_DIM,NUM_EFF_BIN,-DISK_DIM,DISK_DIM);

      sprintf(buffer,"radioClusterSizeP_%d_%s",iD,mFileName);
      radioPlotsClusSizeP[iD]=new TH2D(buffer,buffer,NUM_EFF_BIN,-DISK_DIM,DISK_DIM,NUM_EFF_BIN,-DISK_DIM,DISK_DIM);

      sprintf(buffer,"radioDiskEff_%d_%s",iD,mFileName);
      radioPlotsEff[iD]=new TH2D(buffer,buffer,NUM_EFF_BIN,-DISK_DIM,DISK_DIM,NUM_EFF_BIN,-DISK_DIM,DISK_DIM);

      sprintf(buffer,"radioDiskEffR_%d_%s",iD,mFileName);
      radioPlotsEffR[iD]=new TH2D(buffer,buffer,NUM_EFF_BIN,-DISK_DIM,DISK_DIM,NUM_EFF_BIN,-DISK_DIM,DISK_DIM);

      sprintf(buffer,"radioDiskEffPhi_%d_%s",iD,mFileName);
      radioPlotsEffPhi[iD]=new TH2D(buffer,buffer,NUM_EFF_BIN,-DISK_DIM,DISK_DIM,NUM_EFF_BIN,-DISK_DIM,DISK_DIM);

      sprintf(buffer,"radioDiskEffLoose_%d_%s",iD,mFileName);
      radioPlotsEffLoose[iD]=new TH2D(buffer,buffer,NUM_EFF_BIN,-DISK_DIM,DISK_DIM,NUM_EFF_BIN,-DISK_DIM,DISK_DIM);
      //      cout <<"1" <<endl;
      sprintf(buffer,"rEff_%d_%s",iD,mFileName);
      rEff[iD]=new TH1D(buffer,buffer,100,0,DISK_DIM);
      sprintf(buffer,"rNonEff_%d_%s",iD,mFileName);
      rNonEff[iD]=new TH1D(buffer,buffer,100,0,DISK_DIM);
      sprintf(buffer,"clusterSizeR_Disk_%d_%s",iD,mFileName);
      h_clusterSizeR[iD]=new TH1D(buffer,buffer,20,0,20);
      h_clusterSizeR[iD]->SetFillColor(kYellow);
      sprintf(buffer,"clusterSizePhi_Disk_%d_%s",iD,mFileName);
      h_clusterSizePhi[iD]=new TH1D(buffer,buffer,20,0,20);
      h_clusterSizePhi[iD]->SetFillColor(kYellow);
      sprintf(buffer,"clusterChargeR_Disk_%d_%s",iD,mFileName);
      h_clusterChargeR[iD]=new TH1D(buffer,buffer,100,0,5000);
      h_clusterChargeR[iD]->SetFillColor(kYellow);

      sprintf(buffer,"clusterChargePhi_Disk_%d_%s",iD,mFileName);
      h_clusterChargePhi[iD]=new TH1D(buffer,buffer,100,0,5000);
      h_clusterChargePhi[iD]->SetFillColor(kYellow);
      //      cout <<"2" <<endl;
      for(int nx=0;nx<radioPlotsEff[iD]->GetNbinsX();nx++)
	{
	  for(int ny=0;ny<radioPlotsEff[iD]->GetNbinsY();ny++)
	    {
	      //	       radioPlotsEff[iD]->SetBinContent(nx,ny,0.1);//so that there is no divide by zero
	    }
	}
      //      cout <<"3" <<endl;
      for(int iq=0;iq<4;iq++)
	{
	  sprintf(buffer,"r_phi_ChargeCorrelationInDisk_%d_quad_%d_%s",iD+1,iq,mFileName);
	  chargeCorr[iD*4+iq]=new TH2D(buffer,buffer,200,0,70000,200,0,70000);
	  sprintf(buffer,"clusterSizeRInDisk_%d_quad_%d_%s",iD+1,iq,mFileName);
	  clusterSizeR[iD*4+iq]=new TH1D(buffer,buffer,100,0,100);
	  sprintf(buffer,"clusterSizePInDisk_%d_quad_%d_%d",iD+1,iq,mFileName);
	  clusterSizeP[iD*4+iq]=new TH1D(buffer,buffer,100,0,100);
	}
      sprintf(buffer,"radioDiskNonEff_%d_%s",iD,mFileName);
      radioPlotsNonEff[iD]=new TH2D(buffer,buffer,NUM_EFF_BIN,-DISK_DIM,DISK_DIM,NUM_EFF_BIN,-DISK_DIM,DISK_DIM);
      //      cout <<" created non eff histo " << endl;
      sprintf(buffer,"radioDiskNonEffR_%d_%s",iD,mFileName);
      radioPlotsNonEffR[iD]=new TH2D(buffer,buffer,NUM_EFF_BIN,-DISK_DIM,DISK_DIM,NUM_EFF_BIN,-DISK_DIM,DISK_DIM);
      sprintf(buffer,"radioDiskNonEffPhi_%d_%s",iD,mFileName);
      radioPlotsNonEffPhi[iD]=new TH2D(buffer,buffer,NUM_EFF_BIN,-DISK_DIM,DISK_DIM,NUM_EFF_BIN,-DISK_DIM,DISK_DIM);
      sprintf(buffer,"radioDiskNonEffLoose_%d_%s",iD,mFileName);
      radioPlotsNonEffLoose[iD]=new TH2D(buffer,buffer,NUM_EFF_BIN,-DISK_DIM,DISK_DIM,NUM_EFF_BIN,-DISK_DIM,DISK_DIM);
      for(int nx=0;nx<rEff[iD]->GetNbinsX();nx++)
	{
	  //	   rEff[iD]->SetBinContent(nx,0.1);
	}
      sprintf(buffer,"rPhiRatio_%d_%s",iD,mFileName);
      rPhiRatioPlots[iD]=new TH1D(buffer,buffer,100,-2,10);
    }

  return ierr;
};
ClassImp(StFgtStraightPlotter);
