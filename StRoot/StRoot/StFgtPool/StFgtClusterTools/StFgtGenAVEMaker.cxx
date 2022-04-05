///////estimate efficiencies of disks using straight line
#include "StFgtGenAVEMaker.h"
#include "StRoot/StEvent/StFgtCollection.h"
#include "StRoot/StEvent/StFgtHitCollection.h"
#include "StRoot/StEvent/StFgtHit.h"
#include "StRoot/StFgtUtil/geometry/StFgtGeom.h"
#include "StRoot/StEvent/StEvent.h"
#include "StRoot/StEvent/StEventInfo.h"
#include "StRoot/StFgtUtil/geometry/StFgtGeom.h"

#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuPrimaryVertex.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StarClassLibrary/StThreeVectorF.hh"



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

//#define COSMIC
#ifdef COSMIC
#include "StFgtCosmicAlignment.h"
#endif
#define MAX_PHI_DIFF 0.5//the maximal difference in phi a track is allowed 
#define MAX_CLUSTERS 20
#define CHARGE_MEASURE clusterCharge
#define MAX_DIST_STRIP_R 0.7
#define MAX_DIST_STRIP_PHI 0.03
#include "StRoot/StFgtUtil/geometry/StFgtGeom.h"
//#define LEN_CONDITION
#define PULSE_CONDITION
#define DO_PRINT
#ifndef COSMIC
#define VERTEX_CUT 50
#else
#define VERTEX_CUT 100000000
#endif

#include "StRoot/StEvent/StEvent.h"
#include "StRoot/StEvent/StFgtCollection.h"
//disk for which I want to calculate efficieny


#define MAX_CHARGE_RATIO
#define MIN_CHARGE_RATIO

//#define DISK_EFF 2
#define QUAD_EFF 1
#define MY_PI 3.14159265359
//#define  REFIT_WITH_VERTEX
//#define FIT_WITH_VERTEX


#define MAX_DIST_CHI 1.0
#define MAX_DIST2_EFF 1.0
#define MAX_DIST2 1.0

#define MIN_NUM_POINTS 4
#define DISK_DIM 40
#ifndef COSMIC
#define NUM_EFF_BIN 30
#else
#define NUM_EFF_BIN 100
#endif

Bool_t StFgtGenAVEMaker::fitTheStrip(generalStrip* pStrip, generalStrip* pStripOtherLayer,float* amp, float* t0, float* chi2Ndf, int iD, int iq, int apvBin, Char_t layer)
{
  if(fitCounter>2000)
    return true;
  fitCounter++;
  char buffer[100];
  sprintf(buffer,"d%d_quad%d",iD,iq);
  pulsePictureFile->cd();
  pulsePictureFile->cd(buffer);
  if(layer=='R')
    sprintf(buffer,"apv%d_R",apvBin);
  else
    sprintf(buffer,"apv%d_P",apvBin);
  //  cout <<"changing to " << buffer <<endl;
  gDirectory->cd(buffer);
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


template<class T> void createPlots(T*** pH, int numH, const char* nameBase, int numBin, int first, int last)
{
  char buffer[200];
  (*pH)=new T*[numH];
  
  if (numH==22)
    {
      for(int iD=0;iD<numH;iD++)
        {  
	  sprintf(buffer, "%s_APV%d", nameBase, iD);
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
              sprintf(buffer,"%s_disc%d_quad%d",nameBase,iD+1,iQ);
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
	      sprintf(buffer, "%s_disc%d", nameBase, iD+1);
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
		  sprintf(buffer,"%s_disc%d_quad%d_apvBIN%d",nameBase,iD+1,iQ,binAPVi);
		  (*pH)[iD*40+binAPVi]=new T(buffer,buffer,numBin,first,last);
		}
	    }
	}
    }
}


template void createPlots(TH1I*** pH, int numH,const char* nameBase, int numBin, int first, int last);
template void createPlots(TH1F*** pH, int numH, const char* nameBase, int numBin, int first, int last);

void StFgtGenAVEMaker::doNormalize(TH2D** hEff, TH2D** hNonEff)
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



void StFgtGenAVEMaker::saveSigs(Double_t* sigR, Double_t* sigP, Double_t r, Double_t phi, Int_t maxR, Int_t maxPhi, Int_t discId, Int_t quad)
{
  Char_t buffer[200];
  sprintf(buffer,"Sig_Disc%d_quad%d_Phi_Evt_%d_R_%f_Phi_%f",discId,quad,evtNr,r,phi);
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
  sprintf(buffer,"Sig_Disc%d_quad%d_R_Evt_%d_R_%f_Phi_%f",discId,quad,evtNr,r,phi);
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

pair<double,double> StFgtGenAVEMaker::getDca(  vector<AVTrack>::iterator it)
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


Double_t StFgtGenAVEMaker::findClosestStrip(Char_t layer, double ord, Int_t iD, Int_t iQ)
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
void StFgtGenAVEMaker::fillStripHistos(Float_t r, Float_t phi, Int_t iD, Int_t iq)
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
		  if(pStrip.seedType==kFgtSeedType1|| pStrip.seedType==kFgtSeedType2 || pStrip.seedType==kFgtSeedType3 || pStrip.seedType==kFgtClusterPart || pStrip.seedType==kFgtClusterEndUp ||pStrip.seedType==kFgtClusterEndDown)
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
		  if(pStrip.seedType==kFgtSeedType1|| pStrip.seedType==kFgtSeedType2 || pStrip.seedType==kFgtSeedType3 || pStrip.seedType==kFgtClusterPart || pStrip.seedType==kFgtClusterEndUp ||pStrip.seedType==kFgtClusterEndDown)
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
      
      if(APVfirstTbSigP>-1 && APVmaxPAdc>-1 && APVmaxPTb>-1 && APVnumFSigP>-1 && APVfirstFSigTbP>-1 && APVmaxSigAdcP>-1 && APVsecondToLastRatioP>-1)
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

      if(APVfirstTbSigR>-1 && APVmaxRAdc>-1 && APVmaxRTb>-1 && APVnumFSigR>-1 && APVfirstFSigTbR>-1 && APVmaxSigAdcR>-1 && APVsecondToLastRatioR>-1)
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
      {///this is what is plotted lateron
	chargeCorr[iD*4+iq]->Fill(clusterChargeR,clusterChargeP);
      }
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
//Bool_t StFgtGenAVEMaker::isGoodHit(


//is there something where we expect it? Again, here phi is relative to the quadrant!!
Bool_t StFgtGenAVEMaker::isSomewhatEff(Float_t r, Float_t phi, Int_t iD, Int_t iq)
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
pair<Double_t,Double_t> StFgtGenAVEMaker::findCluChargeSize(Int_t iD,Char_t layer, Double_t ordinate)
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

Double_t StFgtGenAVEMaker::findClosestPoint(float mx, float bx, float my, float by, double xE, double yE, Int_t iD)
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
	  ////	  cout <<"we have " << x <<", " << y <<endl;
	  Double_t mDist=(x-xE)*(x-xE)+(y-yE)*(y-yE);

	  if(mDist<dist2)
	    {
	      dist2=mDist;
	      //recalculate distance with proper alignment
	      float tmpX, tmpY,tmpZ,tmpP,tmpR;
#ifdef COSMIC
	      getAlign(iD,phi,r,tmpX,tmpY,tmpZ,tmpP,tmpR);
	      Double_t xExpUpdate=mx*tmpZ+bx;
	      Double_t yExpUpdate=my*tmpZ+by;
	      //	      cout<<"tmpx: " << tmpX <<" old: " << x <<" xE old: " << xE << " updated: " << xExpUpdate;
	      //	      cout<<"tmpy: " << tmpY <<" old: " << y <<" yE old: " << yE << " updated: " << yExpUpdate<<endl;

	      mDist=(tmpX-xExpUpdate)*(tmpX-xExpUpdate)+(tmpY-yExpUpdate)*(tmpY-yExpUpdate);
#endif
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
Short_t StFgtGenAVEMaker::getQuadFromCoo(Double_t x, Double_t y)
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


Bool_t StFgtGenAVEMaker::printArea1D(Int_t iD,Int_t iq, Int_t centerGeoId)
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
Bool_t StFgtGenAVEMaker::printArea(Float_t r, Float_t phi, Int_t iD, Int_t iq)
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
pair<Double_t,Double_t> StFgtGenAVEMaker::getChargeRatio(Float_t r, Float_t phi, Int_t iD, Int_t iq)
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






Bool_t StFgtGenAVEMaker::getTrack(vector<AVPoint>& points, Double_t ipZ)
{
  //  (*outTxtFile) <<"getTrack" <<endl;
  //   cout <<"get track" <<endl;
  ipZ=-9999; //get ourselves
  ipZ=vtxZ;
  vector<AVPoint>::iterator iter=points.begin();
  Double_t A = 0, B = 0, Cx = 0, Cy = 0, D = 0, Ex = 0, Ey = 0;
  Double_t dist = 0;

  //LOG_INFO << " --------------------------> calling makeLine with " << points.size() << " 0x" << std::hex << discBitArray << std::dec << endm;
  //  cout <<"get track2" <<endl;
  //do the alignment		   

#ifdef COSMIC
  float tmpX, tmpY,tmpZ,tmpP,tmpR;

  for( ; iter != points.end(); ++iter ){
    getAlign(iter->dID,iter->phi,iter->r,tmpX,tmpY,tmpZ,tmpP,tmpR);
    //    cout <<"before: " << iter->phi << ", " << iter->r <<" " << iter->x <<" " << iter->y << ", " << iter->z <<endl;
    iter->phi=tmpP;
    iter->r=tmpR;
    iter->x=tmpX;
    iter->y=tmpY;
    iter->z=tmpZ;
    //    cout <<"after: " << iter->phi << ", " << iter->r <<" " << iter->x <<" " << iter->y << ", " << iter->z <<endl;
  }
#endif
   
  //  cout <<" we have " << points.size() << " points " << endl;
  for( iter=points.begin(); iter != points.end(); ++iter ){

    Double_t x = iter->x;
    Double_t y = iter->y;
    Double_t z = iter->z;
    //    cout <<"x: " << x << " y: " << y << " z: " << z <<endl;
    A += z*z;
    B += z;
    Cx += x*z;
    Cy += y*z;
    Ex += x;
    Ey += y;

    //    cout << "*** Point located at " << x << ' ' << y << ' ' << z << " Disk: " << iter->dID <<endl;
  }
  //    cout <<"ipZ: " << ipZ <<endl;
  //  cout <<"get track3" <<endl;
  D = points.size();
  //invalid is -999
#ifdef FIT_WITH_VERTEX
      if(muDst && ipZ>-100 && ipZ<100)
        {
          A+=ipZ*ipZ;
          B+=ipZ;
          D++;
        }
#endif
      //    cout << "*** Consts " << A << ' ' << B << ' ' << Cx << ' ' << Cy << ' ' << D << ' ' << Ex << ' ' << Ey << endl;
  Double_t denom = D*A - B*B;
  if( denom ){
    Double_t bx = (-B*Cx + A*Ex)/denom;
    Double_t by = (-B*Cy + A*Ey)/denom;
    Double_t mx = ( D*Cx - B*Ex)/denom;
    Double_t my = ( D*Cy - B*Ey)/denom;
    //    cout <<"bx: " << bx <<" by: " << by <<" mx: " << mx << " my: " << my <<endl;    
    for( iter = points.begin(); iter != points.end(); ++iter ){
      //  cout << "--- Location at each disc, " << iter->dID <<" "
      //        << "X: " << mx*iter->z+bx << " vs " << iter->x << ' '
      //		    << "Y: " << my*iter->z+by << " vs " << iter->y << " " <<" charge r: " << iter->rCharge <<" phi: " << iter->phiCharge <<
      //	" r: " << iter->r <<" phi: " << iter->phi <<endl;
      (*outTxtFile) << "--- Location at each disc, " << iter->dID <<" "
              << "X: " << mx*iter->z+bx << " vs " << iter->x << ' '
		    << "Y: " << my*iter->z+by << " vs " << iter->y << " " <<" charge r: " << iter->rCharge <<" phi: " << iter->phiCharge <<
	" r: " << iter->r <<" phi: " << iter->phi <<endl;
    };
    dist = 0;
    //  cout <<"get track4" <<endl;
    vector<AVPoint> redPoints;
    // find closest points...
    for(int iDx=0;iDx<6;iDx++)
      {
	Double_t minDistance=9999;
	Int_t pointIdx=-1;
	Int_t cnt=0;
	for( iter = points.begin(); iter != points.end(); ++iter ){
	  Int_t dId=iter->dID;
	  Double_t distX=fabs((iter->z*mx+bx)-(iter->x));
	  Double_t distY=fabs((iter->z*my+by)-(iter->y));
	  //	  	  cout <<"distX: " << distX <<" distY: " << distY <<endl;
	  Double_t distance=distX*distX+distY*distY;
	  //	  	  cout << " got distance " << distance << endl;
	  if((iDx==dId)&&(distance<minDistance))
	    {
	      minDistance=distance;
	      pointIdx=cnt;
	      //	      	      cout <<"min dist now:" << minDistance<<" for this dik " << iDx <<" pointIdx: " << pointIdx <<endl;
	    }
	  cnt++;
	}
	if(pointIdx>=0)
	  {
	    //	    	    cout <<"pushing back " << pointIdx <<endl;
	    redPoints.push_back(points[pointIdx]);
	  }
      }//end of looping over discs

 //// reduced points
    //////have to do a refit now..
    //       cout <<"doing refit... " <<endl;
//  cout <<"get track5" <<endl;
    A=0.0;
    B=0.0;
    Cx=0.0;
    Cy=0.0;
    Ex=0.0;
    Ey=0.0;

    for(vector<AVPoint>::iterator iterR=redPoints.begin() ; iterR != redPoints.end(); ++iterR )
      {
	Double_t x = iterR->x;
	Double_t y = iterR->y;
	Double_t z = iterR->z;
	
	A += z*z;
	B += z;
	Cx += x*z;
	Cy += y*z;
	Ex += x;
	Ey += y;
      }
    D = redPoints.size();
#ifdef REFIT_WITH_VERTEX
          if(muDst && ipZ>-100 && ipZ<100)
            {
              A+=ipZ*ipZ;
              B+=ipZ;
              D++;
            }
#endif
    Double_t denom = D*A - B*B;
    //      cout <<"get track6" <<endl;
    if( denom ){
      bx = (-B*Cx + A*Ex)/denom;
      by = (-B*Cy + A*Ey)/denom;
     mx = ( D*Cx - B*Ex)/denom;
     my = ( D*Cy - B*Ey)/denom;
    }
    //      cout <<"after refit: bx: " << bx <<" by: " << by <<" mx: " << mx << " my: " << my <<endl;    
    //      cout <<"we have refit line: "<< bx << " by: " << by <<" mx: " << mx << " my: " << my <<endl;
    ///end of refit

    for(vector<AVPoint>::iterator iterR = redPoints.begin(); iterR != redPoints.end(); ++iterR )
      {
	Double_t distX, distY;
	distX=fabs((iterR->z*mx+bx)-(iterR->x));
	distY=fabs((iterR->z*my+by)-(iterR->y));
	//		cout <<"distX: " << distX <<" distY: " << distY <<endl;
	dist += (distX*distX+distY*distY);
	//	cout <<"adding " << (distX*distX+distY*distY) <<" to chi2: " << endl;
	//	cout << "*** DistSq " << dist << endl;
      }
    //          cout <<"get track8" <<endl;
    //        cout <<"dist : " << dist <<" D: "<< D <<endl;
    dist/=D;
    //    cout <<" end chi2: " <<dist <<endl;
    m_tracks.push_back(AVTrack(mx,my,bx,by,ipZ,dist));
    //  cout <<" we have " <<m_tracks.size() <<" track now " <<endl;
    
    points.clear();
    for(vector<AVPoint>::iterator it=redPoints.begin();it!=redPoints.end();it++)
      {
	points.push_back(*it);
      }
    // copy to pointer, if one provided
    ///      if( linePtr )
    ////         *linePtr = line;

    // #ifdef DEBUG
    //       cout << "*** Event " << GetEventNumber() << " final distSQ " << dist << endl;
    // #endif

    // add to vector, if thes OK
    ///      if( line.res > mFitThres )
    ///         mLineVec.pop_back();

    //  cout <<"get track9" <<endl;
    //Double_t zIpExpX0=0;//x(D6Pos-(xD6/xD1)*D1Pos)*1/(1-xD6/xD1);
			  ///at y = 0
    //Double_t zIpExpY0=0;//(D6Pos-yD6/yD1*D1Pos)*1/(1-yD6/yD1);

    for(vector<AVPoint>::iterator iter = points.begin(); iter != points.end(); ++iter ){
      //               cout << "--- Location at each disc at z: " << iter->z << " "
      //                   << "X: " << mx*iter->z+bx << " vs " << iter->x << ' '
      //                 	   << "Y: " << my*iter->z+by << " vs " << iter->y << " "
      //      		<< " charge phi: " << iter->phiCharge <<" rcharge: "<< iter->rCharge <<endl;

    };
    //    cout <<endl<<endl;
    //        cout <<"dist again:  " <<dist <<endl;
    vector<AVTrack>::iterator it_lastTrack=m_tracks.end();
    it_lastTrack--;
    pair<double,double> dca=getDca(it_lastTrack);
    //        cout <<"mx: " << it_lastTrack->mx <<" ax: " << it_lastTrack->ax <<" my: " << it_lastTrack->my <<" ay: " << it_lastTrack->ay <<endl;
    Double_t vertZ = (  -( it_lastTrack->mx*it_lastTrack->ax + it_lastTrack->my*it_lastTrack->ay )/(it_lastTrack->mx*it_lastTrack->mx+it_lastTrack->my*it_lastTrack->my));
    (it_lastTrack)->trkZ=vertZ;
    it_lastTrack->dca=dca.second;
    it_lastTrack->ipZ=dca.first;
    //        cout <<"dca: " << dca.first <<" vertZ: " << vertZ <<endl;
    //  cout <<"get track10" <<endl;
    //        cout <<"dist: "<< dist <<" vertZ: " << vertZ <<endl;
    if(dist< MAX_DIST_CHI && fabs(vertZ)<VERTEX_CUT)// && fabs(bx)<40 && fabs(by)<40)
      {
	//		cout <<" track accepted " <<endl;
	//  cout <<"get track10-10" <<endl;
	//		cout <<"track cuts passed " <<endl;
	set<Short_t> disksHit;
	//	cout <<"track has " << points.size() <<" points " <<endl;
	for(vector<AVPoint>::iterator iterP = points.begin(); iterP != points.end(); ++iterP ){
	  //	  cout <<"testing" << endl;
	  //	  cout <<"get track10-3, " << iterP->dID<<" quad: " << iterP->quadID << endl;
	  //	  cout <<" filling disk " << iterP->dID <<" quad " << iterP->quadID <<" with r charge: " << iterP->rCharge <<" phic " << iterP->phiCharge<<endl;
	  //	  if(iterP->r>20)
	    {
	      chargeCorr[iterP->dID*4+iterP->quadID]->Fill(iterP->rCharge,iterP->phiCharge);
	      clusterSizeP[iterP->dID*4+iterP->quadID]->Fill(iterP->phiSize);
	      clusterSizeR[iterP->dID*4+iterP->quadID]->Fill(iterP->rSize);
	    }
	  //	  cout <<"get track10-31" <<endl;
	  h_clusterChargeR[iterP->dID]->Fill(iterP->rCharge);
	  //	  cout <<"get track10-4" <<endl;
	  h_clusterChargePhi[iterP->dID]->Fill(iterP->phiCharge);
	  h_clusterSizeR[iterP->dID]->Fill(iterP->rSize);
	  //  cout <<"get track10-5" <<endl;
	  h_clusterSizePhi[iterP->dID]->Fill(iterP->phiSize);
	  disksHit.insert(iterP->dID);
	  //  cout <<"get track10-6" <<endl;
	}
	//  cout <<"get track11" <<endl;
	for(int i=0;i<6;i++)
	  {
	    Double_t xExp=mx*getLocDiscZ(i)+bx;
	    Double_t yExp=my*getLocDiscZ(i)+by;
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
	    (*outTxtFile) << " looking at Track with chi2/ndf *[cm}: " << it_lastTrack->chi2 << " z vertex: " << it_lastTrack->ipZ << endl;

	    //this snipplet is only good for the disks in which we don't look for efficiencies, otherwise it overcounts the nonEff
	    //so either use the findClosest point for all, or check if we look at efficient disk or not...
	    /*	    if(disksHit.find(i)!=disksHit.end())
	      {
		radioPlotsEff[i]->Fill(xExp,yExp);
	      }
	    else//not efficient
	      {
		radioPlotsNonEff[i]->Fill(xExp,yExp);

		}*/
	    ///for disk for which we want to compute effi:

	    //	    cout <<"fill strip histos " << endl;
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
	    //	    cout <<" closest point is " << closestPoint <<" away " <<endl;
		//		 (*outTxtFile) <<"closest point t " << xExp <<" , " << yExp << " is : " << closestPoint << " away " << endl;

	    if(findClosestPoint(mx,bx,my,by,xExp,yExp,i)<MAX_DIST2_EFF)
		  {
		    //		    		    cout <<"found point on eff disk, x: " << xExp <<" y: " << yExp <<", dist: " <<closestPoint <<endl;
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
    //    cout <<" returning true " <<endl;
    return true;
  }
  //  cout <<" false... " <<endl;
  return false;
};

Double_t StFgtGenAVEMaker::getRPhiRatio(vector<generalCluster>::iterator hitIterBegin, vector<generalCluster>::iterator hitIterEnd)
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
Int_t StFgtGenAVEMaker::Make()
{
  //    cout <<"ave make " <<endl;
  Int_t ierr = kStOk;
  (*outTxtFile) <<"----------------------------- Event Nr: " << evtNr<<" -----------------" <<endl;
  StFgtGeneralBase::Make();
  unsigned int oldNumTracks=m_tracks.size();

  //  cout <<"eff disk : " << m_effDisk <<endl;

  //    cout <<" mtracks size: " << m_tracks.size() << " oldnum: "<< oldNumTracks <<endl;
  for(int i=0;i<6;i++)
    {
      //      cout <<"there are " << pClusters[i]->size() << " clusters in disk " << i <<endl;
      for(int j=0;j<pClusters[i]->size();j++)
	{
	  /*	  if((*(pClusters[i]))[j].layer=='R')
	    cout <<"R layer, ";
	  else
	  cout <<"Phi layer, ";*/
	  Int_t seedType=(*(pClusters[i]))[j].seedType;
	  Double_t posPhi=(*(pClusters[i]))[j].posPhi;
	  Double_t posR=(*(pClusters[i]))[j].posR;
	  Int_t clusSize=(*(pClusters[i]))[j].clusterSize;
	  Double_t charge=(*(pClusters[i]))[j].clusterCharge;
	  Int_t cntGeoId=(*(pClusters[i]))[j].centralStripGeoId;
	  //	  cout <<"cluster pos phi: " << posPhi <<" posR: " << posR <<" clusSize: " << clusSize << " charge: "<< charge <<" geoId: "<< cntGeoId <<" seedT : " << seedType<<endl;
	}
    }

  for(int iD=0;iD<6;iD++)
    {
          (*outTxtFile) <<" In Disc " << iD << " we have clusters with geo id: ";

      //      cout <<" In Disc " << iD << " we have clusters with geo id: ";
      vector<generalCluster>::iterator hitIter;
      vector<generalCluster> &hitVec=*(pClusters[iD]);

      for(hitIter=hitVec.begin();hitIter != hitVec.end();hitIter++)
	{
	  (*outTxtFile) << hitIter->centralStripGeoId << ", ";
	  clusterGeoId[iD]->Fill(hitIter->centralStripGeoId);
	  //Adding to fill APV noise histograms for disk 1, quad A, RDO=1, ARM=0, APV 0-4, 17-21
	  Int_t rdo, arm,  apv, chan;
	  mDb->getElecCoordFromGeoId(hitIter->centralStripGeoId, rdo,arm,apv,chan);		      
	  //cout<<"  WHAT IS APV="<<apv<<" rdo="<<rdo<<" iD="<<iD<<" arm="<<arm<<endl;
	  if(iD==0 && rdo==1 && arm==0){
	    disk1QuadA[apv]->Fill(chan);
	  }
	  if(hitIter->layer=='R')
	    {
	      (*outTxtFile) <<"R ordinate: " << hitIter->posR<<endl;
	      //added this line under hitIter layer R
	      clustersR[iD]->Fill(hitIter->posR);
	    }
	  else
	    {
	      Double_t phiQ=StFgtGeom::phiQuadXaxis(hitIter->quad);
	      (*outTxtFile) <<"Phi ordinate: " << hitIter->posPhi-phiQ<<endl;
			  //added this line under phi ordinate
	      clustersP[iD]->Fill(hitIter->posPhi);
	    }
	  
	}
      (*outTxtFile)<<endl;
      
      for(int iq=0;iq<4;iq++)
	{
	  for(unsigned int i=0;i<  pStrips[iD*4+iq].size();i++)
	    {
	      generalStrip& pStrip=pStrips[iD*4+iq][i];
	      if(pStrip.seedType==kFgtSeedType1|| pStrip.seedType==kFgtSeedType2 || pStrip.seedType==kFgtSeedType3)
		{
		  
		  for(hitIter=hitVec.begin();hitIter != hitVec.end();hitIter++)
		    {
		      if(abs(hitIter->centralStripGeoId-pStrip.geoId)<2)
			{
			  (*outTxtFile) <<"found cluster with geo id: " << hitIter->centralStripGeoId;
			  Double_t phiQ=StFgtGeom::phiQuadXaxis(iq);
			  if(hitIter->layer=='R')
			    (*outTxtFile) <<"R ordinate: " << hitIter->posR<<endl;
			  else
			    (*outTxtFile) <<"Phi ordinate: " << hitIter->posPhi-phiQ<<endl;
			}
		    }
#ifdef PRINT_1D
		  printArea1D(iD,iq,pStrip.geoId);
#endif
		}
	    }
	}
    }

  
  Float_t x;
  Float_t y;
  //  if(vtxRank<=1)
  //    return kStOk;

  //look at the r phi ratio for each disk
  //  for(int i=0;i<6;i++)
  //    {
  //      Double_t ratio=getRPhiRatio(pClusters[i]->begin(),pClusters[i]->end());
  //      rPhiRatioPlots[i]->Fill(ratio);
      //      cout << "ratio for disk: " << i << " is " << ratio <<" disk has: " << tmpClusterCol->getHitVec().size() << "hits" <<endl;
  //    }

  //vector<generalCluster> &hitVecD1=*(pClusters[0]);
  //vector<generalCluster> &hitVecD6=*(pClusters[5]);


  for(int iD=0;iD<6;iD++)
    {
      //            cout << " there are " << pClusters[iD]->size() <<" cluster in disk : " << iD+1 <<endl;
      int clusCounts[8];
      memset(clusCounts,0,sizeof(int)*8);
      vector<generalCluster> &tHitVec=*(pClusters[iD]);
      vector<generalCluster>::iterator tHit=tHitVec.begin();
      for(;tHit!=tHitVec.end();tHit++)
	{

	  if(!useChargeMatch || tHit->hasMatch)
	    {
	      if(tHit->quad<3)
		{
		  if(tHit->layer=='R')
		    clusCounts[tHit->quad]++;
		  else
		clusCounts[tHit->quad+4]++;
		}
	    }
	}
      for(int iq=0;iq<4;iq++)
	{
	  numClustersR[iD*4+iq]->Fill(clusCounts[iq]);
	  numClustersPhi[iD*4+iq]->Fill(clusCounts[iq+4]);
	}

      int numClus=0;
      int numPulses=0;

      for(int iq=0;iq<4;iq++)
	{
	  for(unsigned int i=0;i<  pStrips[iD*4+iq].size();i++)
	    {
	      if(pStrips[iD*4+iq][i].charge>1000)
		{
		  numClus++;
		}
	      if(validPulse(pStrips[iD*4+iq][i]))
		numPulses++;
	    }
	  //	  cout <<"disk " << iD+1 << " quad " << iq <<" has " << numClus << " charges and " << numPulses <<" pulses " << endl;
	}

    }
  set<Int_t> usedPoints;//saves the points that have been used for tracks (and shouldn't be reused)
  Double_t D1Pos=getLocDiscZ(0);
  Double_t D6Pos=getLocDiscZ(5);
  Double_t zArm=D6Pos-D1Pos;
  vector<generalCluster>::iterator hitIterD1,hitIterD6, hitIterD1R, hitIterD6R, hitIter, hitIter2;

  int clusCountsTemp[24];
  int clusCounts[24];
  memset(clusCounts,0,sizeof(int)*24);
  memset(clusCountsTemp,0,sizeof(int)*24);

  for(int iSeed1=0;iSeed1<5;iSeed1++)
    {
      for(int iSeed2=iSeed1+1;iSeed2<6;iSeed2++)
	{
	  //	  cout<< " using " << iSeed1 <<" and " << iSeed2 << " as seeds " << endl;
	  //	  (*outTxtFile) << " using " << iSeed1 <<" and " << iSeed2 << " as seeds " << endl;
	  if((iSeed2-iSeed1)<1)//to have large enough lever arm. Also, since three points are required shouldn't matter?
	    continue;
	  if(iSeed1==m_effDisk || iSeed2==m_effDisk)
	    continue;
	  if(pClusters[iSeed1]->size() > MAX_CLUSTERS || pClusters[iSeed2]->size() > MAX_CLUSTERS)
	    {
	      //     cout <<"too many clusters in the disk!!!"<<endl<<endl;
	      continue;
	    }
	  //track where we have hits in disks

	  //	  cout <<"using " << iSeed1 << " and " << iSeed2 << " as seed " <<endl;
	  vector<generalCluster> &hitVecSeed1=*(pClusters[iSeed1]);
	  vector<generalCluster> &hitVecSeed2=*(pClusters[iSeed2]);

	  D1Pos=getLocDiscZ(iSeed1);
	  D6Pos=getLocDiscZ(iSeed2);
	  zArm=D6Pos-D1Pos;

	  for(hitIterD1=hitVecSeed1.begin();hitIterD1 != hitVecSeed1.end();hitIterD1++)
	    {
	      //this is from the loose clustering and the cluster doesn't have energy match
	      if(useChargeMatch && !hitIterD1->hasMatch)
		continue;
	      Short_t quadP=hitIterD1->quad;
	      //Short_t disc=hitIterD1->disc;
	      //Short_t strip=hitIterD1->strip;
	      Char_t layer=hitIterD1->layer;
	      
	      Double_t seed1ChargePhi=hitIterD1->CHARGE_MEASURE;
	      Double_t seed1SizePhi=hitIterD1->clusterSize;
	      //	      if(quadP>2)
	      //		continue;

	      //do 1D 'fit' with r strips and the (x,y) thing
	      Int_t geoIdSeed1=hitIterD1->centralStripGeoId;
	      if(usedPoints.find(geoIdSeed1)!=usedPoints.end())
		continue;
	      if(layer!='P')
		continue;
	      //    cout <<"ave make1 " <<endl;
	      Float_t phiD1=hitIterD1->posPhi;
	      for(hitIterD6=hitVecSeed2.begin();hitIterD6 != hitVecSeed2.end();hitIterD6++)
		{
		  if(useChargeMatch &&  !hitIterD6->hasMatch)
		    continue;

		  Int_t geoIdSeed2=hitIterD6->centralStripGeoId;
		  Short_t quadP_2=hitIterD6->quad;
		  //Short_t disc=hitIterD6->disc;
		  //Short_t strip=hitIterD6->strip;
		  Char_t layer=hitIterD6->layer;
		  Double_t seed2ChargePhi=hitIterD6->CHARGE_MEASURE;
		  Double_t seed2SizePhi=hitIterD6->clusterSize;
		  if(layer!='P')
		    continue;
		  if(usedPoints.find(geoIdSeed2)!=usedPoints.end())
		    continue;
		  Float_t phiD6=hitIterD6->posPhi;
		  if(fabs(phiD6-phiD1)>MAX_PHI_DIFF)
		    continue;

		  for(hitIterD1R=hitVecSeed1.begin();hitIterD1R != hitVecSeed1.end();hitIterD1R++)
		    {
		      if(useChargeMatch && !hitIterD1R->hasMatch)
			continue;
		      Int_t geoIdSeed1R=hitIterD1R->centralStripGeoId;
		      Short_t quadR=hitIterD1R->quad;
		      //Short_t disc=hitIterD1R->disc;
		      //Short_t strip=hitIterD1R->strip;
		      Char_t layer=hitIterD1R->layer;
		      Double_t seed1ChargeR=hitIterD1R->CHARGE_MEASURE;
		      Double_t seed1SizeR=hitIterD1R->clusterSize;
		      Int_t quadSeed1=-1;
		      if(layer!='R')
			continue;
		      if(quadR!=quadP)
			continue;
		      quadSeed1=quadR;
		      if(usedPoints.find(geoIdSeed1R)!=usedPoints.end())
			continue;
		  
		      Float_t rD1=hitIterD1R->posR;
		      Float_t xD1=rD1*cos(phiD1);
		      Float_t yD1=rD1*sin(phiD1);
		      //		      cout <<"disk: " << iSeed1<<", phiD1: " << phiD1 <<" xD1: " << xD1 <<" yD1: " << yD1 <<" rD1: " << rD1 <<endl;
		      //    cout <<"ave make3 " <<endl;

		      for(hitIterD6R=hitVecSeed2.begin();hitIterD6R != hitVecSeed2.end();hitIterD6R++)
			{
			  if(useChargeMatch && !hitIterD6R->hasMatch)
			    continue;
			  Int_t geoIdSeed2R=hitIterD6R->centralStripGeoId;
			  Short_t quadR_2=hitIterD6R->quad;
			  //Short_t disc=hitIterD6R->disc;
			  //Short_t strip=hitIterD6R->strip;
			  Char_t layer=hitIterD6R->layer;
			  Double_t seed2ChargeR=hitIterD6R->CHARGE_MEASURE;

			  Double_t seed2SizeR=hitIterD6R->clusterSize;
			  Int_t quadSeed2=-1;

			  if(quadP_2!=quadR_2)
			    continue;
			  quadSeed2=quadP_2;
			  if(layer!='R')
			    continue;

			  clusCountsTemp[iSeed1*4+quadR]++;
			  clusCountsTemp[iSeed2*4+quadR_2]++;
			  if(usedPoints.find(geoIdSeed2R)!=usedPoints.end())
			    continue;
			  Float_t rD6=hitIterD6R->posR;
			  			  //track goes towards smaller radii
#ifndef COSMIC
			  if(rD1>rD6)
			    continue;		  
#endif

			  vector<AVPoint> v_points;
			  //add the seed points to the points

			  Double_t xD6=rD6*cos(phiD6);
			  Double_t yD6=rD6*sin(phiD6);
			  //			  cout <<"Disk " << iSeed2 <<", phiD6: " << phiD6 <<" xD6: " << xD6 <<" yD6: " << yD6 <<" rD6: " << rD6 <<endl;
			  v_points.push_back(AVPoint(xD1,yD1,D1Pos,rD1,phiD1,iSeed1,quadSeed1,seed1ChargeR, seed1ChargePhi, seed1SizeR, seed1SizePhi));
			  v_points.push_back(AVPoint(xD6,yD6,D6Pos,rD6,phiD6,iSeed2,quadSeed2,seed2ChargeR, seed2ChargePhi, seed2SizeR, seed2SizePhi));
			  ///for each combination in d1,d6
			  vector< pair<Int_t,Double_t> > v_x;
			  vector< pair<Int_t,Double_t> > v_y;
			  vector< pair<Int_t,Double_t> > v_r;

			  vector< pair<Int_t,Double_t> > v_xFail;
			  vector< pair<Int_t,Double_t> > v_yFail;
			  vector< pair<Int_t,Double_t> > v_rFail;

			  vector< pair< Int_t, Double_t> > v_rCharge;
			  vector< pair< Int_t, Double_t> > v_phiCharge;

			  vector<Int_t> v_clusterSizeR;
			  vector<Int_t> v_clusterSizePhi;

			  vector<Int_t> v_geoIDsR;
			  vector<Int_t> v_geoIDsPhi;

			  int iFound=0;
			  int iFoundR=0;
			  //    cout <<"ave make4 " <<endl;
			  //zarm is d6 position - D1
			  //Double_t xIpExp=xD1+(xD6-xD1)*(-D1Pos)/zArm;
			  //Double_t yIpExp=yD1+(yD6-yD1)*(-D1Pos)/zArm;
			  ////at x = 0
			  //Double_t zIpExpX0=(D6Pos-(xD6/xD1)*D1Pos)*1/(1-xD6/xD1);
			  ///at y = 0
			  //Double_t zIpExpY0=(D6Pos-yD6/yD1*D1Pos)*1/(1-yD6/yD1);
			  //		  cout <<" 
			  //now find other points
			  vector<generalCluster>::iterator iterClosestPhi;
			  vector<generalCluster>::iterator iterClosestR;

			  Double_t closestDist=999999;
			  Int_t quadTestR=-1;
			  for(int iD=0;iD<6;iD++)
			    {
			      //			      cout <<"testting disk: " << iD <<endl;
			      if(iD==iSeed1 || iD==iSeed2 || iD==m_effDisk)
				continue;
			      //			      cout <<"not seed " << endl;
			      Bool_t found=false;
			      Bool_t foundR=false;
			      //check for hit
			      Double_t diskZ=getLocDiscZ(iD);
			      //expected

			      Double_t xPosExp=xD1+(xD6-xD1)*(diskZ-D1Pos)/zArm;
			      Double_t yPosExp=yD1+(yD6-yD1)*(diskZ-D1Pos)/zArm;
			      //			      cout <<"using disk: " << iD <<" diskZ: " << diskZ <<endl;
			      //			      cout <<"hope to see something x: " << xPosExp <<" y: " << yPosExp <<endl;
			      //			      cout <<"phi1: " << phiD1 <<" phi6: " << phiD6 <<endl;
			      Double_t rPosExp=rD1+(rD6-rD1)*(diskZ-D1Pos)/zArm;
			      vector<generalCluster> &hitVec=*(pClusters[iD]);
			      for(hitIter=hitVec.begin();hitIter!=hitVec.end();hitIter++)
				{
				  if(useChargeMatch &&  !hitIter->hasMatch)
				    continue;
				  //do 1D 'fit' with r strips and the (x,y) thing
				  Int_t geoIdPhi=hitIter->centralStripGeoId;
				  Short_t quad=hitIter->quad;
				  Int_t quadTestPhi=quad;
				  Short_t disc=hitIter->disc;
				  Short_t strip=hitIter->strip;
				  Char_t layer=hitIter->layer;

				  if(usedPoints.find(geoIdPhi)!=usedPoints.end())
				    continue;
				  if(layer!='P')
				    continue;
				  Float_t phi=hitIter->posPhi;

				  if(fabs(phi-phiD1)>MAX_PHI_DIFF)
				    continue;
				  if(fabs(phi-phiD6)>MAX_PHI_DIFF)
				    continue;

				  //Double_t phiCharge=hitIter->CHARGE_MEASURE;

				  //				  								  Double_t phiCharge=hitIter->maxAdc;
				  //			  Int_t clusterSizePhi=hitIter->clusterSize;
				  //				  if(clusterSizePhi<=1)
				  //				    continue;
				  //    cout <<"ave make5 " <<endl;
				  for(hitIter2=hitVec.begin();hitIter2!=hitVec.end();hitIter2++)
				    {
				      if(useChargeMatch && !hitIter2->hasMatch)
					continue;
				      Int_t geoIdR=hitIter2->centralStripGeoId;
				      StFgtGeom::decodeGeoId(geoIdR,disc, quad, layer, strip);//ok
				      //				      cout <<" r? " <<endl;
				      if(usedPoints.find(geoIdR)!=usedPoints.end())
					continue;
				      //				      cout <<"not used yet " <<endl;
				      if(layer!='R')
					continue;
				      quadTestR=quad;
				      if(quadTestR!=quadTestPhi)
					continue;
				      Float_t r=hitIter2->posR;
				      //make sure that the radius makes sense for a track that goes from inner to outer radious
#ifndef COSMIC
				      if(r>rD6 || r<rD1)
					continue;
#endif
				      x=r*cos(phi);
				      y=r*sin(phi);
				      //				      			      cout <<"checking with x: " << x << " y: " << y << " phi: " << phi <<" r: " << r <<endl;
				      //				      				      cout <<" x, y: " << x <<", " << y << " exp: " << xPosExp << " , " << yPosExp <<endl;
				      Double_t dist2=(x-xPosExp)*(x-xPosExp)+(y-yPosExp)*(y-yPosExp);
				      //				      cout <<" dist2: " << dist2 <<endl;

#ifdef ADD_MULTIPLE
				      if(dist2<MAX_DIST2)
					{
					  Double_t rCharge=hitIter2->CHARGE_MEASURE;
					  Double_t phiCharge=hitIter->CHARGE_MEASURE;
					  Int_t clusterSizeR=hitIter2->clusterSize;
					  Int_t clusterSizePhi=hitIter->clusterSize;
					  v_points.push_back(AVPoint(x,y,diskZ,r,phi,iD,quadTestR, rCharge,phiCharge, clusterSizeR,clusterSizePhi));
					}

#endif
				      if(dist2<closestDist)
					{
					  closestDist=dist2;
					  iterClosestPhi=hitIter;
					  iterClosestR=hitIter2;
					}
				    }
				}
			      //			      cout <<"closest dist for disk: "<< iD <<" is : " << closestDist <<endl;
			      //    cout <<"ave make6 " <<endl;
			      if(closestDist<1000 && closestDist<MAX_DIST2)
				{
				  found=true;
				  clusCountsTemp[iD*4+quadTestR]++;

				  //				  cout <<"accepted "  <<endl;
				  double r=iterClosestR->posR;
				  double phi=iterClosestPhi->posPhi;
				  //				  cout <<"found closest phi in disk " << iD <<" : " << phi << ", r: " << r << " x: " << x <<" y: " << y << ", this is good enough, distance: " << closestDist <<endl;
				  Int_t geoIdR=iterClosestR->centralStripGeoId;
				  Int_t geoIdPhi=iterClosestPhi->centralStripGeoId;

				  double x=r*cos(phi);
				  double y=r*sin(phi);

				  Double_t rCharge=iterClosestR->CHARGE_MEASURE;
				  Double_t phiCharge=iterClosestPhi->CHARGE_MEASURE;
				  Int_t clusterSizeR=iterClosestR->clusterSize;
				  Int_t clusterSizePhi=iterClosestPhi->clusterSize;
				  //				  cout <<"charge R of middle disk: "<<  iD<<": "<< rCharge <<" phicharge: " << phiCharge<<endl;

				  v_x.push_back(pair<Int_t,Double_t>(iD,x));
				  v_y.push_back(pair<Int_t,Double_t>(iD,y));
				  v_rCharge.push_back(pair<Int_t, Double_t>(iD,rCharge));
				  v_phiCharge.push_back(pair<Int_t, Double_t>(iD,phiCharge));
				  //to avoid double counting, t
				  v_geoIDsPhi.push_back(geoIdPhi);
				  v_geoIDsR.push_back(geoIdR);
				  v_clusterSizeR.push_back(clusterSizeR);
				  v_clusterSizePhi.push_back(clusterSizePhi);
				  v_points.push_back(AVPoint(x,y,diskZ,r,phi,iD,quadTestR, rCharge,phiCharge, clusterSizeR,clusterSizePhi));
				  //				  cout<<" adding point with r: "<< r <<" phi: " << phi <<" x: " << x <<" y: " << y <<endl;
				}
			      //    cout <<"ave make8 " <<endl;
			      //only one per disk
			      if(found)
				iFound++;
			      else
				{
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
			      closestDist=999999;

			    }

			  //    cout <<"ave make9 " <<endl;
			  //		  if(iFound>=2 && fabs(xIpExp)<20 && fabs(yIpExp)<20 && fabs(zIpExpX0)<40 && fabs(zIpExpY0)<40) //at least one hit plus seed and pointing roughly to the interaction region
			  //with hard cuts on the track we can get the whole vertex distribution
			  //			  cout << " we found " << iFound <<" points " << endl;
			  if(iFound>=(MIN_NUM_POINTS-2)) //at least one hit plus seed and pointing roughly to the interaction region
			    {
			      //			           cout <<"found " <<endl;
			      Bool_t validTrack=false;
			      //			      if(v_x.size()>iFound)
			      {
				Double_t ipZ;
				//				cout <<"about to get track" <<endl;
				//				cout <<"check for valid track " << endl;
				validTrack=getTrack(v_points, ipZ);

				//			      cout <<"found 2" <<endl;
				if(validTrack)
				  {
				    for(Int_t iD=0;iD<kFgtNumDiscs;iD++){
				      for(int iq=0;iq<4;iq++){
					clusCounts[iD*4+iq]+=clusCountsTemp[iD*4+iq];
				      }}


				    //				    cout <<"track was valid, phi1: " << phiD1 <<" phiD6: " << phiD6;
				    //				    cout <<", rD1: " << rD1 <<" rD6: " << rD6<<endl;
				    //				    cout <<"was valid " << endl;
				  }
			      }

			      //			      cout <<"found4 " <<endl;
			      //			      else
			      {
				//    cout <<"ave make8]10 " <<endl;
				//			  		  cout <<"filled hip with " << xIpExp << " / " << yIpExp <<endl;
				for(unsigned int i=0;i<v_x.size();i++)
				  {
				    if(validTrack)
				      {
					//at least don't duplicate seeds..., the other ones might belong to multiple tracks
					usedPoints.insert(geoIdSeed1);
					usedPoints.insert(geoIdSeed1R);
					usedPoints.insert(geoIdSeed2);
					usedPoints.insert(geoIdSeed2R);
					//					usedPoints.insert(v_geoIDsPhi[i]);
					//					usedPoints.insert(v_geoIDsR[i]);
					//					Int_t disk=v_x[i].first;
					//					Double_t x=v_x[i].second;
					//					Double_t y=v_y[i].second;
					//					Double_t rCharge=v_rCharge[i].second;
					//					Double_t phiCharge=v_phiCharge[i].second;
					//				      cout <<"filling disk: " << disk <<" with: " << x <<" / " <<y <<endl;
					//				    				    radioPlotsEff[disk]->Fill(x,y);
					//				    chargeCorr[disk]->Fill(rCharge,phiCharge);
					//				    h_clusterSizeR[disk]->Fill(v_clusterSizeR[i]);
					//				    h_clusterSizePhi[disk]->Fill(v_clusterSizePhi[i]);
					//				    h_clusterChargeR[disk]->Fill(rCharge);
					//				    h_clusterChargePhi[disk]->Fill(phiCharge);
				      }
				  }
				hitCounter++;
			      }
			    }
			  //    cout <<"ave make8]11 " <<endl;

			  //start over
			  iFound=0;
			  iFoundR=0;
			  v_x.clear();
			  v_points.clear();
			  v_y.clear();
			  v_r.clear();
			  v_xFail.clear();
			  v_yFail.clear();
			  v_rFail.clear();
			  memset(clusCountsTemp,0,sizeof(int)*24);
			}

		    }

		}

	    }

	}

    }
  for(Int_t iD=0;iD<kFgtNumDiscs;iD++){
    for(int iq=0;iq<4;iq++){
      //      cout <<"filling clus2: " << iD<<" iq: " << iq << " overall: " << iD*4+iq <<endl;
      //            cout <<"fill disk " << iD <<" quad: " << iq << " with " << clusCounts[iD*4+iq] <<" clusters from tracks"<<endl;
      numTrackHits[iD*4+iq]->Fill(clusCounts[iD*4+iq]);

    }}
  //  cout <<"we have " << m_tracks.size()-oldNumTracks<< " tracks in this event " <<endl;
  //  cout <<"oldNumtracsk: " << oldNumTracks << " new size: " << m_tracks.size() <<endl;
  numTracks->Fill(m_tracks.size()-oldNumTracks);

  return ierr;

};
 
StFgtGenAVEMaker::StFgtGenAVEMaker( const Char_t* name): StFgtGeneralBase( name ),useChargeMatch(false),runningEvtNr(0),hitCounter(0),hitCounterR(0),printCounter(0),fitCounter(0)
{

  //  cout <<"AVE constructor!!" <<endl;
  int numTb=7;
  mPulseShapePtr=new TF1("pulseShape","[0]*(x>[4])*(x-[4])**[1]*exp(-[2]*(x-[4]))+[3]",0,numTb);
  mPulseShapePtr->SetParName( 0, "C" );
  mPulseShapePtr->SetParName( 1, "a" );
  mPulseShapePtr->SetParName( 2, "b" );
  mPulseShapePtr->SetParName( 3, "ped" );
  mPulseShapePtr->SetParName( 4, "t0" );

  mHistPtr=new TH1F("tempFitHist","tempFitHist",numTb,0,numTb);
  mHistPtr2=new TH1F("tempFitHist2","tempFitHist2",numTb,0,numTb);
  mHistPtr2->SetLineColor(kBlue);
  mHistPtr2->SetFillColor(kBlue);
  mHistPtr2->SetMarkerColor(kBlue);
  mCanvas=new TCanvas("tmpCnvs","tmpCnvs");
};

StFgtGenAVEMaker::~StFgtGenAVEMaker()
{

  //delete histogram arrays
};

Int_t StFgtGenAVEMaker::Finish(){
  StFgtGeneralBase::Finish();
  cout <<" closing txt file " << endl;
  outTxtFile->close();
  gStyle->SetPalette(1);
  cout <<"AVE finish function " <<endl;
  Int_t ierr = kStOk;

  ///////////////////////////track collection
  vector<AVTrack>::iterator it=m_tracks.begin();
  cout <<"we found " << m_tracks.size() <<" tracks" <<endl;
  int counter=0;
  int goodTracks=0;
  for(;it!=m_tracks.end();it++)
    {
      cout <<" looking at track " << counter <<endl;
      counter++;
      //      cout <<"This track has parameters: ";
      cout <<" mx: " << it->mx <<" my: " << it->my <<" bx: " << it->ax << " by: " << it->ay << " chi2: " << it->chi2 <<endl;
      Double_t vertZ = (  -( it->mx*it->ax + it->my*it->ay )/(it->mx*it->mx+it->my*it->my));

      pair<double,double> dca=getDca(it);
      //      cout <<"dca: " << dca.second <<" z vertex: " << vertZ <<" or " << dca.first <<endl;
      if(it->chi2<MAX_DIST_CHI && fabs(vertZ)< VERTEX_CUT )
	{
	  goodTracks++;
	  cout <<"looking at track with mx: " << it->mx <<" ax: " << it->ax <<" my: " << it->my <<" ay: " << it->ay <<endl;
	  hIpZ->Fill(dca.first);
	  hIp->Fill(dca.first,dca.second);
	  hTrkZ->Fill(vertZ);
	  hMx->Fill(it->mx);	  
	  hMy->Fill(it->my);
	  hBx->Fill(it->ax);	  
	  hBy->Fill(it->ay);
	  hChi2->Fill(it->chi2);
	  tpcFgtZVertexCorr->Fill(dca.first,it->ipZEv);
	  tpcFgtZVertexCorr2->Fill(vertZ,it->ipZEv);
	  tpcFgtZVertexCorr3->Fill(vertZ,dca.first);
	  hIpDca->Fill(dca.second);
	}
    }
  cout <<" we found " << goodTracks << " good Tracks " <<endl;
  ///  cout <<"canvases etc.. " << endl;
  //////////////////////////////////////////////////
  TCanvas* cRadio=new TCanvas("radioPlots","radioPlot",1000,1500);
  TCanvas* cRadioLoose=new TCanvas("radioPlotsLoose","radioPlotLoose",1000,1500);
  TCanvas* cRadioR=new TCanvas("radioPlotsR","radioPlotR",1000,1500);
  TCanvas* cRadioPhi=new TCanvas("radioPlotsPhi","radioPlotPhi",1000,1500);

  TCanvas* cRadioHits=new TCanvas("radioPlotsHits","radioPlotHits",1000,1500);
  TCanvas* cRadioNonHits=new TCanvas("radioPlotsNonHits","radioPlotNonHits",1000,1500);
  //  cout <<"divide "<<endl;
  cRadio->Divide(2,3); //6 discs
  cRadioR->Divide(2,3); //6 discs
  cRadioPhi->Divide(2,3); //6 discs
  cRadioLoose->Divide(2,3); //6 discs
  cRadioHits->Divide(2,3); //6 discs
  cRadioNonHits->Divide(2,3); //6 discs
  TCanvas* cRPRatio=new TCanvas("rPhiRatio","rPhiRatios",1000,1500);
  cRPRatio->Divide(2,3); //6 discs

  TCanvas* cREff=new TCanvas("crEff","crEff",1000,1500);

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
  char buffer[100];


  sprintf(buffer,"%s/clusterPics.root",fileBase);
  cout <<"setting cluster pic file to : " << buffer <<endl;
  TFile *fClu = new TFile(buffer,"recreate");
  fClu->cd();

  for(unsigned int i=0;i<v_hClusP.size();i++)
    {
      (v_hClusP[i])->Write();
    }
  for(unsigned int i=0;i<v_hClusR.size();i++)
    {
      (v_hClusR[i])->Write();
    }
  fClu->Write();
  fClu->Close();
  sprintf(buffer,"%s/signalShapes.root",fileBase);
  cout <<"setting signal shapes file to : " << buffer <<endl;
  TFile *f1 = new TFile(buffer,"recreate");
  f1->cd();

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

  cout <<"drawing hits2 " <<endl;
  ///---->   cRadioHits->SaveAs("radioPlotsHits.png");
  ///---->  cRadioNonHits->SaveAs("radioPlotsNonHits.png");

  TCanvas* cClusterSizeR=new TCanvas("clusterSizeR","clusterSizeR",1000,1500);
  cClusterSizeR->Divide(2,3);
  TCanvas* cClusterSizePhi=new TCanvas("clusterSizePhi","clusterSizePhi",1000,1500);
  cClusterSizePhi->Divide(2,3);
  TCanvas* cChargeCorr=new TCanvas("chargeCorr","chargeCorr",1000,1500);
  cChargeCorr->Divide(3,4);

  TCanvas* cClusterChargePhi=new TCanvas("clusterChargePhi","clusterChargePhi",1000,1500);
  cClusterChargePhi->Divide(2,3);
  TCanvas* cClusterChargeR=new TCanvas("clusterChargeR","clusterChargeR",1000,1500);
  cClusterChargeR->Divide(2,3);

  TCanvas cIPProj;
  hIp->Draw("colz");
  hIp->Write();
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


Int_t StFgtGenAVEMaker::Init(){
  outTxtFile=new ofstream;
  outTxtFile->open("clusExpectations.txt");
  cluNotFoundTxt=new ofstream;
  cluNotFoundTxt->open("clusNotFound.txt");
  cout <<"AVE!!" <<endl;
  myRootFile=new TFile("clusterEff.root","RECREATE");
  pulsePictureFile=new TFile("pulsePics.root","RECREATE");

  char buffer[100];
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
  myRootFile->cd();
  //  outTxtFile=new ofstream;
  //  outTxtFile->open("clusters.txt");


  Int_t ierr = kStOk;

  chargeRatioInEffDisk=new TH2D("chargeRatioInEffDisk","chargeRatioInEffDisk",NUM_EFF_BIN,-DISK_DIM,DISK_DIM,NUM_EFF_BIN,-DISK_DIM,DISK_DIM);
  chargeRatioInEffDisk->SetMaximum(2.0);
  chargeAsymInEffDisk=new TH2D("chargeAsymInEffDisk","chargeAsymInEffDisk",NUM_EFF_BIN,-DISK_DIM,DISK_DIM,NUM_EFF_BIN,-DISK_DIM,DISK_DIM);
  chargeAsymInEffDisk->SetMaximum(1.0);
  chargeCorrInEffDisk=new TH2D("chargeCorrInEffDisk","chargeCorrInEffDisk",500,0,50000,500,0,50000);
  hChargeAsym=new TH1D("chargeAsym","chargeAsym",100,0,50);
  hChargeRatio=new TH1D("chargeRatio","chargeRatio",100,0,50);


  chargeCorrSum3=new TH2D("chargeCorrSum3","chargeCorrSum3",500,0,50000,500,0,50000);
  chargeCorrMaxStrip=new TH2D("chargeCorrMaxStrip","chargeCorrMaxStrip",500,0,20000,500,0,20000);
  chargeCorrMaxAdc=new TH2D("chargeCorrMaxAdc","chargeCorrMaxAdc",500,0,5000,500,0,5000);


  radioPlotsEff=new TH2D*[kFgtNumDiscs];
  radioPlotsNonEff=new TH2D*[kFgtNumDiscs];
  radioPlotsEffR=new TH2D*[kFgtNumDiscs];
  radioPlotsNonEffR=new TH2D*[kFgtNumDiscs];
  radioPlotsEffPhi=new TH2D*[kFgtNumDiscs];
  radioPlotsEffLoose=new TH2D*[kFgtNumDiscs];
  radioPlotsNonEffPhi=new TH2D*[kFgtNumDiscs];
  radioPlotsNonEffLoose=new TH2D*[kFgtNumDiscs];
  rPhiRatioPlots=new TH1D*[kFgtNumDiscs];


  exPulseMaxAdcNormP=new TH1F("pulseMaxAdcNormP","pulseMaxAdcNormP",7,0,6);
  exPulseSigP=new TH1F("pulseSigNormP","pulseSigNormP",7,0,6);

  exPulseMaxAdcNormR=new TH1F("pulseMaxAdcNormR","pulseMaxAdcNormR",7,0,6);
  exPulseSigR=new TH1F("pulseSigNormR","pulseSigNormR",7,0,6);

  exPulseMaxAdcNormTrackP=new TH1F("pulseMaxAdcNormTrackP","pulseMaxAdcNormTrackP",7,0,6);
  exPulseSigTrackP=new TH1F("pulseSigNormTrackP","pulseSigNormTrackP",7,0,6);

  exPulseMaxAdcNormTrackR=new TH1F("pulseMaxAdcNormTrackR","pulseMaxAdcNormTrackR",7,0,6);
  exPulseSigTrackR=new TH1F("pulseSigNormTrackR","pulseSigNormTrackR",7,0,6);


  pulseCounterP=0;
  pulseCounterR=0;

  pulseCounterTP=0;
  pulseCounterTR=0;

  createPlots(&numClustersR,kFgtNumDiscs*4,"numClustersR",101,0,100);
  createPlots(&numClustersPhi,kFgtNumDiscs*4,"numClustersPhi",101,0,100);
  createPlots(&numTrackHits,kFgtNumDiscs*4,"numTrackHits",101,0,100);
  numTracks=new TH1I("numTracksPerEvent","numTracksPerEvent",101,0,100);

  createPlots(&firstTbSigCloseClusterR,kFgtNumDiscs*4,"firstTbSigCloseClusterR",100,0,20);
  createPlots(&firstTbSigCloseClusterP,kFgtNumDiscs*4,"firstTbSigCloseClusterP",100,0,20);
  createPlots(&firstTbSigTrackClusterR,kFgtNumDiscs*4,"firstTbSigTrackClusterR",100,0,20);
  createPlots(&firstTbSigTrackClusterP,kFgtNumDiscs*4,"firstTbSigTrackClusterP",100,0,20);


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
      sprintf(buffer, "%s_APV%d", "disk1QuadA",iii);
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


  hIp=new TH2D("Proj_to_IP","Proj_to_Ip",50,-100,100,50,-100,100);
  hBx=new TH1D("hBx","hBx",50,-100,100);
  hBy=new TH1D("hBy","hBy",50,-100,100);
  hMx=new TH1D("hMx","hMx",50,-100,100);
  hMy=new TH1D("hMy","My",50,-0.1,0.1);
  hIpZ=new TH1D("IP_Z","IP_Z",50,-100,100);

  hIpDca=new TH1D("ipDCA","ipDCA",50,-100,100);
  hTrkZ=new TH1D("z_Vtx_From_trk_fit","z_Vtx_From_trk_fit",50,-100,100);
  hResidua=new TH1D("residua","residua",100,0,2);
  hResiduaX=new TH2D("residuaX","residuaX",100,-10,10,200,0,1.0);
  hResiduaY=new TH2D("residuaY","residuaY",100,-40,-20,200,0,1.0);
  hResiduaR=new TH2D("residuaR","residuaR",100,12,32,200,0,1.0);
  hResiduaP=new TH2D("residuaP","residuaP",100,0,0.8,200,0,1.0);


  hChi2=new TH1D("chi2","chi2",50,0,2);
  tpcFgtZVertexCorr=new TH2D("tpc_fgt_corr","tpc_fgt_corr",100,-120,120,100,-120,120);
  tpcFgtZVertexCorr2=new TH2D("tpc_fgt_corr2","tpc_fgt_corr2",100,-120,120,100,-120,120);
  tpcFgtZVertexCorr3=new TH2D("fgt_fgt_corr","fgt_fgt_corr",50,-50,50,50,-50,50);

  for(int iD=0;iD<kFgtNumDiscs;iD++)
    {

      sprintf(buffer,"radioDiskEff_%d",iD);
      radioPlotsEff[iD]=new TH2D(buffer,buffer,NUM_EFF_BIN,-DISK_DIM,DISK_DIM,NUM_EFF_BIN,-DISK_DIM,DISK_DIM);

      sprintf(buffer,"radioDiskEffR_%d",iD);
      radioPlotsEffR[iD]=new TH2D(buffer,buffer,NUM_EFF_BIN,-DISK_DIM,DISK_DIM,NUM_EFF_BIN,-DISK_DIM,DISK_DIM);

      sprintf(buffer,"radioDiskEffPhi_%d",iD);
      radioPlotsEffPhi[iD]=new TH2D(buffer,buffer,NUM_EFF_BIN,-DISK_DIM,DISK_DIM,NUM_EFF_BIN,-DISK_DIM,DISK_DIM);

      sprintf(buffer,"radioDiskEffLoose_%d",iD);
      radioPlotsEffLoose[iD]=new TH2D(buffer,buffer,NUM_EFF_BIN,-DISK_DIM,DISK_DIM,NUM_EFF_BIN,-DISK_DIM,DISK_DIM);
      //      cout <<"1" <<endl;
      sprintf(buffer,"rEff_%d",iD);
      rEff[iD]=new TH1D(buffer,buffer,100,0,DISK_DIM);
      sprintf(buffer,"rNonEff_%d",iD);
      rNonEff[iD]=new TH1D(buffer,buffer,100,0,DISK_DIM);
      sprintf(buffer,"clusterSizeR_Disk_%d",iD);
      h_clusterSizeR[iD]=new TH1D(buffer,buffer,20,0,20);
      h_clusterSizeR[iD]->SetFillColor(kYellow);
      sprintf(buffer,"clusterSizePhi_Disk_%d",iD);
      h_clusterSizePhi[iD]=new TH1D(buffer,buffer,20,0,20);
      h_clusterSizePhi[iD]->SetFillColor(kYellow);
      sprintf(buffer,"clusterChargeR_Disk_%d",iD);
      h_clusterChargeR[iD]=new TH1D(buffer,buffer,100,0,5000);
      h_clusterChargeR[iD]->SetFillColor(kYellow);

      sprintf(buffer,"clusterChargePhi_Disk_%d",iD);
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
	  sprintf(buffer,"r_phi_ChargeCorrelationInDisk_%d_quad_%d",iD+1,iq);
	  chargeCorr[iD*4+iq]=new TH2D(buffer,buffer,200,0,70000,200,0,70000);
	  sprintf(buffer,"clusterSizeRInDisk_%d_quad_%d",iD+1,iq);
	  clusterSizeR[iD*4+iq]=new TH1D(buffer,buffer,100,0,100);
	  sprintf(buffer,"clusterSizePInDisk_%d_quad_%d",iD+1,iq);
	  clusterSizeP[iD*4+iq]=new TH1D(buffer,buffer,100,0,100);
	}
      sprintf(buffer,"radioDiskNonEff_%d",iD);
      radioPlotsNonEff[iD]=new TH2D(buffer,buffer,NUM_EFF_BIN,-DISK_DIM,DISK_DIM,NUM_EFF_BIN,-DISK_DIM,DISK_DIM);
      //      cout <<" created non eff histo " << endl;
      sprintf(buffer,"radioDiskNonEffR_%d",iD);
      radioPlotsNonEffR[iD]=new TH2D(buffer,buffer,NUM_EFF_BIN,-DISK_DIM,DISK_DIM,NUM_EFF_BIN,-DISK_DIM,DISK_DIM);
      sprintf(buffer,"radioDiskNonEffPhi_%d",iD);
      radioPlotsNonEffPhi[iD]=new TH2D(buffer,buffer,NUM_EFF_BIN,-DISK_DIM,DISK_DIM,NUM_EFF_BIN,-DISK_DIM,DISK_DIM);
      sprintf(buffer,"radioDiskNonEffLoose_%d",iD);
      radioPlotsNonEffLoose[iD]=new TH2D(buffer,buffer,NUM_EFF_BIN,-DISK_DIM,DISK_DIM,NUM_EFF_BIN,-DISK_DIM,DISK_DIM);
      for(int nx=0;nx<rEff[iD]->GetNbinsX();nx++)
	{
	  //	   rEff[iD]->SetBinContent(nx,0.1);
	}
      sprintf(buffer,"rPhiRatio_%d",iD);
      rPhiRatioPlots[iD]=new TH1D(buffer,buffer,100,-2,10);
    }

  return ierr;
};
ClassImp(StFgtGenAVEMaker);
