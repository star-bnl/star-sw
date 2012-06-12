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


//max num clusters any disk is allowed to have
#define MAX_CLUSTERS 12
#define CHARGE_MEASURE clusterCharge
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
#define MY_PI 3.14159
//#define  REFIT_WITH_VERTEX
//#define FIT_WITH_VERTEX


#define MAX_DIST_CHI 1.0
#define MAX_DIST2_EFF 1.0
#define MAX_DIST2 1.0

#define MIN_NUM_POINTS 3
#define DISK_DIM 40
#define NUM_EFF_BIN 30




template<class T> void createPlots(T*** pH, int numH, char* nameBase, int numBin, int first, int last)
{
  char buffer[100];
  (*pH)=new T*[numH];
  for(int iD=0;iD<kFgtNumDiscs;iD++)
    {
      for(int iQ=0;iQ<4;iQ++)
	{
	  sprintf(buffer,"%s_disc%d_quad%d",nameBase,iD+1,iQ);
	  (*pH)[iD*4+iQ]=new T(buffer,buffer,numBin,first,last);
	}
    }
}
template void createPlots(TH1I*** pH, int numH, char* nameBase,  int numBin, int first, int last);
template void createPlots(TH1F*** pH, int numH, char* nameBase, int numBin, int first, int last);

void doNormalize(TH2D** hEff, TH2D** hNonEff)
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



void StFgtGenAVEMaker::saveSigs(Double_t* sigR, Double_t* sigP, Double_t r, Double_t phi, Int_t maxR, Int_t maxPhi)
{
  Char_t buffer[200];
  sprintf(buffer,"Sig_Phi_Evt_%d_R_%f_Phi_%f",evtNr,r,phi);
  TH2D* histoP=new TH2D(buffer,buffer,7,0,6,maxPhi,0,maxPhi-1);
  for(int i=0;i<maxPhi;i++)
    {
      for(int j=0;j<7;j++)
	{
	  histoP->SetBinContent(j,i,sigP[i*7+j]);
	  cout <<"P: setting bin : i: " << i << " j: " << j << " index: "<< i*7+j << " sig: " << sigP[i*7+j]<<endl;
	}
    }
  v_hClusP.push_back(histoP);
  sprintf(buffer,"Sig_R_Evt_%d_R_%f_Phi_%f",evtNr,r,phi);
  TH2D* histoR=new TH2D(buffer,buffer,7,0,6,maxR,0,maxR-1);
  for(int i=0;i<maxR;i++)
    {
      for(int j=0;j<7;j++)
	{
	  histoR->SetBinContent(j,i,sigR[i*7+j]);
	  cout <<"R: setting bin : i: " << i << " j: " << j << " index: "<< i*7+j << " sig: " << sigR[i*7+j]<<endl;
	}
    }
  v_hClusR.push_back(histoR);
  cout <<" done saving sigs" <<endl;
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
      for(int i=0;i<  pStrips[iD*4+iq].size();i++)
	{
	  Int_t geoId=pStrips[iD*4+iq][i].geoId;
	  generalStrip& pStrip=pStrips[iD*4+iq][i];



	  StFgtGeom::getPhysicalCoordinate(geoId,disc,quadrant,layer,ordinate,lowerSpan,upperSpan);
	  StFgtGeom::decodeGeoId(geoId,disc, quadrant, layer, strip);
	  char buffer[100];
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

		      if(pStrip.adc[6]>0)
			secondToLastRatioP=pStrip.adc[5]/(float)pStrip.adc[6];
		      if(pStrip.pedErr>0)
			firstTbSigP=pStrip.adc[0]/(float)pStrip.pedErr;
		      for(int iAdc=0;iAdc<7;iAdc++)
			{
			  if(pStrip.adc[iAdc]>5*pStrip.pedErr)
			    {
			      numFSigP++;
			      if(firstFSigTbP<0)
				firstFSigTbP=iAdc;
			    }
			  if(pStrip.adc[iAdc]>maxPAdc)
			    {
			      maxPAdc=pStrip.adc[iAdc];
			      maxPTb=iAdc;
			      if(pStrip.pedErr>0)
				maxSigAdcP=(Double_t)maxPAdc/pStrip.pedErr;
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

		      if(pStrip.adc[6]>0)
			secondToLastRatioR=pStrip.adc[5]/(float)pStrip.adc[6];
		      if(pStrip.pedErr>0)
			firstTbSigR=pStrip.adc[0]/(float)pStrip.pedErr;
		      for(int iAdc=0;iAdc<7;iAdc++)
			{
			  if(pStrip.adc[iAdc]>5*pStrip.pedErr)
			    {
			      numFSigR++;
			      if(firstFSigTbR<0)
				firstFSigTbR=iAdc;

			    }
			  if(pStrip.adc[iAdc]>maxRAdc)
			    {
			      maxRAdc=pStrip.adc[iAdc];
			      maxRTb=iAdc;
			      if(pStrip.pedErr>0)
				maxSigAdcR=(Double_t)maxRAdc/pStrip.pedErr;
			    }
			}
		    }
		}
	    }
	}
      //fill histos
      if(maxPhiCharge>1000)
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

      if(maxRCharge>1000)
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


      if(maxRCharge> 1000 && maxPhiCharge>1000 && iD==m_effDisk)// && (float)pStrips[iD*4+iq][maxRInd].charge)
	{
	  StFgtGeom::getPhysicalCoordinate((float)pStrips[iD*4+iq][maxRInd].geoId,disc,quadrant,layer,ordinate,lowerSpan,upperSpan);
	  //	    if(ordinate>20)
	    {
	      chargeCorrMaxStrip->Fill(maxRCharge,maxPhiCharge);
	      chargeCorrMaxAdc->Fill(maxRAdc,maxPAdc);
	      
	      if(maxRInd>0 && maxPInd>0 && maxRInd< pStrips[iD*4+iq].size()&& maxPInd < pStrips[iD*4+iq].size())
		{
		  float intRCharge=(float)pStrips[iD*4+iq][maxRInd].charge+(float)pStrips[iD*4+iq][maxRInd-1].charge+(float)pStrips[iD*4+iq][maxRInd+1].charge;
		  float intPCharge=(float)pStrips[iD*4+iq][maxPInd].charge+(float)pStrips[iD*4+iq][maxPInd-1].charge+(float)pStrips[iD*4+iq][maxPInd+1].charge;
		  chargeCorrSum3->Fill(intRCharge,intPCharge);
		}
	    }
	}
      //	  if(iD==2 && iq==1 && (float)pStrips[iD*4+iq][maxRInd].pedErr>0)
      
      
      
      if(partOfClusterR&& partOfClusterP && iD==m_effDisk)
	{
	  chargeCorrInEffDisk->Fill(clusterChargeR,clusterChargeP);
	  StFgtGeom::getPhysicalCoordinate((float)pStrips[iD*4+iq][maxRInd].geoId,disc,quadrant,layer,ordinate,lowerSpan,upperSpan);
	  //	    if(ordinate>20)
	      {
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
      for(int i=0;i<  pStrips[iD*4+iq].size();i++)
	{
	  Int_t geoId=pStrips[iD*4+iq][i].geoId;
	  generalStrip& pStrip=pStrips[iD*4+iq][i];
	  Short_t disc, quadrant,strip;
	  Char_t layer;
	  Double_t ordinate, lowerSpan, upperSpan;//, prvOrdinate;
	  StFgtGeom::getPhysicalCoordinate(geoId,disc,quadrant,layer,ordinate,lowerSpan,upperSpan);
	  StFgtGeom::decodeGeoId(geoId,disc, quadrant, layer, strip);
	  char buffer[100];
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
	  if(maxRInd< (pStrips[iD*4+iq].size()-1))
	    maxRCharge+=pStrips[iD*4+iq][maxRInd+1].charge;
	  if(maxPInd>0)
	    maxPhiCharge+=pStrips[iD*4+iq][maxPInd-1].charge;
	  if(maxPInd< (pStrips[iD*4+iq].size()-1))
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

      if(fabs(ordinate-ord)<minDist)
	{
	  charge=it->clusterCharge;
	  cluSize=it->clusterSize;
	  minDist=fabs(ordinate-ord);
	}
    }
  return pair<Double_t,Double_t>(charge,cluSize);
}

Double_t StFgtGenAVEMaker::findClosestPoint(double xE, double yE, Int_t iD)
{
  if(iD<0 || iD >5)
    {
      return -99999;
    }
  vector<generalCluster> &hitVec=*(pClusters[iD]);
  Double_t dist2=99999;
  for(vector<generalCluster>::iterator it=hitVec.begin();it!=hitVec.end();it++)
    {
      for(vector<generalCluster>::iterator it2=hitVec.begin();it2!=hitVec.end();it2++)
	{
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
	  Double_t mDist=(x-xE)*(x-xE)+(y-yE)*(y-yE);
	  if(mDist<dist2)
	    dist2=mDist;
	}
    }
  return dist2;
}

Short_t StFgtGenAVEMaker::getQuadFromCoo(Double_t x, Double_t y)
{
  if(x>0 && y>0)
    return 0;
  if(x>0 && y<0)
    return 1;
  if(x<0 && y<0)
    return 2;
  if(x<0 && y>0)
    return 3;
}
//print the strips around the place where we expect hit
Bool_t StFgtGenAVEMaker::printArea(Float_t r, Float_t phi, Int_t iD, Int_t iq)
{
  //just print the first 1000 clusters

  if(printCounter>1000)
    return true;
  printCounter++;
  //first r: 
  Double_t signalsP[500];
  Double_t signalsR[500];

  Int_t counterR=0;
  Int_t counterP=0;

  for(int i=0;i<  pStrips[iD*4+iq].size();i++)
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
      if(disc==iD && iq==quadrant && ((layer =='R' && fabs(ordinate-r)<0.7) || (layer=='P' && fabs(ordinate-phi)<0.03) || (layer=='P' && fabs(ordinate-phi+2*MY_PI)<0.03 ) || (layer=='P' && fabs(ordinate-phi-2*MY_PI)<0.03)|| (layer=='P' && fabs(ordinate-phi+MY_PI)<0.03 ) || (layer=='P' && fabs(ordinate-phi-MY_PI)<0.03)))
	{
	  //	  cout <<" found!!!" << endl;
	  (*outTxtFile) <<StFgtGeom::encodeGeoName(iD,iq,layer,strip)<<" ord: " << ordinate <<" layer: " <<layer<<" ped: " << pStrip.ped <<" pedErr: " << pStrip.pedErr <<" seedType: " <<buffer<<" ";
	  for(int iT=0;iT<7;iT++)
	    {
	      if(pStrip.adc[iT]<pStrip.pedErr)
		(*outTxtFile) << setw(4) << " .  "<< " ";
	      else
		(*outTxtFile) <<  setw(4) <<pStrip.adc[iT] <<" ";


	      if(layer=='P'&& counterP<40)
		{
		  signalsP[counterP*7+iT]=pStrip.adc[iT];
		  cout <<"first p: setting bin : i: " << counterP << " j: " << iT << " index: "<< counterP*7+iT << " sig: " << signalsP[counterP*7+iT]<<endl;
		  if(iT==6)
		    counterP++;
		}
	      if(layer=='R'&& counterR<40)
		{
		  signalsR[counterR*7+iT]=pStrip.adc[iT];
		  cout <<"first R: setting bin : i: " << counterR << " j: " << iT << " index: "<< counterR*7+iT << " sig: " << signalsR[counterP*7+iT]<<endl;
		  if(iT==6)
		    counterR++;
		}
	    }
	  (*outTxtFile) <<endl;
	}
    }
  cout <<"save signals.." <<endl;
  saveSigs(signalsR,signalsP,r,phi,counterR,counterP);
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
  for(int i=0;i<  pStrips[iD*4+iq].size();i++)
    {
      Int_t geoId=pStrips[iD*4+iq][i].geoId;
      generalStrip& pStrip=pStrips[iD*4+iq][i];
      Short_t disc, quadrant,strip;
      Char_t layer;
      Double_t ordinate, lowerSpan, upperSpan;//, prvOrdinate;
      StFgtGeom::getPhysicalCoordinate(geoId,disc,quadrant,layer,ordinate,lowerSpan,upperSpan);
      StFgtGeom::decodeGeoId(geoId,disc, quadrant, layer, strip);
      char buffer[100];
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
	  if(maxRInd< (pStrips[iD*4+iq].size()-1))
	    maxRCharge+=pStrips[iD*4+iq][maxRInd+1].charge;
	  if(maxPInd>0)
	    maxPhiCharge+=pStrips[iD*4+iq][maxPInd-1].charge;
	  if(maxPInd< (pStrips[iD*4+iq].size()-1))
	    maxPhiCharge+=pStrips[iD*4+iq][maxPInd+1].charge;

	  return pair<Double_t,Double_t>(maxRCharge,maxPhiCharge);
	}

    }
  return pair<Double_t,Double_t>(-9999,-9999);
}

















Bool_t StFgtGenAVEMaker::getTrack(vector<AVPoint>& points, Double_t ipZ)
{
  //  (*outTxtFile) <<"getTrack" <<endl;
  //  cout <<"get track" <<endl;
  ipZ=-9999; //get ourselves
  ipZ=vtxZ;
  vector<AVPoint>::iterator iter=points.begin();
  Double_t A = 0, B = 0, Cx = 0, Cy = 0, D = 0, Ex = 0, Ey = 0;
  Double_t dist = 0;

  //LOG_INFO << " --------------------------> calling makeLine with " << points.size() << " 0x" << std::hex << discBitArray << std::dec << endm;
  //  cout <<"get track2" <<endl;
  for( ; iter != points.end(); ++iter ){

    Double_t x = iter->x;
    Double_t y = iter->y;
    Double_t z = iter->z;

    A += z*z;
    B += z;
    Cx += x*z;
    Cy += y*z;
    Ex += x;
    Ey += y;

    //    cout << "*** Point located at " << x << ' ' << y << ' ' << z << " Disk: " << iter->dID <<endl;
  }
  //  cout <<"ipZ: " << ipZ <<endl;
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
    //cout << "*** Consts " << A << ' ' << B << ' ' << Cx << ' ' << Cy << ' ' << D << ' ' << Ex << ' ' << Ey << endl;
  Double_t denom = D*A - B*B;
  if( denom ){
    Double_t bx = (-B*Cx + A*Ex)/denom;
    Double_t by = (-B*Cy + A*Ey)/denom;
    Double_t mx = ( D*Cx - B*Ex)/denom;
    Double_t my = ( D*Cy - B*Ey)/denom;
    //    cout <<"bx: " << bx <<" by: " << by <<" mx: " << mx << " my: " << my <<endl;    
    for( iter = points.begin(); iter != points.end(); ++iter ){
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
	  //	  cout <<"distX: " << distX <<" distY: " << distY <<endl;
	  Double_t distance=distX*distX+distY*distY;
	  //	  cout << " got distance " << distance << endl;
	  if((iDx==dId)&&(distance<minDistance))
	    {
	      minDistance=distance;
	      pointIdx=cnt;
	      //	      cout <<"min dist now:" << minDistance<<" for this dik " << iDx <<" pointIdx: " << pointIdx <<endl;
	    }
	  cnt++;
	}
	if(pointIdx>=0)
	  {
	    ///	    cout <<"pushing back " << pointIdx <<endl;
	    redPoints.push_back(points[pointIdx]);
	  }
      }//end of looping over discs

 //// reduced points
    //////have to do a refit now..
    //    cout <<"doing refit... " <<endl;
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
  //    cout <<"after refit: bx: " << bx <<" by: " << by <<" mx: " << mx << " my: " << my <<endl;    
  //    cout <<"we have refit line: "<< bx << " by: " << by <<" mx: " << mx << " my: " << my <<endl;
    ///end of refit

    for(vector<AVPoint>::iterator iterR = redPoints.begin(); iterR != redPoints.end(); ++iterR )
      {
	Double_t distX, distY;
	distX=fabs((iterR->z*mx+bx)-(iterR->x));
	distY=fabs((iterR->z*my+by)-(iterR->y));
	//	cout <<"distX: " << distX <<" distY: " << distY <<endl;
	dist += (distX*distX+distY*distY);
	//	cout <<"adding " << (distX*distX+distY*distY) <<" to chi2: " << endl;
	//cout << "*** DistSq " << dist << endl;
      }
    //          cout <<"get track8" <<endl;
    dist/=D;
    //       cout <<" end chi2: " <<dist <<endl;
    m_tracks.push_back(AVTrack(mx,my,bx,by,ipZ,dist));
    //    cout <<" we have " <<m_tracks.size() <<" track now " <<endl;
    
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
    Double_t zIpExpX0=0;//x(D6Pos-(xD6/xD1)*D1Pos)*1/(1-xD6/xD1);
			  ///at y = 0
    Double_t zIpExpY0=0;//(D6Pos-yD6/yD1*D1Pos)*1/(1-yD6/yD1);

    for(vector<AVPoint>::iterator iter = points.begin(); iter != points.end(); ++iter ){
      //           cout << "--- Location at each disc at z: " << iter->z << " "
      //             << "X: " << mx*iter->z+bx << " vs " << iter->x << ' '
      //           	   << "Y: " << my*iter->z+by << " vs " << iter->y << " "
      //		<< " charge phi: " << iter->phiCharge <<" rcharge: "<< iter->rCharge <<endl;

    };
    //    cout <<endl<<endl;
    //    cout <<"dist again:  " <<dist <<endl;
    vector<AVTrack>::iterator it_lastTrack=m_tracks.end();
    it_lastTrack--;
    pair<double,double> dca=getDca(it_lastTrack);
    Double_t vertZ = (  -( it_lastTrack->mx*it_lastTrack->ax + it_lastTrack->my*it_lastTrack->ay )/(it_lastTrack->mx*it_lastTrack->mx+it_lastTrack->my*it_lastTrack->my));
    (it_lastTrack)->trkZ=vertZ;
    it_lastTrack->dca=dca.second;
    it_lastTrack->ipZ=dca.first;

    //  cout <<"get track10" <<endl;
    if(dist< MAX_DIST_CHI && fabs(vertZ)<50)// && fabs(bx)<40 && fabs(by)<40)
      {
	//	cout <<" track accepted " <<endl;
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
	    Double_t xExp=mx*StFgtGeom::getDiscZ(i)+bx;
	    Double_t yExp=my*StFgtGeom::getDiscZ(i)+by;
	    Int_t quad=-1;
	    //x=r*cos(phi)
	    //y=r*sin(phi)
	    Double_t r=sqrt(xExp*xExp+yExp*yExp);
	    Double_t phi=atan(yExp/xExp);
	    if(phi<0)
	      phi+=MY_PI;
	    if(phi>MY_PI)
	      phi-=2*MY_PI;
	    if(phi<-MY_PI)
	      phi+=2*MY_PI;

	    quad=getQuadFromCoo(xExp,yExp);
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
		Double_t closestPoint=findClosestPoint(xExp,yExp,i);
		//		cout <<"cloest point t " << xExp <<" , " << yExp << " is : " << closestPoint << " away " << endl;
		if(findClosestPoint(xExp,yExp,i)<MAX_DIST2_EFF)
		  {
		    //		    cout <<"found point on eff disk, x: " << xExp <<" y: " << yExp <<endl;
		    radioPlotsEff[i]->Fill(xExp,yExp);
		    hResidua->Fill(sqrt(closestPoint));
		    //		    if(i==m_effDisk)
		      (*outTxtFile) <<"***** found hit in disk " <<i << " at " << xExp<<", " << yExp<<" r: " << r <<" phi: " <<phi << endl;

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
  cout <<"ave make " <<endl;
  Int_t ierr = kStOk;
  (*outTxtFile) <<"----------------------------- Event Nr: " << evtNr<<" -----------------" <<endl;
  StFgtGeneralBase::Make();
  Float_t x;
  Float_t y;
  //  Int_t prvGeoId=-1;
  //  if(vtxRank<=1)
  //    return kStOk;

  //look at the r phi ratio for each disk
  //  for(int i=0;i<6;i++)
  //    {
  //      Double_t ratio=getRPhiRatio(pClusters[i]->begin(),pClusters[i]->end());
  //      rPhiRatioPlots[i]->Fill(ratio);
      //      cout << "ratio for disk: " << i << " is " << ratio <<" disk has: " << tmpClusterCol->getHitVec().size() << "hits" <<endl;
  //    }

  vector<generalCluster> &hitVecD1=*(pClusters[0]);
  vector<generalCluster> &hitVecD6=*(pClusters[5]);


  for(int iD=0;iD<6;iD++)
    {
      cout << " there are " << pClusters[iD]->size() <<" cluster in disk : " << iD+1 <<endl;



      int numClus=0;
      int numPulses=0;

      for(int iq=0;iq<4;iq++)
	{
	  for(int i=0;i<  pStrips[iD*4+iq].size();i++)
	    {
	      if(pStrips[iD*4+iq][i].charge>1000)
		{
		  numClus++;
		}
	      if(validPulse(pStrips[iD*4+iq][i]))
		numPulses++;
	    }
	  cout <<"disk " << iD+1 << " quad " << iq <<" has " << numClus << " charges and " << numPulses <<" pulses " << endl;
	}

    }

  set<Int_t> usedPoints;//saves the points that have been used for tracks (and shouldn't be reused)
  Double_t D1Pos=StFgtGeom::getDiscZ(0);
  Double_t D6Pos=StFgtGeom::getDiscZ(5);
  Double_t zArm=D6Pos-D1Pos;
  vector<generalCluster>::iterator hitIterD1,hitIterD6, hitIterD1R, hitIterD6R, hitIter, hitIter2;
  //  cout <<" there are " << hitVecD1.size() << " hits in d1 "<<endl;
  for(hitIterD1=hitVecD1.begin();hitIterD1 != hitVecD1.end();hitIterD1++)
    {
       Short_t quad=hitIterD1->quad;
           Short_t disc=hitIterD1->disc;
            Short_t strip=hitIterD1->strip;
            Char_t layer=hitIterD1->layer;
      //      if(layer=='P')
      //	cout <<"found phi d1 " <<endl;
      //      else
      //	cout <<"found r d1 " <<endl;
    }
  //  cout <<" there are " << hitVecD6.size() << " hits in d6 "<<endl;
  for(hitIterD1=hitVecD6.begin();hitIterD1 != hitVecD6.end();hitIterD1++)
    {
            Short_t quad=hitIterD1->quad;
            Short_t disc=hitIterD1->disc;
            Short_t strip=hitIterD1->strip;
            Char_t layer=hitIterD1->layer;

      //      if(layer=='P')
      //	cout <<"found phi d6 " <<endl;
      //      else
      //	cout <<"found r d6 " <<endl;
    }

  ///all seed combinations
  for(int iSeed1=0;iSeed1<5;iSeed1++)
    {
      for(int iSeed2=iSeed1+1;iSeed2<6;iSeed2++)
	{
	  //	  (*outTxtFile) << " using " << iSeed1 <<" and " << iSeed2 << " as seeds " << endl;
	  if((iSeed2-iSeed1)<2)//to have large enough lever arm. Also, since three points are required shouldn't matter?
	    continue;

	  if(iSeed1==m_effDisk || iSeed2==m_effDisk)
	    continue;
	  if(pClusters[iSeed1]->size() > MAX_CLUSTERS || pClusters[iSeed2]->size() > MAX_CLUSTERS)
	    {
	      cout <<"too many clusters in the disk!!!"<<endl<<endl;
	      continue;
	    }
	  	  cout <<"using " << iSeed1 << " and " << iSeed2 << " as seed " <<endl;
	  vector<generalCluster> &hitVecSeed1=*(pClusters[iSeed1]);
	  vector<generalCluster> &hitVecSeed2=*(pClusters[iSeed2]);

	  D1Pos=StFgtGeom::getDiscZ(iSeed1);
	  D6Pos=StFgtGeom::getDiscZ(iSeed2);
	  zArm=D6Pos-D1Pos;

	  for(hitIterD1=hitVecSeed1.begin();hitIterD1 != hitVecSeed1.end();hitIterD1++)
	    {
	      //this is from the loose clustering and the cluster doesn't have energy match
	      if(!hitIterD1->hasMatch)
		continue;
	      Short_t quadP=hitIterD1->quad;
	      	      Short_t disc=hitIterD1->disc;
	      	      Short_t strip=hitIterD1->strip;
	      Char_t layer=hitIterD1->layer;
	      
	      Double_t seed1ChargePhi=hitIterD1->CHARGE_MEASURE;
	      //	      cout <<"seed1ChargeP: " << seed1ChargePhi <<endl;
	      //	      	      Double_t seed1ChargePhi=hitIterD1->maxAdcInt;
	      //	      Double_t seed1ChargePhi=hitIterD1->maxAdc;

	      //	      cout <<"seed charge: " << hitIterD1->maxAdcInt <<endl;
	      Double_t seed1SizePhi=hitIterD1->clusterSize;

	      if(quadP>2)
		continue;

	      //do 1D 'fit' with r strips and the (x,y) thing
	      Int_t geoIdSeed1=hitIterD1->centralStripGeoId;
	      if(usedPoints.find(geoIdSeed1)!=usedPoints.end())
		continue;
	      if(layer!='P')
		continue;
	      //    cout <<"ave make1 " <<endl;
	      Float_t phiD1=hitIterD1->posPhi;
	      for(hitIterD1R=hitVecSeed1.begin();hitIterD1R != hitVecSeed1.end();hitIterD1R++)
		{
		  if(!hitIterD1R->hasMatch)
		    continue;
		  Int_t geoIdSeed1R=hitIterD1R->centralStripGeoId;
		  Short_t quadR=hitIterD1R->quad;
		  Short_t disc=hitIterD1R->disc;
		  Short_t strip=hitIterD1R->strip;
		  Char_t layer=hitIterD1R->layer;
		  Double_t seed1ChargeR=hitIterD1R->CHARGE_MEASURE;
				  //	      cout <<"seed1ChargeR: " << seed1ChargeR <<endl;
				  //				  			  Double_t seed1ChargeR=hitIterD1R->maxAdcInt;
				  //			  Double_t seed1ChargeR=hitIterD1R->maxAdc;
		  //		  cout <<"seed chargeR: " << hitIterD1R->maxAdcInt <<endl;
		  Double_t seed1SizeR=hitIterD1R->clusterSize;
		  //		  cout <<"strip idx fro first seed;" << hitIterD1R->centerStripIdx<<endl;
		  //		  cout <<"strip adc2 fro first seed;" << (pStrips[disc*2+quadR])[hitIterD1R->centerStripIdx].adc[2]<<endl;
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
		  for(hitIterD6=hitVecSeed2.begin();hitIterD6 != hitVecSeed2.end();hitIterD6++)
		    {
		      if( !hitIterD6->hasMatch)
			continue;

		      Int_t geoIdSeed2=hitIterD6->centralStripGeoId;
		      Short_t quadP_2=hitIterD6->quad;
		      		      Short_t disc=hitIterD6->disc;
		      		      Short_t strip=hitIterD6->strip;
		      		      Char_t layer=hitIterD6->layer;

				      		      		      Double_t seed2ChargePhi=hitIterD6->CHARGE_MEASURE;
								      //	      cout <<"seed2ChargeP: " << seed2ChargePhi <<endl;
				      //				      Double_t seed2ChargePhi=hitIterD6->maxAdcInt;
				      //				      				      Double_t seed2ChargePhi=hitIterD6->maxAdc;
		      Double_t seed2SizePhi=hitIterD6->clusterSize;
		      if(layer!='P')
			continue;
		      if(usedPoints.find(geoIdSeed2)!=usedPoints.end())
			continue;
		      Float_t phiD6=hitIterD6->posPhi;

		      //    cout <<"ave make3 " <<endl;

		      for(hitIterD6R=hitVecSeed2.begin();hitIterD6R != hitVecSeed2.end();hitIterD6R++)
			{
			  if(!hitIterD6R->hasMatch)
			    continue;
			  Int_t geoIdSeed2R=hitIterD6R->centralStripGeoId;
			  Short_t quadR_2=hitIterD6R->quad;
			  Short_t disc=hitIterD6R->disc;
			  Short_t strip=hitIterD6R->strip;
			  Char_t layer=hitIterD6R->layer;
			  Double_t seed2ChargeR=hitIterD6R->CHARGE_MEASURE;
						  //						  cout <<"seed2ChargeR: " << seed2ChargeR <<endl;

						  //						  			   			  Double_t seed2ChargeR=hitIterD6R->maxAdcInt;
												  //  Double_t seed2ChargeR=hitIterD6R->maxAdc;
			  Double_t seed2SizeR=hitIterD6R->clusterSize;
			  Int_t quadSeed2=-1;

			  if(quadP_2!=quadR_2)
			    continue;
			  quadSeed2=quadP_2;
			  if(layer!='R')
			    continue;
			  if(usedPoints.find(geoIdSeed2R)!=usedPoints.end())
			    continue;

			  vector<AVPoint> v_points;
			  //add the seed points to the points
			  Float_t rD6=hitIterD6R->posR;
			  Double_t xD6=rD6*cos(phiD6);
			  Double_t yD6=rD6*sin(phiD6);
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
			  Double_t xIpExp=xD1+(xD6-xD1)*(-D1Pos)/zArm;
			  Double_t yIpExp=yD1+(yD6-yD1)*(-D1Pos)/zArm;
			  ////at x = 0
			  Double_t zIpExpX0=(D6Pos-(xD6/xD1)*D1Pos)*1/(1-xD6/xD1);
			  ///at y = 0
			  Double_t zIpExpY0=(D6Pos-yD6/yD1*D1Pos)*1/(1-yD6/yD1);
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
			      Double_t diskZ=StFgtGeom::getDiscZ(iD);
			      //expected

			      Double_t xPosExp=xD1+(xD6-xD1)*(diskZ-D1Pos)/zArm;
			      Double_t yPosExp=yD1+(yD6-yD1)*(diskZ-D1Pos)/zArm;
			      Double_t rPosExp=rD1+(rD6-rD1)*(diskZ-D1Pos)/zArm;
			      vector<generalCluster> &hitVec=*(pClusters[iD]);
			      for(hitIter=hitVec.begin();hitIter!=hitVec.end();hitIter++)
				{
				  if( !hitIter->hasMatch)
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
				  //				  cout <<" got phi: " << phi <<endl;
				  				  Double_t phiCharge=hitIter->CHARGE_MEASURE;

				  //				  Double_t phiCharge=hitIter->maxAdcInt;
				  //				  								  Double_t phiCharge=hitIter->maxAdc;
								  //			  Int_t clusterSizePhi=hitIter->clusterSize;
				  //				  if(clusterSizePhi<=1)
				  //				    continue;
				  //    cout <<"ave make5 " <<endl;
				  for(hitIter2=hitVec.begin();hitIter2!=hitVec.end();hitIter2++)
				    {
				  if(!hitIter2->hasMatch)
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
				      x=r*cos(phi);
				      y=r*sin(phi);
				      //				      cout <<"checking with x: " << x << " y: " << y <<endl;
				      //				      cout <<" x, y: " << x <<", " << y << " exp: " << xPosExp << " , " << yPosExp <<endl;
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
					  //					  cout <<"found point with distance : "<< dist2 <<endl;
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
				  //				  cout <<"accepted "  <<endl;
				  double r=iterClosestR->posR;
				  double phi=iterClosestPhi->posPhi;
				  Int_t geoIdR=iterClosestR->centralStripGeoId;
				  Int_t geoIdPhi=iterClosestPhi->centralStripGeoId;

				  double x=r*cos(phi);
				  double y=r*sin(phi);
				 				 // cout <<" point is : " << x <<"," <<y <<endl;



				  //				  Double_t rCharge=iterClosestR->maxAdcInt;
				  //		  Double_t rCharge=iterClosestR->maxAdc;
				  //				  Double_t phiCharge=iterClosestPhi->maxAdcInt;

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
			      //			      cout <<"found " <<endl;
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
					//at least don't duplicate seeds...
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
	 
			}
		    }
		}
	    }

	}
    }

  return ierr;

};
 
StFgtGenAVEMaker::StFgtGenAVEMaker( const Char_t* name): StFgtGeneralBase( name ),runningEvtNr(0),hitCounter(0),hitCounterR(0),printCounter(0)
{
  cout <<"AVE constructor!!" <<endl;

};

StFgtGenAVEMaker::~StFgtGenAVEMaker()
{

  //delete histogram arrays
};

Int_t StFgtGenAVEMaker::Finish(){
  StFgtGeneralBase::Finish();
  cout <<" closing txt file " << endl;
  outTxtFile->close();
  cout <<" 2 " << endl;
  gStyle->SetPalette(1);
  cout <<"AVE finish function " <<endl;
  Int_t ierr = kStOk;
  cout <<" 3 " << endl;

  ///////////////////////////track collection
  vector<AVTrack>::iterator it=m_tracks.begin();
    cout <<"we found " << m_tracks.size() <<" tracks" <<endl;
  int counter=0;
  for(;it!=m_tracks.end();it++)
    {
      cout <<" looking at track " << counter <<endl;
counter++;
      //      cout <<"This track has parameters: ";
      cout <<" mx: " << it->mx <<" my: " << it->my <<" bx: " << it->ax << " by: " << it->ay << " chi2: " << it->chi2 <<endl;
      Double_t vertZ = (  -( it->mx*it->ax + it->my*it->ay )/(it->mx*it->mx+it->my*it->my));

      pair<double,double> dca=getDca(it);

      if(it->chi2<MAX_DIST_CHI && fabs(vertZ)< 50 )
	{
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
  cout <<"canvases etc.. " << endl;
  //////////////////////////////////////////////////
  TCanvas* cRadio=new TCanvas("radioPlots","radioPlot",1000,1500);
  TCanvas* cRadioLoose=new TCanvas("radioPlotsLoose","radioPlotLoose",1000,1500);
  TCanvas* cRadioR=new TCanvas("radioPlotsR","radioPlotR",1000,1500);
  TCanvas* cRadioPhi=new TCanvas("radioPlotsPhi","radioPlotPhi",1000,1500);

  TCanvas* cRadioHits=new TCanvas("radioPlotsHits","radioPlotHits",1000,1500);
  TCanvas* cRadioNonHits=new TCanvas("radioPlotsNonHits","radioPlotNonHits",1000,1500);
  cout <<"divide "<<endl;
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
  cout <<"drawing hits " <<endl;
  for(Int_t iD=0;iD<kFgtNumDiscs;iD++)
    {
      //      cRadio->cd(iD+1)->SetLogz();
      cout <<iD <<" 1 " << endl;
      cRadioHits->cd(iD+1);
      cout <<iD <<" 2 " << endl;
      radioPlotsEff[iD]->Draw("colz");
      cout <<iD <<" 3 " << endl;
      cRadioNonHits->cd(iD+1);
      cout <<iD <<" 4 " << endl;
      radioPlotsNonEff[iD]->Draw("colz");
      cout <<iD <<" 5 " << endl;
    }
  char buffer[100];


  sprintf(buffer,"%s/clusterPics.root",fileBase);
  cout <<"setting cluster pic file to : " << buffer <<endl;
  TFile *fClu = new TFile(buffer,"recreate");
  fClu->cd();

  for(int i=0;i<v_hClusP.size();i++)
    {
      (v_hClusP[i])->Write();
    }
  for(int i=0;i<v_hClusR.size();i++)
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
  
  for(int iD=0;iD<kFgtNumDiscs;iD++)
    {
      for(int iq=0;iq<4;iq++)
	{


	  cout <<"writing id: " << iD <<  " iq: " << iq <<endl;
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


  cout <<"writen and closed " << endl;

  cout <<"drawing hits2 " <<endl;
  cRadioHits->SaveAs("radioPlotsHits.png");
  cRadioNonHits->SaveAs("radioPlotsNonHits.png");
  cout <<" 5 " << endl;

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
  cIPProj.SaveAs("ipProj.png");

  hBx->Draw();
  hBx->Write();
  cIPProj.SaveAs("hBx.png");
  hBy->Draw();
  hBy->Write();
  cIPProj.SaveAs("hBy.png");
  hMx->Draw();
  hMx->Write();
  cIPProj.SaveAs("hMx.png");
  hMy->Draw();
  hMy->Write();
  cIPProj.SaveAs("hMy.png");

  
  hIpZ->Draw();
  hIpZ->Write();
  cIPProj.SaveAs("ipZ.png");


  hIpDca->Draw();
  hIpDca->Write();
  cIPProj.SaveAs("ipDca.png");

  hTrkZ->Draw();
  hTrkZ->Write();
  cIPProj.SaveAs("hTrkZ.png");

  hResidua->Draw();
  hResidua->Write();
  cIPProj.SaveAs("hResidua.png");

  hChi2->Draw();
  hChi2->Write();
  cIPProj.SaveAs("chi2Dist.png");

  tpcFgtZVertexCorr->Draw("colz");
  tpcFgtZVertexCorr->Write();
  cIPProj.SaveAs("tpcFgtCorr.png");

  tpcFgtZVertexCorr2->Draw("colz");
  tpcFgtZVertexCorr2->Write();
  cIPProj.SaveAs("tpcFgtCorr2.png");
  tpcFgtZVertexCorr3->Draw("colz");
  tpcFgtZVertexCorr3->Write();
  cIPProj.SaveAs("trackTrackZCorr.png");


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

  cClusterSizeR->SaveAs("clusterSizeR.png");
  cClusterSizePhi->SaveAs("clusterSizePhi.png");
  cChargeCorr->SaveAs("chargeCorrelation.png");

  cClusterChargeR->SaveAs("clusterChargeR.png");
  cClusterChargePhi->SaveAs("clusterChargePhi.png");

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
      cout <<" disk: " << iD <<endl;

      ////argh, overflow bins!!
      for(int nx=1;nx<radioPlotsEff[iD]->GetNbinsX()+1;nx++)
	{
	  for(int ny=1;ny<radioPlotsEff[iD]->GetNbinsY()+1;ny++)
	    {
	      Double_t denom=radioPlotsEff[iD]->GetBinContent(nx,ny);
	      if(denom>0 && (tmpAllCounts->GetBinContent(nx,ny)/denom)<=1.0)
		{
		  cout <<" efficiency bin nx: " << nx << " ny: " << ny << ", num counts " << tmpAllCounts->GetBinContent(nx,ny) << " / " << denom << " : " << tmpAllCounts->GetBinContent(nx,ny)/denom <<endl;
		  radioPlotsEff[iD]->SetBinContent(nx,ny,tmpAllCounts->GetBinContent(nx,ny)/denom);
		  if(iD==m_effDisk)
		    {
		      cout <<"chargeRatio is: " << chargeRatioInEffDisk->GetBinContent(nx,ny)<<endl;
		      chargeRatioInEffDisk->SetBinContent(nx,ny,chargeRatioInEffDisk->GetBinContent(nx,ny)/denom);
		      chargeAsymInEffDisk->SetBinContent(nx,ny,chargeAsymInEffDisk->GetBinContent(nx,ny)/denom);
		    }
		}
	      else
		{
		  cout <<" efficiency bin nx: " << nx << " ny: " << ny << ", num counts " << tmpAllCounts->GetBinContent(nx,ny) << " / " << denom << " : 0.0" <<endl;
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

  cRadio->SaveAs("radioPlotsEff.png");
  cRadio->SaveAs("radioPlotsEff.pdf");

  cRadioR->SaveAs("radioPlotsR.png");
  cRadioPhi->SaveAs("radioPlotsPhi.png");
  cRadioLoose->SaveAs("radioPlotsLoose.png");

  cRadio->cd(0);
  chargeRatioInEffDisk->Draw("colz");
  cRadio->SaveAs("chargeRatioInEffDisk.png");
  chargeAsymInEffDisk->Draw("colz");
  cRadio->SaveAs("chargeAsymInEffDisk.png");
  chargeCorrInEffDisk->Draw("colz");
  chargeCorrInEffDisk->Write();
  cRadio->SaveAs("chargeCorrInEffDisk.png");
  hChargeAsym->Draw();
  cRadio->SaveAs("chargeAsym.png");
  hChargeRatio->Draw();
  cRadio->SaveAs("chargeRatio.png");


  cREff->SaveAs("rEff.png");
  cREff->SaveAs("rEff.pdf");

  cRPRatio->SaveAs("rpRatio.png");
  cRPRatio->SaveAs("rpRatio.pdf");
  f1->Write();
  ////this has to be the last thing!!!! Otherwise the histos become invalid and the code seg faults...
  f1->Close();
  myRootFile->Write();
  myRootFile->Close();
  cout <<"returning after finish" <<endl;
  return ierr;
};


/**
   construct histograms

*/
Int_t StFgtGenAVEMaker::Init(){
  outTxtFile=new ofstream;
  outTxtFile->open("clusExpectations.txt");
  cout <<"AVE!!" <<endl;
  myRootFile=new TFile("clusterEff.root","RECREATE");
  //  outTxtFile=new ofstream;
  //  outTxtFile->open("clusters.txt");


  Int_t ierr = kStOk;

  char buffer[100];


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


  createPlots(&firstTbSigCloseClusterR,kFgtNumDiscs*4,"firstTbSigCloseClusterR",100,0,20);
  createPlots(&firstTbSigCloseClusterP,kFgtNumDiscs*4,"firstTbSigCloseClusterP",100,0,20);
  createPlots(&firstTbSigTrackClusterR,kFgtNumDiscs*4,"firstTbSigTrackClusterR",100,0,20);
  createPlots(&firstTbSigTrackClusterP,kFgtNumDiscs*4,"firstTbSigTrackClusterP",100,0,20);


  createPlots(&maxAdcTrackClusterR,kFgtNumDiscs*4,"maxAdcTrackClusterR",100,0,5000);
  createPlots(&maxAdcCloseClusterR,kFgtNumDiscs*4,"maxAdcCloseClusterR",100,0,5000);
  createPlots(&maxSigTrackClusterR,kFgtNumDiscs*4,"maxSigTrackClusterR",100,1,200);
  createPlots(&maxSigCloseClusterR,kFgtNumDiscs*4,"maxSigCloseClusterR",100,1,200);
  createPlots(&numFSigTrackClusterR,kFgtNumDiscs*4,"numFSigTrackClusterR",8,0,7);
  createPlots(&maxTbCloseClusterR,kFgtNumDiscs*4,"maxTbCloseClusterR",8,0,7);

  createPlots(&maxTbCloseClusterP,kFgtNumDiscs*4,"maxTbCloseClusterP",8,0,7);

  createPlots(&maxTbTrackClusterR,kFgtNumDiscs*4,"maxTbTrackClusterR",8,0,7);

  createPlots(&maxTbTrackClusterP,kFgtNumDiscs*4,"maxTbTrackClusterP",8,0,7);
  createPlots(&numFSigCloseClusterR,kFgtNumDiscs*4,"numFSigCloseClusterR",8,0,7);

  createPlots(&numFirstHighTrackClusterR,kFgtNumDiscs*4,"numFirstHighTrackClusterR",8,0,7);
  createPlots(&numFirstHighCloseClusterR,kFgtNumDiscs*4,"numFirstHighCloseClusterR",8,0,7);

  createPlots(&maxAdcTrackClusterP,kFgtNumDiscs*4,"maxAdcTrackClusterP",100,0,5000);
  createPlots(&maxAdcCloseClusterP,kFgtNumDiscs*4,"maxAdcCloseClusterP",100,0,5000);
  createPlots(&maxSigTrackClusterP,kFgtNumDiscs*4,"maxSigTrackClusterP",100,1,200);
  createPlots(&maxSigCloseClusterP,kFgtNumDiscs*4,"maxSigCloseClusterP",100,1,200);
  createPlots(&numFSigTrackClusterP,kFgtNumDiscs*4,"numFSigTrackClusterP",8,0,7);
  createPlots(&numFSigCloseClusterP,kFgtNumDiscs*4,"numFSigCloseClusterP",8,0,7);
  createPlots(&numFirstHighTrackClusterP,kFgtNumDiscs*4,"numFirstHighTrackClusterP",8,0,7);
  createPlots(&numFirstHighCloseClusterP,kFgtNumDiscs*4,"numFirstHighCloseClusterP",8,0,7);
  createPlots(&secondToLastRatioCloseClusterP,kFgtNumDiscs*4,"secondToLastRatioClosClusterP",100,0,5);

  createPlots(&secondToLastRatioCloseClusterR,kFgtNumDiscs*4,"secondToLastRatioClosClusterR",100,0,5);

  createPlots(&secondToLastRatioTrackClusterP,kFgtNumDiscs*4,"secondToLastRatioTrackClusterP",100,0,5);

  createPlots(&secondToLastRatioTrackClusterR,kFgtNumDiscs*4,"secondToLastRatioTrackClusterR",100,0,5);


  rEff=new TH1D*[kFgtNumDiscs];
  rNonEff=new TH1D*[kFgtNumDiscs];
  cout <<"ave2" << endl;

  clusterSizeP=new TH1D*[kFgtNumDiscs*4];
  clusterSizeR=new TH1D*[kFgtNumDiscs*4];
  chargeCorr=new TH2D*[kFgtNumDiscs*4];
  h_clusterSizeR=new TH1D*[kFgtNumDiscs];
  h_clusterSizePhi=new TH1D*[kFgtNumDiscs];

  h_clusterChargeR=new TH1D*[kFgtNumDiscs];
  h_clusterChargePhi=new TH1D*[kFgtNumDiscs];
  cout <<"ave3" << endl;

  hIp=new TH2D("Proj_to_IP","Proj_to_Ip",50,-100,100,50,-100,100);
  hBx=new TH1D("hBx","hBx",50,-100,100);
  hBy=new TH1D("hBy","hBy",50,-100,100);
  hMx=new TH1D("hMx","hMx",50,-100,100);
  hMy=new TH1D("hMy","My",50,-0.1,0.1);
  hIpZ=new TH1D("IP_Z","IP_Z",50,-100,100);

  hIpDca=new TH1D("ipDCA","ipDCA",50,-100,100);
  hTrkZ=new TH1D("z_Vtx_From_trk_fit","z_Vtx_From_trk_fit",50,-100,100);
  hResidua=new TH1D("residua","residua",100,0,50);
  hChi2=new TH1D("chi2","chi2",50,0,2);
  tpcFgtZVertexCorr=new TH2D("tpc_fgt_corr","tpc_fgt_corr",100,-120,120,100,-120,120);
  tpcFgtZVertexCorr2=new TH2D("tpc_fgt_corr2","tpc_fgt_corr2",100,-120,120,100,-120,120);
  tpcFgtZVertexCorr3=new TH2D("fgt_fgt_corr","fgt_fgt_corr",50,-50,50,50,-50,50);
  cout <<"ave4" << endl;
  for(int iD=0;iD<kFgtNumDiscs;iD++)
    {
      cout <<"id: " << iD <<endl;
      sprintf(buffer,"radioDiskEff_%d",iD);
      radioPlotsEff[iD]=new TH2D(buffer,buffer,NUM_EFF_BIN,-DISK_DIM,DISK_DIM,NUM_EFF_BIN,-DISK_DIM,DISK_DIM);
      cout <<"-1" <<endl;
      sprintf(buffer,"radioDiskEffR_%d",iD);
      radioPlotsEffR[iD]=new TH2D(buffer,buffer,NUM_EFF_BIN,-DISK_DIM,DISK_DIM,NUM_EFF_BIN,-DISK_DIM,DISK_DIM);
      cout <<"-2" <<endl;
      sprintf(buffer,"radioDiskEffPhi_%d",iD);
      radioPlotsEffPhi[iD]=new TH2D(buffer,buffer,NUM_EFF_BIN,-DISK_DIM,DISK_DIM,NUM_EFF_BIN,-DISK_DIM,DISK_DIM);
      cout <<"-3" <<endl;
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
      cout <<" created non eff histo " << endl;
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
      //      cout <<"4" <<endl;
      sprintf(buffer,"rPhiRatio_%d",iD);
      rPhiRatioPlots[iD]=new TH1D(buffer,buffer,100,-2,10);
    }

  return ierr;
};
ClassImp(StFgtGenAVEMaker);
