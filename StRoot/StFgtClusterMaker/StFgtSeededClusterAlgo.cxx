//
//  $Id: StFgtSeededClusterAlgo.cxx,v 1.31 2014/01/16 18:22:51 xuanli Exp $
//  $Log: StFgtSeededClusterAlgo.cxx,v $
//  Revision 1.31  2014/01/16 18:22:51  xuanli
//  Fix the error for stardev production
//
//  Revision 1.30  2013/03/24 20:25:24  jeromel
//  SeededClusterAlgo created a canvas on exit, removed
//
//  Revision 1.29  2013/03/15 15:53:49  avossen
//  check seed before overwriting w/ next to cluster flag
//
//  Revision 1.28  2013/03/13 15:57:35  akio
//  Fix a bug with ZS data and phi-even strip clustering logic
//  Also remove some kStFgtNumTimebins and use dynamic local mMaxTimeBin from StFgtCollection
//
//  Revision 1.27  2013/02/20 23:33:28  avossen
//  add strips on both sides of the cluster
//
//  Revision 1.26  2013/02/20 02:18:19  avossen
//  *** empty log message ***
//
//  Revision 1.25  2013/02/20 01:32:27  avossen
//  added n strips before and after cluster
//
//  Revision 1.24  2013/02/19 18:24:04  avossen
//  *** empty log message ***
//
//  Revision 1.23  2012/12/10 23:18:01  avossen
//  merged cluster finder
//
//  Revision 1.22  2012/11/27 18:00:07  akio
//  - Filling NStrip/SeedType/MaxTimebin/EvenOddChargeAsy in StFgtHit
//  - Accepting kFgtSeedTypes4 & 5 for clustring
//  - Setting kFgtClusterTooBig instead of kFgtClusterNo into strips when cluster is too big
//    (Seedtype of the seed strip will not be overwritten)
//  - Adding setThreshold2AddStrip() [proposed default 2 ~ 3 from cosmic data... no idea for run12 data]
//    This will be used in isSameCluster() and this number times ChargeUncert() will be the threshold on charge
//    (timebin sum) for a strip to be included in cluster if neighbore. It was hard coded to be 1.0 before.
//    Too low threshold sometimes kill cluster because "cluster too big"
//  - Slight code change to make trying different weight for getting R/PHI easy...
//  - Slight code change for phi/even strip logic... hopefully improved
//
//  Revision 1.21  2012/07/03 23:21:24  avossen
//  *** empty log message ***
//
//  Revision 1.20  2012/07/03 03:32:29  avossen
//  ordinate now computed correctly...
//
//  Revision 1.19  2012/06/30 23:30:37  avossen
//  small changes in seeded cluster finder
//
//  Revision 1.18  2012/06/14 04:43:03  avossen
//  fixed geoId bug
//
//  Revision 1.17  2012/06/12 19:28:48  avossen
//  *** empty log message ***
//
//  Revision 1.16  2012/06/10 01:05:19  avossen
//  *** empty log message ***
//
//  Revision 1.15  2012/06/09 18:03:50  avossen
//  fixed bug
//
//  Revision 1.14  2012/06/08 20:22:21  avossen
//  updated cluster algo to find even p clusters
//
//  Revision 1.13  2012/06/08 03:52:41  avossen
//  added one sigma strips to clusters
//
//  Revision 1.12  2012/03/17 23:30:16  avossen
//  fudged the cluster maker
//
//  Revision 1.11  2012/03/16 19:50:18  balewski
//  *** empty log message ***
//
//  Revision 1.10  2012/03/16 19:49:46  avossen
//  *** empty log message ***
//
//  Revision 1.9  2012/03/16 19:43:19  avossen
//  added option to allow to jump strips
//
//  Revision 1.8  2012/03/16 19:41:15  avossen
//  added option to allow to jump strips
//
//  Revision 1.7  2012/03/08 17:43:40  avossen
//  added default cluster algo, made StFgtIClusterAlgo destructor =0
//
//  Revision 1.6  2012/03/07 18:07:45  sgliske
//  StFgtStrip::getClusterSeed() -> StFgtStrip::getClusterSeedType
//  StFgtStrip::setClusterSeed() -> StFgtStrip::setClusterSeedType
//
//  Revision 1.5  2012/03/07 03:57:23  avossen
//  various updates
//
//  Revision 1.4  2012/03/06 18:54:28  avossen
//  added weighted mean and error to seeded clustering
//
//  Revision 1.3  2012/03/01 16:38:13  avossen
//  implemented tweaks to clustering
//
//  Revision 1.2  2012/02/29 20:29:08  avossen
//  changes to seed and cluster algo
//
//  Revision 1.1  2012/02/28 19:34:29  avossen
//   added new cluster maker
//

// \class StFgtSeededClusterAlgo
// \author Anselm Vossen (avossen@indiana.edu)
//
#include "TStyle.h"
#include "StFgtSeededClusterAlgo.h"
#include "StRoot/StFgtUtil/geometry/StFgtGeom.h"
//#include <TCanvas.h>
#include "StRoot/StEvent/StFgtCollection.h"
#include "StRoot/StEvent/StFgtStripCollection.h"
#include "StRoot/StEvent/StFgtStrip.h"
#include "StRoot/StEvent/StFgtHitCollection.h"
#include "StRoot/StEvent/StFgtHit.h"
#include "StRoot/StFgtUtil/StFgtConsts.h"
#include "StRoot/StFgtDbMaker/StFgtDbMaker.h"
#include "StRoot/StFgtDbMaker/StFgtDb.h"


//for floor
#include <math.h>
#include <TH1F.h>
#include <TH2F.h>
#include <TF1.h>
#include <TGraphAsymmErrors.h>
#include <TGraphErrors.h>
#include <TGraph.h>

//#define KEEP_BIG_AND_NOISY_CLUSTERS


StFgtSeededClusterAlgo::StFgtSeededClusterAlgo():up(true),down(false),stepTwo(true),mThreshold2AddStrip(3.0),numAdditionalStrips(2),mDb(0)
{
  //nothing else to do....
};


Int_t StFgtSeededClusterAlgo::Init()
{
  hGaussFitStatus=new TH1D("fgt_gFitStatus","gFitStatus",10,-1,8);
  hGaussFitChi2=new TH1D("fgt_gFitChi2overNdf","gFitChi2overNdf",100,0,10);
  hTbFitStatus=new  TH1D("fgt_tbFitStatus","tbFitStatus",10,-1,8);
  hTbFitChi2=new  TH1D("fgt_tbFitChi2","tbFitChi2",100,0,10);

  hTbMaxCorr=new TH2D("fgt_tbmaxcorr","tbmaxcorr",200,0,1000,200,0,1000);
  hTbMaxRatio=new TH1D("fgt_tbmaxratio","tbmaxratio",200,0,1);
  hTbSideCorr=new TH2D("fgt_tbsidecorr","tbsidecorr",200,0,1000,200,0,1000);
  hTbSideRatio=new TH1D("fgt_tbsideratio","tbsideratio",200,0,1);

  return kStOk;
};
//void StFgtSeededClusterAlgo::doStripFit(stripWeightMap_t &strips)
void StFgtSeededClusterAlgo::doStripFit(void* stripsT)
{
  stripWeightMap_t& strips = *((stripWeightMap_t*) stripsT);
  Double_t x[40];
  Double_t y[40];
  Double_t ex[40];
  Double_t ey[40];
  int numStripsInClu=strips.size();
  //    TF1 *func = new TF1("func","[0]*(x>[1])*(x-[1])**2*exp(-(x-[1])*[2])");
  TF1 *func = new TF1("func","[0]*(x>[1])*(x-[1])**2*exp(-(x-[1])*0.55)");
  func->SetParName(0,"A");
  func->SetParName(1,"t0");
  //  func->SetParName(2,"invtau");

  int stripIt=0;
  int stripMaxPos=-1;
  int stripMaxAdc=-1;
  for(stripWeightMap_t::iterator it=strips.begin();it!=strips.end();it++)
    {
      //      cout <<"setting bin " << binCounter << " to " << it->first->getCharge()<<endl;
      //      h1->SetBinContent(binCounter,it->first->getCharge());
      //      StFgtGeom::getPhysicalCoordinate(it->first->getGeoId(),disc,quadrant,layer,ordinate,lowerSpan,upperSpan);

      for(int tb=0;tb<mMaxTimeBin;tb++)
	{
	  y[tb]=it->first->getAdc(tb);
	  if(y[tb]>stripMaxAdc)
	    {
	      stripMaxPos=stripIt;
	      stripMaxAdc=y[tb];
	    }
	}
      stripIt++;
    }
  stripIt=0;
  for(stripWeightMap_t::iterator it=strips.begin();it!=strips.end();it++)
    {
      //      cout <<"setting bin " << binCounter << " to " << it->first->getCharge()<<endl;
      //      h1->SetBinContent(binCounter,it->first->getCharge());
      //      StFgtGeom::getPhysicalCoordinate(it->first->getGeoId(),disc,quadrant,layer,ordinate,lowerSpan,upperSpan);
      //      cout <<"adc vals: ";
      for(int tb=0;tb<mMaxTimeBin;tb++)
	{
	  x[tb]=tb;
	  y[tb]=it->first->getAdc(tb);
	  ey[tb]=it->first->getPedErr();
	  ex[tb]=0;
	  //	  cout <<y[tb] <<", ";
	}
      //      cout <<endl;
      TGraphErrors* tg1=new TGraphErrors(mMaxTimeBin,x,y,ex,ey);
      Int_t tbFitStatus=tg1->Fit(func);
      float amp,t0,invtau,chi2Ndf, chi2;
      amp=func->GetParameter(0);
      t0=func->GetParameter(1);
      //      invtau=func->GetParameter(2);
      chi2Ndf=func->GetChisquare()/(float)func->GetNDF();
      chi2=func->GetChisquare();
      if(chi2Ndf>0.1 && chi2Ndf<2&& tbFitStatus==0)
	{
	  if(stripIt==stripMaxPos)
	    {
	      hTbMaxCorr->Fill(it->first->getCharge(),amp);
	      hTbMaxRatio->Fill(amp/it->first->getCharge());
	    }
	  else
	    {
	      hTbSideCorr->Fill(it->first->getCharge(),amp);
	      hTbSideRatio->Fill(amp/it->first->getCharge());
	    }
	  it->first->setCharge(amp);
	}
      stripIt++;
      hTbFitStatus->Fill(tbFitStatus);
      hTbFitChi2->Fill(chi2Ndf);
      delete tg1;
    }
  delete func;
}


Float_t StFgtSeededClusterAlgo::doClusterShapeFit(void* stripsT)
{
  stripWeightMap_t& strips = *((stripWeightMap_t*) stripsT);
  Double_t x[40];
  Double_t y[40];
  Double_t ex[40];
  Double_t ey[40];
  Double_t ordinate, lowerSpan, upperSpan;
  Short_t disc, quadrant;
  Char_t layer;
  Double_t firstOrd,lastOrd;
  StFgtGeom::getPhysicalCoordinate(strips.begin()->first->getGeoId(),disc,quadrant,layer,firstOrd,lowerSpan,upperSpan);
  StFgtGeom::getPhysicalCoordinate((strips.rbegin())->first->getGeoId(),disc,quadrant,layer,lastOrd,lowerSpan,upperSpan);
  if(firstOrd>lastOrd)
    {
      Double_t tmpOrd=lastOrd;
      lastOrd=firstOrd;
      firstOrd=tmpOrd;
    }
  Double_t sigGuess=(lastOrd-firstOrd)/2;
  Double_t ampGuess=-9999;//set later
  Double_t meanGuess=0;//set later

  //    TH1F* h1=new TH1F("h1","h1",numStripsInClu,firstOrd,lastOrd);

  int binCounter=0;//1 for histo

  for(stripWeightMap_t::iterator it=strips.begin();it!=strips.end();it++)
    {
      //      cout <<"setting bin " << binCounter << " to " << it->first->getCharge()<<endl;
      //      h1->SetBinContent(binCounter,it->first->getCharge());
      StFgtGeom::getPhysicalCoordinate(it->first->getGeoId(),disc,quadrant,layer,ordinate,lowerSpan,upperSpan);
      x[binCounter]=ordinate;
      y[binCounter]=it->first->getCharge();
      if(y[binCounter]>ampGuess)
	ampGuess=y[binCounter];
      meanGuess+=ordinate;
      ey[binCounter]=it->first->getChargeUncert()*1.6;//akio's factor
      ex[binCounter]=0;
      //      cout <<"counter: " << binCounter << " x: " << ordinate <<" charge: "<< y[binCounter] <<" error: " << ey[binCounter] <<endl;
      binCounter++;
    }
  meanGuess/=strips.size();

  TGraphErrors* tg=new TGraphErrors(binCounter,x,y,ex,ey);
  //  h1->Fit("gaus");


  TF1* f1=new TF1("f1","gaus",firstOrd-0.001,lastOrd+0.001);
  f1->SetParameters(ampGuess,meanGuess,sigGuess);  

  Int_t fitStatus=tg->Fit("f1");
  hGaussFitChi2->Fill(f1->GetChisquare()/(float)f1->GetNDF());
  hGaussFitStatus->Fill(fitStatus);
  //  TF1* f1=(TF1*)h1->GetFunction("gaus");
  //  TF1* f1=(TF1*)tg->GetFunction("gaus");
  float fAmp=f1->GetParameter(1);
  //  delete h1;
  delete f1;
  delete tg;
  //  cout <<"del function" << endl;
  //probably not a good idea...
  //  delete f1;

  /////end fitting
  if(binCounter>=3&&fitStatus==0&& fAmp>=firstOrd && fAmp<=lastOrd)
    return fAmp;
  else 
    return -1;
}

///fill in cluster info like charge from the strips 
void StFgtSeededClusterAlgo::FillClusterInfo(StFgtHit* cluster,StFgtStripCollection& allStrips)
{
  Double_t ordinate, lowerSpan, upperSpan;
  Short_t disc, quadrant;
  Char_t layer;
  Double_t accuCharge=0;
  Double_t accuChargeWeight=0;
  Double_t accuChargeWeightEven=0;
  Double_t accuChargeEven=0;
  Double_t accuChargeUneven=0;
  Double_t accuChargeSq=0;
  Double_t accuChargeSqEven=0;
  Double_t accuChargeSqWeight=0;
  Double_t accuChargeError=0;
  Double_t accuChargeErrorEven=0;
  Int_t numStrips=0;
  Int_t numStripsEven=0;
  Double_t meanOrdinate=0;
  Double_t meanOrdinateWeight=0;
  Double_t meanSqOrdinate=0;
  Double_t meanOrdinateEven=0;
  Double_t meanOrdinateEvenWeight=0;
  Double_t meanSqOrdinateEven=0;
  Double_t meanGeoId=0;
  Double_t meanGeoIdEven=0;
  Bool_t chargeEven=false;///phi strip in r <20

  stripWeightMap_t &strips = cluster->getStripWeightMap();

  float a[kFgtNumTimeBins]; memset(a,0,sizeof(a)); // adc vs timebin (sum of all strips in cluster)
  float e[kFgtNumTimeBins]; memset(e,0,sizeof(e)); // adc err vs timebin (sum of all strips in cluster)
  int flag[kFgtNumTimeBins]; memset(flag,0,sizeof(flag)); //if saturated
  int seedtype=0;


  ///begin fitting
#ifdef DO_FGT_STRIP_FIT
  doStripFit(&strips);
#endif

  Float_t clusterShapeFitAmp=-1;
#ifdef DO_FGT_CLUSTER_SHAPE_FIT
  clusterShapeFitAmp=doClusterShapeFit(&strips);
#endif

  for(stripWeightMap_t::iterator it=strips.begin();it!=strips.end();it++)
    {
      Double_t charge=it->first->getCharge();
      //Double_t charge=it->first->getMaxAdc();
      Double_t weight=charge;
      //Double_t weight=charge*charge;
      //Double_t weight=sqrt(charge);
      //Double_t weight=log(charge);
      accuCharge+=charge;
      accuChargeWeight+=weight;
      for(int i=0; i<mMaxTimeBin; i++) {
	a[i]+=it->first->getAdc(i);
	e[i]+=it->first->getPedErr() * it->first->getPedErr();
	if(it->first->getAdc(i) > 2800) {flag[i]=1;}
      }
      int type=it->first->getClusterSeedType();
      if(type>=kFgtSeedType1 && type<kFgtSeedTypeMax) {
	if(seedtype==0) {seedtype=type;}
	else if(type<seedtype) {seedtype=type;}   //take smallest seedtype besides kFgtSeedTypeNo as cluster seed type
      }
      //      cout <<"looking at geo id " << it->first->getGeoId() <<" for cluster info filling " <<endl;
      StFgtGeom::getPhysicalCoordinate(it->first->getGeoId(),disc,quadrant,layer,ordinate,lowerSpan,upperSpan);
      if(it->first->getGeoId()%2)
	accuChargeUneven+=charge;
      else
	{
	  accuChargeWeightEven+=weight;
	  numStripsEven++;
	  accuChargeEven+=charge;
	  //accuChargeSqEven+=(charge*charge);
	  accuChargeSqEven+=(weight*weight);
	  accuChargeErrorEven+=it->first->getChargeUncert();
	  //meanSqOrdinateEven+=ordinate*ordinate*charge*charge;
	  meanSqOrdinateEven+=ordinate*ordinate*weight*weight;
	  //meanGeoIdEven+=((it->first->getGeoId())*(charge));
	  meanGeoIdEven+=((it->first->getGeoId())*(weight));
	  meanOrdinateEven+=ordinate*charge;
	  meanOrdinateEvenWeight+=ordinate*weight;
	}

      //accuChargeSq+=(charge*charge);
      accuChargeSq+=(weight*weight);
      accuChargeError+=it->first->getChargeUncert();
      numStrips++;
      
      meanOrdinate+=ordinate*charge;
      meanOrdinateWeight+=ordinate*weight;
      //meanSqOrdinate+=ordinate*ordinate*charge*charge;
      meanSqOrdinate+=ordinate*ordinate*weight*weight;
      //      cout <<"charge: " << charge << " ordinate: " << ordinate << " meanSqOrd: " << meanSqOrdinate << endl;
      //meanGeoId+=((it->first->getGeoId())*(charge));
      meanGeoId+=((it->first->getGeoId())*(weight));
    }
  //  cout <<"setting seedtype: " << seedtype <<endl;
  cluster->setSeedType(seedtype);

  int maxtbin;
  float maxadc=0.0;
  for(int i=0; i<mMaxTimeBin; i++){
    e[i]=sqrt(e[i]);
    if(a[i]>maxadc){
      maxadc=a[i];      
      maxtbin=i;
    }
  }
  cluster->setMaxTimeBin(maxtbin);
  cluster->calcMaxAdc();
  
#ifdef __LANDAU_FIT__
  //landau fitting with asymm error
  TGraphAsymmErrors *g = new TGraphAsymmErrors(mMaxTimeBin);
  for(int i=0; i<mMaxTimeBin; i++){
    g->SetPoint(i,float(i),float(a[i]));
    float eh=e[i];
    if(flag[i]==1) eh=2000;
    g->SetPointError(i,0,0,e[i],eh);
    //printf("tmax=%2d i=%2d adc=%5.0f err=%4.0f\n",mMaxTimeBin,i,a[i],e[i]);
  }
  int res=g->Fit("landau","0Q");
  TF1 *f = g->GetFunction("landau");  
  float chi2=f->GetChisquare()/float(f->GetNDF());
  cluster->setLandau(f->GetParameter(0),f->GetParameter(1),f->GetParameter(2),chi2);
  //printf("Norm=%6.1f Mpv=%6.2f Sigma=%6.2f Chi2=%6.2f\n",f->GetParameter(0),f->GetParameter(1),f->GetParameter(2),chi2);
#endif

  float asy=(accuChargeEven-accuChargeUneven)/accuCharge;
  cluster->setEvenOddChargeAsy(asy);
  if(asy>0.8 && layer=='P' && numStripsEven>=2)
    {
      //      cout <<"charge even cluster " << endl;
      chargeEven=true;
      //cout <<"r probably < 20"<<endl;
      //r is probably in r<20
      //      accuCharge-=accuChargeUneven;
      stripWeightMap_t::iterator it=strips.begin();
      while(it!=strips.end())
	{
	  // cout <<"strip geoId: "<< it->first->getGeoId() << endl;
	  if(it->first->getGeoId()%2)
	    {
	      //	      cout <<"erased! " << endl;
	      //      it->first->setClusterSeedType(kFgtSeedTypeNo);
	      ///let's keep it though...
	      if(it->first->getClusterSeedType()==kFgtSeedTypeNo || it->first->getClusterSeedType()>kFgtClusterSeedInSeaOfNoise)
		it->first->setClusterSeedType(kFgtNextToCluster);
	      strips.erase(it);
	      if(!strips.empty())
		it=strips.begin();
	      else
		break;
	    }
	  else
	    it++;
	}
      //            cout <<"done erasing"<<endl;
    }

  stripWeightMap_t::reverse_iterator itBack=strips.rbegin();
  //  if(strips.size()>1)
  //    {
  if(itBack->first->getClusterSeedType()==kFgtClusterPart)
    itBack->first->setClusterSeedType(kFgtClusterEndUp);
  //      if(strips.begin()->first->getClusterSeedType()>=kFgtSeedType1 && strips.begin()->first->getClusterSeedType()<=kFgtSeedType3)
  if(strips.begin()->first->getClusterSeedType()==kFgtClusterPart)
    (strips.begin())->first->setClusterSeedType(kFgtClusterEndDown);
  //    }

  if(chargeEven)
    {
      cluster->setCharge(accuChargeEven);
      meanOrdinateEven /= (Double_t)accuChargeEven;
      meanOrdinateEvenWeight /= (Double_t)accuChargeWeightEven;
      //	  	  cout <<" event charge: geoevn: " << meanGeoIdEven <<" charge: " << accuChargeWeightEven << " geo: " << meanGeoIdEven/accuChargeWeightEven <<endl;
      meanGeoIdEven /= accuChargeWeightEven;
      meanSqOrdinateEven /= (Double_t)accuChargeSqEven;

      meanGeoId=meanGeoIdEven;
      meanOrdinate=meanOrdinateEven;
      meanOrdinateWeight=meanOrdinateEvenWeight;
      //	  meanSqOrdinate -= meanOrdinateEven*meanOrdinateEven;
      meanSqOrdinate=meanSqOrdinateEven;
      meanSqOrdinate=-meanOrdinate*meanOrdinate;
      accuChargeError=accuChargeErrorEven;
      numStrips=numStripsEven;
    }
  else
    {
      cluster->setCharge(accuCharge);
      meanOrdinate /= (Double_t)accuCharge;
      meanOrdinateWeight /= (Double_t)accuChargeWeight;
      meanGeoId /= accuChargeWeight;
      meanSqOrdinate /= (Double_t)accuChargeSq;
      meanSqOrdinate -= meanOrdinate*meanOrdinate;
    }
  numStrips > 1 ? cluster->setChargeUncert(sqrt(accuChargeError/((Double_t)numStrips-1))) : cluster->setChargeUncert(sqrt(accuChargeError/((Double_t)numStrips)));


  //cout <<" meanOrdinate: " << meanOrdinate <<" error: " << meanSqOrdinate <<endl;
  //cout <<" meanOrdinate= " << meanOrdinate <<"  meanOrdinateWeight=" << meanOrdinateWeight <<endl;

  if( meanSqOrdinate > 0 )
    meanSqOrdinate = sqrt(meanSqOrdinate);
  // meanSqOrdinate is now the st. dev. of the ordinate
  // avoid unreasonable small uncertainty, due to small cluster sizes

  Double_t pitch = ( layer == 'R' ? StFgtGeom::radStrip_pitch() : StFgtGeom::phiStrip_pitch() );
  //cout <<" pitch is : " <<pitch <<endl;
  //  if( meanSqOrdinate < 2*pitch )
  //  cout <<"got " << meanOrdinate << " as weighted result " <<endl;
  //replace with fit result
  if(clusterShapeFitAmp>0)
    meanOrdinate=clusterShapeFitAmp;
  if( meanSqOrdinate < 0.001 )
    meanSqOrdinate = pitch;
  if(layer=='R')
    {
      cluster->setPositionR(meanOrdinateWeight);
      cluster->setErrorR(meanSqOrdinate);
    }
  else
    {
      cluster->setPositionPhi(meanOrdinateWeight);
      cluster->setErrorPhi(meanSqOrdinate);
    }
  if(meanGeoId<0)
    {
      //      cout <<"geo id < 0, " <<" even? " << chargeEven <<" accuChargeWeight: " << accuChargeWeight <<endl;
    }
  //  cout <<"mean geo Id: " << meanGeoId << " setting to: " <<floor(meanGeoId+0.5) <<endl;
  cluster->setCentralStripGeoId(floor(meanGeoId+0.5));
  //  cout <<"setting geo id cluster: " << cluster->getCentralStripGeoId()<<endl;
  //  if(cluster->getCentralStripGeoId()<=0)
  //if(cluster->getCentralStripGeoId()==18974)
  /*    {
  //      cout <<" cluster has strips: " <<endl;
  for(stripWeightMap_t::iterator it=strips.begin();it!=strips.end();it++)
  {
  Double_t charge=it->first->getCharge();
  cout <<"charge: " <<	  it->first->getCharge() << " geoId: "<< it->first->getGeoId() <<endl;
  }
  cout <<"cluster charge: "<< cluster->charge() <<endl;
  }*/

  cluster->setNstrip(numStrips);
  
  ////now also add strips on both sides of the cluster:

    if(!strips.empty())
  //  if(false)
    {
      stripWeightMap_t::iterator itFirstStrip=strips.begin();
      stripWeightMap_t::reverse_iterator itLastStrip=strips.rbegin();
      Int_t firstGeoId=itFirstStrip->first->getGeoId();
      Int_t lastGeoId=itLastStrip->first->getGeoId();
      StFgtGeom::getPhysicalCoordinate(firstGeoId,disc,quadrant,layer,ordinate,lowerSpan,upperSpan);
      //lets assume that the strip exist, since it is part of the cluster
      Int_t clusterQuad=quadrant;
      Int_t clusterLayer=layer;
      Int_t clusterDisk=disc;
      //      cout <<" looking at disk: " << disc <<endl;
      Int_t rdo, arm, apv, chan; 
      firstGeoId--;
      
      //      cout <<"last geo Id: " << lastGeoId <<endl;
      lastGeoId++;
      //      cout <<"looking at cluster with first geo: " << firstGeoId <<" and last: " << lastGeoId <<endl;
      for(int iGeo=firstGeoId;iGeo>(firstGeoId-(chargeEven+1)*numAdditionalStrips);iGeo--)
    {
      Int_t ret=StFgtGeom::getPhysicalCoordinate(iGeo,disc,quadrant,layer,ordinate,lowerSpan,upperSpan);
      ///      cout <<"disc: " <<disc << endl;
      if(ret!=kFgtError)
	{
	  if(disc==clusterDisk&&layer==clusterLayer&&quadrant==clusterQuad)
	    {
	      for( StSPtrVecFgtStripIterator it=allStrips.getStripVec().begin();it!=allStrips.getStripVec().end();++it)
		{
		  if((*it)->getGeoId()==iGeo)
		    {
		      if((*it)->getClusterSeedType()==kFgtSeedTypeNo || (*it)->getClusterSeedType()>kFgtClusterSeedInSeaOfNoise)
			(*it)->setClusterSeedType(kFgtNextToCluster);
		    }
		  
		}
	      /////////////doesn't work because of the implementation of the getStrip, returning invalid pointer
	      /////probably not updated after sortByGeoId....
	      /*
	      if(mDb)
		{
		  mDb->getElecCoordFromGeoId(iGeo, rdo,arm,apv,chan);
		  Int_t elecId = StFgtGeom::encodeElectronicId( rdo, arm, apv, chan );
		  StFgtStrip* stripPtr = allStrips.getStrip( elecId );
		  if(elecId>=kFgtNumElecIds)
		    cout <<"wrong elec id!!!!!!!!!!!!!!" << endl;
		  //existed data
		  if(!stripPtr)
		  cout <<"no Strip!!!  " <<endl;
 //shouldn't happen
		  if(stripPtr->getGeoId()>=0)
		  {
		    //		    cout <<"read geo id: " << stripPtr->getGeoId() <<endl;
		    //		    cout <<" the pointer is: " << stripPtr <<endl;

		    //	    stripPtr->setClusterSeedType(kFgtNextToCluster);
		    //		    cout <<"added geo: " << iGeo <<endl;
		  }
		}
	      else
		{
		  //		  cout <<"no db! " <<endl;
		}
	      */

	    }
	}
    }
  for(int iGeo=lastGeoId;iGeo<(lastGeoId+(chargeEven+1)*numAdditionalStrips);iGeo++)
    {
      Int_t ret=StFgtGeom::getPhysicalCoordinate(iGeo,disc,quadrant,layer,ordinate,lowerSpan,upperSpan);
      if(ret!=kFgtError)
	{
	  if(disc==clusterDisk&&layer==clusterLayer&&quadrant==clusterQuad)
	    {

	      for( StSPtrVecFgtStripIterator it=allStrips.getStripVec().begin();it!=allStrips.getStripVec().end();++it)
		{
		  if((*it)->getGeoId()==iGeo)
		    {
		      //			  cout <<"from vec: "<< (*it) <<endl;
		      if((*it)->getClusterSeedType()==kFgtSeedTypeNo || (*it)->getClusterSeedType()>kFgtClusterSeedInSeaOfNoise)
			(*it)->setClusterSeedType(kFgtNextToCluster);
		    }
		  
		}

	      ///////////
	      /*
	      if(mDb)
		{
		  cout <<"got db " <<endl;
		  mDb->getElecCoordFromGeoId(iGeo, rdo,arm,apv,chan);
		  Int_t elecId = StFgtGeom::encodeElectronicId( rdo, arm, apv, chan );
		  if(elecId>=kFgtNumElecIds)
		    cout <<"wrong elec id!!!!!!!!!!!!!!" << endl;
		  StFgtStrip* stripPtr = allStrips.getStrip( elecId );
		  //existed data
		  if(!stripPtr)
		    cout <<"no Strip!!!  " <<endl; //shouldn't happen
		  if(stripPtr->getGeoId()>=0)
		    {
		      cout <<"read geo id: " << stripPtr->getGeoId() <<endl;
		      stripPtr->setClusterSeedType(kFgtNextToCluster);
		      cout <<"added geo: " << iGeo <<endl;
		    }
		}
	      */
	      ////////

	    }
	}
    }
    }


//  cout <<"end fill.." <<endl;
}/////end of fill clusterinfo



Int_t StFgtSeededClusterAlgo::Finish()
{
#if 0
  if(gFile){
  //TCanvas c;
  hGaussFitStatus->Write();
  hGaussFitChi2->Write();
  hTbFitStatus->Write();
  hTbFitChi2->Write();
  hTbMaxCorr->Write();
  hTbMaxRatio->Write();
  hTbSideCorr->Write();
  hTbSideRatio->Write();
  }
#endif
  return kStOk;
}


///function to check if the strip belongs to the cluster. If it returns false we stop adding strips to the cluster
Bool_t StFgtSeededClusterAlgo::isSameCluster(StSPtrVecFgtStripIterator itSeed,StSPtrVecFgtStripIterator nextStrip)
{
  //  Float_t chargeUncert = (*itSeed)->getChargeUncert() > (*nextStrip)->getChargeUncert() ? (*itSeed)->getChargeUncert() : (*nextStrip)->getChargeUncert();
  //  if((*itSeed)->getCharge()  > (*nextStrip)->getCharge() - 2*chargeUncert && (*nextStrip)->getCharge() > 2*(*nextStrip)->getChargeUncert())


  //prevent problems in the pedstals so that low noise makes the cluster go on forever and thus makes it look like a cluster that is too big:
  //if two relatively low strips  follow each other, end cluster. We could check in addition if the ratio between the two is close to one
  if((*nextStrip)->getCharge()< 2*(*nextStrip)->getChargeUncert() && ((*itSeed)->getCharge()<2*(*itSeed)->getChargeUncert()))
    return false;

  if((*nextStrip)->getCharge() > mThreshold2AddStrip*(*nextStrip)->getChargeUncert() && (*nextStrip)->getChargeUncert() >0 && (*nextStrip)->getClusterSeedType()!=kFgtDeadStrip)
    return true;
  else
    return false;
}

///function to add strips to clusters, used recursively
Int_t StFgtSeededClusterAlgo::addStrips2Cluster(StFgtHit* clus, StSPtrVecFgtStripIterator itSeed, StSPtrVecFgtStripIterator itVecBegin, StSPtrVecFgtStripIterator itVecEnd,
						Bool_t direction, Int_t sidedSize)
{
  bool isPhi, isR;
  Short_t seedDisc, seedQuad, seedStrip;
  Short_t disc, quadrant,strip;
  //,noLayer='z';
  Char_t layer,seedLayer;
  //Double_t ordinate, lowerSpan, upperSpan;
  int debug=0;

  StFgtGeom::decodeGeoId((*itSeed)->getGeoId(),seedDisc,seedQuad,seedLayer,seedStrip);
  if(debug) cout <<"Layer="<<seedLayer<<" adding strips from gid=" << (*itSeed)->getGeoId();

  Int_t inc=1;
  if(direction==down) {
    if(debug) cout <<" down : ";
    inc=(-1);
  }else{
    if(debug) cout <<" up   : ";
  }
  isPhi=(seedLayer=='P');
  isR=(!isPhi);

  
  StSPtrVecFgtStripIterator nextStrip=itSeed+inc;
  if(nextStrip<itVecBegin || nextStrip >=itVecEnd) {
    if(debug) { cout << " Not added because run out of strips"<<endl;}
    return true;
  }
  StFgtGeom::decodeGeoId((*nextStrip)->getGeoId(),disc,quadrant,layer,strip);
  if(seedLayer!=layer || seedQuad!=quadrant || seedDisc!=disc){
    if(debug) { cout << " Not added because not in same quad/layer"<<endl;}
    return true;
  }
  
  Int_t deadStripsSkipped=0;
  //if(debug) cout << " Next strip gid="<<(*nextStrip)->getGeoId();
  
  //jump over dead strips while its still in same quad/layer
  while(nextStrip>=itVecBegin && nextStrip <itVecEnd && (*nextStrip)->getClusterSeedType()==kFgtDeadStrip)    {
    nextStrip+=inc;
    deadStripsSkipped++;
    StFgtGeom::decodeGeoId((*nextStrip)->getGeoId(),disc,quadrant,layer,strip);
    if(seedLayer!=layer || seedQuad!=quadrant || seedDisc!=disc){
      if(debug) { cout << " Skipped dead strip, and next is not added because not in same quad/layer"<<endl;}
      return true;
    }
    //the next printout might lead to a crash...
    //if(debug) cout <<"\n    Dead strip ("<<deadStripsSkipped<<"). Skip and looking at next gid="<< (*nextStrip)->getGeoId()<<endl;
  }

  bool isHit = false;
  bool adjacentStrip = false;
  if(nextStrip >=itVecBegin && nextStrip <itVecEnd){
    //cout <<"still looking at "<< (*nextStrip)->getGeoId()<<endl;
    StFgtGeom::decodeGeoId((*nextStrip)->getGeoId(),disc,quadrant,layer,strip);
    //      if(isPhi)
    //cout <<"stepTwo: " << stepTwo << " nextStripID: " << (*nextStrip)->getGeoId() << " seedLayer: " << seedLayer <<" layer: " << layer <<endl;
    ///doesn't matter if it is adjacent... dead strips are deleted, so the the next strip can be further away and there is no telling how far.
    //in the end we check what range the geoId's of the clusters span, then we know if it is too big
    // bool adjacentStrip=((abs((*nextStrip)->getGeoId()-(*itSeed)->getGeoId()))<(2+deadStripsSkipped));
    //adjacentStrip=((layer==seedLayer)&&(quadrant==seedQuadrant)&&((abs((*nextStrip)->getGeoId()-(*itSeed)->getGeoId()))<(2+deadStripsSkipped)));
    adjacentStrip=((layer==seedLayer)&&(quadrant==seedQuad)&&((abs(strip-seedStrip)<(2+deadStripsSkipped))));
    //cout <<"looking at "<< (*nextStrip)->getGeoId()<< " adjacent: " << adjacentStrip <<endl;
    if(adjacentStrip) isHit=isSameCluster(itSeed,nextStrip);
     
    //So the next strip is not dead (we skipped those) and the same layer. So if it is not in the cluster, we give one more chance to the one after that if it is even. 
    //But the condition is that it is exactly 2 away. If it is more and still even (so it could be that there is an even
    //hit then nothing due to short phi and then a dead strip and then nothing due to short and then a signal... that would be quite a stretch..
    //adjacent strip is only checking if in same layer, if that is not true, the strip after that will not be in the same layer either
    if(stepTwo && isPhi && adjacentStrip && !isHit && seedStrip%2==0){
      if(debug) cout <<"\n    adjacent has no charge but phi and even. Looking for next gid="<<(*(nextStrip+inc))->getGeoId();
      if((nextStrip+inc) >=itVecBegin && (nextStrip+inc)<itVecEnd)
	{
	  ///this only goes on for a maximum distance of two, and has to be same layer and even as well
	  short disc2,quadrant2,strip2;
	  char layer2;
	  StFgtGeom::decodeGeoId((*(nextStrip+inc))->getGeoId(),disc2,quadrant2,layer2,strip2);
	  adjacentStrip=(layer2==seedLayer && quadrant2==seedQuad && abs(strip2-seedStrip)<=(2+deadStripsSkipped) && strip2%2==0);
	  isHit=isSameCluster(itSeed,nextStrip);	    
	  if(isHit) nextStrip+=inc;
	  if(debug){
	    cout <<"geoid of next next: " << (*(nextStrip+inc))->getGeoId() <<" of seed: " << (*itSeed)->getGeoId() <<" deadStrips skipped: " << deadStripsSkipped<<endl;
	    cout <<"diff in geo: " <<(abs((*nextStrip+inc)->getGeoId()-(*itSeed)->getGeoId()==(2+deadStripsSkipped)))<< endl;
	    cout <<"adjacentStrip="<<adjacentStrip;
	  }
	}
    }
    
    //if zero suppressed data, odd strip may not exist... So check if this is even and should be added
    if(stepTwo && isPhi && !isHit && seedStrip%2==0){
      if(debug) cout <<"\n   This is not adjacent but phi and even. Check this strips"<<endl;
      if(nextStrip>=itVecBegin && nextStrip<itVecEnd)
        {
	  StFgtGeom::decodeGeoId((*nextStrip)->getGeoId(),disc,quadrant,layer,strip);
	  adjacentStrip=(layer==seedLayer && quadrant==seedQuad &&  abs(strip-seedStrip)==2);
	  if(adjacentStrip) {
	    isHit=isSameCluster(itSeed,nextStrip);
	    if(debug){ cout << "   Checked this strip because phi and even isHit=" << isHit << endl; }
          }
        }
    }    
  }
   
  //if the new strip is adjacent and it seems to belong to the same cluster, add it
  if(isHit){
    (*nextStrip)->setClusterSeedType(kFgtClusterPart);
    stripWeightMap_t &stripWeightMap = clus->getStripWeightMap();
    stripWeightMap[ *nextStrip ] = 1;
    ///if the last add was successful and the cluster is not too big, go to next one...
    if(sidedSize+1<=kFgtMaxClusterSize/2){
      if(debug) cout << " Added\n";      
      return addStrips2Cluster(clus,nextStrip,itVecBegin,itVecEnd,direction,sidedSize+1); //to return a kFgtClusterTooBig which is discovered downthe line
    }else{
      //cout <<" Cluster too big, stopping here : sidedSize+1="<<sidedSize+1<<" kFgtMaxClusterSize="<<kFgtMaxClusterSize<<endl;       
      return kFgtClusterTooBig;
    }
  }else{     
    if(debug) {
      if(adjacentStrip) {cout << " Not added because not adjacent"<<endl;}
      else              {cout << " Not added because no charge"<<endl;}
    }
  }
  return true;

}

/**
   main interface to the clustering. 
*/
Int_t StFgtSeededClusterAlgo::doClustering(const StFgtCollection& fgtCollection, StFgtStripCollection& strips, StFgtHitCollection& clusters )
{
  mMaxTimeBin=0;
  if(&fgtCollection){ mMaxTimeBin=fgtCollection.getNumTimeBins(); }
  if(mMaxTimeBin==0) return kStOk;

  //  cout.precision(10);
  //we make use of the fact, that the hits are already sorted by geoId
  strips.sortByGeoId();
  Float_t defaultError = 0.001;
  Short_t disc, quadrant;
  //,noLayer='z';
  Char_t layer;
  Double_t ordinate, lowerSpan, upperSpan;//, prvOrdinate;
  Double_t accuCharge=0; 
  bool isPhi, isR;
  StFgtHit* newCluster=0;
  //to compute energy weighted strip id
  Double_t meanGeoId=0;
  //for R < R/2 cm the difference in geo id of the phi strips is 2 and only even numbers are used...
  //const 

  /**
     run over all strips, find seeds, use those to start clusters
  */
  Int_t lastGeoIdInCluster=-1;
  for( StSPtrVecFgtStripIterator it=strips.getStripVec().begin();it!=strips.getStripVec().end();++it)
    {
      //the last cluster includes this seed strip
      //      cout <<"last geo id is: " << lastGeoIdInCluster <<endl;
      ///this code only ensures that we don't swallow other seeds, however it does not prevent that a close seed eats into the cluster we are building
      ///but since we check for rising strips, the next seed is a true seed of its own, 
      //so maybe the strip just has to be shared (which it is) because it is eaten by both
      if((*it)->getGeoId()<lastGeoIdInCluster && lastGeoIdInCluster>0)
	continue;
      //found seed for a cluster
      if( (*it)->getClusterSeedType() >=kFgtSeedType1 && (*it)->getClusterSeedType() < kFgtSeedTypeMax ) 
	{
	  //	  cout <<"found seed for geo id: " << (*it)->getGeoId() <<endl;
	  //if((*it)->getGeoId()==31318)
	  // {
	      //    	      cout <<"seed type:  "<< (*it)->getClusterSeedType()<< "charge: " << (*it)->getCharge()<<" adc: ";
	  // for(int i=0;i<mMaxTimeBin;i++)
	  //{
		  //		  cout <<(*it)->getAdc(i) <<" ";
	  //}
	      //	   cout <<endl;
	  //}
	  ////
	  ///check for ringing around cluster
	  //	  if(ringing( it, strips.getStripVec().begin(),strips.getStripVec().end(), down,0,layer))
	  //	    continue;  //was it
	  ///this might not be such a good idea since it is not taking into account geo ids. If we run zs, this will not work--->fixed
	  int searchRange=(int)(floor(kFgtMaxClusterSize/2)+kFgtNumAdditionalStrips);
	  StSPtrVecFgtStripIterator firstStrip=it-(int)(floor(kFgtMaxClusterSize/2)+kFgtNumAdditionalStrips);
	  StSPtrVecFgtStripIterator lastStrip=it+(int)(floor(kFgtMaxClusterSize/2)+kFgtNumAdditionalStrips);
	  if(firstStrip<strips.getStripVec().begin())
	    firstStrip=strips.getStripVec().begin();
	  Int_t stripsW_Charge=0;
	  Int_t stripsWO_Charge=0;
	  //compare with energy in cluster
	  //	  cout << " looking around " << (*it)->getGeoId() << ": " << (*firstStrip)->getGeoId() <<" to something... " <<endl;
	  for(StSPtrVecFgtStripIterator it2=firstStrip;(it2!=strips.getStripVec().end())&&(it2<=lastStrip);it2++)
	    {
	      if(fabs((*it)->getGeoId()-(*it2)->getGeoId())<searchRange)
		{
		  if((*it2)->getClusterSeedType()!=kFgtDeadStrip)
		    {
		      if((*it2)->getCharge()>2*(*it2)->getChargeUncert())
			{
			  //		      cout <<"   strip: " << (*it2)->getGeoId() << " has high charge " <<endl;
			  stripsW_Charge++;
			}
		    }
		}
	    }
	  //	  cout<<" high charge strips: " << stripsW_Charge <<" low: " << stripsWO_Charge<<endl;
	  //if more than half the strips have charge, we don't count strips w/o charge due to zero suppression
	  if(stripsW_Charge>searchRange && stripsW_Charge > kFgtMaxClusterSize)
	    {
	      (*it)->setClusterSeedType(kFgtClusterSeedInSeaOfNoise);
#ifndef KEEP_BIG_AND_NOISY_CLUSTERS
	      continue;
#endif
	    }

	  /////


	  StFgtGeom::getPhysicalCoordinate((*it)->getGeoId(),disc,quadrant,layer,ordinate,lowerSpan,upperSpan);
	  isPhi=(layer=='P');
	  isR=(!isPhi);
	  newCluster=new StFgtHit(clusters.getHitVec().size(),meanGeoId,accuCharge, disc, quadrant, layer, ordinate, defaultError,ordinate, defaultError,0.0,0.0);
	  stripWeightMap_t &stripWeightMap = newCluster->getStripWeightMap();
	  stripWeightMap[ *it ] = 1;
	  //add strips to cluster going down
	  //the function is recursive, stops if the condition that the next strip 
	  //belongs to the cluster is not met anymore, 
	  //big clusters return clusterToBig, then we erase the seed because ist was probably noise
	  if(addStrips2Cluster(newCluster, it, strips.getStripVec().begin(),strips.getStripVec().end(), down,0)==kFgtClusterTooBig)
	    {
	      //cout <<"cluster too big!, begin at: " << newCluster->getStripWeightMap().begin()->first->getGeoId()<<endl;
	      //reset strips

#ifndef KEEP_BIG_AND_NOISY_CLUSTERS
	      //akio's change: keep seed flags
	      for(stripWeightMap_t::iterator it=newCluster->getStripWeightMap().begin();it!=newCluster->getStripWeightMap().end();it++)
		{
		  if(it->first->getClusterSeedType()<kFgtSeedType1 || it->first->getClusterSeedType()>kFgtSeedTypeMax)
		    it->first->setClusterSeedType(kFgtClusterTooBig);
		}
	      delete newCluster;
	      continue;
#endif
	    }
	  //add strips to cluster going up
	  if(addStrips2Cluster(newCluster, it, strips.getStripVec().begin(),strips.getStripVec().end(), up,0)==kFgtClusterTooBig)
	    {

#ifndef KEEP_BIG_AND_NOISY_CLUSTERS

	      //	      cout <<"cluster too big!, begin at: " << newCluster->getStripWeightMap().begin()->first->getGeoId()<<endl;
	      for(stripWeightMap_t::iterator it=newCluster->getStripWeightMap().begin();it!=newCluster->getStripWeightMap().end();it++)
		{
		  if(it->first->getClusterSeedType()<kFgtSeedType1 || it->first->getClusterSeedType()>kFgtSeedTypeMax)
		    it->first->setClusterSeedType(kFgtClusterTooBig);
		}
	      delete newCluster;
	      continue;
#endif
	    }

	  /////make sure that cluster is at least 3 strips wide for fit
	  stripWeightMap_t &stripWM = newCluster->getStripWeightMap();
	  //	  if(stripWM.size()==1)
	  if(false)
	    {
	      StSPtrVecFgtStripIterator stripInClu=0;
	      for(StSPtrVecFgtStripIterator it=firstStrip;it<=lastStrip&&it!=strips.getStripVec().end();it++)
		{
		  if((*it)->getGeoId()==stripWM.begin()->first->getGeoId())
		    stripInClu=it;
		  break;
		}
	      if(stripInClu!=0)
		{
		  StSPtrVecFgtStripIterator stripLow=stripInClu;
		  stripLow--;
		  if(stripLow>=strips.getStripVec().begin())
		    {
		      //		      cout <<"adding low " <<endl;
		      stripWM[*stripLow]=1;
		    }
		  StSPtrVecFgtStripIterator stripHigh=stripInClu;
		  stripHigh++;
		  if(stripHigh<strips.getStripVec().end())
		    {
		      //		      cout <<"adding high " <<endl;
		      stripWM[*stripHigh]=1;
		    }
		}
	    }
	  //	  if(stripWM.size()==2)
	  if(false)
	    {
	      //	      cout <<"only two strips in cluster " <<endl;
	      StSPtrVecFgtStripIterator stripInClu=0;
	      for(StSPtrVecFgtStripIterator it=firstStrip;it<=lastStrip&& it<=lastStrip&&it!=strips.getStripVec().end();it++)
		{
		  //		  cout <<"looping" <<endl;
		  //		  cout <<"checking "<<(*it)->getGeoId() <<" with: "<<stripWM.begin()->first->getGeoId()<<" last: "<< (*lastStrip)->getGeoId()<<endl;
		  if((*it)->getGeoId()==stripWM.begin()->first->getGeoId())
		    {
		      stripInClu=it;
		      //		    cout <<"found " <<endl;
		      break;
		    }

		}
	      //	      cout <<"end of loop " << endl;

	      if(stripInClu!=0)
		{
		  //		  cout <<"found one geo id: " << (*stripInClu)->getGeoId()<<endl;
		  StSPtrVecFgtStripIterator stripHigh=stripInClu;
		  StSPtrVecFgtStripIterator stripNew=stripInClu;
		  stripHigh++;
		  //must be since we have two strips
		  if(stripHigh< strips.getStripVec().end())
		    {
		      if((*stripHigh)->getCharge() >(*stripInClu)->getCharge())
			{
			  stripNew=stripHigh;
			  stripNew++;
			}
		      else
			{
			  stripNew=stripInClu;
			  stripNew--;
			}

		      if(stripNew>=strips.getStripVec().begin()&& stripNew<strips.getStripVec().end())
			{
			  //		   cout <<"adding strip with geo id: " << (*stripNew)->getGeoId()<<endl;
			  stripWM[*stripNew]=1;
			}
		    }
		}
	    }
	  ////


	  //
	  //compute errors etc
	  FillClusterInfo(newCluster,strips);
	  clusters.getHitVec().push_back(newCluster);
	  //now of course we have to check where the cluster ends so that we don't start another cluster if there is another seed
	  lastGeoIdInCluster=newCluster->getStripWeightMap().rbegin()->first->getGeoId();
	}
    }
  return kStOk;
}

//Bool_t StFgtSeededClusterAlgo::up;
//Bool_t StFgtSeededClusterAlgo::down;

StFgtSeededClusterAlgo::~StFgtSeededClusterAlgo()
{
}

ClassImp(StFgtSeededClusterAlgo);
