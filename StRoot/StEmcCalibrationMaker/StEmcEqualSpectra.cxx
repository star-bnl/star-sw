/**********************************************************************
* StEmcEqualSpectra
* Author: Alexandre A. P. Suaide 
*
* This is responsible for emc equalization
***********************************************************************/
#include "StEmcEqualSpectra.h"
#include <iostream.h>
#include <math.h>
#include "emc_def.h"
#include "TCanvas.h"
#include "TH1.h"
#include "TGraphErrors.h"

ClassImp(StEmcEqualSpectra);
//_____________________________________________________________________________
StEmcEqualSpectra::StEmcEqualSpectra(const char* cdet):StEmcSpectra(cdet)
{  
}
//_____________________________________________________________________________
StEmcEqualSpectra::~StEmcEqualSpectra()
{ }
//_____________________________________________________________________________
void StEmcEqualSpectra::DrawEqualConst()
{
  TCanvas* canvas5=new TCanvas("canvas5","EMC Equalization Slope",500,700);
  canvas5->Divide(1,2);
  TCanvas* canvas6=new TCanvas("canvas6","EMC Equalization Shift",500,700);
  canvas6->Divide(1,2);
  
  emcEqualization_st* rows=EqualTable->GetTable();
  TH1F* distr1=new TH1F("distr1","Relative gain distribution",200,0,4);
  TH1F* distr2=new TH1F("distr2","Relative shift distribution",200,-4,4);
  
  const int nbins=GetNBin();
  
  Float_t x[18000],ex[18000],y[18000],ey[18000],y1[18000],ey1[18000];
  
  for(Int_t j=0;j<nbins;j++) 
  {
    x[j]=(Float_t)j+1;
    ex[j]=0;
    if (rows[j].EqStatus>0)
    { 
      y[j]=rows[j].EqSlope;
      ey[j]=rows[j].EqSlopeError;
      y1[j]=rows[j].EqShift;
      ey1[j]=rows[j].EqShiftError;
      if(y[j]>2 || y[j]<0.5) cout <<"tower = "<<x[j]<<"  slope ="<<y[j]<<"+-"<<ey[j]<<"  shift = "<<y1[j]<<"+-"<<ey1[j]<<endl;
      distr1->Fill(y[j]);
      distr2->Fill(y1[j]);
    } 
    else
    {
      y[j]=0;
      ey[j]=0;
      y1[j]=0;
      ey1[j]=0;   
    }
    
  }
  TGraphErrors* graph1=new TGraphErrors(nbins,x,y,ex,ey);
  graph1->SetTitle("Equalization Relative gain");
  TGraphErrors* graph2=new TGraphErrors(nbins,x,y1,ex,ey1);
  graph2->SetTitle("Equalization Relative shift");

  canvas5->cd(1);
  graph1->Draw("A*");
  canvas5->cd(2);
  distr1->Draw();
  
  canvas6->cd(1);
  graph2->Draw("A*");
  canvas6->cd(2);
  distr2->Draw();
  
  return;
}

//_____________________________________________________________________________
Bool_t StEmcEqualSpectra::Equalize(Int_t position1,Int_t position2,Int_t mode)
{ 
  if(GetStatus(position1)==0) return kFALSE;
  if(GetStatus(position2)==0) return kFALSE;
  emcEqualization_st* rows=EqualTable->GetTable();
  
  const int nadcMax = GetNAdcMax();
   
  Bool_t EqDone=kFALSE;
    
  Float_t pwd1=20;
  Float_t pwd2=20;

  Float_t a=0,b=0,erra=0,errb=0,cov=0,chi=0;
  const int npoints=6;
  
  // mean and RMS modes  
  // 0 -  mean and RMS with liear average
  // 1 -  mean with linear average
  // 2 -  mean and rms with log average
  // 3 -  mean with log average 
  if(mode>=0 && mode <=3)  
  {
    Int_t max=nadcMax;
    max=140;
    Float_t m1,r1,m2,r2;
    if(mode==0 || mode==1)
    {
      GetMeanAndRms(position1,(Int_t)pwd1,max,&m1,&r1);
      GetMeanAndRms(position2,(Int_t)pwd1,max,&m2,&r2);
    }
    if(mode==2 || mode==3)
    {
      GetLogMeanAndRms(position1,(Int_t)pwd1,max,&m1,&r1);
      GetLogMeanAndRms(position2,(Int_t)pwd1,max,&m2,&r2);
    }
    if(mode==0 || mode==2)
    {
      a=r1/r2;
      b=m1-a*m2;
    }
    if(mode==1 || mode==3)
    {    
      a=m1/m2;
      b=0;
    }
    EqDone=kTRUE;
    cout <<"  id = "<<position2<<"  ref = "<<position1<<"  mean = "<<m1<<" , "<<m2
         <<"  rms = "<<r1<<" , "<<r2
         <<"  a = "<<a<<"  b = "<<b<<endl;
  }

  // fit modes
  if(mode==4) 
  {
    Float_t x1[4096],x2[4096];
    Int_t ini1=nadcMax-1,ini2=nadcMax-1;
    x1[nadcMax-1]=GetAdcValue(position1,nadcMax-1);
    x2[nadcMax-1]=GetAdcValue(position2,nadcMax-1);
    Float_t max=pwd1;
    
    if(pwd2>max) max=pwd2;
    
    for(Int_t i=nadcMax-2;i>=(Int_t)(max);i--)
    {
      x1[i]=x1[i+1]+GetAdcValue(position1,i);
      if(x1[i]==0) ini1=i;
      x2[i]=x2[i+1]+GetAdcValue(position2,i);
      if(x2[i]==0) ini2=i;
    }
    
    Float_t limit;
    
    if(x1[(int)max]<x2[(int)max]) limit=0.95*x1[(int)max];
    else limit=0.95*x2[(int)max];
    cout <<"id = "<<position2<<"  ref = "<<position1<<"  limit = "<<limit<<endl;
    
    if(limit>=10)
    {    
      Float_t dl=log10(limit)/(Float_t)npoints;
    
      Float_t cc1[npoints],ec1[npoints],cc2[npoints],ec2[npoints];
      Float_t ss=0,sx=0,sx2=0,sy=0,sxy=0;
      for(Int_t i=0;i<npoints;i++)
      {
        Float_t x=pow(10,((Float_t)i+1)*dl);
        Float_t xmin=x-sqrt(x);
        Float_t xmax=x+sqrt(x);
        Float_t a1=5000,a2=5000;
        Int_t ji=ini1;
        if (i>0) ji=(Int_t)(cc1[i-1]+ec1[i-1]);
        for(Int_t j=ji;j>=0;j--)
        {
          if(x1[j]>xmin && a1==5000) a1=(Float_t)j;
          if(x1[j]>xmax && a2==5000) a2=(Float_t)j-1.;
        }
        Float_t b1=5000,b2=5000;
        ji=ini2;
        if (i>0) ji=(Int_t)(cc2[i-1]+ec2[i-1]);
        for(Int_t j=ji;j>=0;j--)
        {
          if(x2[j]>xmin && b1==5000) b1=(Float_t)j;
          if(x2[j]>xmax && b2==5000) b2=(Float_t)j-1.;
          }
        cc1[i]=(a1+a2)/2.;
        cc2[i]=(b1+b2)/2.;
        ec1[i]=fabs(a1-a2)/2.;  
        ec2[i]=fabs(a1-a2)/2.;
        //cc1[i]=a1;
        //cc2[i]=b1;
        //ec1[i]=cc1[i]/sqrt(x);
        //ec2[i]=cc2[i]/sqrt(x);
        cout <<"i = "<<i<<"  x = "<<x<<"  cc1 = "<<cc1[i]<<"+-"<<ec1[i]<<"  cc2 = "<<cc2[i]<<"+-"<<ec2[i]<<endl;
        ss+=1/(ec1[i]*ec1[i]);
        sx+=cc2[i]/(ec1[i]*ec1[i]);
        sx2+=(cc2[i]*cc2[i])/(ec1[i]*ec1[i]);
        sy+=cc1[i]/(ec1[i]*ec1[i]);
        sxy+=(cc1[i]*cc2[i])/(ec1[i]*ec1[i]);
      }
      cout <<"--------------------\n";
      Float_t delta=ss*sx2-sx*sx;
      a=(ss*sxy-sx*sy)/delta; // preliminar fit ...
      b=0;erra=0;errb=0;cov=0;chi=0;
      ss=0;sx=0;sx2=0;sy=0;sxy=0;
    
      for(Int_t i=0;i<npoints;i++)  // final fit
      {
        Float_t sigma=sqrt(ec1[i]*ec1[i]+a*a*ec2[i]*ec2[i]);
        ss+=1/(sigma*sigma);
        sx+=cc2[i]/(sigma*sigma);
        sx2+=(cc2[i]*cc2[i])/(sigma*sigma);
        sy+=cc1[i]/(sigma*sigma);
        sxy+=(cc1[i]*cc2[i])/(sigma*sigma);  
      }
      delta=ss*sx2-sx*sx;
      a=(ss*sxy-sx*sy)/delta;
      b=(sx2*sy-sx*sxy)/delta;
      erra=sqrt(ss/delta);
      errb=sqrt(sx2/delta);
      cov=-sx/delta;
      
      for(Int_t i=0;i<npoints;i++)  // chi calc
      {
        Float_t sigma=sqrt(ec1[i]*ec1[i]+a*a*ec2[i]*ec2[i]);
        chi+=(cc1[i]-(a*cc2[i]+b))*(cc1[i]-(a*cc2[i]+b))/(sigma*sigma);
      }
      EqDone=kTRUE;
      cout <<"a = "<<a<<"+-"<<erra<<"  b = "<<b<<"+-"<<errb<<"  cov = "<<cov<<"  chi = "<<sqrt(chi/(npoints-2))<<endl;
    }
  }
  
  if (EqDone)
  {
    rows[position2-1].EqStatus=1;
    rows[position2-1].EqRef=position1;
    rows[position2-1].EqSlope=a;
    rows[position2-1].EqSlopeError=erra;
    rows[position2-1].EqShift=b;
    rows[position2-1].EqShiftError=errb;
    rows[position2-1].EqCovariance=cov;
    rows[position2-1].EqChiSqr=sqrt(chi/(npoints-2));    
  }
  
  return EqDone;
}
