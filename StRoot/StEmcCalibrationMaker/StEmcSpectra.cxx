/**********************************************************************
* StEmcSpectra
* Author: Alexandre A. P. Suaide 
*
* This is a general EMCSpectra class
***********************************************************************/
#include "StEmcSpectra.h"
#include "iostream.h"
#include "math.h"
#include "emc_def.h"
#include "TCanvas.h"
#include "TH1.h"
#include "TGraphErrors.h"
#include "StEmcUtil/StEmcGeom.h"

ClassImp(StEmcSpectra);
StEmcGeom* geo;
//_____________________________________________________________________________
const char* StEmcSpectra::GetDetName()   
{ 
  #include "StEmcUtil/emcDetectorName.h"
  return detname[detNum].Data(); 
}
//_____________________________________________________________________________
StEmcSpectra::StEmcSpectra(const char* cdet):St_DataSet(cdet)
{
  #include "StEmcUtil/emcDetectorName.h"
  Int_t nadc[4]={4096,1024,1024,1024};
  SetTitle(cdet);
  //geo=new StEmcGeom(cdet);
  geo=StEmcGeom::getEmcGeom(cdet);
  for(Int_t i=0;i<4;i++) if(!strcmp(cdet,detname[i].Data())) detNum=i;
  nAdcMax=nadc[detNum];
  nModules=geo->NModule();
  nEta=geo->NEta();
  nSub=geo->NSub();
  nBins=nModules*nEta*nSub;
}
//_____________________________________________________________________________
void StEmcSpectra::Init()
{
                 
  Spectra.ResizeTo(nBins,nAdcMax);
  Sum.Set(nBins);
  IsOnOff.Set(nBins);

  ZeroAll();
  return;
}
//_____________________________________________________________________________
StEmcSpectra::~StEmcSpectra()
{ 
  delete geo;
}
//_____________________________________________________________________________
Bool_t StEmcSpectra::Zero(Int_t position)
{
  for(Int_t k=0;k<nAdcMax;k++)
  {
    Spectra(position-1,k)=0;
  }
  Sum[position-1]=0;
  return kTRUE;
}
//_____________________________________________________________________________
Bool_t StEmcSpectra::ZeroAll()
{
  for(Int_t j=1;j<=nBins;j++) Zero(j);
  return kTRUE;
}
//_____________________________________________________________________________
Int_t StEmcSpectra::GetID(Int_t m,Int_t e,Int_t s)
{
  Int_t position;
  geo->getId(m,e,s,position);
  return position;
}
//_____________________________________________________________________________
Bool_t StEmcSpectra::FillSpectra(Int_t position,Int_t adc)
{
  if(GetStatus(position)==0)
  {
    cout <<"***** StEmcSpectra: This position is turned off. Can not fill\n"; 
    return kFALSE;
  }
    
  if(adc<0 || adc>=nAdcMax) return kFALSE;
  if(position<1 || position>nBins) return kFALSE;
  
  Sum[position-1]++;
  Spectra(position-1,adc)++;
  return kTRUE;
}
//_____________________________________________________________________________
Float_t StEmcSpectra::GetAdcValue(Int_t position,Int_t adc)
{
  if(GetStatus(position)==0)
  {
    cout <<"***** StEmcSpectra: This position is turned off. Can not get ADC\n"; 
    return -1;
  }
  if(adc<0 || adc>=nAdcMax) return 0;
  if(position<1 || position>nBins) return 0;
  
  return Spectra(position-1,adc);

}
//_____________________________________________________________________________
Int_t StEmcSpectra::GetStatus(Int_t position) 
{ 
  if(position<1 || position>nBins) return 0;
  emcCalibration_st*     Calib_st = CalibTable->GetTable();
  return Calib_st[position-1].Status;
}
//_____________________________________________________________________________
Float_t StEmcSpectra::GetSum(Int_t position) 
{ 
  if(GetStatus(position)==0)
  {
    cout <<"***** StEmcSpectra: This position is turned off. Can not get SUM\n"; 
    return 0;
  }
  if(position<1 || position>nBins) return 0;
  return Sum[position-1];
}
//_____________________________________________________________________________
Bool_t StEmcSpectra::GetMeanAndRms(Int_t position,Float_t* m,Float_t* r)
{
  if(GetStatus(position)==0)
  {
    cout <<"***** StEmcSpectra: This position is turned off. Can not get AVG and RMS\n"; 
    return kFALSE;
  }
  
  Float_t mean=0,rms=0,sum=0;
  for(Int_t j=0;j<nAdcMax;j++)
  {
    Float_t temp=GetAdcValue(position,j);
    mean+=temp*(Float_t)j;
    sum+=temp;
    rms+=(Float_t)j*(Float_t)j*temp;
  }
  mean/=sum;
  rms=sqrt(rms/sum-mean*mean);
  *m=mean;
  *r=rms;
  return kTRUE;
}

//_____________________________________________________________________________
Bool_t StEmcSpectra::GetMeanAndRms(Int_t position,Int_t amin,Int_t amax,
                                   Float_t* m,Float_t* r)
{
  if(GetStatus(position)==0)
  {
    cout <<"***** StEmcSpectra: This position is turned off. Can not get AVG and RMS\n"; 
    return kFALSE;
  }
  
  Float_t mean=0,rms=0,sum=0;
  for(Int_t j=amin;j<amax;j++)
  {
    Float_t temp=GetAdcValue(position,j);
    mean+=temp*(Float_t)j;
    sum+=temp;
    rms+=(Float_t)j*(Float_t)j*temp;
  }
  mean/=sum;
  rms=sqrt(rms/sum-mean*mean);
  *m=mean;
  *r=rms;
  return kTRUE;
}
//_____________________________________________________________________________
Bool_t StEmcSpectra::GetLogMeanAndRms(Int_t position,Int_t amin,Int_t amax,
                                      Float_t* m,Float_t* r)
{
  if(GetStatus(position)==0)
  {
    cout <<"***** StEmcSpectra: This position is turned off. Can not get AVG and RMS\n"; 
    return kFALSE;
  }
  
  Float_t mean=0,rms=0,sum=0;
  for(Int_t j=amin;j<amax;j++)
  {
    if(GetAdcValue(position,j)>0)
    {
      Float_t temp=log(GetAdcValue(position,j));
      mean+=temp*(Float_t)j;
      sum+=temp;
      rms+=(Float_t)j*(Float_t)j*temp;
    }
  }
  if(sum>0)
  {
    mean/=sum;
    rms=sqrt(rms/sum-mean*mean);
  }
  *m=mean;
  *r=rms;
  return kTRUE;
}
//_____________________________________________________________________________
Bool_t StEmcSpectra::GetOccupancy(Int_t minimum,Float_t* m,
                                  Float_t* r,Float_t* fr)
{
  Float_t avg=0,sigma=0,number=0,temp=0;
  for(Int_t id=1;id<=nBins;id++)
  {
    if(GetStatus(id)!=0) 
    {
      Float_t s;
      s=GetSum(id);
      if (s>minimum) number++;
      avg+=s;
      sigma+=s*s;
      temp++;
      //cout <<"id = "<<id<<"  sum = "<<s<<"  n = "<<temp<<endl;
    }
  }
  if(temp==0) return kFALSE;
  avg=avg/temp;
  sigma=sqrt(sigma/temp-avg*avg);
  number=number/temp;
  *m=avg;
  *r=sigma;
  *fr=number;
  return kTRUE;
}
//_____________________________________________________________________________
Bool_t StEmcSpectra::GetOccupancyEtaBin(Int_t minimum,Float_t* m,
                                        Float_t* r,Float_t* fr)
{
  Float_t avg=0,sigma=0,number=0,temp=0;
  for(Int_t bin=1;bin<=nEtaBins;bin++)
  {
    Float_t s;
    s=GetSumEtaBin(bin);
    if(s>0)
    {
     if (s>=minimum) number++;
     avg+=s;
     sigma+=s*s;
     temp++;
    }
    cout <<"etabin = "<<bin<<"  sum = "<<s<<"  n = "<<temp<<endl;
  }
  if(temp==0) return kFALSE;
  avg=avg/temp;
  sigma=sqrt(sigma/temp-avg*avg);
  number=number/temp;
  *m=avg;
  *r=sigma;
  *fr=number;
  return kTRUE;
}

//_____________________________________________________________________________
void StEmcSpectra::Draw(Int_t position)
{
  if(GetStatus(position)==0)
  {
    cout <<"***** StEmcSpectra: This position is turned off. Can not DRAW\n"; 
    return;
  }
  TCanvas* canvas1=new TCanvas("canvas1","EMC Spectra",500,350);
  char title[80];
  sprintf(title,"EMC Spectrum channel %05d",position);
  TH1F* hist=new TH1F("hist",title,nAdcMax,0,(Float_t)nAdcMax-1);
  for(Int_t j=0;j<nAdcMax;j++) 
  {
    Float_t k=GetAdcValue(position,j);
    hist->Fill(j,k);
  }
  canvas1->cd();
  hist->SetFillColor(11);
  hist->Draw();
  return;
}

//_____________________________________________________________________________
void StEmcSpectra::DrawOccupancy()
{
  TCanvas* canvas4=new TCanvas("canvas4","EMC Occupancy",500,700);
  canvas4->Divide(1,2);
  TH1F* hist=new TH1F("hist","Equalization Occupancy",nBins,1,(Float_t)nBins);
  Float_t min=1e9,max=0;
  for(Int_t j=1;j<=nBins;j++) 
  {
    Float_t k=0;
    if (GetStatus(j)) k=GetSum(j);
    if (k>max) max=k;
    if (k<min) min=k;
    hist->Fill(j,k);
  }
  canvas4->cd(1);
  hist->Draw();
  Int_t nc=100;
  TH1F* hist2=new TH1F("hist2","Occupancy distribution",nc,min-1,max+1);
  for(Int_t j=1;j<=nBins;j++) 
  {
    Float_t k=0;
    if (GetStatus(j)) 
    {
      k=GetSum(j);
      hist2->Fill(k);
    }
  }
  canvas4->cd(2);
  hist2->Draw();
}
//_____________________________________________________________________________
void StEmcSpectra::DrawEtaBin(Int_t etabin)
{
  if(etabin>nEtaBins || etabin<1) return;

  Int_t nAdcMax=GetNAdcMax();

  TCanvas* canvas7=new TCanvas("canvas7","EMC Eta Bin Spectrum",500,350);
  
  char title[90];
  sprintf(title,"Eta Bin %02d spectrum",etabin);
  TH1F* hist=new TH1F("hist",title,nAdcMax,0,(Float_t)nAdcMax-1);

  TArrayF temp=GetEtaBinSpectra(etabin);
  for(Int_t j=0;j<nAdcMax;j++) hist->Fill(j,temp[j]);
  
  canvas7->cd();
  hist->SetFillColor(11);
  hist->Draw();
}
//_____________________________________________________________________________
TArrayF StEmcSpectra::GetSpectra(Int_t position)
{
  return GetSpectra(position,1,0);
}
//_____________________________________________________________________________
TArrayF StEmcSpectra::GetSpectra(Int_t position,Float_t a,Float_t b)
{
  TArrayF tmp=ReBin(position,a,b);
  
  //subtract effective pedestals. If not calculated, effective pedestal=0
  emcCalSettings_st* Settings_st=SettingsTable->GetTable();
  if(Settings_st[0].DoEffPedCalculation==1 && 
     Settings_st[0].DoEffPedSubtraction==1)
  {
    emcCalibration_st* Calib_st=CalibTable->GetTable();
    Float_t avg = Calib_st[position-1].AdcPedestal;
    Int_t shift=(Int_t) avg;
    if(shift!=0)
    {
      for(Int_t i=0;i<nAdcMax;i++)
      {
        Int_t j=i+shift;
        Float_t x;
        if(j>=nAdcMax) x=0; else x=tmp[j];
        tmp[i]=x;
      }
    }
  }  
  return tmp;
}
//_____________________________________________________________________________
TArrayF StEmcSpectra::ReBin(Int_t position,Float_t a,Float_t b)
{
  TArrayF temp(nAdcMax);
  for(Int_t i=0;i<nAdcMax;i++) temp[i]=0;
  
  if(a<=0) return temp;
  
  for(Int_t i=0;i<nAdcMax;i++)
  {
    Float_t adcvalue=GetAdcValue(position,i);
    if(a==1 && b==0) temp[i]=adcvalue;
    else
    {
      if(adcvalue>0)
      {
        Float_t ci=(Float_t)i*a+b;
        Float_t cf=(Float_t)(i+1)*a+b;
        Int_t ici=(Int_t)ci;
        Int_t icf=(Int_t)cf;
        if(ci>=0 & cf<=4095)
        {
          if(ici==icf) temp[ici]+=adcvalue;
          else
          {
            temp[ici]+=((Float_t)(ici+1)-ci)*adcvalue/a;
            temp[icf]+=(cf-(Float_t)icf)*adcvalue/a;
            Int_t tmp = icf-ici;
            if(tmp<0) tmp=ici-icf;
            if(tmp>1)
              for(Int_t j=ici+1;j<=icf-1;j++)
                temp[j]+=adcvalue/a;
          }
        }
      }
    }
  }
  return temp;
}
//_____________________________________________________________________________
void StEmcSpectra::CalcEtaBin(Int_t i,Float_t ebin,
                              Int_t* mi,Int_t* mf,Int_t* ei,Int_t* ef)
{
  emcCalSettings_st* Settings_st=SettingsTable->GetTable();  
  Int_t nb=Settings_st[0].NEtaBins;
  Int_t neta=geo->NEta();
  Int_t eei=1;
  Int_t eef=eei+(Int_t)ebin-1;
  Int_t mmi=1;
  Int_t mmf=60;
  
  for(Int_t j=1;j<=nb;j++)
  {
    if (i==j) goto etabinok;
    eei=eef+1;
    if(eei>neta) {eei=1;mmi=61; mmf=120;}
    eef=eei+(Int_t)ebin-1;
    if(eef>neta) eef=neta;
  }
  etabinok:
  *ei=eei; *ef=eef;
  *mi=mmi; *mf=mmf;
  Float_t etai,etaf;
  geo->getEta(mmi,eei,etai);
  geo->getEta(mmi,eef,etaf);

}

//_____________________________________________________________________________
TArrayF StEmcSpectra::GetEtaBinSpectra(Int_t etabin)
{
  if(etabin>nEtaBins || etabin<1) return 0;
  
  if(!EqualTable) return 0;

  emcEqualization_st* rows=EqualTable->GetTable(); 
  
  TArrayF temp(GetNAdcMax());
  for(Int_t i=0;i<GetNAdcMax();i++) temp[i]=0;

  Int_t mi,mf,ei,ef,si,sf;
  CalcEtaBin(etabin,etaBinWidth,&mi,&mf,&ei,&ef);
  si=1; sf=GetNSub();

  for(Int_t m=mi;m<=mf;m++)
    for(Int_t e=ei;e<=ef;e++)
      for(Int_t s=si;s<=sf;s++)
      {
        Int_t id=GetID(m,e,s);
        if(rows[id-1].EqStatus==1 && GetStatus(id)==1)
        {
          Float_t a=rows[id-1].EqSlope;
          Float_t b=rows[id-1].EqShift;
          if(a!=0) 
          {
            TArrayF temp1=GetSpectra(id,a,b); 
            for(Int_t j=0;j<GetNAdcMax();j++) temp[j]+=temp1[j];
          } 
        }
      }
  
  return temp;
}
//_____________________________________________________________________________
Float_t StEmcSpectra::GetSumEtaBin(Int_t etabin)
{
  if(etabin>nEtaBins || etabin<1) return 0;
  Int_t mi,mf,ei,ef,si,sf;
  CalcEtaBin(etabin,etaBinWidth,&mi,&mf,&ei,&ef);
  si=1; sf=GetNSub();
  
  Float_t sum=0;
  for(Int_t m=mi;m<=mf;m++)
    for(Int_t e=ei;e<=ef;e++)
      for(Int_t s=si;s<=sf;s++)
      {
        Int_t id=GetID(m,e,s);
        if(GetStatus(id)==1) sum+=GetSum(id);
      }
  return sum;
}
//_____________________________________________________________________________
Int_t StEmcSpectra::GetEtaBinId(Int_t m,Int_t e)
{
  for(Int_t etabin=1;etabin<=nEtaBins;etabin++)
  {
    Int_t mi,mf,ei,ef;
    CalcEtaBin(etabin,etaBinWidth,&mi,&mf,&ei,&ef);
    if (m>=mi && m<=mf && e>=ei && e<=ef) return etabin;
  }
  return -1;
}
//_____________________________________________________________________________
void StEmcSpectra::SaveAll(char *filename)
{
  ofstream file(filename);
  for(Int_t i=0;i<nBins;i++)
  {
    if(Sum[i]>0)
    {
      file <<i<<"  "<<Sum[i]<<endl;
      if(Sum[i]>0)
      {
        for(Int_t k=0;k<nAdcMax;k++)
          if(Spectra(i,k)>0) file << k<<"  "<<Spectra(i,k)<<endl;
        file <<"9999 0"<<endl;
      }
    }
  }
  file <<"99999 0"<<endl;
  file.close();
}
//_____________________________________________________________________________
void StEmcSpectra::LoadAll(char *filename)
{
  ifstream file(filename);
  cout <<"Loading spectra data from file "<<filename<<endl;
  Int_t i;
  Float_t sumt;
  next:
    file >>i>>sumt;
    if(i<99999)
    {
      Sum[i]+=sumt;
      Int_t k;
      Float_t value;
      cont:
        file >> k >> value;
        if(k<9999)
        {
          Spectra(i,k)+=value;
          goto cont;
        }
      goto next;
    }  
  file.close();
}
//_____________________________________________________________________________
StEmcGeom* StEmcSpectra::GetGeo()
{
  return geo;
}



