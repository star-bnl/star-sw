//*-- Author : Alexandre Suaide 
// 
// $Id: StEmcGainMonitor.cxx,v 1.1 2001/12/29 20:33:51 suaide Exp $
// $Log: StEmcGainMonitor.cxx,v $
// Revision 1.1  2001/12/29 20:33:51  suaide
// added documentation
//
// Revision 1.6  2001/11/07 17:57:40  suaide
// warnings removed
//
// Revision 1.5  2001/11/07 17:54:10  suaide
// some modifications for real data
//
// Revision 1.3  2001/10/26 21:00:33  suaide
// Many modifications to optimize for real data
//
// Revision 1.2  2001/10/17 13:51:31  suaide
// new modifications to work with real data
//
// Revision 1.1  2001/09/24 13:30:49  suaide
// Added Effective pedestal calculation and Pre Calibration Maker to
// generate EMC and L3 StEvent objects from Daq file
//
// Revision 1.13  2000/05 16:07:01  
// Add README
//
#include "StEmcGainMonitor.h"
#include <iostream.h>
#include <math.h>
#include <stdlib.h>
#include "TSystem.h"
#include "StIOMaker/StIOMaker.h"
#include "TPad.h"
#include "TCanvas.h"
ClassImp(StEmcGainMonitor)

//_____________________________________________________________________________
StEmcGainMonitor::StEmcGainMonitor()
{
  m_TowerTmp=new TH2F("tmp","tmp",4800,0.5,4800.5,500,0,500);  
  m_TowerMean=new TH2F("TowerMean","Tower mean values",4800,0.5,4800.5,500,0,500);  
  m_TowerRelGain=new TH2F("TowerRelGain","Tower Relative Gain",4800,0.5,4800.5,500,0,3);  
  m_TowerRelGainDayByDay=new TH2F("TowerRelGainDayByDay","Tower Relative Day by Day",365,0,365,500,0,3);  
  m_TowerRMS=new TH2F("TowerRMS","Tower RMS values",4800,0.5,4800.5,500,0,500);  
  nDir = 0;
  nFilesMax=50000;
  histFileName="./GainMonitor.hist.root";
}
//_____________________________________________________________________________
StEmcGainMonitor::~StEmcGainMonitor()
{
}
//_____________________________________________________________________________
void StEmcGainMonitor::GetYearDay(const char* dirname,Int_t& year,Int_t& day)
{
  TString tmp=dirname;
  Int_t size=tmp.Length();
  TString tmp2=tmp(size-3,3);
  day=atoi(tmp2.Data());
  tmp2=tmp(size-8,4);
  year=atoi(tmp2.Data());
  cout <<"year = "<<year<<"  day = "<<day<<endl;
  return;
}
//_____________________________________________________________________________
Bool_t StEmcGainMonitor::GetHist(const char* filename,const char* dirname,Int_t& run,Int_t& fileId)
{            
  m_TowerTmp->Reset();
  
  TString file=filename;
  TString dirtmp=dirname;
  cout <<"    Processing file: "<<file.Data()<<endl;
  TString tmp = file(11,7);
  run = atoi(tmp.Data());
  tmp = file(23,4);
  fileId = atoi(tmp.Data());
  cout <<"       run number = "<<run<<"  fileId = "<<fileId<<endl;
          
  TString fullfile = dirtmp+"/"+file;
  StIOMaker *IOMk=new StIOMaker("IO","r",fullfile.Data(),"bfcTree");
  IOMk->SetIOMode("r");
  IOMk->SetBranch("*",0,"0");                 //deactivate all branches
  IOMk->SetBranch("histBranch",0,"r"); //activate hist Branch
  IOMk->Init();
  IOMk->Shunt();
  //IOMk->Clear();
  Int_t istat = IOMk->Open();
  istat = IOMk->Make();
          
  TDataSet *Event = IOMk->GetDataSet("hist");
  if (!Event) 
  {
    cout << " No histogram branch found in file!" << endl;
    return kFALSE;
  }
          
  TDataSet *os = Event->Find("EreadHist");
  TList *in;
  if(os) in = (TList*) os->GetObject();
          
  TH2F *h;
  if(in)
  {
    TIter nextHist(in);
    while((h=(TH2F*)nextHist())) 
    {
      if(!strcmp(h->GetName(),"TowerSpectra")) goto end;
    }
  }
  end:
  if(h) 
  {
    cout <<"Found Histogram\n";
    m_TowerTmp->Add(h);
    //m_TowerTmp->Draw();
    //gPad->Update();
    IOMk->Close();
    delete IOMk;
    return kTRUE;
  }
  
  IOMk->Close();
  delete IOMk;
  return kFALSE;

}
//_____________________________________________________________________________
Bool_t StEmcGainMonitor::ProcessFiles()
{
  if(nDir==0) return kFALSE;
  
  Init();
  
  for(Int_t i=0;i<nDir;i++)
  {
    cout <<"Processing files from dir: "<<dir[i].Data()<<endl;
    
    Int_t nfiles =0;
    
    void *d = gSystem->OpenDirectory(dir[i].Data());
    const char* entry;
    
    Int_t year,day;
    GetYearDay(dir[i].Data(),year,day);
    if(i==0) {baseday=day; baseyear=year;}
    
    char name[40],title[100];
    sprintf(name,"BTOW-%03d-%04d-%03d",i,year,day);
    sprintf(title,"BTOW spectrum (%03d) for year %04d and day %03d",i,year,day);
    m_Tower=new TH2F(name,title,4800,0.5,4800.5,500,0,500);  
    
    do
    {
      entry=gSystem->GetDirEntry(d);
      if(entry!=0)
      {
        const char* filename=gSystem->BaseName(entry);
        TString file=filename;
        if(file.EndsWith(".hist.root") && file.BeginsWith("st_physics"))
        {
          Int_t run,fileId;
          nfiles++;
          if(GetHist(file.Data(),dir[i].Data(),run,fileId)) m_Tower->Add(m_TowerTmp);
        }
      }
    } while (entry!=0 && nfiles<=nFilesMax);
    CalcMeanRMS(year,day,0);
    WriteTower();
  }
  WriteMeanRMS();
  return kTRUE;
}
//_____________________________________________________________________________
void StEmcGainMonitor::Init()
{
  TFile output(histFileName.Data(),"RECREATE");
  output.Close();
  gainTable=new St_emcTowerGain("TowerGainTable");
  return;
}
//_____________________________________________________________________________
void StEmcGainMonitor::WriteTower()
{
  TFile output(histFileName.Data(),"UPDATE");
  m_Tower->Write();
  output.Close();
  return;
}
//_____________________________________________________________________________
void StEmcGainMonitor::WriteMeanRMS()
{
  TFile output(histFileName.Data(),"UPDATE");
  m_TowerMean->Write();
  m_TowerRMS->Write();
  m_TowerRelGain->Write();
  m_TowerRelGainDayByDay->Write();
  gainTable->Write();
  output.Close();
  return;
}
//_____________________________________________________________________________
Bool_t StEmcGainMonitor::CalcMeanRMS(Int_t year,Int_t day,Int_t run)
{
  emcTowerGain_st row;
  row.year = year;
  row.day = day;
  row.run = run;
  Float_t sum=0,sumAbove30=0;
  
  for(Int_t i=1;i<4801;i++) // loop over towers
  {
    row.Mean[i-1]=0;
    row.RMS[i-1]=0;
    Float_t x=0,x2=0,n=0,nlin=0;
    for(Int_t j=0;j<500;j++) // loop over adc
    {
      sum+=m_Tower->GetBinContent(i-1,j);
      if(j>=30)
      {
        sumAbove30+=m_Tower->GetBinContent(i-1,j);
        if(j<140)
        {
          Float_t xt=m_Tower->GetBinContent(i-1,j);
          if(xt>1)
          {
            Float_t wt=log(xt);
            //if(xt==1) wt=1;
            x+=wt*(Float_t)j;
            x2+=pow((Float_t)j,2)*wt;
            n+=wt;
            nlin++;
          }
        }
      }
    }
    row.Sum[i-1]=sum;
    row.SumAbove30[i-1]=sumAbove30;
    if(n>0)
    {
      Float_t mean=x/n;
      Float_t rms=sqrt(x2/n-mean*mean);
      m_TowerMean->Fill((Float_t)i,mean);
      m_TowerRMS->Fill((Float_t)i,rms);
      row.Mean[i-1]=mean;
      row.RMS[i-1]=rms;
      //cout <<"Tower "<<i<<"  mean = "<<mean<<"  rms = "<<rms<<"  sum = "<<nlin<<"  n = "<<n<<endl;
    }
  }
  gainTable->AddAt(&row);
  emcTowerGain_st *ref=gainTable->GetTable();
  for(Int_t i=1;i<4801;i++) // loop over towers to calculate relative gain
  {
    Float_t mean = row.Mean[i-1];
    Float_t refMean = ref[0].Mean[i-1];
    Float_t thisSum=0,refSum=0;
    
    if(ref[0].Sum[i-1]>0) refSum = ref[0].SumAbove30[i-1]/ref[0].Sum[i-1];
    if(row.Sum[i-1]>0)    thisSum = row.SumAbove30[i-1]/row.Sum[i-1];
    //cout <<"tower "<<i<<"  rel gain = "<<refMean<<"  row = "<<row.Mean[i-1]<<endl;

    /*if(refMean>0)
    {
      Float_t gain=mean/refMean;
      m_TowerRelGain->Fill((Float_t)i,gain);
      m_TowerRelGainDayByDay->Fill((Float_t)(gainTable->GetNRows()-1),gain);
    }*/
    if(refSum>0)
    {
      Float_t gain=thisSum/refSum;
      m_TowerRelGain->Fill((Float_t)i,gain);
      m_TowerRelGainDayByDay->Fill((Float_t)(gainTable->GetNRows()-1),gain);
    }
  }
}


