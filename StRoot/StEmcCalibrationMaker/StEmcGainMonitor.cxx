//*-- Author : Alexandre Suaide 
// 
// $Id: StEmcGainMonitor.cxx,v 1.8 2003/09/02 17:57:59 perev Exp $
// $Log: StEmcGainMonitor.cxx,v $
// Revision 1.8  2003/09/02 17:57:59  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.7  2003/01/11 23:34:50  suaide
// added back. Deleted by mistake before
//
// Revision 1.5  2002/12/02 21:30:56  suaide
// New EMC calibration maker
//
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
#include <Stiostream.h>
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
  mSpec = new StEmcEqualSpectra("bemc");
  mSpec->Init();
  mAdc1 = 30;
  mAdc2 = 150;
  mMode = 0;
}
//_____________________________________________________________________________
StEmcGainMonitor::~StEmcGainMonitor()
{
}
//_____________________________________________________________________________
Bool_t StEmcGainMonitor::ProcessFiles()
{
  if(mNDir==0) return kFALSE;
  mReference =-1;
  Int_t data = 20400101;
  Int_t hour = 000000;
  for(Int_t i=0;i<mNDir;i++)
  {
    cout <<"Processing files from dir: "<<mDir[i].Data()<<endl;    
    void *d = gSystem->OpenDirectory(mDir[i].Data());
    const char* entry;        
    do
    {
      entry=gSystem->GetDirEntry(d);
      if(entry!=0)
      {
        const char* filename=gSystem->BaseName(entry);
        TString file=filename;
        if(file.EndsWith(".data.root") && file.BeginsWith("bemcGainMonitor"))
        {
          TString f = mDir[i]+"/"+file;
          Int_t s=16;
          TString ts = file(s,15);
          TString t = file(s,8);
          Int_t d = atoi(t.Data());
          t = file(s+9,6);
          Int_t h = atoi(t.Data());
          if((d<data) || (d==data && h<hour)) { data = d; hour = h; mReference = mNFiles; } 
          SetGainFile(mNFiles,(char*)f.Data(),(char*)ts.Data());       
          cout <<"File = "<<mFiles[mNFiles]<<"  timeStamp = "<<mTimeStamp[mNFiles].Data()<<endl;
          mNFiles++;
        }
      }
    } while (entry!=0);
  }
  cout <<"Number of files = "<<mNFiles <<"  basic time = "<<data<<"."<<hour<<"  ref ="<<mReference<<endl;
  
  CalculateRefGain(mReference);
  
  mHistGain = new TH2F("mHistGain","mHistGain",mNFiles,0,mNFiles,1000,0,3);
  
  for(Int_t i=0;i<mNFiles;i++) CalculateGain(i);
 
  mHistGain->Draw();
  return kTRUE;
}
//_____________________________________________________________________________
void StEmcGainMonitor::CalculateRefGain(Int_t i)
{
  mSpec->ZeroAll();
  mSpec->LoadAll((char*)mFiles[i].Data());
  
  Float_t AVG,RMS;
  Int_t nValid = 0;
  
  for(Int_t id =1;id<=4800;id++)
  {
    if(mMode==0) mSpec->GetMeanAndRms(id,mAdc1,mAdc2,&AVG,&RMS);
    else mSpec->GetLogMeanAndRms(id,mAdc1,mAdc2,&AVG,&RMS);
    if (AVG>0) 
    { 
      mAvgRef[id-1] = AVG; 
      mRmsRef[id-1] = RMS; 
      TH1D* h = mSpec->GetSpectra(id);
      Int_t ibin0 = h->FindBin(mAdc1);
      Int_t ibin1 = h->FindBin(mAdc2);
      mStatRef[id-1] = h->Integral(ibin0,ibin1);
      nValid ++; 
    }
  }
  cout <<"Number of valid channels = "<<nValid<<endl;
}
//_____________________________________________________________________________
void StEmcGainMonitor::CalculateGain(Int_t i)
{
    cout <<"Processing id "<<i<<endl;
    mSpec->ZeroAll();
    mSpec->LoadAll((char*)mFiles[i].Data());
    
    Float_t AVG,RMS,avgErr = 0,n = 0;
    for(Int_t id =1;id<=4800;id++)
    {
      if(mMode==0) mSpec->GetMeanAndRms(id,mAdc1,mAdc2,&AVG,&RMS);
      else mSpec->GetLogMeanAndRms(id,mAdc1,mAdc2,&AVG,&RMS);
      if (AVG>0 && mAvgRef[id-1]>0) 
      { 
        TH1D* h = mSpec->GetSpectra(id);
        Int_t ibin0 = h->FindBin(mAdc1);
        Int_t ibin1 = h->FindBin(mAdc2);
        Float_t stat = h->Integral(ibin0,ibin1);
        mGain[id-1] = mAvgRef[id-1]/AVG; 
        mGainErr[id-1] = mGain[id-1]*::sqrt(1/mStatRef[id-1]+1/stat);
        avgErr+=(mGainErr[id-1]/mGain[id-1]);
        n++;
        if(mHistGain)mHistGain->Fill((Float_t)i,mGain[id-1]);
      }
      else mGain[id-1] = 0.0;
    }
    if(n>0)cout <<"Average statistical error (%) = "<<avgErr/n*100<<endl;
}
//_____________________________________________________________________________
void StEmcGainMonitor::SaveHist(char* file)
{
}




