// 
// $Id: StEmcADCtoEMaker.cxx,v 1.25 2001/12/27 16:23:44 suaide Exp $
// $Log: StEmcADCtoEMaker.cxx,v $
// Revision 1.25  2001/12/27 16:23:44  suaide
// changed documentation
//
// Revision 1.24  2001/12/26 19:25:34  suaide
// Added documentation and few modifications
//
// Revision 1.23  2001/12/06 17:50:08  suaide
// changes to save ADC without pedestal subtraction
//
// Revision 1.22  2001/12/05 22:31:12  suaide
// Modifications to include SMD
//
// Revision 1.21  2001/12/04 22:05:50  suaide
// new QA histogram for tower
//
// Revision 1.20  2001/11/05 17:09:11  suaide
// small changes
//
// Revision 1.19  2001/10/31 22:24:17  suaide
// modified Finish() method
//
// Revision 1.11  2001/10/24 23:06:54  suaide
// log messages included for easier debug
//
// Revision 1.8  2001/10/24 14:47:16  suaide
// type correction
//
// Revision 1.7  2001/10/24 14:41:44  suaide
// huge change on StEmcADCtoEMaker to a different software.
// The other version is kept as *.old for future debug 
//
// Revision 1.13  2000/05 16:07:01  
// Add README
//

/*!\class StEmcADCtoEMaker
\author Alexandre A. P. Suaide

This class gets EMC raw ADC's and convert them to calibrated energy.<br><br>

The main variables that should be set are:<br>
kCalib[det] - Set to kTRUE if you want to calibrate an EMC subdetector.<br>
isDaqFile - Set to kTRUE if you are reading a DAQ dataset.<br>
subtractPedestal - Set to kTRUE if pedestal should be subtracted.<br>
saveOnlyCalibHits - Set to kTRUE if you want to save only calibrated hits. Otherwise, all
hits with ADC>0 are saved.<br><br>

The defaults values are:<br>
kCalib[det] = {kTRUE,kFALSE,kTRUE,kTRUE}<br>
isDaqFile = kTRUE<br>
subtractPedestal = kTRUE<br>
saveOnlyCalibHits = kFALSE<br>
*/
#include "StEmcADCtoEMaker.h"
#include "StChain.h"
#include "St_DataSetIter.h"
#include "StEventTypes.h"
#include "StEvent.h"
#include <iostream.h>
#include <math.h>
#include "StMessMgr.h"
#include "StEmcSimulatorMaker/StEmcSimulatorMaker.h"
#include "St_ems_Maker/St_ems_Maker.h"
#include "StEmcUtil/emcDetectorName.h"
#include "StEmcUtil/StEmcGeom.h"
#include "TStopwatch.h"
// DAQ Libraries
#include "StDaqLib/GENERIC/EventReader.hh"
#include "StDaqLib/EMC/EMC_Reader.hh"
#include "StDAQMaker/StDAQReader.h"
#include "StDaqLib/EMC/StEmcDecoder.h"

ClassImp(StEmcADCtoEMaker)

//_____________________________________________________________________________
///StEmcADCtoEMaker constructor
StEmcADCtoEMaker::StEmcADCtoEMaker(const char *name):StMaker(name)
{
  for(Int_t i=0;i<MAXDET;i++) kCalib[i]=kFALSE; // all detectors off
  kCalib[0]=kTRUE;   // bemc in
  kCalib[2]=kTRUE;   // bsmde in
  kCalib[3]=kTRUE;   // bsmdp in
  isDaqFile=kTRUE;   
  subtractPedestal=kTRUE;
  saveOnlyCalibHits=kFALSE;
}
//_____________________________________________________________________________
///StEmcADCtoEMaker destructor
StEmcADCtoEMaker::~StEmcADCtoEMaker()
{
}
//_____________________________________________________________________________
///Init function. This method initializes the histograms
Int_t StEmcADCtoEMaker::Init()
{   
  gMessMgr->SetLimit("StEmcADCtoEMaker",1000);
  
  //Making QA histgrams
  const Int_t   nx[] = {40,40,300,20,12,12,12,12};
  const Float_t xl[] = {-1.0,-1.0,-1.0,-1.0, 0.5 , 0.5, 0.5, 0.5};
  const Float_t xu[] = { 1.0, 1.0, 1.0, 1.0, 12.5,12.5,12.5,12.5};
  const Int_t   ny[] = {120, 120, 60, 900, 60, 60, 60, 60};
 
  m_nhit = new TH2F("EmcNHitsVsDet" ,"Number of hit(log) with energy > 0 .vs. Detector #",500,0.0,15.0,8,0.5,8.5);
  m_etot = new TH2F("EmcEtotVsDet" ,"Total energy(log) .vs. Detector #",500,-4.0,15.0,8,0.5,8.5);

  //tower spectra for gain monitoring  
  m_tower=new TH2F("TowerSpectra","Tower Spectra up to ADC = 500",4800,0.5,4800.5,500,0,500);
  m_towerMean=new TH1F("TowerMean","Mean ADC value for tower",4800,0.5,4800.5);
  m_towerRMS=new TH1F("TowerRMS","RMS of ADC value for tower",4800,0.5,4800.5);
  m_towerSum=new TH1F("TowerSum","Total number of hits for tower",4800,0.5,4800.5);
       
  // SMD time bin
  m_smdTimeBin = new TH2F("SmdTimeBin","SMD Time bin",8,-0.5,7.5,128,0.5,128.5);

  for (Int_t i=0; i<MAXDET; i++) if(kCalib[i])
  {
    TString name_h = detname[i] + "_Hits";
    TString name_e = detname[i] + "_Energy";
    TString name_a = detname[i] + "_ADC";
    TString name_a1= detname[i] + "_ADC1D";
    TString title_h= detname[i] + " Hits distribution with energy > 0";
    TString title_e= detname[i] + " Energy distribution";
    TString title_a= detname[i] + " ADC distribution";
    TString title_a1= detname[i] +" ADC distribution (log)";
    Float_t rpi = M_PI + 0.00001; 
    m_hits[i]   = new TH2F(name_h,title_h,nx[i],xl[i],xu[i],ny[i],-rpi, rpi);
    m_energy[i] = new TH2F(name_e,title_e,nx[i],xl[i],xu[i],ny[i],-rpi, rpi);
    m_adc[i]    = new TH2F(name_a,title_a,nx[i],xl[i],xu[i],ny[i],-rpi, rpi);
    m_adc1d[i]  = new TH1F(name_a1,title_a1,1000,0,8);
    
    // creating geometry ...
    geo[i]=StEmcGeom::getEmcGeom(detname[i].Data());

  }
  
  return StMaker::Init();
}
//_____________________________________________________________________________
///This method creates mean ADC and RMS histograms. It runs only in the end of the job
Int_t StEmcADCtoEMaker::Finish()
{
  cout <<"Creating gain monitor histogram for EMC towers\n";
  for(Int_t i=1;i<4801;i++) // loop over towers
  {
    Float_t x=0,x2=0,n=0,nlin=0;
    for(Int_t j=30;j<140;j++) // loop over adc
    {
      Float_t xt=m_tower->GetBinContent(i-1,j);
      if(xt>0)
      {
        x+=log(xt)*(Float_t)j;
        x2+=pow((Float_t)j,2)*log(xt);
        n+=log(xt);
        nlin++;
      }
    }
    if(n>0)
    {
      Float_t mean=x/n;
      Float_t rms=sqrt(x2/n-mean*mean);
      m_towerMean->Fill((Float_t)i,mean);
      m_towerRMS->Fill((Float_t)i,rms);
      m_towerSum->Fill((Float_t)i,nlin);
      //cout <<"Tower "<<i<<"  mean = "<<mean<<"  rms = "<<rms<<"  sum = "<<nlin<<"  n = "<<n<<endl;
    }
  }
  return kStOk;
}
//_____________________________________________________________________________
///Process the event. Basicaly it get the status database and makes a loop over
///EMC subdetectors. For each sub detector it gets the calibration tables, subtract
///pedestals and apply calibration constants. In the end, update StEvent with
///calibrated hits
Int_t StEmcADCtoEMaker::Make()
{  
  TStopwatch clock;
  clock.Start();
  cout <<"\n\n\nStEmcADCtoEMaker::Make()******************************************************************\n";
      
  for(Int_t i=0;i<MAXDET;i++) kCalibTemp[i]=kCalib[i];
  
  if(decoder) delete decoder;
  decoder=new StEmcDecoder(GetDate(),GetTime());

  m_StatusDb=NULL;
  m_StatusDb=GetInputDB("Calibrations/emc/status");
  if(!m_StatusDb) gMessMgr->Warning("StEmcADCtoEMaker::Make() - Can not get new status tables.... will use default ");
  
  if(!GetEmcEvent())
  {
    gMessMgr->Warning("StEmcADCtoEMaker::Make() - Can not get pointer to EMC Event");
    return kStWarn;
  }
   
  // loop over detectors...
  for(Int_t det=0;det<MAXDET;det++)
    if(kCalibTemp[det]) if(nChannels[det]>0)
    {
      cout <<"**** Calibrating detector "<<detname[det].Data()<<endl;
      Bool_t ok=kTRUE;
      
      cout <<"***** Getting calibration tables\n";
            
      m_CalibDb=NULL;
      TString DbName="Calibrations/emc/"+detname[det];      
      m_CalibDb=GetInputDB(DbName.Data());
      if(!m_CalibDb) 
      {
        gMessMgr->Warning("StEmcADCtoEMaker::Make() - Can not get new calib tables.... ");
        ok=kFALSE;       
      }
                              
      if(ok)
      {
        Int_t nhits=0;
        Float_t energy=0;
        Bool_t CalOk=Calibrate(det,&nhits,&energy);
        if(CalOk) FillHistograms(det,nhits,energy);
      }
    }
    
  if(!FillStEvent()) gMessMgr->Warning("StEmcADCtoEMaker::Make() - No StEvent to save EmcCollection");
  
  clock.Stop();
  cout <<"Time to run StEmcADCtoEMaker::Make() real = "<<clock.RealTime()<<"  cpu = "<<clock.CpuTime()<<" \n";
  cout <<"*******************************************************************************************\n\n\n";

  return kStOK;
}
//_____________________________________________________________________________
///This method gets the status tables for a given detector and stores it in the
///array status[det][index].
void StEmcADCtoEMaker::GetStatus(Int_t det)
{
  cout <<"Getting status table for detector "<<detname[det].Data()<<endl;
  TString TableName=detname[det]+"Running";
  Int_t total=0;
  nChannels[det]=0;
  
  for(Int_t i=1;i<=18000;i++) status[det][i-1]=0;
  Int_t d = GetDate();
  Int_t t = GetTime();
  cout <<"date = "<<d<<"  time = "<<t<<endl;
  
  if(det<2) // bemc and bprs
  {
    total = 4800;
    St_emcRunning* run = NULL;
    if(m_StatusDb) run = (St_emcRunning*)m_StatusDb->Find(TableName.Data());
    if(run)
    {
      cout <<"Got status table from DataBase\n";
      emcRunning_st* runst=run->GetTable(); 
      for(Int_t i=1;i<=4800;i++) 
      {
        status[det][i-1]=(Int_t) runst[0].IsRunning[i-1];
        //cout <<" i = "<<i<<"  status = "<<status[i-1]<<"  "<<(char)(runst[0].IsRunning[i-1]+65)<<endl;
        if(status[det][i-1]==1) nChannels[det]++;
      }
    } 
    else  // standard status table for bemc
    {
      if(det==0)
      {
        cout <<"Using bemc default status table\n ";
        for(Int_t i=1;i<=4800;i++)
        {
          status[det][i-1]=0;
          if (i>=1861 && i<=2340) status[det][i-1]=1; // initial 2001 configuration
          if (i>=2021 && i<=2100) status[det][i-1]=0; // initial 2001 configuration
          if (i>=1861 && i<=2020)  // remove crate 0x12
          {
            if(d>=20011015 && d<=20011020) status[det][i-1]=0;
          }
          if(status[det][i-1]==1) nChannels[det]++;

        }

        status[det][2309-1]=0;
        status[det][1986-1]=0;
        status[det][1979-1]=0;
        nChannels[det]-=3;
      }
    }
  }
  else
  {
    total = 18000;
    St_smdRunning* run = NULL;
    if(m_StatusDb) run = (St_smdRunning*)m_StatusDb->Find(TableName.Data());
    if(run)
    {
      smdRunning_st* runst=run->GetTable(); 
      for(Int_t i=1;i<=18000;i++) 
      {
        status[det][i-1]=(Int_t)runst[0].IsRunning[i-1];
        if(status[det][i-1]==1) nChannels[det]++;
      }
    }
  }
  cout <<"Total number of channels = "<<total<<"  active = "<<nChannels[det]<<endl;
  return;

}
//_____________________________________________________________________________
///This method gets EMC collection from DAQ dataset. It also gets the capacitor number
///for SMD and saves it in the calibrationType member of StEmcRawHit. It looks for
///hits in the detector only if kCalib[det] is set as kTRUE
StEmcCollection* StEmcADCtoEMaker::GetEmcCollectionFromDaq(TDataSet* daq)
{
  cout <<"***** Getting EMC event from daq file\n";
  UInt_t eta[]={20,20,150,10};
  UInt_t sub[]={2,2,1,15};

  StEmcCollection* emcDaqUtil=new StEmcCollection();

  cout <<"***** Getting Daq Reader\n";
  StDAQReader* TheDataReader=(StDAQReader*)(daq->GetObject());
  if(!TheDataReader) return NULL;
  if(!TheDataReader->EMCPresent()) return NULL;

  cout <<"***** Getting point to EMC data bank\n";
  StEMCReader* TheEmcReader=TheDataReader->getEMCReader();
  if(!TheEmcReader) return NULL;
  
  cout <<"***** Loop over detectors\n";
  for(Int_t det=0;det<4;det++) if(kCalib[det]) 
  {
    //if(nChannels[det]>0) // check if there are valid channels.
    {
      Float_t sum=0,validChannels=0;
      StDetectorId id = static_cast<StDetectorId>(det+kBarrelEmcTowerId);
    
      Bool_t Ok=kTRUE;
    
      if(det==0) // check if data is Ok for bemc
      {
        cout <<"EMC VALID TOWER HITS = "<<TheEmcReader->NTowerHits()<<" OFFLINE ACTIVE CHANNELS = "<<nChannels[det]<<endl;
        if(TheEmcReader->NTowerHits()==0) Ok=kFALSE;
      }
      
      if(det==2 || det ==3) // get time bin
      {
        unsigned int timebin;
        for(Int_t RDO=0;RDO<8;RDO++)
        {
          TheEmcReader->getSMD_TIMEBIN(RDO,timebin);
          if(timebin>255) timebin=255;
          smdTimeBin[det-2][RDO]=(Int_t)timebin;
        }
      }
    
      if(Ok)
      {
        StEmcDetector* detector = new StEmcDetector(id,120);
        for(UInt_t m=1;m<=120;m++)
          for(UInt_t e=1;e<=eta[det];e++)
            for(UInt_t s=1;s<=sub[det];s++)
            {
              unsigned short ADC=0;
              if(det==0) if(!TheEmcReader->getTowerADC((int)m,(int)e,(int)s,ADC)) goto next;
              if(det==2) if(!TheEmcReader->getSMDE_ADC((int)m,(int)e,ADC)) goto next;
              if(det==3) if(!TheEmcReader->getSMDP_ADC((int)m,(int)e,(int)s,ADC)) goto next;
              Int_t idh;
              geo[det]->getId(m,e,s,idh);
              //cout <<"m = "<<m<<"  e = "<<e<<"  s = "<<s<<"  adc = "<<ADC<<endl;
              if(ADC>0)
              {
                sum+=(Float_t)ADC;
                validChannels++;
                StEmcRawHit* hit=new StEmcRawHit(id,m,e,s,(UInt_t)ADC);
                if(det==2 || det==3) // get time bin
                {
                  int RDO,index;
                  decoder->GetSmdRDO(det+1,m,e,s,RDO,index);
                  hit->setCalibrationType(smdTimeBin[det-2][RDO]);
                }
                detector->addHit(hit);
              }
              next: continue;
            }
        cout <<"Total ADC sum = "<<sum<<"  validChannels = "<<validChannels<<endl;
        if(validChannels==0) kCalibTemp[det]=kFALSE;
        emcDaqUtil->setDetector(detector); 
      }
      else
      {
        cout <<"***** BAD event for detector "<<detname[det].Data()<<endl;
        kCalibTemp[det]=kFALSE;
      }
      
    }
  }

  return emcDaqUtil;
}
//_____________________________________________________________________________
///This method gets EMC hits from different sources. First it looks for DAQ datasets.
///if Not present, looks for Simulated hits and, in the end, for old StEvent hits to
///recalibrate.
Bool_t StEmcADCtoEMaker::GetEmcEvent()
{
  StEmcCollection* emctemp=NULL;
  
  for(Int_t det=0;det<4;det++) GetStatus(det);
  
// check if there is event from DAQ
  if(isDaqFile)
  {
    cout <<"***** trying to get Daq dataset\n";
    TDataSet* mTheEmcData   = GetDataSet("StDAQReader");
 	  if(!mTheEmcData) return kFALSE;
    emctemp = GetEmcCollectionFromDaq(mTheEmcData);
    if(!emctemp) return kFALSE;
    m_emc=emctemp;
    return kTRUE;
  }
  else
  {  
    // check if there is event from StEmcSimulator
    cout <<"Trying to get event from Simulator\n";
    StEmcSimulatorMaker* simnew = (StEmcSimulatorMaker*)GetMaker("emcRaw");
    if(simnew)
    { 
      emctemp = (StEmcCollection*)simnew->getEmcCollection();
      if(emctemp)
      {
        m_emc=emctemp;
        return kTRUE;
      }
    }
    else 
    {
      St_ems_Maker* simold = (St_ems_Maker*)GetMaker("emc_raw");;
      if(simold) 
      {
        emctemp = (StEmcCollection*)simold->getEmcCollection();
        if(emctemp)
        {
          m_emc=emctemp;
          return kTRUE;
        }
      }
    } 
    
    // check if there is event from StEvent
    cout <<"Trying to get event from StEvent\n";
    StEvent* event = (StEvent*)GetInputDS("StEvent");
    if(!event) return kFALSE;
    emctemp=event->emcCollection();
    if(!emctemp)  return kFALSE;
  
    m_emc=emctemp;
  }

  return kTRUE;
  
}
//_____________________________________________________________________________
///This method creates a temporary ADC vector for each detector. This ADC vector
///is pedestal subtracted and it is used for calibration, once the original ADC
///value present at StEvent is not pedestal subtracted.
Bool_t StEmcADCtoEMaker::CreateVector(Int_t detnum)
{
  cout <<"organizing data for detector "<<detname[detnum].Data()<<endl;
  
  for(Int_t i=0;i<18000;i++) {ADCTemp[i]=0; smdTimeBinTemp[i]=999;}
  
  StDetectorId id = static_cast<StDetectorId>(detnum+kBarrelEmcTowerId);  
  StEmcDetector* detector=m_emc->detector(id);
  if(!detector) 
  {
    cout <<"No detector "<<detname[detnum].Data()<<" present on EmcCollection\n";
    return kFALSE;
  }
  
  for(UInt_t j=1;j<121;j++)
  {
    StEmcModule* module = detector->module(j);
    if(module)
    {
      StSPtrVecEmcRawHit& rawHit=module->hits();
      for(UInt_t k=0;k<rawHit.size();k++)
      {
        Int_t m=rawHit[k]->module();
        Int_t e=rawHit[k]->eta();
        Int_t s=abs(rawHit[k]->sub());
        Float_t adc=(Float_t) rawHit[k]->adc();
       
        Int_t idh;
        geo[detnum]->getId(m,e,s,idh);
        ADCTemp[idh-1]=adc;
        if(detnum==2 || detnum==3) smdTimeBinTemp[idh-1]=rawHit[k]->calibrationType()%1000;
      }
    }
  }
          
  if(subtractPedestal)
  {
    cout <<"getting pedestal tables for "<<detname[detnum].Data()<<endl;
    TString TableName=detname[detnum]+"Pedestal";
    emcPedestal_st *emcpedst=NULL;
    smdPedestal_st *smdpedst=NULL;
   
    if(detnum<2) //bemc and bprs
    {
      St_emcPedestal* ped=(St_emcPedestal*)m_CalibDb->Find(TableName.Data());
      if(!ped) return kFALSE;
      emcpedst=ped->GetTable();
      if(!emcpedst) return kFALSE;
    }
    else // SMD
    {
      St_smdPedestal* ped=(St_smdPedestal*)m_CalibDb->Find(TableName.Data());
      if(!ped) return kFALSE;
      smdpedst=ped->GetTable();
      if(!smdpedst) return kFALSE;  
    }
    
    Int_t nMax=120*geo[detnum]->NEta()*geo[detnum]->NSub();
    for(Int_t id=1;id<=nMax;id++)
    {
      Float_t pedestal=0;
        
      if(detnum<2) //bemc and bprs
        pedestal=emcpedst[id-1].AdcPedestal;
      else // SMD
      {
        Int_t timeBin=smdTimeBinTemp[id-1];
        if(timeBin>=0 && timeBin<128) pedestal=smdpedst[id-1].AdcPedestal[timeBin];
      }
      
      if(ADCTemp[id-1]>0) 
      {
        //cout <<"id = "<<id<<"  ADC = "<<ADCTemp[id-1]<<"  ped = "<<pedestal;
        ADCTemp[id-1]-=pedestal;
        if(ADCTemp[id-1]<0) ADCTemp[id-1]=0;
        //cout <<"  ADCSub = "<<ADCTemp[id-1]<<endl;
      }
      else ADCTemp[id-1]=0;
    }
  }  
  return kTRUE;
}
//_____________________________________________________________________________
///This method applies the calibration constants to get the hit energy. The calibration
///is applied only to the hits which tower/strip status is set to 1 (status[det][index==1).
///It also checks if the calibration is done for that bin
Bool_t StEmcADCtoEMaker::Calibrate(Int_t detnum,Int_t* NHITS,Float_t* ENERGY)
{
  cout <<"applying calibration for detector "<<detname[detnum].Data()<<endl;
  Int_t nhits=0;
  Float_t totalenergy=0;

  StDetectorId id = static_cast<StDetectorId>(detnum+kBarrelEmcTowerId);
  StEmcDetector* detector=m_emc->detector(id);
  
  if(!detector) return kFALSE;

  CreateVector(detnum);
    
  cout <<"getting calibration table \n";
  TString TableName=detname[detnum]+"Calibration";
  St_emcCalibration* cal=(St_emcCalibration*)m_CalibDb->Find(TableName.Data());
  if(!cal) return kFALSE;

  emcCalibration_st* cal_st=cal->GetTable();
  if(!cal_st) return kFALSE;

  cout <<"applying calibration... loop over modules \n";
  for(UInt_t j=1;j<121;j++)
  {
    StEmcModule* module = detector->module(j);
    if(module)
    {
      StSPtrVecEmcRawHit& rawHit=module->hits();
      for(UInt_t k=0;k<rawHit.size();k++)
      {
        Int_t m=rawHit[k]->module();
        Int_t e=rawHit[k]->eta();
        Int_t s=abs(rawHit[k]->sub());
      

        Int_t idh;
        geo[detnum]->getId(m,e,s,idh);

        Float_t adc=ADCTemp[idh-1];
       
        Float_t energy=0;
        Bool_t st=kFALSE;
        if(adc>0 && status[detnum][idh-1]==1 && cal_st[idh-1].Status==1 && cal_st[idh-1].CalibStatus==1)
        {
           Float_t adcpower=1;
           for(Int_t i=0;i<5;i++) {energy+=cal_st[idh-1].AdcToE[i]*adcpower; adcpower*=adc;}            
           if (energy<0) energy=0;
           nhits++;
           totalenergy+=energy;
        }
        rawHit[k]->setEnergy(energy);
        /*if (rawHit[k]->adc()>0)
          cout <<"hit "<<k<<"  m = "<<m<<"  e = "<<e<<"  s = "<<s
               <<"  ADC = "<<rawHit[k]->adc()<<"  adc = "<<adc<<"  en = "<<energy
               <<"  CalType = "<<rawHit[k]->calibrationType()<<endl;*/
      }
    }
  }
  *NHITS=nhits;
  *ENERGY=totalenergy;
  return kTRUE;
}
//_____________________________________________________________________________
///This method fills QA histograms
Bool_t StEmcADCtoEMaker::FillHistograms(Int_t detnum,Int_t nhits,Float_t energy)
{
  cout <<"***** Filling histograms for detector "<<detname[detnum].Data()<<endl;
  if(nhits>0)  m_nhit->Fill(log((Float_t)nhits),(Float_t)detnum+1);
  if(energy>0) m_etot->Fill(log(energy),(Float_t)detnum+1);
    
  StDetectorId id = static_cast<StDetectorId>(detnum+kBarrelEmcTowerId);
  StEmcDetector* detector=m_emc->detector(id);
  if(!detector) return 0;
  
  Float_t totaladc=0;
  
  if(detnum==2) // fill time bin for SMDE only becasue they are the same for SMDP
  {
    for(Int_t RDO=0;RDO<8;RDO++)
    {
      if(smdTimeBin[0][RDO]<128) m_smdTimeBin->Fill(RDO,smdTimeBin[0][RDO]);
    }
  }
  
  if(detnum==0) // gain monitor histogram for tower
  {
    for(Int_t i=0;i<4800;i++)
      if(ADCTemp[i]>0 && ADCTemp[i]<500) m_tower->Fill(i+1,ADCTemp[i]);
  }
  
  for(UInt_t j=1;j<121;j++)
  {
    StEmcModule* module = detector->module(j);
    if(module)
    {
      StSPtrVecEmcRawHit& rawHit=module->hits();
      for(UInt_t k=0;k<rawHit.size();k++)
      {
        Int_t m=rawHit[k]->module();
        Int_t e=rawHit[k]->eta();
        Int_t s=abs(rawHit[k]->sub());
        Float_t energy=rawHit[k]->energy();
        Float_t adc=(Float_t)rawHit[k]->adc();
        Int_t idh;
        geo[detnum]->getId(m,e,s,idh);
        Float_t eta,phi;
        geo[detnum]->getEtaPhi(idh,eta,phi);
        if(adc>0) 
        {
          m_hits[detnum]->Fill(eta,phi); 
          m_adc[detnum]->Fill(eta,phi);
          totaladc+=adc;
        }
        if(energy>0) m_energy[detnum]->Fill(eta,phi,energy);
      }
    }
  }
  if(totaladc>0) 
  {
    cout <<"Total ADC sum for detector "<<detname[detnum].Data()<<" = "<<totaladc<<endl;
    m_adc1d[detnum]->Fill(log(totaladc));
  }
  return kTRUE;
}
//_____________________________________________________________________________
///This method makes a clean up of StEvent before store it in the .data
///The clean up corresponds to eliminate all hits with non calibrated energy,
///if this option is set (saveOnlyCalibHits)
Bool_t StEmcADCtoEMaker::FillStEvent()
{  
  // first need to clean hits with adc = 0
  
  StEmcCollection *emcold=m_emc;
  m_emc =new StEmcCollection(); 
  
  cout <<"pointers for emcold = "<<emcold<<"  m_emc = "<<m_emc<<endl;
  for(Int_t det=0;det<4;det++) 
  {
    Int_t totalhits=0,totalhitsused=0;
    StDetectorId id = static_cast<StDetectorId>(det+kBarrelEmcTowerId);
    StEmcDetector* detectorold = emcold->detector(id);
    if(detectorold)
    {
      StEmcDetector* detector = new StEmcDetector(id,120);
      for(UInt_t mod=1;mod<=120;mod++)
      {
        StEmcModule *moduleold=detectorold->module(mod);
        if(moduleold)
        {
          StSPtrVecEmcRawHit& hitsold=moduleold->hits();
          for(Int_t k=0;k<(Int_t)hitsold.size();k++)
          {
            Int_t m=hitsold[k]->module();
            Int_t e=hitsold[k]->eta();
            Int_t s=abs(hitsold[k]->sub());
            Float_t energy=hitsold[k]->energy();
            Float_t adc=(Float_t)hitsold[k]->adc();
            
            Bool_t save = kTRUE;
            
            if(saveOnlyCalibHits)
            {
              if(energy>0) save = kTRUE;
              else save = kFALSE;
            }
            
            if (save) 
            {
              StEmcRawHit* hit=new StEmcRawHit(id,m,e,s,(UInt_t)adc);
              hit->setEnergy(energy);
              detector->addHit(hit);
              totalhitsused++;
            }
            totalhits++;
          }
        }
      }     
      m_emc->setDetector(detector);
      cout <<"Total hits for detector "<<detname[det].Data()<<" = "<<totalhits<<"  after clean up = "<<totalhitsused<<endl;
    }
  }
  delete emcold;
  // finished clean up
  
  StEvent* event = (StEvent*)GetInputDS("StEvent");
  if(!event) return kFALSE;
  StEmcCollection* emctemp=event->emcCollection();
  if(emctemp) delete emctemp;
  event->setEmcCollection(m_emc);
  return kTRUE;
}







