#include "EMC_SmdReader.hh"
#include <assert.h>
#define MAX_ADC 0xFFF
#include <time.h>
#include <stdlib.h>
#include <Stiostream.h>
#include <stdio.h>

/////////////////////////////////////////////////////////////////////
EMC_SmdReader::EMC_SmdReader(EventReader* er,Bank_EMCP *pEMCP): pBankEMCP(pEMCP),ercpy(er)
{
  EventInfo info=er->getEventInfo();
  unsigned int UnixTime=info.UnixTime;
  struct tm *time=gmtime((time_t*) &UnixTime);
  int year=1900+time->tm_year;
  int month=1+time->tm_mon;
  int day=time->tm_mday;
  int hour1=time->tm_hour;
  int min=time->tm_min;
  int sec=time->tm_sec;  
  char text1[80],text2[80];
  sprintf(text1,"%04d%02d%02d",year,month,day);
  sprintf(text2,"%02d%02d%02d",hour1,min,sec);
  unsigned int date=atoi(text1);
  unsigned int hour=atoi(text2);
  decoder = new StEmcDecoder(date,hour);
  cout<<"EMC_SMDreader** Event time (Unix time) = "<<UnixTime<<endl;
  Initialize();
}
/////////////////////////////////////////////////////////////////////
EMC_SmdReader::~EMC_SmdReader()
{
  if(decoder) delete decoder;
}
/////////////////////////////////////////////////////////////////////

void EMC_SmdReader::Initialize()
{
  mTheSmdAdcR.NSmdHits = 0;
  for(int RDO=0;RDO<8;RDO++) mTheSmdAdcR.TimeBin[RDO]=999;

  // Initialize SMDDATA array to 0's
  for(int i = 0 ; i <120 ; i++) 
  {
    for(int j = 0 ; j < 150; j++) mTheSmdAdcR.SmdE_ADCMatrix[i][j] = 0;    //SMD_eta
    for(int j = 0 ; j < 10; j++) for(int k = 0;k < 15; k++) mTheSmdAdcR.SmdP_ADCMatrix[i][j][k] = 0;  //SMD_Phi
  }
}
/////////////////////////////////////////////////////////////////////
Bank_EMCSECP* EMC_SmdReader::getBarrelSmdSection(const Bank_EMCP* pBankEMCP,int section)
{
  if((!pBankEMCP->EMCSecPointer[section].offset) || (!pBankEMCP->EMCSecPointer[section].length))
  {
    char str0[40];
    sprintf(str0,"getBarrelsection(hs %d)",section);
    ercpy->fprintError(INFO_MISSING_BANK,__FILE__,__LINE__,str0);
    cout<<" SmdReader::getBarrelSection** , no offset or length for section**"<<section<<endl;
    return NULL;
  } 

  Bank_EMCSECP * ptr=(Bank_EMCSECP*)(((INT32 *)pBankEMCP)+pBankEMCP->EMCSecPointer[section].offset); // 2nd pointer is for SMD 
 
  //put some checks
  if(strncmp(ptr->header.BankType,"EMCSECP",7)) 
  {
    char str0[40];
    cout<<" error in header name**"<<endl;
    sprintf(str0,"getBarrelSection(hs %d)",section);
    ercpy->fprintError(ERR_BAD_HEADER,__FILE__,__LINE__, str0); return NULL;
  }
  if(!ptr->test_CRC()) 
  {
    char str0[40];
    cout<<"error in CRC**"<<endl;
    sprintf(str0,"getBarrelsection(hs %d)",section);
    ercpy->fprintError(ERR_CRC,__FILE__,__LINE__,str0); return NULL;
  }
  printf("Byte order of header for EMCSECP Before swap**: %x\n",ptr->header.ByteOrder);
  if(ptr->swap() < 0) 
  {
    char str0[40];
    cout<<"error in swap**"<<endl;
    sprintf(str0,"getBarrelsection(hs %d)",section);
    ercpy->fprintError(ERR_SWAP,__FILE__,__LINE__,str0); return NULL;
  }
  ptr->header.CRC = 0;
  return ptr;
}
/////////////////////////////////////////////////////////////////////
Bank_EMCRBP* EMC_SmdReader::getBarrelSmdFiber(Bank_EMCSECP* secp,int section)
{
  if((!secp->FiberPointer[section].offset) || (!secp->FiberPointer[section].length))
  {
    char str0[40];
    sprintf(str0,"getBarrelSmdFiber(hs %d)",section);
    ercpy->fprintError(INFO_MISSING_BANK,__FILE__,__LINE__,str0);
    //cout<<" getBarrelSmdfiber** , no offset or length for section**"<<section<<endl;
    return NULL;
  }
   

  Bank_EMCRBP* ptr =(Bank_EMCRBP*)(((INT32 *)secp)+secp->FiberPointer[section].offset); 
 
  // some checks and swap

  if(strncmp(ptr->header.BankType,"EMCRBP",6)) 
  {
    char str0[40];
    cout<<" error in header name**"<<endl;
    sprintf(str0,"getBarrelSmdfiber(hs %d)",section);
    ercpy->fprintError(ERR_BAD_HEADER,__FILE__,__LINE__, str0); return NULL;
  }
  if(!ptr->test_CRC()) 
  {
    char str0[40];
    cout<<"error in CRC**"<<endl;
    sprintf(str0,"getBarrelSmdfiber(hs %d)",section);
    ercpy->fprintError(ERR_CRC,__FILE__,__LINE__,str0); return NULL;
  }
  if(ptr->swap() < 0) 
  {
    char str0[40];
    cout<<"error in swap**"<<endl;
    sprintf(str0,"getBarrelSmdFiber(hs %d)",section);
    ercpy->fprintError(ERR_SWAP,__FILE__,__LINE__,str0); return NULL;
  }
  ptr->header.CRC = 0;
  return ptr;
}
/////////////////////////////////////////////////////////////////////
Bank_SMDADCR* EMC_SmdReader::getSmdADC(Bank_EMCRBP* rbp)
{
  if((!rbp->EMCADCR.offset) || (!rbp->EMCADCR.length))
  {
    char str0[40];
    sprintf(str0,"getSmdADC(hs )");
    ercpy->fprintError(INFO_MISSING_BANK,__FILE__,__LINE__,str0);
    cout<<" getBarrelADC** , no offset or length for ADCR**"<<endl;
    return NULL;
  } 
  Bank_SMDADCR* pADCR =(Bank_SMDADCR*)(((INT32 *)rbp)+rbp->EMCADCR.offset); 

  // some checks and swap

  if(strncmp(pADCR->header.BankType,"EMCADCR",7)) 
  {
    char str0[40];
    cout<<" error in header name**"<<endl;
    sprintf(str0,"getSmdADC(hs)");
    ercpy->fprintError(ERR_BAD_HEADER,__FILE__,__LINE__, str0); return NULL;
  }
  if(!pADCR->test_CRC()) 
  {
    char str0[40];
    cout<<"error in CRC**"<<endl;
    sprintf(str0,"getSmdADC(hs)");
    ercpy->fprintError(ERR_CRC,__FILE__,__LINE__,str0); return NULL;
  }
  if(pADCR->swap() < 0) 
  {
    char str0[40];
    cout<<"error in swap**"<<endl;
    sprintf(str0,"getBankSVTSECP(hs)");
    ercpy->fprintError(ERR_SWAP,__FILE__,__LINE__,str0); return NULL;
  }
  pADCR->header.CRC = 0; 
  return pADCR;
}
/////////////////////////////////////////////////////////////////////
int EMC_SmdReader::FillBarrelSmd(Bank_SMDADCR* pADCR,int RDO)
{
 //fiber header
 //for(int i=0;i<128;i+=2) cout <<"Fiber header "<<i/2<<" = "<< pADCR->fiberHeader[i]<<"   "<<pADCR->fiberHeader[i+1]<<endl;

 //
 // Fiber data and fill the struct

  mTheSmdAdcR.EventNumber=pADCR->header.Token;     //Token number
  mTheSmdAdcR.PedFlag=0;                           //Pedestal subtracted (1) or not (0)
  mTheSmdAdcR.SMDErrorFlag=0;                      // Error from SMD (0=good)
  mTheSmdAdcR.TimeBin[RDO]=pADCR->fiberHeader[32]; // To be taken from header
  cout <<"SMD Time bin for RDO "<<RDO<<" = "<<mTheSmdAdcR.TimeBin[RDO]<<endl;

  //SMD_data
  //fiberdata
  for(int i=0;i<4800;i++)
  {
    int index=i;

    int det=0,mod=0,eta=0,sub=0;
    
    mTheSmdAdcR.SMDADCArray[RDO][i]=pADCR->fiberData[index];
    mTheSmdAdcR.BankType="BSMDADCR\n";
    if(pADCR->fiberData[index]>0)mTheSmdAdcR.NSmdHits++;
    int binstat=decoder->GetSmdCoord(RDO,index,det,mod,eta,sub);
    
    if(binstat)
    {
      //cout <<"RDO = "<<RDO<<"  index = "<<i<<"  det = "<<det<<"  m = "<<mod<<"  e = "<<eta<<"  s = "<<sub<<"  adc = "<<pADCR->fiberData[index]<<endl;
      mTheSmdAdcR.DetFlag=det; //Detector flag for BSMDE=3,BSMDP=4
      //SMDE
      if(det==3) mTheSmdAdcR.SmdE_ADCMatrix[mod-1][eta-1]=pADCR->fiberData[index];

      //SMDP
      if(det==4)
      {
        mTheSmdAdcR.SmdP_ADCMatrix[mod-1][eta-1][sub-1]=pADCR->fiberData[index];
      }
    }
  }
  return 1;
}
/////////////////////////////////////////////////////////////////////
int EMC_SmdReader::ProcessBarrelSmd(const Bank_EMCP* EmcPTR)
{
  // First Barrel Tower
  Bank_EMCSECP* barrelsmd=getBarrelSmdSection(EmcPTR,1);  // 1 is for SMD

  if(barrelsmd)
  {
    for(int RDO=0;RDO<8;RDO++)
    {
      //cout<<" taking SMDfiber RDO *** "<<RDO<<endl;
      Bank_EMCRBP* smdfiber=getBarrelSmdFiber(barrelsmd,RDO);
      if(smdfiber)
      {
        //cout<<"Got SMD fiber "<<endl;
        Bank_SMDADCR* smdadc=getSmdADC(smdfiber);

        if(smdadc)
        {
          int fillstat= FillBarrelSmd(smdadc,RDO);
          if(!fillstat) cout<<" Error on SMD Filling  "<<endl;
        }
        else
        {
	        cout<<" ADCR absent"<<endl;
        }
        if(smdadc)   smdadc=0;
        if(smdfiber) smdfiber=0;
      } 
    }
  }//barrelsmd
  else
  {
    cout<<" BANK_EMCSECP for SMD absent**"<<endl;
    return 0;
  }//barrelsmd
  return 1;
}
/////////////////////////////////////////////////////////////////////
Bank_BSMDADCR& EMC_SmdReader::getBSMDADCR()
{
  return mTheSmdAdcR;
}
