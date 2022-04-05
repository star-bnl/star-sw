#include "EMC_SmdReader.hh"
#include "StMessMgr.h"
#include "TTimeStamp.h"
#include <assert.h>
#define MAX_ADC 0xFFF
#include <time.h>
#include <stdlib.h>
#include <Stiostream.h>
#include <stdio.h>

using namespace OLDEVP;

/////////////////////////////////////////////////////////////////////
EMC_SmdReader::EMC_SmdReader(EventReader* er,Bank_EMCP *pEMCP): pBankEMCP(pEMCP),ercpy(er)
{
    mNSMD = 8;

    EventInfo info=er->getEventInfo();
    TTimeStamp ts(info.UnixTime);
    UInt_t year,month,day,hour1,min,sec;
    ts.GetDate(1,0,&year,&month,&day);
    ts.GetTime(1,0,&hour1,&min,&sec);
    UInt_t date= day + 100*(month+100*year );  
    UInt_t hour= sec + 100*(min  +100*hour1);
    
    decoder = new StEmcDecoder(date,hour);

    if(date>20041201) mNSMD = 12;
    LOG_INFO<<"EMC_SMDreader** Event time (Unix time) = "<<info.UnixTime<<"  NSMD = "<<mNSMD<<endm;
    Initialize();
}
/////////////////////////////////////////////////////////////////////
EMC_SmdReader::~EMC_SmdReader()
{
    if(decoder)
        delete decoder;
}
/////////////////////////////////////////////////////////////////////

void EMC_SmdReader::Initialize()
{
    mTheSmdAdcR.NSmdHits = 0;
    for(int RDO=0;RDO<12;RDO++)
        mTheSmdAdcR.TimeBin[RDO]=999;
    for(int RDO=0;RDO<12;RDO++)
        mTheSmdAdcR.HasData[RDO]=0;

    // Initialize SMDDATA array to 0's
    for(int i = 0 ; i <120 ; i++)
    {
        for(int j = 0 ; j < 150; j++)
            mTheSmdAdcR.SmdE_ADCMatrix[i][j] = 0;    //SMD_eta
        for(int j = 0 ; j < 10; j++)
            for(int k = 0;k < 15; k++)
                mTheSmdAdcR.SmdP_ADCMatrix[i][j][k] = 0;  //SMD_Phi
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
        LOG_WARN<<" SmdReader::getBarrelSection** , no offset or length for section**"<<section<<endm;
        return NULL;
    }

    Bank_EMCSECP * ptr=(Bank_EMCSECP*)(((INT32 *)pBankEMCP)+pBankEMCP->EMCSecPointer[section].offset); // 2nd pointer is for SMD

    //put some checks
    if(strncmp(ptr->header.BankType,"EMCSECP",7))
    {
        char str0[40];
        LOG_WARN<<" error in header name**"<<endm;
        sprintf(str0,"getBarrelSection(hs %d)",section);
        ercpy->fprintError(ERR_BAD_HEADER,__FILE__,__LINE__, str0);
        return NULL;
    }
    if(!ptr->test_CRC())
    {
        char str0[40];
        LOG_WARN<<"error in CRC**"<<endm;
        sprintf(str0,"getBarrelsection(hs %d)",section);
        ercpy->fprintError(ERR_CRC,__FILE__,__LINE__,str0);
        return NULL;
    }
    printf("Byte order of header for EMCSECP Before swap**: %x\n",ptr->header.ByteOrder);
    if(ptr->swap() < 0)
    {
        char str0[40];
        LOG_WARN<<"error in swap**"<<endm;
        sprintf(str0,"getBarrelsection(hs %d)",section);
        ercpy->fprintError(ERR_SWAP,__FILE__,__LINE__,str0);
        return NULL;
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
        LOG_WARN<<" error in header name**"<<endm;
        sprintf(str0,"getBarrelSmdfiber(hs %d)",section);
        ercpy->fprintError(ERR_BAD_HEADER,__FILE__,__LINE__, str0);
        return NULL;
    }
    if(!ptr->test_CRC())
    {
        char str0[40];
        LOG_WARN<<"error in CRC**"<<endm;
        sprintf(str0,"getBarrelSmdfiber(hs %d)",section);
        ercpy->fprintError(ERR_CRC,__FILE__,__LINE__,str0);
        return NULL;
    }
    if(ptr->swap() < 0)
    {
        char str0[40];
        LOG_WARN<<"error in swap**"<<endm;
        sprintf(str0,"getBarrelSmdFiber(hs %d)",section);
        ercpy->fprintError(ERR_SWAP,__FILE__,__LINE__,str0);
        return NULL;
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
        LOG_WARN<<" getBarrelADC** , no offset or length for ADCR**"<<endm;
        return NULL;
    }
    Bank_SMDADCR* pADCR =(Bank_SMDADCR*)(((INT32 *)rbp)+rbp->EMCADCR.offset);

    // some checks and swap

    if(strncmp(pADCR->header.BankType,"EMCADCR",7))
    {
        char str0[40];
        LOG_WARN<<" error in header name**"<<endm;
        sprintf(str0,"getSmdADC(hs)");
        ercpy->fprintError(ERR_BAD_HEADER,__FILE__,__LINE__, str0);
        return NULL;
    }
    if(!pADCR->test_CRC())
    {
        char str0[40];
        LOG_WARN<<"error in CRC**"<<endm;
        sprintf(str0,"getSmdADC(hs)");
        ercpy->fprintError(ERR_CRC,__FILE__,__LINE__,str0);
        return NULL;
    }
    if(pADCR->swap() < 0)
    {
        char str0[40];
        LOG_WARN<<"error in swap**"<<endm;
        sprintf(str0,"getBankSVTSECP(hs)");
        ercpy->fprintError(ERR_SWAP,__FILE__,__LINE__,str0);
        return NULL;
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
    LOG_DEBUG <<"SMD Time bin for RDO "<<RDO<<" = "<<mTheSmdAdcR.TimeBin[RDO]<<endm;
    for(int i=0;i<128;i++)
        mTheSmdAdcR.SmdHeader[RDO][i]=pADCR->fiberHeader[i];
    int sum =0;
    //SMD_data
    //fiberdata
    for(int i=0;i<4800;i++)
    {
        int index=i;

        int det=0,mod=0,eta=0,sub=0;

        mTheSmdAdcR.SMDADCArray[RDO][i]=pADCR->fiberData[index];
        sum+=mTheSmdAdcR.SMDADCArray[RDO][i];
        mTheSmdAdcR.BankType="BSMDADCR\n";
        if(RDO<8) // these are the SMD. PSD has header >=8
        {
            if(pADCR->fiberData[index]>0)
                mTheSmdAdcR.NSmdHits++;
            int binstat=decoder->GetSmdCoord(RDO,index,det,mod,eta,sub);

            if(binstat)
            {
                //cout <<"RDO = "<<RDO<<"  index = "<<i<<"  det = "<<det<<"  m = "<<mod<<"  e = "<<eta<<"  s = "<<sub<<"  adc = "<<pADCR->fiberData[index]<<endl;
                mTheSmdAdcR.DetFlag=det; //Detector flag for BSMDE=3,BSMDP=4
                //SMDE
                if(det==3)
                    mTheSmdAdcR.SmdE_ADCMatrix[mod-1][eta-1]=pADCR->fiberData[index];
                //SMDP
                if(det==4)
                    mTheSmdAdcR.SmdP_ADCMatrix[mod-1][eta-1][sub-1]=pADCR->fiberData[index];
            }
        }
    }
    if(sum>0)
        mTheSmdAdcR.HasData[RDO] = 1;
    return 1;
}
/////////////////////////////////////////////////////////////////////
int EMC_SmdReader::ProcessBarrelSmd(const Bank_EMCP* EmcPTR)
{
    // First Barrel Tower
    Bank_EMCSECP* barrelsmd=getBarrelSmdSection(EmcPTR,1);  // 1 is for SMD

    if(barrelsmd)
    {
        for(int RDO=0;RDO<mNSMD;RDO++)
        {
            LOG_DEBUG<<" taking SMDfiber RDO *** "<<RDO<<endm;
            Bank_EMCRBP* smdfiber=getBarrelSmdFiber(barrelsmd,RDO);
            if(smdfiber)
            {
                //cout<<"Got SMD fiber "<<endl;
                Bank_SMDADCR* smdadc=getSmdADC(smdfiber);

                if(smdadc)
                {
                    int fillstat= FillBarrelSmd(smdadc,RDO);
                    if(!fillstat)
                        LOG_WARN<<" Error on SMD Filling  "<<endm;
                }
                else
                {
                    LOG_WARN<<" ADCR absent"<<endm;
                }
                if(smdadc)
                    smdadc=0;
                if(smdfiber)
                    smdfiber=0;
            }
        }
    }//barrelsmd
    else
    {
        LOG_WARN<<" BANK_EMCSECP for SMD absent**"<<endm;
        return 0;
    }//barrelsmd
    return 1;
}
/////////////////////////////////////////////////////////////////////
Bank_BSMDADCR& EMC_SmdReader::getBSMDADCR()
{
    return mTheSmdAdcR;
}
