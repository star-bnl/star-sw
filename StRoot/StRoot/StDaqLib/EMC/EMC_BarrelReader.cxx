#include "EMC_BarrelReader.hh"
#include <assert.h>
#define MAX_ADC 0xFFF
#include "Stiostream.h"
#include <time.h>
#include "TTimeStamp.h"
#include <stdlib.h>
#include <Stiostream.h>
#include <stdio.h>
#include "StMessMgr.h"
#include "RTS/include/rts.h" // swap16(), swap32()
//ofstream fout("decode.out");
using namespace OLDEVP;

EMC_BarrelReader::EMC_BarrelReader(EventReader *er,Bank_EMCP *pEMCP):pBankEMCP(pEMCP),ercpy(er)
{
    EventInfo info=er->getEventInfo();
    TTimeStamp ts(info.UnixTime);
    UInt_t year,month,day,hour1,min,sec;
    ts.GetDate(1,0,&year,&month,&day);
    ts.GetTime(1,0,&hour1,&min,&sec);
    UInt_t date= day + 100*(month+100*year );  
    UInt_t hour= sec + 100*(min  +100*hour1);

    decoder=new StEmcDecoder(date,hour);
    cout<<"EMC_Barrelreader** Event time (Unix time) = "<<info.UnixTime<<endl;
    Initialize();
}
EMC_BarrelReader::~EMC_BarrelReader()
{
    if(decoder)
        delete decoder;
}
////////////////////////////////////////////////////////////
void EMC_BarrelReader::Initialize()
{
    mTheTowerAdcR.NTowerHits=0;
    for(int i=0;i<4800;i++)
    {
        mTheTowerAdcR.TowerADCArray[i]=0;
        mTheTowerAdcD.TowerADCArray[i]=0;
        mTheTowerPedR.TowerADCArray[i]=0;
        mTheTowerRMSR.TowerADCArray[i]=0;
    }
    for(int i=0;i<120;i++)
    {
        mTheTowerAdcR.TDCHeader[i]=0;
        mTheTowerAdcD.TDCHeader[i]=0;
        mTheTowerPedR.TDCHeader[i]=0;
        mTheTowerRMSR.TDCHeader[i]=0;
    }

    // Initialize TOWERDATA array to 0's
    for(int i = 0 ; i <120 ; i++)
        for(int j = 0 ; j < 20; j++)
            for(int k = 0 ; k < 2; k++)
            {
                mTheTowerAdcR.TowerMatrix[i][j][k] = 0;
                mTheTowerAdcD.TowerMatrix[i][j][k] = 0;
                mTheTowerPedR.TowerMatrix[i][j][k] = 0;
                mTheTowerRMSR.TowerMatrix[i][j][k] = 0;
            }
}

Bank_EMCSECP* EMC_BarrelReader::getBarrelSection(const Bank_EMCP* pBankEMCP,int section)
{

    if((!pBankEMCP->EMCSecPointer[section].offset) || (!pBankEMCP->EMCSecPointer[section].length))
    {
        char str0[40];
        sprintf(str0,"getBarrelSection(hs %d)",section);
        ercpy->fprintError(INFO_MISSING_BANK,__FILE__,__LINE__,str0);
        cout<<" getBarrelSection** , no offset or length for section**"<<section<<endl;
        return NULL;
    }

    Bank_EMCSECP * ptr=(Bank_EMCSECP*)(((INT32 *)pBankEMCP)+pBankEMCP->EMCSecPointer[0].offset);

    //put some checks
    if(strncmp(ptr->header.BankType,"EMCSECP",7))
    {
        char str0[40];
        cout<<" error in header name**"<<endl;
        sprintf(str0,"getBarrelsection(hs )");
        ercpy->fprintError(ERR_BAD_HEADER,__FILE__,__LINE__, str0);
        return NULL;
    }
    if(!ptr->test_CRC())
    {
        char str0[40];
        cout<<"error in CRC**"<<endl;
        sprintf(str0,"getBarrelsection(hs )");
        ercpy->fprintError(ERR_CRC,__FILE__,__LINE__,str0);
        return NULL;
    }
    printf("Byte order of header for EMCSECP Before swap**: %x\n",ptr->header.ByteOrder);
    if(ptr->swap() < 0)
    {
        char str0[40];
        cout<<"error in swap**"<<endl;
        sprintf(str0,"getBarrelsection(hs)");
        ercpy->fprintError(ERR_SWAP,__FILE__,__LINE__,str0);
        return NULL;
    }

    ptr->header.CRC = 0;
    return ptr;
}
////////////////////////////////////////////////////////////////////
Bank_EMCRBP* EMC_BarrelReader::getBarrelTowerFiber(Bank_EMCSECP* secp,int section)
{
    if((!secp->FiberPointer[section].offset) || (!secp->FiberPointer[section].length))
    {
        char str0[40];
        sprintf(str0,"getBarrelTowerFiber(hs %d)",section);
        ercpy->fprintError(INFO_MISSING_BANK,__FILE__,__LINE__,str0);
        cout<<" getBarrelSection** , no offset or length for section**"<<section<<endl;
        return NULL;
    }

    Bank_EMCRBP* ptr =(Bank_EMCRBP*)(((INT32 *)secp)+secp->FiberPointer[section].offset);

    // some checks and swap

    if(strncmp(ptr->header.BankType,"EMCRBP",6))
    {
        char str0[40];
        cout<<" error in header name**"<<endl;
        sprintf(str0,"getBarrelTowerFiber(hs %d)",section);
        ercpy->fprintError(ERR_BAD_HEADER,__FILE__,__LINE__, str0);
        return NULL;
    }
    if(!ptr->test_CRC())
    {
        char str0[40];
        cout<<"error in CRC**"<<endl;
        sprintf(str0,"getBarrelTowerFiber(hs %d)",section);
        ercpy->fprintError(ERR_CRC,__FILE__,__LINE__,str0);
        return NULL;
    }
    if(ptr->swap() < 0)
    {
        char str0[40];
        cout<<"error in swap**"<<endl;
        sprintf(str0,"getBarrelTowerFiber(hs %d)",section);
        ercpy->fprintError(ERR_SWAP,__FILE__,__LINE__,str0);
        return NULL;
    }
    ptr->header.CRC = 0;
    return ptr;
}
////////////////////////////////////////////////////////////////////
Bank_TOWERADCR* EMC_BarrelReader::getBarrelADC(Bank_EMCRBP* rbp)
{
    if((!rbp->EMCADCR.offset) || (!rbp->EMCADCR.length))
    {
        char str0[40];
        sprintf(str0,"getBarrelADC(hs)");
        ercpy->fprintError(INFO_MISSING_BANK,__FILE__,__LINE__,str0);
        cout<<" getBarrelADC** , no offset or length for ADCR**"<<endl;
        return NULL;
    }
    Bank_TOWERADCR* pADCR =(Bank_TOWERADCR*)(((INT32 *)rbp)+rbp->EMCADCR.offset);

    // some checks and swap

    if(strncmp(pADCR->header.BankType,"EMCADCR",7))
    {
        char str0[40];
        cout<<" error in header name**"<<endl;
        sprintf(str0,"getBarrelADC(hs)");
        ercpy->fprintError(ERR_BAD_HEADER,__FILE__,__LINE__, str0);
        return NULL;
    }
    if(!pADCR->test_CRC())
    {
        char str0[40];
        cout<<"error in CRC**"<<endl;
        sprintf(str0,"getBarrelADC(hs)");
        ercpy->fprintError(ERR_CRC,__FILE__,__LINE__,str0);
        return NULL;
    }
    if(pADCR->swap() < 0)
    {
        char str0[40];
        cout<<"error in swap**"<<endl;
        sprintf(str0,"getBarrelADC(hs)");
        ercpy->fprintError(ERR_SWAP,__FILE__,__LINE__,str0);
        return NULL;
    }

    pADCR->header.CRC = 0;
    return pADCR;
}
////////////////////////////////////////////////////////////////////
int EMC_BarrelReader::FillBarrelTower(Bank_TOWERADCR* pADCR)
{
    mTheTowerAdcR.BankType="TOWRADCR\n";
    mTheTowerAdcR.DetFlag=1;                          // Detector flag for BEMC=1,
    mTheTowerAdcR.EventNumber=pADCR->header.Token;    // Token number
    mTheTowerAdcR.PedFlag=0;                          // Pedestal subtracted (1) or not (0)
    mTheTowerAdcR.TDCErrorFlag=1;                     // Error from TDC (0=good)
    mTheTowerAdcR.NTDCChannels=0;                     // Number of valid TDC channels

    for(int TDC=0;TDC<120;TDC++)
        mTheTowerAdcR.TDCHeader[TDC] = pADCR->TDCHeader[TDC];

    for(int TDC=0;TDC<30;TDC++)  // filling TDC channels information
    {
        mTheTowerAdcR.TDCCount[TDC]=pADCR->TDCHeader[TDC];
        mTheTowerAdcR.TDCError[TDC]=pADCR->TDCHeader[TDC+30];
        mTheTowerAdcR.TDCToken[TDC]=pADCR->TDCHeader[TDC+60];
        mTheTowerAdcR.TDCTrigger[TDC]=(pADCR->TDCHeader[TDC+90] & 0xF00) >> 8;
        mTheTowerAdcR.TDCCrateId[TDC]=(pADCR->TDCHeader[TDC+90] & 0x0FF);
        /*cout <<"  TDC channel "<<TDC
             <<"  Byte count = "<<mTheTowerAdcR.TDCCount[TDC]
             <<"  Error = "<<mTheTowerAdcR.TDCError[TDC]
             <<"  Token = "<<mTheTowerAdcR.TDCToken[TDC]
             <<"  Trigger = "<<mTheTowerAdcR.TDCTrigger[TDC]
             <<"  CrateId = "<<mTheTowerAdcR.TDCCrateId[TDC]<<endl;*/
        int CId = 0;
        decoder->GetTowerCrateFromTDC(TDC,CId);

        //if(mTheTowerAdcR.TDCError[TDC]==0      &&
        //   mTheTowerAdcR.TDCCount[TDC]==164    &&
        //   mTheTowerAdcR.TDCCrateId[TDC]==CId)
        {
            mTheTowerAdcR.TDCErrorFlag=0;                     // at least one TDC channel is Ok
            mTheTowerAdcR.NTDCChannels++;                     // Number of valid TDC channels

            for(int i=0;i<160;i++) // loop over data in this TDC channel if there is no error
            {
                int index=i*30+TDC;

                mTheTowerAdcR.TDCData[TDC][i]=pADCR->fiberData[index];

                //Fill towerrawdata struct
                mTheTowerAdcR.TowerADCArray[index]=pADCR->fiberData[index];

                //Total number of towers for valid TDC channels
                mTheTowerAdcR.NTowerHits++;

                int index_jose=-1;
                int stat_index=decoder->GetTowerIdFromDaqId(index,index_jose);
                // Index-jose runs here from 1 to 4800
                //cout <<"index "<<index<<" index_jose "<<index_jose<<" stat_index "<<stat_index<<endl;

                int m=0,e=0,s=0;
                if(index_jose!=-1)
                {
                    if(stat_index)
                    {
                        int binstat=decoder->GetTowerBin(index_jose,m,e,s);
                        //LOG_DEBUG <<"index = "<<index<<"  soft = "<<index_jose<<"  module = "<<m<<"  eta = "<<e<<"  sub = "<<s<<" adc = "<<pADCR->fiberData[index]<<endm;

                        if(!binstat)
                            cout<<" problem in bin conversion "<<index<<endl;
                        else
                            mTheTowerAdcR.TowerMatrix[m-1][e-1][s-1]=pADCR->fiberData[index];
                    }//stat_index
                }
            }
        }
    }
    
    return 1;
}
////////////////////////////////////////////////////////////////////
int EMC_BarrelReader::ProcessBarrelTower(const Bank_EMCP* EmcPTR, const Bank_TRGP* TrgPTR)
{
  // first check if tower data is in trgp
  if (TrgPTR) {
    bool swap = TrgPTR->header.ByteOrder != 0x04030201;
    int offset = TrgPTR->theData.offset;
    if (swap) offset = swap32(offset);
    char* cTTT = (char*)TrgPTR + offset * 4 + sizeof(TrgPTR->header);
    if (cTTT) {
      TrgTowerTrnfer2008* TTT = (TrgTowerTrnfer2008*)cTTT;
      int byteCount_Version = TTT->byteCount_Version;
      if (swap) byteCount_Version = swap32(TTT->byteCount_Version);
      if ((byteCount_Version & 0xff) == 0x10) {
	int offset = TTT->OffsetBlock[y8TRG_INDEX].offset;
	if (swap) offset = swap32(offset);
	char trg_version = cTTT[offset + 3];
	if (trg_version == 0x32) {
	  int offset = TTT->OffsetBlock[y8BTOW_INDEX].offset;
	  int length = TTT->OffsetBlock[y8BTOW_INDEX].length;

	  if (swap) {
	    offset = swap32(offset);
	    length = swap32(length);
	  }

	  if (length != y8BTOW_LEN) {
	    LOG_ERROR << "Bad BTOW length found in TrgTowerTrnfer block" << endm;
	    return 0;
	  }

	  Bank_TOWERADCR* fakeADCRptr = (Bank_TOWERADCR*)((char*)TTT + offset - sizeof(Bank_Header));

	  return FillBarrelTower(fakeADCRptr);
	}
      }
    }
  }

  if (!EmcPTR) return 0;

  // now look in Bank_EMCP
  Bank_EMCSECP* barreltower=getBarrelSection(EmcPTR,0);
  if(barreltower) {
    Bank_EMCRBP* towerfiber=getBarrelTowerFiber(barreltower,0);
    if(towerfiber) {
      Bank_TOWERADCR* toweradc=getBarrelADC(towerfiber);

      if(toweradc) { 
	FillBarrelTower(toweradc); 
	return 1;
      }
      else { LOG_INFO <<" ADCR absent , looking for ADCD"<<endm; }

      toweradc=0;
    }
    else { LOG_INFO <<" BANK_EMRBP absent**"<<endm; }
  }
  else { LOG_INFO <<" BANK_EMCSECP absent**"<<endm; }
    
  return 0; // if we got here we couldn't find tower data
}
////////////////////////////////////////////////////////////////////
void EMC_BarrelReader::PrintTowerArray()
{

    cout<<"BankType **"<<mTheTowerAdcR.BankType<<endl;
    cout<<"Det flag **"<<mTheTowerAdcR.DetFlag<<endl;
    cout<<"Event no**"<<mTheTowerAdcR.EventNumber<<endl;
    cout<<"Pedflag**"<<mTheTowerAdcR.PedFlag<<endl;
    cout<<"TDCERR **"<<mTheTowerAdcR.TDCErrorFlag<<endl;
    for(int i=0;i<120;i++)
        for(int j=0;j<20;j++)
            for(int k=0;k<2;k++)
                if(mTheTowerAdcR.TowerMatrix[i][j][k]>0)
                    cout<<"ADCDATA** mod"<<i+1<<"eta "<<j+1<<"phi "<<k+1<<"ADC "<<mTheTowerAdcR.TowerMatrix[i][j][k]<<endl;
}
////////////////////////////////////////////////////////////////////
Bank_BTOWERADCR& EMC_BarrelReader::getBTOWERADCR()
{
    return mTheTowerAdcR;
}
