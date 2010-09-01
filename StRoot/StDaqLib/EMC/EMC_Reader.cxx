#include "EMC_Reader.hh"
#include <assert.h>
#include "EMC_BarrelReader.hh"
#include "EMC_SmdReader.hh"
#define MAX_ADC 0xFFF

using namespace OLDEVP;

void EMC_Reader::ProcessEvent(const Bank_EMCP * EmcPTR, const Bank_TRGP * TrgPTR)
{
    // will process the event to fill the arrays for different detectors.
    //Towers
    EMC_BarrelReader* barreltowerreader = new EMC_BarrelReader(ercpy, const_cast<Bank_EMCP *>(EmcPTR));
    int towerresult = barreltowerreader->ProcessBarrelTower(EmcPTR, TrgPTR);
    mTheTowerAdcR=barreltowerreader->getBTOWERADCR();
    if(!towerresult)
    {
        cout<<" Barrel TOWER processing is not successful**"<<endl;
        mTowerPresent=false;
    } // 0 is bad
    else
    {
        mTowerPresent=true;
    }

    delete barreltowerreader;
    barreltowerreader=0;

    if(EmcPTR) {
        //SMDs
        EMC_SmdReader* barrelsmdreader = new EMC_SmdReader(ercpy, const_cast<Bank_EMCP *>(EmcPTR));
        int smdresult = barrelsmdreader->ProcessBarrelSmd(EmcPTR);
        mTheSmdAdcR=barrelsmdreader->getBSMDADCR();

        if(!smdresult)
        {
            cout<<" Barrel SMD processing is not successful**"<<endl;
            mSmdPresent=false;
        } // 0 is bad
        else
        {
            mSmdPresent=true;
        }
        delete barrelsmdreader;
        barrelsmdreader=0;
    }
}

//EMC_Reader::~EMC_Reader(){}

EMC_Reader::EMC_Reader(EventReader *er, Bank_EMCP *pEMCP, Bank_TRGP *pTRGP)
{
  mSmdPresent=false;
  mTowerPresent=true;
    printf(" SP in EMC_Reader\n");
    pBankEMCP = pEMCP; //copy into class data member for use by other methods
    ercpy = er; // squirrel away pointer eventreader for our friends

    printf("This is the EMC_Reader ctor in %s.\n",__FILE__);

    if(pBankEMCP) {
        pBankEMCP->header.BankType[7]=0;
        cout<<"header bank type "<<pBankEMCP->header.BankType<<endl;

        if (!pBankEMCP->test_CRC())
            printf("CRC error in EMCP: %s %d\n",__FILE__,__LINE__) ;
        if (pBankEMCP->swap() < 0)
            printf("swap error in EMCP: %s %d\n",__FILE__,__LINE__) ;

        pBankEMCP->header.CRC = 0;
        int Token = pBankEMCP->header.Token;
            
        Bank_DATAP *dp = (Bank_DATAP *)ercpy->getDATAP();
        if(Token !=dp->header.Token)
            printf("Token mismatch between global %d and RICH %d\n",dp->header.Token,Token);
    }

    ProcessEvent(pBankEMCP, pTRGP);
}
int EMC_Reader::NTowerHits()
{
    if(mTowerPresent)
    {
        if(strncmp(mTheTowerAdcR.BankType,"TOWRADCR",8))
        {
            cout<<"EMC_Reader::NTowerHits() -> error in header"<<endl;
            return 0;
        }
        int nhits=mTheTowerAdcR.NTowerHits;
        return nhits;
    }
    else
        return 0;
}
int EMC_Reader::NSmdHits()
{
    if(mSmdPresent)
    {
        if(strncmp(mTheSmdAdcR.BankType,"BSMDADCR",8))
        {
            cout<<" error in header name**"<<endl;
            return 0;
        }
        int nhits=mTheSmdAdcR.NSmdHits;
        return nhits;
    }
    else
        return 0;
}

int EMC_Reader::getTowerADC(int module,int eta, int sub,unsigned short& ADC )
{
    //
    // module, eta and sub begins with 1
    // see StEmcTowerInput.cxx
    //
    if(mTowerPresent)
    {
        if(strncmp(mTheTowerAdcR.BankType,"TOWRADCR",8))
        {
            cout<<" error in header name**"<<endl;
            return 0;
        }
        else
        {
            ADC=mTheTowerAdcR.TowerMatrix[module-1][eta-1][sub-1];
            return 1;    // 1 is good
        }
    }
    else
        return 0; // 0 is bad
}

int EMC_Reader::getTowerADC(int index, unsigned short& ADC )
{
    //
    // index is daq index (0-4799)
    //
    if(mTowerPresent)
    {
        if(strncmp(mTheTowerAdcR.BankType,"TOWRADCR",8))
        {
            cout<<" error in header name**" << endl;
            return 0;
        }
        ADC = mTheTowerAdcR.TowerADCArray[index];
        return 1; // 1 is good
    }
    else
        return 0;
}

int EMC_Reader::getSMDE_ADC(int modInd,int stripInd,unsigned short& ADC )
{
    //
    // modInd and stripInd are indexes - 1-oct-2001
    // see StEmcSmdInput.cxx
    //
    if(mSmdPresent)
    {
        if(strncmp(mTheSmdAdcR.BankType,"BSMDADCR",8))
        {
            cout<<" getSMDE_ADC::error in header name**"<<endl;
            return 0;
        }
        else
        {
            ADC = mTheSmdAdcR.SmdE_ADCMatrix[modInd-1][stripInd-1];
            return 1;  // 1 is good
        }
    }
    else
        return 0;
}

int EMC_Reader::getSMDP_ADC(int modInd,int binInd,int stripInd,unsigned short& ADC )
{
    //
    // modInd, binInd and stripInd are indexes starting with 1- 1-oct-2001
    // see StEmcSmdInput.cxx
    //
    if(mSmdPresent)
    {
        if(strncmp(mTheSmdAdcR.BankType,"BSMDADCR",8))
        {
            cout<<" getSMDE_ADC::error in header name**"<<endl;
            return 0;
        }
        else
        {
            ADC=mTheSmdAdcR.SmdP_ADCMatrix[modInd-1][binInd-1][stripInd-1];
            return 1;  // 1 is good
        }
    }
    else
        return 0;
}

int EMC_Reader::getSMD_ADC(int index, int fiber,unsigned short& ADC )
{
    // index and fiber is index starting with 0
    if(mSmdPresent)
    {
        if(strncmp(mTheSmdAdcR.BankType,"BSMDADCR",8))
        {
            cout<<" getSMD_ADC::error in header name**"<<endl;
            return 0;
        }
        else
        {
            ADC=mTheSmdAdcR.SMDADCArray[fiber][index];
            return 1;   // 1 is good
        }
    }
    else
        return 0;
}

int EMC_Reader::getSMD_TIMEBIN(int fiber, unsigned int& TimeBin)
{
    if(mSmdPresent)
    {
        if(strncmp(mTheSmdAdcR.BankType,"BSMDADCR",8))
        {
            //cout<<" getSMDE_ADC::error in header name**"<<endl;
            TimeBin=999;
            return 0;
        }
        else
        {
            TimeBin=mTheSmdAdcR.TimeBin[fiber];
            return 1; // 1 is good
        }
    }
    else
        return 0;
}
bool EMC_Reader::isTowerPresent()
{
    return mTowerPresent;
}
bool EMC_Reader::isSmdPresent()
{
    return mSmdPresent;
}
Bank_BTOWERADCR& EMC_Reader::getBTOWERADCR()
{
    return mTheTowerAdcR;
}
Bank_BSMDADCR& EMC_Reader::getSMD_ADCR()
{
    return mTheSmdAdcR;
}
