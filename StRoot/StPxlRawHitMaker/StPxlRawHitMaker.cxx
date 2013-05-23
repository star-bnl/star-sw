/*!
 * \class StPxlRawHitMaker 
 * \author Jan Rusnak, Qiu Hao, Jan 2013, according codes from Xiangming Sun
 */
/***************************************************************************
 * 
 * $Id: StPxlRawHitMaker.cxx,v 1.1 2013/05/23 20:57:30 qiuh Exp $
 *
 * Author: Jan Rusnak, Qiu Hao, Jan 2013, according codes from Xiangming Sun 
 ***************************************************************************
 *
 * Description:
 * Read pixel raw hits from daq format. One raw hit is one fired pixel.
 *
 ***************************************************************************
 *
 * $Log: StPxlRawHitMaker.cxx,v $
 * Revision 1.1  2013/05/23 20:57:30  qiuh
 * *** empty log message ***
 * 
 **************************************************************************/ 

#include "StPxlRawHitMaker.h"
#include "StEventTypes.h"
#include "StPxlUtil/StPxlRawHit.h"
#include "StPxlUtil/StPxlRawHitCollection.h"
#include "StMessMgr.h"
#include "StRtsTable.h"  

ClassImp(StPxlRawHitMaker)
    
#define HEADER_LENGTH 16
#define HARDWARE_ID 6

#define HEADER_TOKEN 0xaaaaaaaa
#define SEPARATOR_TOKEN 0xcccccccc
#define END_TOKEN 0xbbbbbbbb

#define CHIP_ID_START 13
#define CHIP_ID_END 16
#define CHIP_ID_POW 8
#define OVF_POS 1
#define RAW_FLAG_POS 12
#define CODING_START 0
#define CODING_END 2
#define DATA_START 2
#define DATA_END 12

StPxlRawHitMaker::StPxlRawHitMaker(const Char_t *name) : StRTSBaseMaker("pxl",name)
{
    //    SetDebug(3);
}
//_______________________________________________
Int_t StPxlRawHitMaker::Init()
{
    LOG_INFO<<"StPxlRawHitMaker::Init()"<<endm;
    m_pxlRawHitCollection = 0;
    return kStOk;
}
//_______________________________________________
Int_t StPxlRawHitMaker::Make()
{
    LOG_INFO<<"StPxlRawHitMaker::Make()"<<endm;
    
    //prepare output data collection
    TObjectSet* pxlRawHitDataSet = new TObjectSet("pxlRawHit");
    m_DataSet = pxlRawHitDataSet;
    m_pxlRawHitCollection = new StPxlRawHitCollection();
    pxlRawHitDataSet->AddObject(m_pxlRawHitCollection);

    //input data loop
    StRtsTable *rts_table = 0;
    while ( rts_table = GetNextDaqElement("pxl/raw") ) {
        m_sectorData = (UInt_t*)rts_table->At(0);
        m_sectorDataLength = rts_table->GetNRows();

        if(Debug()>1)
            {
                LOG_INFO<< "Found pxl sector raw data at "<<hex<<m_sectorData<<dec<<"    length in byte: "<<m_sectorDataLength<<"   in UInt_t: "<<m_sectorDataLength/sizeof(UInt_t)<<endm;
            }
        
        DecodeSectorData();
        
        DecodeHitsData();
      }

    return kStOk;
}
//_______________________________________________
void StPxlRawHitMaker::DecodeSectorData()
{
    int index = 0;
    m_sector = 0;
    m_headerData = 0;
    m_hitsData = 0;
    m_hitsDataLength = 0;
    m_trailerData = 0;
    m_trailerDataLength = 0;
    
    if(m_sectorDataLength==0 || !m_sectorData) {
        LOG_WARN<<"no sector data"<<endm;
        return;
    }
    
    //header
    if(m_sectorData[0]!=HEADER_TOKEN){
        LOG_WARN<<"no pxl sector HEADER_TOKEN"<<endm;
        return;
    }
    else{
        if(Debug()>1)
            {
                LOG_INFO<<"sector data header token correct: 0x"<<hex<<m_sectorData[0]<<dec<<endm;
            }    
    }
    
    m_headerData = m_sectorData + index;
    index += HEADER_LENGTH;
    
    m_sector = m_headerData[HARDWARE_ID]&0xf;
    if(m_sector > nPxlSectors || m_sector < 1)
        {
            LOG_WARN<<"wrong sector number: "<<m_sector<<endm;
            return;
        }

    //hits data length
    m_hitsDataLength=GetHitsDataLength();
    if(Debug()>1)
        {
            LOG_INFO<<"sector: "<<m_sector<<"     HitsDataLength: "<<m_hitsDataLength<<endm;
        }
    index ++;
    
    //hits data
    m_hitsData=m_sectorData+index;
    
    index+=m_hitsDataLength;
    
    //separater
    if(m_sectorData[index] == SEPARATOR_TOKEN)
        {
            if(Debug()>1)
                {
                    LOG_INFO<<"sector data separator token correct: 0x"<<hex<<m_sectorData[index]<<dec<<endm;
                }
        }
    else
        {
            LOG_WARN<<"sector data separator token wrong: 0x"<<hex<<m_sectorData[index]<<dec<<endm;
            m_hitsData = 0;
            m_hitsDataLength = 0;
            return;
        }
    index++;

    //end
    if(m_sectorData[m_sectorDataLength/sizeof(UInt_t)-1] == END_TOKEN)
        {
            if(Debug()>1)
                {
                    LOG_INFO<<"sector data end token corret: 0x"<<hex<<m_sectorData[m_sectorDataLength/sizeof(UInt_t)-1]<<dec<<endm;
                }
        }
    else
        {
            LOG_WARN<<"sector data end token wrong: 0x"<<hex<<m_sectorData[m_sectorDataLength/sizeof(UInt_t)-1]<<dec<<endm;
            return;
        }

    m_trailerData=m_sectorData+index;
    m_trailerDataLength=m_sectorDataLength/sizeof(UInt_t)-1-index;

}
//_______________________________________________
Int_t StPxlRawHitMaker::GetHitsDataLength()
{
    UInt_t a=0;
    a=*(m_headerData+HEADER_LENGTH);
    return (a>>16)+(a&0xffff);
}
//_______________________________________________
void StPxlRawHitMaker::DecodeHitsData()
{
    if(m_hitsDataLength==0 || !m_hitsData) {
        LOG_WARN<<"no hits data"<<endm;
        return;
    }

    m_overFlowCount = 0;
    for(Int_t i=0;i<m_hitsDataLength;i++){
	DecodeWord(m_hitsData[i]);
    }
    if(Debug()>1)
        {
            LOG_INFO<<"sector: "<<m_sector<<"   overflow count: "<<m_overFlowCount<<endm;
        }
}
//_______________________________________________
void StPxlRawHitMaker::DecodeWord(UInt_t val)
{
    
    int val1=val>>16;
    int val0=val&(0xffff);  //from UInt_t to Int_t
    
    int chip_id=Mid(CHIP_ID_START,CHIP_ID_END,val0)+Mid(CHIP_ID_START,CHIP_ID_END,val1)*CHIP_ID_POW;
    if(chip_id < 1 || chip_id > nPxlLaddersPerSector*nPxlSensorsPerLadder)
        {
            LOG_WARN<<"wrong chip id: "<<chip_id<<endm;
            return;
        }
    m_ladder = (chip_id-1)/10+1;
    m_sensor = (chip_id-1)%10+1;
    int row_flag0 = ElementGetBit(val0,RAW_FLAG_POS);
    int row_flag1 = ElementGetBit(val1,RAW_FLAG_POS);
    
    if(row_flag0==1){
	DecodeState0(val0);
    }else{
	DecodeStateN(val0);
    }
    
    if(row_flag1==1){
	DecodeState0(val1);
    }else{
	DecodeStateN(val1);
    }
    
}
//_______________________________________________
UInt_t StPxlRawHitMaker::Mid(Int_t start,Int_t end, UInt_t input)
{
    UInt_t buf;
    buf=input<<(32-end);
    buf=buf>>(32-end);
    return buf>>start;
}
//_______________________________________________
Int_t StPxlRawHitMaker::ElementGetBit(UInt_t data,Int_t position)
{
    UInt_t sd=data>>position;
    return sd%2;
}
//_______________________________________________
Int_t StPxlRawHitMaker::DecodeState0(Int_t val)
{
    m_row=Mid(DATA_START,DATA_END,val);
    if(ElementGetBit(val,OVF_POS))
        {
            if(Debug()>2) LOG_WARN<<"pxl overflow at sector: "<<m_sector<<" ladder: "<<m_ladder<<" sensor: "<<m_sensor<<" row: "<<m_row<<endm;
            m_overFlowCount++;
        }
    if(m_row>=nPxlRowsOnSensor && m_row<0)
        {
            LOG_WARN<<"wrong row: "<<m_row<<" at sector: "<<m_sector<<" ladder: "<<m_ladder<<" sensor: "<<m_sensor<<endm;
        }
    return 0;
}
//_______________________________________________
Int_t StPxlRawHitMaker::DecodeStateN(Int_t val)
{
    int coding=Mid(CODING_START,CODING_END,val);
    m_column=Mid(DATA_START,DATA_END,val);
    for(int c=0;c<coding+1;c++)
        {
            if(m_sector>0 && m_sector<=nPxlSectors && m_ladder>0 && m_ladder<=nPxlLaddersPerSector && m_sensor>0 && m_sensor<=nPxlSensorsPerLadder && m_row>=0 && m_row<nPxlRowsOnSensor && m_column+c<nPxlColumnsOnSensor && m_column+c>=0) 
                {
                    
                    StPxlRawHit *pxlRawHit = new StPxlRawHit();
                    
                    pxlRawHit->setSector(m_sector);
                    pxlRawHit->setLadder(m_ladder);
                    pxlRawHit->setSensor(m_sensor);
                    pxlRawHit->setRow(m_row);
                    pxlRawHit->setColumn(m_column+c);
                    pxlRawHit->setIdTruth(0);
                    if(Debug()>4) pxlRawHit->print();
                                       
                    m_pxlRawHitCollection->pxlRawHitVec[m_sector-1][m_ladder-1][m_sensor-1].push_back(pxlRawHit);

                }
            else if(m_column != 1023) //1023: dummy state when the last state from a sensor ends on the lower 16 bits of a 32-bit word
                {
                    LOG_WARN<<"wrong senctor/ladder/sensor/row/column: "<<m_sector<<"/"<<m_ladder<<"/"<<m_sensor<<"/"<<m_row<<"/"<<m_column<<endm;
                }
    
        }
    return 0;
}

