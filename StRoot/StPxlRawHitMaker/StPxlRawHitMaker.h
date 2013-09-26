/*!
 * \class StPxlRawHitMaker 
 * \author Jan Rusnak, Qiu Hao, Jan 2013, according codes from Xiangming Sun
 */
/***************************************************************************
 * 
 * $Id: StPxlRawHitMaker.h,v 1.2 2013/09/26 14:58:49 qiuh Exp $
 *
 * Author: Jan Rusnak, Qiu Hao, Jan 2013, according codes from Xiangming Sun 
 ***************************************************************************
 *
 * Description:
 * Read pixel raw hits from daq format. One raw hit is one fired pixel.
 *
 ***************************************************************************
 *
 * $Log: StPxlRawHitMaker.h,v $
 * Revision 1.2  2013/09/26 14:58:49  qiuh
 * *** empty log message ***
 *
 * Revision 1.1  2013/05/23 20:57:30  qiuh
 * *** empty log message ***
 * 
 **************************************************************************/ 

#ifndef STAR_StPxlRawHitMaker
#define STAR_StPxlRawHitMaker

#include "StMaker.h"
#include "StRTSBaseMaker.h"
#include "StPxlUtil/StPxlConstants.h"
class StPxlRawHitCollection;
class St_pxlSensorStatus;
class St_pxlSensorRowColumnStatus;
class StPxlRawHitMaker : public StRTSBaseMaker {
public:
    StPxlRawHitMaker(const char *name="pxl_raw_hit");
    virtual Int_t Init();
    virtual Int_t InitRun(Int_t runumber);
    virtual Int_t Make();
    void DecodeSectorData();
    Int_t GetHitsDataLength();    
    void DecodeHitsData();
    void DecodeWord(UInt_t val);
    UInt_t Mid(Int_t start, Int_t end, UInt_t input);
    Int_t ElementGetBit(UInt_t data, Int_t position);
    Int_t DecodeState0(Int_t val);
    Int_t DecodeStateN(Int_t val);

 protected:
    UInt_t* m_sectorData;
    Int_t m_sectorDataLength;
    UInt_t* m_headerData;
    UInt_t* m_hitsData;
    Int_t m_hitsDataLength;
    UInt_t* m_trailerData;
    Int_t m_trailerDataLength;
    
    Int_t m_overFlowCount;

    Int_t m_sector;
    Int_t m_ladder;
    Int_t m_sensor;
    Int_t m_row;
    Int_t m_column;

    Int_t sensorStatus[400];
    std::map<unsigned int,short> mapRow;
    std::map<unsigned int,short>::const_iterator gotRow;
    std::map<unsigned int,short> mapCol;
    std::map<unsigned int,short>::const_iterator gotCol;
    // row_id = 928*sensor_id + k
    // col_id = 960*sensor_id + k
    // sensor_id = (sector)*40 + ladder*10 + sensor
    // with :
    // 0<= sector <10
    // 0<= ladder < 4
    // 0<= sensor <10

    Bool_t doCalibRowColumn;
    Bool_t doCalibSensor;
    StPxlRawHitCollection* m_pxlRawHitCollection;
    ClassDef(StPxlRawHitMaker,1)   //StAF chain virtual base class for Makers
};

#endif

