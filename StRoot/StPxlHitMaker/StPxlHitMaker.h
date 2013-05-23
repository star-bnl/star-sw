/*!
 * \class StPxlHitMaker 
 * \author Qiu Hao, Jan 2013
 */
/***************************************************************************
 * 
 * $Id: StPxlHitMaker.h,v 1.1 2013/05/23 20:57:24 qiuh Exp $
 *
 * Author: Qiu Hao, Jan 2013 
 ***************************************************************************
 *
 * Description:
 * Create pxl hits according to clusters and calculate their global position.
 *
 ***************************************************************************
 *
 * $Log: StPxlHitMaker.h,v $
 * Revision 1.1  2013/05/23 20:57:24  qiuh
 * *** empty log message ***
 * 
 **************************************************************************/ 

#ifndef StPxlHitMaker_hh
#define StPxlHitMaker_hh

#include "StMaker.h"
#include "StPxlUtil/StPxlConstants.h"

class TGeoHMatrix;
class Tps;

class StPxlHitMaker : public StMaker {
public:
    StPxlHitMaker(const char *name="pxl_hit");
    virtual ~StPxlHitMaker();
    
    virtual Int_t Init();
    virtual Int_t InitRun(Int_t runnumber);
    virtual Int_t Make();
    virtual const Char_t *GetCVS() const {
        static const char cvs[]="Tag $Name:  $ $Id: StPxlHitMaker.h,v 1.1 2013/05/23 20:57:24 qiuh Exp $ built "__DATE__" "__TIME__;
        return cvs;
    }
protected:
    THashList *listGeoMSensorOnGlobal;

    Tps* tps[nPxlSectors][nPxlLaddersPerSector][nPxlSensorsPerLadder];
    
private:
    
    ClassDef(StPxlHitMaker,0)
};

#endif
