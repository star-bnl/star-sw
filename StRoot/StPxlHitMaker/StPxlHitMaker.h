/*!
 * \class StPxlHitMaker 
 * \author Qiu Hao, Jan 2013
 * \Initial Revision.
 */
/***************************************************************************
 * 
 * $Id: StPxlHitMaker.h,v 1.2 2014/01/23 01:04:53 qiuh Exp $
 *
 * Author: Qiu Hao, Jan 2013 
 ***************************************************************************
 *
 * Description:
 * Create pxl hits according to clusters and calculate pxl hit global positions.
 * More information at
 * https://www.star.bnl.gov/protected/heavy/qiuh/HFT/software/PXL_software.pdf
 * 
 ***************************************************************************
 *
 * $Log: StPxlHitMaker.h,v $
 * Revision 1.2  2014/01/23 01:04:53  qiuh
 * *** empty log message ***
 *
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
    ~StPxlHitMaker();
    
    Int_t Init();
    Int_t InitRun(Int_t runnumber);
    Int_t Make();
    virtual const char *GetCVS() const {
        static const char cvs[]="Tag $Name:  $ $Id: StPxlHitMaker.h,v 1.2 2014/01/23 01:04:53 qiuh Exp $ built "__DATE__" "__TIME__ ;
        return cvs;
    }

protected:
    Tps* mTps[nPxlSectors][nPxlLaddersPerSector][nPxlSensorsPerLadder]; ///< pointers to Thin Plate Spline functions for sensors 

    Float_t mPixelSize; ///< size of a pxiel
    
    ClassDef(StPxlHitMaker,0)
};

#endif
