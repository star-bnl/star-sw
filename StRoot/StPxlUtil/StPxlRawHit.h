/*!
 * \class StPxlRawHit 
 * \author Qiu Hao, Jan 2013
 */
/***************************************************************************
 * 
 * $Id: StPxlRawHit.h,v 1.1 2013/05/23 20:57:33 qiuh Exp $
 *
 * Author: Qiu Hao, Jan 2013 
 ***************************************************************************
 *
 * Description:
 * pixel raw hit before clustering. One raw hit is one fired pixel.
 *
 ***************************************************************************
 *
 * $Log: StPxlRawHit.h,v $
 * Revision 1.1  2013/05/23 20:57:33  qiuh
 * *** empty log message ***
 * 
 **************************************************************************/ 

#ifndef StPxlRawHit_hh
#define StPxlRawHit_hh

#include "Rtypes.h"
#include "TObject.h"

class StPxlRawHit: public TObject
{
public:
    StPxlRawHit();
    StPxlRawHit(int sector, int ladder, int sensor, int row, int column, int idTruth);
    ~StPxlRawHit();

    int  sector() const;
    int  ladder() const;
    int  sensor() const;
    int  row() const;
    int  column() const;
    int  idTruth() const; //!< for embedding, 0 as background

    void setSector(int sector);
    void setLadder(int ladder);
    void setSensor(int sensor);
    void setRow(int row);
    void setColumn(int column);
    void setIdTruth(int idTruth);

    void print(); //!< print all information

protected:
    Char_t mSector; //!< 1-10
    Char_t mLadder; //!< 1-4
    Char_t mSensor; //!< 1-10
    Short_t mRow;
    Short_t mColumn;
    UShort_t mIdTruth; //!< for embedding, 0 as background

    ClassDef(StPxlRawHit, 1)
};
#endif
