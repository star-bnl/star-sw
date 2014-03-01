/***************************************************************************
*
* $Id: StIstDigiHit.h,v 1.2 2014/03/01 00:19:38 ypwang Exp $
*
* Author: Yaping Wang (Thank Dmitri Smirnov's updates)
****************************************************************************
* Description: 
* Data structure for additional function of StIstHit..
****************************************************************************
*
* $Log: StIstDigiHit.h,v $
* Revision 1.2  2014/03/01 00:19:38  ypwang
* correct return value of getMeanRow() and Log added
*
*
*
****************************************************************************
* StIstDigiHit.h,v 1.0
* Revision 1.0 2014/02/25 21:00:00 Yaping
* Initial version
****************************************************************************/


#ifndef StIstDigiHit_h
#define StIstDigiHit_h

#include "StIstUtil/StIstConsts.h"
#include "StEvent/StIstHit.h"


class StIstDigiHit: public StIstHit
{
public:
   StIstDigiHit();
   StIstDigiHit(const StIstHit& istHit);

   void setApv(unsigned char apvId);
   void setMeanColumn(float meanColumn);
   void setMeanRow(float meanRow);
   void setClusterSizeFlag(bool flag);

   unsigned char getApv() const;
   float getMeanColumn() const;
   float getMeanRow() const;
   bool  getClusterSizeFlag() const;
   float localPositionErr(unsigned int i) const;

private:
   UChar_t mApv;
   Float_t mMeanColumn;
   Float_t mMeanRow;
   Bool_t  mClusterSizeFlag; //0: cluster Size in r-phi < 2; 1: cluster Size in Z
};

#endif
