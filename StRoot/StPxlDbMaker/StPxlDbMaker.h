/*!
 * \class StPxlDbMaker
 * \author J. Bouchet, M. Lomnitz, May 2013
 * \Initial Revision.
 */
/***************************************************************************
 *
 * $Id: StPxlDbMaker.h,v 1.6 2014/01/27 02:37:11 qiuh Exp $
 *
 * Author: J. Bouchet, M. Lomnitz, May 2013
 ***************************************************************************
 *
 * Description:
 * Read DB and prepare information on pxl geometry and sensor/row/column status
 * More information at
 * https://www.star.bnl.gov/protected/heavy/qiuh/HFT/software/PXL_software.pdf
 *
 ***************************************************************************
 *
 * $Log: StPxlDbMaker.h,v $
 * Revision 1.6  2014/01/27 02:37:11  qiuh
 * *** empty log message ***
 *
 *
 **************************************************************************/

#ifndef STPXLDBMAKER_H
#define STPXLDBMAKER_H

#include "StMaker.h"

class StPxlDb;

class StPxlDbMaker : public StMaker
{
public:
   StPxlDbMaker(const char *name = "pxl_db");
   Int_t  InitRun(Int_t runNumber);

   virtual const char *GetCVS() const {
      static const char cvs[] = "Tag $Name:  $ $Id: StPxlDbMaker.h,v 1.6 2014/01/27 02:37:11 qiuh Exp $ built "__DATE__" "__TIME__ ;
      return cvs;
   }

private:
   StPxlDb *mPxlDb;

   ClassDef(StPxlDbMaker, 0)  //StAF chain virtual base class for Makers
};
#endif


