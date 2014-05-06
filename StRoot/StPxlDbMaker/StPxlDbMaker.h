/*!
 * \class StPxlDbMaker
 * \author J. Bouchet, M. Lomnitz, May 2013
 * \Initial Revision.
 */
/***************************************************************************
 *
 * $Id: StPxlDbMaker.h,v 1.9 2014/05/06 20:18:40 jeromel Exp $
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
 * Revision 1.9  2014/05/06 20:18:40  jeromel
 * Changed pxl_db to pxlDb as discussed with Hao
 *
 * Revision 1.8  2014/01/28 19:29:37  qiuh
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
   StPxlDbMaker(const char *name = "pxlDb");
   Int_t  InitRun(Int_t runNumber);

   virtual const char *GetCVS() const {
      static const char cvs[] = "Tag $Name:  $ $Id: StPxlDbMaker.h,v 1.9 2014/05/06 20:18:40 jeromel Exp $ built "__DATE__" "__TIME__ ;
      return cvs;
   }

private:
   StPxlDb *mPxlDb;

   ClassDef(StPxlDbMaker, 0)  //StAF chain virtual base class for Makers
};
#endif


