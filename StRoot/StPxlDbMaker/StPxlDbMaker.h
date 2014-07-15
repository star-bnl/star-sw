/*!
 * \class StPxlDbMaker
 * \author J. Bouchet, M. Lomnitz, May 2013
 * \Initial Revision.
 */
/***************************************************************************
 *
 * $Id: StPxlDbMaker.h,v 1.10 2014/07/15 23:28:34 smirnovd Exp $
 *
 * Author: J. Bouchet, M. Lomnitz, May 2013
 *
 ***************************************************************************
 *
 * $Log: StPxlDbMaker.h,v $
 * Revision 1.10  2014/07/15 23:28:34  smirnovd
 * .msg
 *
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


/*!
 * This maker retrieves data from the PXL detector survey position measurements,
 * pixel/channel status, and other run time information via the standard STAR
 * database interface. A data structure of type StPxlDb is filled with the
 * corresponding values.
 *
 * More information about the PXL software packages and organization can be
 * found at
 * https://www.star.bnl.gov/protected/heavy/qiuh/HFT/software/PXL_software.pdf
 */
class StPxlDbMaker : public StMaker
{
public:
   StPxlDbMaker(const char *name = "pxlDb");
   Int_t  InitRun(Int_t runNumber);

   virtual const char *GetCVS() const {
      static const char cvs[] = "Tag $Name:  $ $Id: StPxlDbMaker.h,v 1.10 2014/07/15 23:28:34 smirnovd Exp $ built "__DATE__" "__TIME__ ;
      return cvs;
   }

private:
   StPxlDb *mPxlDb; ///< See StPxlDb for details on created data structure. The ownership is passed to the STAR framework via ToWhiteBoard()

   ClassDef(StPxlDbMaker, 0)  //StAF chain virtual base class for Makers
};
#endif


