/*!
 * \class StPxlDbMaker
 * \author J. Bouchet, M. Lomnitz, May 2013
 * \Initial Revision.
 */
/***************************************************************************
 *
 * $Id: StPxlDbMaker.h,v 1.15 2015/02/04 07:55:41 smirnovd Exp $
 *
 * Author: J. Bouchet, M. Lomnitz, May 2013
 *
 ***************************************************************************
 *
 * $Log: StPxlDbMaker.h,v $
 * Revision 1.15  2015/02/04 07:55:41  smirnovd
 * Create StPxlDb object in constructor and pass it to the framework via ToWhiteConst() in Init()
 *
 * It makes perfect sense to do it this way because the StPxlDb obect is created
 * once by the maker and later reused/updated only at every new run.
 *
 * Revision 1.14  2014/11/19 18:29:47  genevb
 * Use flags to indicate DbMaker readiness
 *
 * Revision 1.13  2014/08/27 16:52:14  qiuh
 * change pxlRowColumnStatus to pxlBadRowColumns to decrease DB szie
 *
 * Revision 1.12  2014/08/06 11:43:34  jeromel
 * Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
 *
 * Revision 1.11  2014/07/15 23:28:48  smirnovd
 * Minor style changes
 *
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
   virtual Int_t Init();
   Int_t  InitRun(Int_t runNumber);
   Int_t  Make();

   virtual const char *GetCVS() const {
      static const char cvs[] = "Tag $Name:  $ $Id: StPxlDbMaker.h,v 1.15 2015/02/04 07:55:41 smirnovd Exp $ built " __DATE__ " " __TIME__ ;
      return cvs;
   }

private:
   StPxlDb *mPxlDb; ///< See StPxlDb for details on created data structure. The ownership is passed to the STAR framework via ToWhiteBoard()
   Int_t mReady;
   int readAllRowColumnStatus;

   ClassDef(StPxlDbMaker, 0)
};
#endif


