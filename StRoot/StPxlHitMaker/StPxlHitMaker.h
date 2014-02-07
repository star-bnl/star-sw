/*!
 * \class StPxlHitMaker
 * \author Qiu Hao, Jan 2013
 * \Initial Revision.
 */
/***************************************************************************
 *
 * $Id: StPxlHitMaker.h,v 1.7 2014/02/07 22:56:39 smirnovd Exp $
 *
 * Author: Qiu Hao, Jan 2013
 **************************************************************************/

#ifndef StPxlHitMaker_hh
#define StPxlHitMaker_hh

#include "StMaker.h"

class StPxlDb;


/**
 * This maker adds in the event a StPxlHitCollection of hits to be used in the
 * tracking algorithms. The hits are formed from the clusters contained in
 * the event's StPxlClusterCollection. The hits global position is calculated.
 * More information at
 * https://www.star.bnl.gov/protected/heavy/qiuh/HFT/software/PXL_software.pdf
 */
class StPxlHitMaker : public StMaker
{
public:
   StPxlHitMaker(const char *name = "pxl_hit");
   Int_t InitRun(Int_t runnumber);
   Int_t Make();
   virtual const char *GetCVS() const {
      static const char cvs[] = "Tag $Name:  $ $Id: StPxlHitMaker.h,v 1.7 2014/02/07 22:56:39 smirnovd Exp $ built "__DATE__" "__TIME__ ;
      return cvs;
   }

private:
   StPxlDb *mPxlDb;     ///< db structure containing geometry, status information and so on
   Double_t mPixelSize; ///< size of a pixel

   ClassDef(StPxlHitMaker, 0)
};

#endif


/***************************************************************************
 *
 * $Log: StPxlHitMaker.h,v $
 * Revision 1.7  2014/02/07 22:56:39  smirnovd
 * Moved CVS log list to the bottom of file
 *
 * Revision 1.6  2014/02/07 22:38:12  smirnovd
 * Doxygen comments reshuffled
 *
 * Revision 1.5  2014/02/07 22:18:06  smirnovd
 * Set stricter access modifier for member variables
 *
 * Revision 1.4  2014/01/28 19:29:40  qiuh
 * *** empty log message ***
 *
 *
 **************************************************************************/
