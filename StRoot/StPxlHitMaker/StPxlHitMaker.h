/*!
 * \class StPxlHitMaker
 * \author Qiu Hao, Jan 2013
 * \Initial Revision.
 */
/***************************************************************************
 *
 * $Id: StPxlHitMaker.h,v 1.5 2014/02/07 22:18:06 smirnovd Exp $
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
 * Revision 1.5  2014/02/07 22:18:06  smirnovd
 * Set stricter access modifier for member variables
 *
 * Revision 1.4  2014/01/28 19:29:40  qiuh
 * *** empty log message ***
 *
 *
 **************************************************************************/

#ifndef StPxlHitMaker_hh
#define StPxlHitMaker_hh

#include "StMaker.h"

class StPxlDb;

class StPxlHitMaker : public StMaker
{
public:
   StPxlHitMaker(const char *name = "pxl_hit");
   Int_t InitRun(Int_t runnumber);

   //! The input data can be both clusters and pxl hits.
   //! If there are already pxl hits, their positions will be recalculated.
   //! If there are clusters but no pxl hits collection, a new pxl hit collection will be created.
   //! If there are both pxl hits and clusters, new pxl hits from clusters will be added to hits collection.
   //! Hit sensor local positions are calculated with the thin plate spline funciton which describe the sensor surface
   //! Then global positions are obtained from local positions through rotation + shift by geoHMatrix
   Int_t Make();
   virtual const char *GetCVS() const {
      static const char cvs[] = "Tag $Name:  $ $Id: StPxlHitMaker.h,v 1.5 2014/02/07 22:18:06 smirnovd Exp $ built "__DATE__" "__TIME__ ;
      return cvs;
   }

private:
   StPxlDb *mPxlDb; ///< db structure containing geometry, status information and so on
   Double_t mPixelSize; ///< size of a pxiel

   ClassDef(StPxlHitMaker, 0)
};

#endif
