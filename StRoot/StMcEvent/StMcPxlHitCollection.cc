/***************************************************************************
 *
 * $Id: StMcPxlHitCollection.cc,v 2.1 2013/03/25 23:50:36 perev Exp $
 * $Log: StMcPxlHitCollection.cc,v $
 * Revision 2.1  2013/03/25 23:50:36  perev
 * Mustafa.Pxl add
 *
 *
 * Author: Manuel Calderon de la Barca Sanchez
 *
 *  ***************************************************************************
 *
 * Description: Monte Carlo PXL Hit Collection class
 *
 **************************************************************************/
#include "StMcPxlHitCollection.hh"
#include "StMcPxlHit.hh"

static const char rcsid[] = "$Id: StMcPxlHitCollection.cc,v 2.1 2013/03/25 23:50:36 perev Exp $";

ClassImp(StMcPxlHitCollection)

StMcPxlHitCollection::StMcPxlHitCollection()
{
   /* noop */
}

StMcPxlHitCollection::~StMcPxlHitCollection()
{
   /* noop */
}

bool
StMcPxlHitCollection::addHit(StMcPxlHit* hit)
{
   unsigned int s, l, w;
   if (hit &&
         (s = hit->sector() - 1) < kNumberOfSectors &&
         (l = hit->ladder() - 1) < mSectors[s].numberOfLadders() &&
         (w = hit->sensor() - 1) < mSectors[s].ladder(l)->numberOfSensors())
   {
      mSectors[s].ladder(l)->sensor(w)->hits().push_back(hit);
      return true;
   }
   else
      return false;
}

unsigned int
StMcPxlHitCollection::numberOfHits() const
{
   unsigned int sum = 0;
   for (int iSec = 0; iSec < kNumberOfSectors; iSec++)
   {
      for (unsigned int iLad = 0; iLad < mSectors[iSec].numberOfLadders(); iLad++)
      {
         for (unsigned int iSen = 0; iSen < mSectors[iSec].ladder(iLad)->numberOfSensors(); iSen++)
         {
            sum += mSectors[iSec].ladder(iLad)->sensor(iSen)->hits().size();
         }
      }
   }
   return sum;
}

StMcPxlSectorHitCollection*
StMcPxlHitCollection::sector(unsigned int i)
{
   return (i < kNumberOfSectors) ? &(mSectors[i]) : 0;
}

const StMcPxlSectorHitCollection*
StMcPxlHitCollection::sector(unsigned int i) const
{
   return (i < kNumberOfSectors) ? &(mSectors[i]) : 0;
}
/***************************************************************************
 *
 * $Id: StMcPxlHitCollection.cc,v 2.1 2013/03/25 23:50:36 perev Exp $
 *
 * Author: Manuel Calderon de la Barca Sanchez, Oct 1999
 ***************************************************************************
 *
 * Description: Monte Carlo Pixel Hit Collection class
 *
 ***************************************************************************
 *
 * $Log: StMcPxlHitCollection.cc,v $
 * Revision 2.1  2013/03/25 23:50:36  perev
 * Mustafa.Pxl add
 *
 * Revision 2.2  2005/07/06 21:47:45  calderon
 * remove old ifdef, now all classes are persistent.
 *
 * Revision 2.1  2003/08/20 18:50:21  calderon
 * Addition of Tof classes and Pixel classes.  Modified track, event, and
 * container code to reflect this.
 * Fix bug in StMcVertex and in clearing of some hit collections.
 *
 * Revision 2.1  1999/12/14 07:04:49  calderon
 * Numbering scheme as per SVT request.
 *
 * Revision 2.0  1999/11/17 02:00:59  calderon
 * Completely revised for new StEvent
 *
 *
 **************************************************************************/
