/*!
 * \class StPxlCluster
 * \author Qiu Hao, Jan 2013, according codes from Xiangming Sun
 * \Initial Revision.
 */
/***************************************************************************
 *
 * $Id: StPxlCluster.h,v 1.7 2014/08/06 11:43:34 jeromel Exp $
 *
 * Author: Qiu Hao, Jan 2013, according codes from Xiangming Sun
 ***************************************************************************
 *
 * Description:
 * a group of neighboring pixel raw hits
 * More information at
 * https://www.star.bnl.gov/protected/heavy/qiuh/HFT/software/PXL_software.pdf
 *
 ***************************************************************************
 *
 * $Log: StPxlCluster.h,v $
 * Revision 1.7  2014/08/06 11:43:34  jeromel
 * Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
 *
 * Revision 1.6  2014/02/27 00:44:25  smirnovd
 * Minor corrections
 *
 * Revision 1.5  2014/02/27 00:44:15  smirnovd
 * Make sorting function static class method
 *
 * Revision 1.4  2014/02/21 21:11:06  smirnovd
 * Minor style and empty space adjustments
 *
 * Revision 1.3  2014/01/28 19:29:35  qiuh
 * *** empty log message ***
 *
 *
 **************************************************************************/

#ifndef StPxlCluster_hh
#define StPxlCluster_hh

#include <utility>

#include "Rtypes.h"

class StPxlRawHit;


class StPxlCluster
{
public:
   StPxlCluster();
   Int_t nRawHits() const;                              ///< number of raw hits
   void addRawHit(const StPxlRawHit *rawHit);           ///< add a raw hit to the cluster
   void summarize(int embeddingShortCut = 0);           ///< calculate column center, row center, and most frequent idTruth among raw hits
   Float_t columnCenter() const {return mColumnCenter;} ///< average raw hit column
   Float_t rowCenter() const {return mRowCenter;}       ///< average raw hit row
   Int_t idTruth() const {return mIdTruth;}             ///< for embedding, 0 as background, most frequent raw hit idTruth as idTruth of the cluster
   virtual const char *GetCVS() const {
      static const char cvs[] = "Tag $Name:  $ $Id: StPxlCluster.h,v 1.7 2014/08/06 11:43:34 jeromel Exp $ built " __DATE__ " " __TIME__ ;
      return cvs;
   }

   static bool compareSecond(const std::pair<int, int> &pair1, const std::pair<int, int> &pair2);

protected:
   std::vector<const StPxlRawHit *> mRawHitVec; ///< vector of raw hits
   Float_t mColumnCenter;                       ///< average raw hit column
   Float_t mRowCenter;                          ///< average raw hit row
   Int_t   mIdTruth;                            ///< for embedding, 0 as background, most frequent raw hit idTruth as idTruth of the cluster

   ClassDef(StPxlCluster, 1)
};

#endif
