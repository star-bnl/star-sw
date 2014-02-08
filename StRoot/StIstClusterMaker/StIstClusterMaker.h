/***************************************************************************
*
* $Id: StIstClusterMaker.h,v 1.4 2014/02/08 03:34:16 ypwang Exp $
*
* Author: Yaping Wang, March 2013
****************************************************************************
* Description: 
* IST cluster maker by calling implemented algorithms.
****************************************************************************
*
* $Log: StIstClusterMaker.h,v $
* Revision 1.4  2014/02/08 03:34:16  ypwang
* updating scripts
*
*
****************************************************************************
* StIstClusterMaker.h,v 1.0
* Revision 1.0 2013/11/04 15:55:30 Yaping
* Initial version
****************************************************************************/

#ifndef StIstClusterMaker_hh
#define StIstClusterMaker_hh

#include "StMaker.h"
#include "StIstIClusterAlgo.h"

class StIstDbMaker;
class St_istControl;

class StIstClusterMaker : public StMaker
{
 public:
  StIstClusterMaker( const char* name="ist_cluster");

  Int_t Init();
  Int_t InitRun(Int_t runumber);
  Int_t Make();

  Int_t setClusterAlgo(StIstIClusterAlgo*);
  void setUsedTimeBin(unsigned char tb = -1);			//time bin to be used
  void setClusterSplitFlag( bool splitFlag = 1);	//cluster splitting switch

  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StIstClusterMaker.h,v 1.4 2014/02/08 03:34:16 ypwang Exp $ built "__DATE__" "__TIME__ ; return cvs;}

 protected:
  StIstIClusterAlgo* mClusterAlgoPtr;
  StIstDbMaker *mIstDbMaker;

  UChar_t mTimeBin;
  Bool_t mSplitCluster;
  UShort_t mMinNumOfRawHits, mMaxNumOfRawHits;

 private:
  ClassDef(StIstClusterMaker,1);
};

inline void StIstClusterMaker::setUsedTimeBin(unsigned char tb){ mTimeBin = tb; };
inline void StIstClusterMaker::setClusterSplitFlag( bool splitFlag ){ mSplitCluster = splitFlag; };
#endif

