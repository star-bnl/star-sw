#ifndef StFstDbMaker_hh
#define StFstDbMaker_hh

#include "StMaker.h"

class StFstDb;


/**
 * FST calibration/geometry DBs access maker.
 *
 * \author Shenghui Zhang
 * \date Oct. 2021
 */
class StFstDbMaker : public StMaker
{
public:
   StFstDbMaker(const char *name = "fstDb");
   virtual Int_t Init();
   Int_t  InitRun(Int_t runNumber);
   Int_t  Make();

private:
   /// See StFstDb for details on created data structure. The ownership is passed to the STAR
   /// framework via ToWhiteBoard()
   StFstDb *mFstDb;
   Int_t mReady;

   ClassDef(StFstDbMaker, 0)
};

#endif
