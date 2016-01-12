#ifndef StIstDigiHit_h
#define StIstDigiHit_h

#include "StIstUtil/StIstConsts.h"
#include "StEvent/StIstHit.h"


/**
 * Data structure for additional function of StIstHit.
 *
 * \author Yaping Wang
 * \author Dmitri Smirnov
 */
class StIstDigiHit: public StIstHit
{
public:
   StIstDigiHit();
   StIstDigiHit(const StIstHit &istHit);

   void setApv(unsigned char apvId);
   void setMeanColumn(float meanColumn);
   void setMeanRow(float meanRow);
   void setClusterSizeFlag(bool flag);

   unsigned char getApv() const;
   float getMeanColumn() const;
   float getMeanRow() const;
   bool  getClusterSizeFlag() const;
   float localPositionErr(unsigned int i) const;

   void* operator new(size_t sz, void *p) { return p; }
   void* operator new(size_t)             { return mPool.alloc(); }
   void  operator delete(void* p)         { mPool.free(p); }

private:
   UChar_t mApv;
   Float_t mMeanColumn;
   Float_t mMeanRow;
   Bool_t  mClusterSizeFlag; //0: cluster Size in r-phi < 2; 1: cluster Size in Z

   static StMemoryPool mPool;  //! Hide inherited static member and assign a new value
};

#endif
