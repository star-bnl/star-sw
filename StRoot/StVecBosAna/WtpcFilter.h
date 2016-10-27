#ifndef W_TPC_FILTER_HH
#define W_TPC_FILTER_HH

#include <TString.h>

class TObjArray;
class TH1;
class StMuTrack;


// Sector dependent TPC track filter
class WtpcFilter
{
public:

   WtpcFilter();
   void init(const char* core, int sec, TObjArray* HListX, bool barrel);
   void setCuts(int x, float y, float r1, float r2)
   {
      mMinNumHits = x;
      mMinHitFrac = y;
      mMinRadius  = r1;
      mMaxRadius  = r2;
   }

   static int getTpcSec(float phiRad, float etaDet);
   bool accept(const StMuTrack* mStMuTrack);

private:

   TString name;
   int     secID;
   int     mMinNumHits;
   float   mMinHitFrac;
   float   mMinRadius;
   float   mMaxRadius;

   TObjArray* HList;
   enum {mxHA = 8};
   TH1* hA[mxHA];
   void initHistos(bool barrel);
};

#endif
