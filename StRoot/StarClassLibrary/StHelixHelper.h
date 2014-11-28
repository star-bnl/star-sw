/*!
 * \class StHelixHelper 
 * \author Valeri Fine, Sep 2009
 */
/***************************************************************************/
#ifndef ROOT_StHelixHelper
#define ROOT_StHelixHelper

//////////////////////////////////////////////////////////////////////////
//                                                                      //
/// StHelixHelper is to convert the track object defined
/// by 2 StHelix objects and length
/// into the array of 3D points
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "TObject.h"
#include "StHelixD.hh"
#include "StPhysicalHelixD.hh"
#include "StHelixD.hh"

class THelixTrack;
class StPhysicalHelix;

class StHelixHelper: public TObject
{
  protected: 
      StHelixHelper();
  public:
      static THelixTrack *MyHelix(THelixTrack *myHlx,const StHelixD *evHlx);
      enum {kInnerHelix, kOutterHelix};
      StHelixHelper(const StPhysicalHelix &helix
        ,const StPhysicalHelix &outerHelix, double length);
      StHelixHelper(const StHelixHelper &helper);
     ~StHelixHelper();
      float     GetLength()     const;

      virtual StPhysicalHelixD *GetHelix(int idx=0) const;
      virtual THelixTrack *GetTHelix(int idx=0)     const;
      virtual Float_t  *GetPoints(int &npoints)     const;


private:
   mutable StPhysicalHelixD  *fHelx[kOutterHelix+1];
   mutable THelixTrack       *fTHlx[kOutterHelix+1];
   float fLength;
   ClassDef(StHelixHelper,0)
};

#endif //ROOT_StHelixHelper
