// $Id: StSsdDbMaker.h,v 1.11 2014/12/05 21:59:25 smirnovd Exp $
//
// $Log: StSsdDbMaker.h,v $
// Revision 1.11  2014/12/05 21:59:25  smirnovd
// Fixed style with astyle -s3 -p -H -A3 -k3 -O -o -y -Y -f
//
// Revision 1.10  2014/08/06 11:43:43  jeromel
// Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
//
// Revision 1.9  2008/08/12 22:45:47  bouchet
// use of SsdLaddersOnSectors,SsdOnGlobal,SsdSectorsOnGlobal,SsdWafersOnLadders tables to calculate ssdWafersPositions;add Get methods to access the tables
//
// Revision 1.8  2007/09/25 13:36:55  bouchet
// add m_Mode to constructor
//
// Revision 1.7  2007/03/21 17:17:16  fisyak
// use TGeoHMatrix for coordinate transformation, eliminate ssdWafersPostion
//
// Revision 1.6  2006/10/16 19:53:24  fisyak
// Adjust for new Ssd chain
//
// Revision 1.5  2005/06/20 14:21:38  lmartin
// CVS tags added
//

/***************************************************************************
 * Author: christelle roy
 * Description: SSD DB access Maker
 **************************************************************************/

#ifndef STSSDDBMAKER_H
#define STSSDDBMAKER_H

#ifndef StMaker_H
#include "StMaker.h"
#endif
class St_ssdDimensions;
class St_ssdWafersPosition;
class St_slsCtrl;
class slsCtrl_st;
class ssdConfiguration_st;
#include "StSsdUtil/StSsdBarrel.hh"
#include "THashList.h"

class StSsdDbMaker : public StMaker
{
private:
   StSsdBarrel           *mySsd;
   St_ssdDimensions      *m_dimensions;//!
   St_ssdWafersPosition  *m_positions;//!
   ssdConfiguration_st   *m_config;//!
   slsCtrl_st            *m_ctrl;//!
   Int_t                   mode;//!
   static THashList *fRotList;

public:
   StSsdDbMaker(const char *name = "SsdDb");
   virtual       ~StSsdDbMaker();
   virtual Int_t  Init();
   virtual Int_t  InitRun(Int_t runNumber);
   virtual Int_t  Make();
   virtual Int_t  Finish();
   virtual void   Clear(const char *opt);
   virtual THashList *GetRotations() {return fRotList;}
   virtual St_ssdWafersPosition *CalculateWafersPosition();
   virtual StSsdBarrel  *GetSsd() {return mySsd;}
   virtual slsCtrl_st   *GetSlsCtrl() {return m_ctrl;}
   virtual Int_t        GetMode() {return mode;}
   virtual St_ssdWafersPosition *GetssdWafersPos() {return m_positions;}
   virtual St_ssdDimensions     *GetssdDimensions() {return m_dimensions;}

   virtual const char *GetCVS() const
   {static const char cvs[] = "Tag $Name:  $ $Id: StSsdDbMaker.h,v 1.11 2014/12/05 21:59:25 smirnovd Exp $ built " __DATE__ " " __TIME__ ; return cvs;}
   ClassDef(StSsdDbMaker, 0)  //StAF chain virtual base class for Makers
};
// Global pointers:
R__EXTERN StSsdDbMaker *gStSsdDbMaker;
#endif


