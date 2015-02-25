/* $Id: StIstFastSimMaker.h,v 1.13 2015/02/04 16:36:09 ypwang Exp $ */

#ifndef STAR_StIstFastSimMaker
#define STAR_StIstFastSimMaker
#ifndef StMaker_H
#include "StMaker.h"
#endif

class StRandom;
class StIstDb;
class THashList;

/**
 * Fast simulation maker for IST.
 *
 * \author: Yaping Wang
 * \date August 2012
 */
class StIstFastSimMaker : public StMaker
{
public:

   StIstFastSimMaker(const Char_t *name = "istFastSim");
   virtual ~StIstFastSimMaker();
   Int_t Init();
   //retrieve GEANT hit information, and transfer hit position to ideal/misaligned geometry of realistic IST detector
   Int_t Make();
   Int_t InitRun( Int_t runNo);
   //Selects whether ideal or misalgined geometry is used
   //mBuildIdealGeom kTRUE=ideal, kFALSE=misaligned
   void buildIdealGeom(Bool_t isIdealGeom) {mBuildIdealGeom = isIdealGeom;}
   virtual void  Clear(Option_t *option="");

   virtual const char *GetCVS() const {
      static const char cvs[] = "Tag $Name:  $ $Id: StIstFastSimMaker.h,v 1.13 2015/02/04 16:36:09 ypwang Exp $ built " __DATE__ " " __TIME__ ;
      return cvs;
   }

protected:
   THashList *mIstRot;
   StIstDb *mIstDb;
   Bool_t mBuildIdealGeom;

   StRandom *mRandom;

   Double_t mResXIst1;
   Double_t mResZIst1;
   Bool_t mSmear; //to turn smearing on and off

private:

   /// Routine to smear hit by resolution with gaussian, mean zero and width res
   Double_t distortHit(const Double_t x, const Double_t res, const Double_t detLength) const;

   ClassDef(StIstFastSimMaker, 0)
};

#endif


/***************************************************************************
*
* $Log: StIstFastSimMaker.h,v $
* Revision 1.13  2015/02/04 16:36:09  ypwang
* Further general codeing style updates according to Jason W. reviews
*
* Revision 1.12  2015/02/02 14:14:19  ypwang
* STAR Coding Standards style upates according to Jason W. comments
*
* Revision 1.11  2015/01/29 04:43:21  smirnovd
* Minor refactoring of StPxlFastSim::distortHit() to include a new warning for unphysical hit position
*
* Revision 1.10  2014/10/13 22:35:07  smirnovd
* StIstFastSimMaker: Corrected style with astyle -s3 -p -H -A3 -k3 -O -o -y -Y -f
*
* Revision 1.9  2014/10/13 22:33:04  smirnovd
* Minor adjustments to the code and comments
*
* Revision 1.8  2014/10/13 22:28:18  smirnovd
* Do not use automatic ROOT I/O as this is a StMaker. Makers are not persistent
*
* Revision 1.7  2014/10/13 22:28:11  smirnovd
* Removed pointless methods. ::Init() and ::Finish() do not do much. Data members initialized in constructor
*
* Revision 1.6  2014/10/13 22:21:56  smirnovd
* Moved CVS log to the end of file and updated doxygen-style comments
*
* Revision 1.4  2014/08/05 03:28:42  ypwang
* buildIdealGeom() added to switch between ideal VMC geometry or DB geometry, Db geometry was built by default
*
* Revision 1.3  2014/02/08 03:34:16  ypwang
* updating scripts
*
*
****************************************************************************
* StIstFastSimMaker.h,v 1.0
* Revision 1.0 2013/11/04 16:25:30 Yaping
* Initial version
* IST GEANT hit is transformed to either ideal or misaligned geometry of 
* realistic detector, with smearing or pixelization. The GEANT hit dE is 
* directly propagated to IST hit in GeV.
****************************************************************************/
