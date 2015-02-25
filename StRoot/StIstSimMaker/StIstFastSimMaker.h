/* $Id: StIstFastSimMaker.h,v 1.9 2015/02/25 20:39:51 smirnovd Exp $ */

#ifndef STAR_StIstFastSimMaker
#define STAR_StIstFastSimMaker
#ifndef StMaker_H
#include "StMaker.h"
#endif
#include "StThreeVectorF.hh"
#include "StThreeVectorD.hh"
#include <vector>

class StEvent;
class StMcEvent;
class StRandom;
class StMcIstHitCollection;
class StIstHitCollection;
class St_g2t_ist_hit;

class StIstDb;
class THashList;

class TString;


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
   Int_t Make();
   Int_t InitRun( Int_t runNo);
   void buildIdealGeom(Bool_t isIdealGeom) {mBuildIdealGeom = isIdealGeom;}

   virtual const char *GetCVS() const {
      static const char cvs[] = "Tag $Name:  $ $Id: StIstFastSimMaker.h,v 1.9 2015/02/25 20:39:51 smirnovd Exp $ built "__DATE__" "__TIME__ ;
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
* Revision 1.9  2015/02/25 20:39:51  smirnovd
* STAR Coding Standards style upates according to Jason W. comments
*
* Revision 1.8  2015/02/25 20:39:43  smirnovd
* Minor refactoring of StPxlFastSim::distortHit() to include a new warning for unphysical hit position
*
* Revision 1.7  2015/02/25 20:36:26  smirnovd
* StIstFastSimMaker: Corrected style with astyle -s3 -p -H -A3 -k3 -O -o -y -Y -f
*
* Revision 1.6  2015/02/25 20:32:14  smirnovd
* Minor adjustments to the code and comments
*
* Revision 1.5  2015/02/25 20:32:05  smirnovd
* Do not use automatic ROOT I/O as this is a StMaker. Makers are not persistent
*
* Revision 1.4  2015/02/25 20:31:58  smirnovd
* Removed pointless methods. ::Init() and ::Finish() do not do much. Data members initialized in constructor
*
* Revision 1.3  2015/02/25 20:20:08  smirnovd
* Minor style, comments and whitespace changes
*
* Revision 1.2  2015/02/25 20:20:00  smirnovd
* Moved CVS log to the end of file and updated doxygen-style comments
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
****************************************************************************/
