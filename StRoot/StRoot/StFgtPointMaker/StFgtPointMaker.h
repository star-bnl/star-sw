//
/*! Point Maker
This is a general framework that runs a specified point maker algorithm that can be set.

\class StFgtPointMaker
\author Anselm Vossen (avossen@indiana.edu)

A macro to run the point maker can be found here;
StRoot/StFgtPool/StFgtClusterTools/macros/agvEffsPoints.C
Please see instructions in the macro.


*/


#ifndef STAR_StFgtPointMaker_HH
#define STAR_StFgtPointMaker_HH

#include "StMaker.h"
#include "StFgtIPointAlgo.h"

class StFgtDb;

class StFgtPointMaker : public StMaker
{
 public:
  StFgtPointMaker( const Char_t* name="FgtPoint" );
  virtual ~StFgtPointMaker();
  virtual Int_t Init();
  virtual Int_t Make();
  virtual void Clear( Option_t *opts = "" );
  Int_t setPointAlgo(StFgtIPointAlgo*);
  void setSkipEvent(int v){mSkipEvent=v;} //Return kStSkip if max number of disc hit per quad is less than setSkipEvent (default 0)
                                          //For debugging prpose only. Use it with SetAttr(".Privilege",1)
  void setFakeData(float v){mFakeData=v;} //Create fake hits for FGT and TPC at eta=v (default=0, no fake hit and real data)

  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StFgtPointMaker.h,v 1.4 2014/08/06 11:43:09 jeromel Exp $ built " __DATE__ " " __TIME__ ; return cvs;}

 protected:
  StFgtIPointAlgo* mPointAlgoPtr;
  StFgtDb* mDb;
  int mSkipEvent;
  float mFakeData;

 private:
  ClassDef(StFgtPointMaker,1);
};
#endif

/*
 * $Id: StFgtPointMaker.h,v 1.4 2014/08/06 11:43:09 jeromel Exp $ 
 * $Log: StFgtPointMaker.h,v $
 * Revision 1.4  2014/08/06 11:43:09  jeromel
 * Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
 *
 * Revision 1.3  2013/04/25 11:52:31  akio
 * *** empty log message ***
 *
 * Revision 1.2  2013/04/04 20:24:50  akio
 * - Filling StHit with xyz, error on xyz and detectorId
 * - Add option to return kStSkip if max number of disc hit per quad is less than setSkipEvent (default 0)
 *    This is for expert only, and not for production. Use it with SetAttr(".Privilege",1)
 *
 * Revision 1.1  2013/03/13 20:36:28  jeromel
 * Initial revision, Anselm Vossen
 *
 * Revision 1.4  2011/11/01 18:48:34  sgliske
 * Updated to correspond with StEvent containers, take 2.
 *
 * Revision 1.3  2011/10/28 14:41:27  sgliske
 * Changed to get StFgtEvent from StEvent rather than another maker.
 * Also pPointrAlgo changed to mPointerAlgoPtr to conform with STAR guidelines.
 *
 */
