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

class StFgtPointMaker : public StMaker
{
 public:
  StFgtPointMaker( const Char_t* name="FgtPoint" );
  virtual ~StFgtPointMaker();
  virtual Int_t Init();
  virtual Int_t Make();
  virtual void Clear( Option_t *opts = "" );
  Int_t setPointAlgo(StFgtIPointAlgo*);
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StFgtPointMaker.h,v 1.1 2013/03/13 20:36:28 jeromel Exp $ built "__DATE__" "__TIME__ ; return cvs;}

 protected:
  StFgtIPointAlgo* mPointAlgoPtr;

 private:
  ClassDef(StFgtPointMaker,1);
};
#endif

/*
 * $Id: StFgtPointMaker.h,v 1.1 2013/03/13 20:36:28 jeromel Exp $ 
 * $Log: StFgtPointMaker.h,v $
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
