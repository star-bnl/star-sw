/*
 * $Id: StPxlSimMaker.h,v 1.11 2017/11/08 23:14:36 smirnovd Exp $
 *
 * Author: M. Mustafa
 *
 * 
 **********************************************************
 * $Log: StPxlSimMaker.h,v $
 * Revision 1.11  2017/11/08 23:14:36  smirnovd
 * StPxlSimMaker: Don't generate ROOT streamer. Makers are not persistent
 *
 * Revision 1.10  2016/04/13 19:15:06  mstftsm
 * The choice of geometry should be mutually exclusive
 *
 * streamline inlined methods and define them outside the header
 *
 * Use one flag to control geometry source
 *
 * Use fundamental C++ types; this is not a persistent class
 *
 * Fix a logic bug
 *
 * fix bug: missing inline keyword
 *
 * The choice of geometry source shoule be mutually exclusive
 *
 * Revision 1.9  2015/03/13 18:45:01  perev
 * Roll back
 *
 * Revision 1.7  2014/08/06 11:43:35  jeromel
 * Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
 *
 * Revision 1.6  2014/07/17 01:47:43  mstftsm
 * Fix a bug in creating a new StPxlHitCollection.
 * Random seed is set to default.
 * DB geometry is set to default.
 *
 * Revision 1.5  2014/07/03 19:46:37  mstftsm
 * Revereted the changes made for the pileup adder. That does not belong to the master branch.
 *
 * Revision 1.3  2014/03/13 17:00:19  mstftsm
 * StPxlSimMaker has a method to switch on random seed for StRandom generatos in simulators. Default is not a random seed.
 *
 * Revision 1.1  2013/05/12 21:43:33  jeromel
 * Initial revision, code peer review closed 2013/05/06
 *
 * Revision 1.4  2013/05/03 15:08:19  mstftsm
 *
 */

/**
   \class StPxlSimMaker

   \brief maker for all PXL simulators

   This maker should be called for all PXL simulators.
   The simulation algorithm depends on the StPxlISim used. 
   The default algorithm is StPxlFastSim.
   Options for other alogrithms should be set using SetAttr method.

   This class conforms to the STAR StMaker standards.
*/

#ifndef STAR_StPxlSimMaker
#define STAR_StPxlSimMaker

#ifndef StMaker_H
#include "StMaker.h"
#endif

class StPxlISim;

class StPxlSimMaker : public StMaker 
{
 public:

  /*! \brief Constructor uses standard Maker text naming convention,
   *  with value "pxlSimMaker"*/
  StPxlSimMaker(const Char_t *name="pxlSimMaker");

  /*! \brief StEvent will own any hits created by this maker, and is responsible for cleanup.
  */
  virtual ~StPxlSimMaker();


  /*! \brief calls the StPxlISim methods.
   * 
   *  Returns kStOk always.
  */
  virtual Int_t  Make();

  /*! \brief checks if other simulators have been requested and initializes StPxlISim accordingly.
   */
  virtual Int_t Init();

 /*! \brief gets the DB and initializes StPxlISim for this run.
  */
  virtual Int_t InitRun(Int_t);

  void useDIGMAPSSim();
  void useIdealGeom(bool ideal = true);
  void useDbGeom(bool db = true);
  void useRandomSeed(bool use = true);

  /*! \brief Documentation method. GetCVS can be called from the chain, providing a list
   *  of all maker versions in use.
  */
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StPxlSimMaker.h,v 1.11 2017/11/08 23:14:36 smirnovd Exp $ built " __DATE__ " " __TIME__ ; return cvs;}

private:
    StPxlISim* mPxlSimulator;

    bool mUseFastSim;
    bool mUseDIGMAPSSim;

    bool mUseDbGeom;
    bool mUseRandomSeed;


  ClassDef(StPxlSimMaker, 0)   //StAF chain virtual base class for Makers
};
inline void StPxlSimMaker::useDIGMAPSSim() {SetAttr("useDIGMAPSSim",true);}
inline void StPxlSimMaker::useIdealGeom(bool ideal) { useDbGeom(!ideal); }
inline void StPxlSimMaker::useDbGeom(bool db) {mUseDbGeom = db;}
inline void StPxlSimMaker::useRandomSeed(bool use) {mUseRandomSeed = use;}
#endif

/*
 * $Id: StPxlSimMaker.h,v 1.11 2017/11/08 23:14:36 smirnovd Exp $
 *
 * 
 **********************************************************
 * $Log: StPxlSimMaker.h,v $
 * Revision 1.11  2017/11/08 23:14:36  smirnovd
 * StPxlSimMaker: Don't generate ROOT streamer. Makers are not persistent
 *
 * Revision 1.10  2016/04/13 19:15:06  mstftsm
 * The choice of geometry should be mutually exclusive
 *
 * streamline inlined methods and define them outside the header
 *
 * Use one flag to control geometry source
 *
 * Use fundamental C++ types; this is not a persistent class
 *
 * Fix a logic bug
 *
 * fix bug: missing inline keyword
 *
 * The choice of geometry source shoule be mutually exclusive
 *
 * Revision 1.9  2015/03/13 18:45:01  perev
 * Roll back
 *
 * Revision 1.7  2014/08/06 11:43:35  jeromel
 * Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
 *
 * Revision 1.6  2014/07/17 01:47:43  mstftsm
 * Fix a bug in creating a new StPxlHitCollection.
 * Random seed is set to default.
 * DB geometry is set to default.
 *
 * Revision 1.5  2014/07/03 19:46:37  mstftsm
 * Revereted the changes made for the pileup adder. That does not belong to the master branch.
 *
 * Revision 1.3  2014/03/13 17:00:19  mstftsm
 * StPxlSimMaker has a method to switch on random seed for StRandom generatos in simulators. Default is not a random seed.
 *
 * Revision 1.1  2013/05/12 21:43:33  jeromel
 * Initial revision, code peer review closed 2013/05/06
 *
 * Revision 1.4  2013/05/03 15:08:19  mstftsm
 *
 */

