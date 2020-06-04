/*
 **********************************************************
 * $Log: StPxlSimMaker.cxx,v $
 * Revision 1.13  2017/10/19 19:38:17  jeromel
 * Merging PXL201709UPD back to MAIN
 *
 * Revision 1.12.6.1  2017/09/11 20:15:14  dongx
 * Pxl slow simulator added
 *
 * Revision 1.12  2016/04/14 23:10:19  smirnovd
 * Cosmetic changes
 *
 * * Make sure we call functions from std library as it was probably intended
 *
 * * Removed unnecessary #included dependency
 *   - Nothing in the code seems to come from the stdio.h header
 *
 * * End log messages with endm to flush the internal buffer
 *
 * Revision 1.11  2016/04/13 19:15:02  mstftsm
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
 * Revision 1.10  2015/03/13 18:45:01  perev
 * Roll back
 *
 * Revision 1.8  2014/07/17 01:47:43  mstftsm
 * Fix a bug in creating a new StPxlHitCollection.
 * Random seed is set to default.
 * DB geometry is set to default.
 *
 * Revision 1.7  2014/07/10 18:52:09  mstftsm
 * Check if StPxlHitCollection exists in StEvent and add simulated hits to it. Otherwise, create a new collection.
 *
 * Revision 1.6  2014/07/03 19:46:37  mstftsm
 * Revereted the changes made for the pileup adder. That does not belong to the master branch.
 *
 * Revision 1.4  2014/06/25 19:21:57  mstftsm
 * pxl DB DataSet has been change to pxl_db. Name changed in this code.
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

#include "StPxlSimMaker.h"
#include "StPxlFastSim.h"
#include "StPxlDigmapsSim.h"
#include "StPxlISim.h"
#include "StMcEvent/StMcPxlHitCollection.hh"
#include "StEvent/StPxlHitCollection.h"
#include "StPxlRawHitMaker/StPxlRawHit.h"
#include "StPxlRawHitMaker/StPxlRawHitCollection.h"

#include "Stiostream.h"
#include "StHit.h"
#include "StEventTypes.h"
#include "StEvent.h"
#include "StMcEvent.hh"
#include "StMcEventTypes.hh"

#include "TGeoManager.h"
#include "TGeoMatrix.h"

#include "TObjectSet.h"

ClassImp(StPxlSimMaker)

using namespace std;

StPxlSimMaker::StPxlSimMaker(const Char_t* name) : StMaker(name) , mPxlSimulator(0), mUseFastSim(false), mUseDIGMAPSSim(false) , mUseDbGeom(true), mUseRandomSeed(true)
{
}
//____________________________________________________________
StPxlSimMaker::~StPxlSimMaker()
{
   delete mPxlSimulator;
}
//____________________________________________________________
Int_t StPxlSimMaker::Init()
{
   LOG_INFO << "StPxlSimMaker::Init()" << endm;

   mUseDIGMAPSSim = IAttr("useDIGMAPSSim");

   if(mUseDIGMAPSSim)
   {
     mPxlSimulator = new StPxlDigmapsSim();
     LOG_INFO << "StPxlSimMaker: using StPxlDigmapsSim " << endm;     
   }
   else
   {
     mUseFastSim = true;
     mPxlSimulator = new StPxlFastSim("pxlFastSim",mUseRandomSeed);
     LOG_INFO << "StPxlSimMaker: using StPxlFastSim " << endm;
   }

   return kStOk;
}

//____________________________________________________________
Int_t StPxlSimMaker::InitRun(Int_t RunNo)
{
   LOG_INFO << "StPxlSimMaker::InitRun" << endm;

   TDataSet *hitErrSet = GetDataBase("Calibrations/tracker/PixelHitError");
   if (!hitErrSet)
   {
      LOG_ERROR << "StPxlSimMaker - E - could not Get Calibrations/tracker." << endm;
      return kStErr;
   }

   TObjectSet *pxlDbDataSet = 0; 
   if(mUseDbGeom)
   {
	   pxlDbDataSet = (TObjectSet*)GetDataSet("pxl_db");
	   if (!pxlDbDataSet)
	   {
		   LOG_ERROR << "StPxlSimMaker - E - pxlDb  is not available" << endm;
		   return kStErr;
	   }
   }

   return mPxlSimulator->initRun(*hitErrSet, pxlDbDataSet, RunNo);
}
//____________________________________________________________

Int_t StPxlSimMaker::Make()
{
   LOG_INFO << "StPxlSimMaker::Make()" << endm;

   // Get the input data structures from StEvent and StMcEvent
   StEvent* rcEvent = (StEvent*) GetInputDS("StEvent");
   if (! rcEvent)
   {
      LOG_INFO << "No StEvent on input" << endm;
      return kStWarn;
   }

   StMcEvent* mcEvent = (StMcEvent *) GetInputDS("StMcEvent");
   if (! mcEvent)
   {
      LOG_INFO << "No StMcEvent on input" << endm;
      return kStWarn;
   }

   //Get MC Pxl hit collection. This contains all PXL hits.
   StMcPxlHitCollection* mcPxlHitCol = mcEvent->pxlHitCollection();
   if (!mcPxlHitCol)
   {
      LOG_INFO << "StPxlSimMaker no PXL hits in this StMcEvent!" << endm;
      return kStOk;
   }

   if (!mUseDbGeom && !gGeoManager) GetDataBase("VmcGeometry");
   if (!mUseDbGeom && !gGeoManager)
   {
      LOG_ERROR << " StPxlSimMaker - E - gGeoManager is not available." << endm;
      return kStErr;
   }

   // call the requested simulator
   if (mUseFastSim)
   {
      StPxlHitCollection *pxlHitCol = rcEvent->pxlHitCollection();

      bool newCollection = false;
      if (!pxlHitCol)
      {
	 LOG_INFO << "No existing StPxlHiCollection. Creating a new one..." <<endm;
	 pxlHitCol = new StPxlHitCollection();
	 newCollection = true;
      }

      mPxlSimulator->addPxlHits(*mcPxlHitCol, *pxlHitCol);

      if(newCollection) rcEvent->setPxlHitCollection(pxlHitCol);
      LOG_DEBUG << " size of hit collection : " << pxlHitCol->numberOfHits() << endm;
   }
   else //if (mUseDIGMAPSSim)
   {
      StPxlRawHitCollection* pxlRawHitCol = 0;

      TObjectSet* pxlRawHitDataSet = (TObjectSet*)GetDataSet("pxlRawHit");

      if (!pxlRawHitDataSet)
      {
           LOG_INFO << " pxlRawHit does NOT exist! Create a new one! " << endm;
//           pxlRawHitDataSet = new TObjectSet("pxlRawHit");
//           m_DataSet = pxlRawHitDataSet;
           pxlRawHitCol = new StPxlRawHitCollection();
           ToWhiteBoard("pxlRawHit", pxlRawHitCol);
//           pxlRawHitDataSet->AddObject(pxlRawHitCol);
      }
      else
      {
           LOG_INFO << " pxlRawHit exists! Append raw hits to this collection! " << endm;
           pxlRawHitCol= (StPxlRawHitCollection*)pxlRawHitDataSet->GetObject();
      }

      if(!pxlRawHitCol)
      {
      LOG_ERROR << "Make() - no pxlRawHitCollection."<<endm;
      return kStErr;
      }
      mPxlSimulator->addPxlRawHits(*mcPxlHitCol,*pxlRawHitCol);         
      
      LOG_INFO << " Finishing DIGMAPS simulator. Number of PxlRawHits = " << pxlRawHitCol->numberOfRawHits() << endm;
   }


   return kStOK;
}
/*
 **********************************************************
 * $Log: StPxlSimMaker.cxx,v $
 * Revision 1.13  2017/10/19 19:38:17  jeromel
 * Merging PXL201709UPD back to MAIN
 *
 * Revision 1.12.6.1  2017/09/11 20:15:14  dongx
 * Pxl slow simulator added
 *
 * Revision 1.12  2016/04/14 23:10:19  smirnovd
 * Cosmetic changes
 *
 * * Make sure we call functions from std library as it was probably intended
 *
 * * Removed unnecessary #included dependency
 *   - Nothing in the code seems to come from the stdio.h header
 *
 * * End log messages with endm to flush the internal buffer
 *
 * Revision 1.11  2016/04/13 19:15:02  mstftsm
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
 * Revision 1.10  2015/03/13 18:45:01  perev
 * Roll back
 *
 * Revision 1.8  2014/07/17 01:47:43  mstftsm
 * Fix a bug in creating a new StPxlHitCollection.
 * Random seed is set to default.
 * DB geometry is set to default.
 *
 * Revision 1.7  2014/07/10 18:52:09  mstftsm
 * Check if StPxlHitCollection exists in StEvent and add simulated hits to it. Otherwise, create a new collection.
 *
 * Revision 1.6  2014/07/03 19:46:37  mstftsm
 * Revereted the changes made for the pileup adder. That does not belong to the master branch.
 *
 * Revision 1.4  2014/06/25 19:21:57  mstftsm
 * pxl DB DataSet has been change to pxl_db. Name changed in this code.
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

