//
// \class StFgtRawMaker
//  \author Anselm Vossen
//
//   $Id: StFgtRawMaker.cxx,v 1.21 2012/01/18 03:10:51 avossen Exp $
//
//  $Log: StFgtRawMaker.cxx,v $
//  Revision 1.21  2012/01/18 03:10:51  avossen
//  added db access to the raw maker
//
//  Revision 1.20  2012/01/17 21:56:26  sgliske
//  Short_t geoId -> Int_t geoId
//
//  Revision 1.19  2011/11/01 18:45:32  sgliske
//  Updated to correspond with StEvent containers, take 2.
//  Note: new FGT containers (and StEvent access) no longer
//  motivate the use of a common base class
//
//  Revision 1.18  2011/10/27 21:09:47  jeromel
//  Small info added in Init() + ident
//
//  Revision 1.17  2011/10/26 21:32:01  avossen
//  fixed mFgtEvent pointer name
//
//  Revision 1.16  2011/10/26 20:57:48  avossen
//  hopefully made cosmic and raw maker compatible with bfc (again), added clear in make. Unnecessary if member fkt clear() is called after every event
//
//  Revision 1.15  2011/10/06 15:19:43  sgliske
//  StFgtRawHitArray::PushBack -> pushBack
//
//  Revision 1.14  2011/09/26 14:23:06  sgliske
//  Update for new 'Char_t mType' field in StFgtRawHit
//
//  Revision 1.13  2011/09/21 17:49:34  sgliske
//  alternate base class with more
//   functionality and not an StMaker
//
//  Revision 1.11  2011/09/20 15:53:09  sgliske
//  Update so that everything compiles nicely
//  and so that one can execute the macro/simpleTestStandTest.C file
//
//  Revision 1.10  2011/09/19 21:12:36  sgliske
//  update
//
//  Revision 1.9  2011/09/14 17:21:19  avossen
//  using dev allows cint to compile.
//
//  Revision 1.8  2011/09/14 15:44:11  avossen
//  took out the root cint stuff so it compiles
//
//  Revision 1.7  2011/09/13 18:35:42  avossen
//  added RTS header files
//
//  Revision 1.6  2011/09/13 10:06:43  avossen
//  *** empty log message ***
//
//  Revision 1.5  2011/09/11 08:06:36  avossen
//  added cosmic maker
//
//  Revision 1.4  2011/08/24 14:30:44  avossen
//  Continued raw maker development
//
//

#include "StRoot/St_base/StMessMgr.h"
#include "StRoot/St_base/Stypes.h"
#include "StRoot/StFgtUtil/geometry/StFgtGeomDefs.h"
#include "StRoot/StFgtUtil/geometry/StFgtGeom.h"
#include "StRoot/StChain/StRtsTable.h"
#include "StRoot/StEvent/StEvent.h"
#include "DAQ_FGT/daq_fgt.h"
#include "DAQ_READER/daq_dta.h"

#include "StRoot/StEvent/StFgtCollection.h"
#include "StRoot/StEvent/StFgtStripCollection.h"
#include "StRoot/StEvent/StFgtStrip.h"
#include "StFgtDbMaker/StFgtDb.h"

#include "StFgtRawMaker.h"


Int_t StFgtRawMaker::PrepareEnvironment()
{
  StEvent* eventPtr=0;
  eventPtr= (StEvent*)StRTSBaseMaker::GetInputDS("StEvent");

  mFgtCollectionPtr=NULL;
  if(eventPtr)
    {
      mFgtCollectionPtr=eventPtr->fgtCollection();
    }
  else
    {
      eventPtr=new StEvent();
      StRTSBaseMaker::AddData(eventPtr);
      mFgtCollectionPtr=eventPtr->fgtCollection();
    }
  if(!mFgtCollectionPtr)
    {
      mFgtCollectionPtr=new StFgtCollection();
      eventPtr->setFgtCollection(mFgtCollectionPtr);
      LOG_DEBUG <<"::prepareEnvironment() has added a non existing StFgtCollection()"<<endm;
    }
  else
    {
      //this should be unncessary if the member clear function is called
      mFgtCollectionPtr->Clear();
    }
  return kStOK;
};


Int_t StFgtRawMaker::Make()
{
  TStopwatch clock;
  clock.Start();
  LOG_DEBUG <<"StEmcRawMaker::Make()******************************************************************"<<endm;

  if( PrepareEnvironment()!=kStOK )
     {
        LOG_ERROR << "Error preparing enviroment" << endm;
        return kStFatal;
     }
  else
     {
        return FillHits();
     };
};

Int_t StFgtRawMaker::FillHits()
{

   Short_t quadrant=0;      
   Char_t layer=0;
   Double_t ordinate=0;
   Double_t lowerSpan=0;
   Double_t upperSpan=0;
   Int_t rdo=0;
   Int_t arm=0;
   Int_t apv=0;
   Int_t channel=0;
   Short_t adc=0;
   Short_t timebin=0;
   Short_t discIdx=0;

   //now grab the constants from the header file, loop over the raw data and fill the hits...
   while(this->GetNextDaqElement("fgt/adc"))
      {
         StRtsTable* rts_tbl=DaqDta();
         //works because '*' operator is giving your the row
         for(StRtsTable::iterator it=rts_tbl->begin();it!=rts_tbl->end();it++)
            {
               fgt_adc_t *mFgtRawData=(fgt_adc_t*)*it;
               rdo=rts_tbl->Rdo();
               //this is different from rts_example
               channel=mFgtRawData->ch;
               timebin=mFgtRawData->tb;
               //look at rts_example for the mapping 
               adc=mFgtRawData->adc;
               arm=rts_tbl->Sector();
               apv=rts_tbl->Pad();
	       Int_t geoId=-1;
	       if(!fgtDb)
		 geoId=StFgtGeom::getNaiveGeoIdFromElecCoord(rdo,arm,apv,channel);
	       else
		 geoId=fgtDb->getGeoIdFromElecCoord(rdo, arm, apv, channel);

               StFgtGeom::getNaivePhysCoordFromElecCoord(rdo,arm,apv,channel,discIdx,quadrant,layer,ordinate,lowerSpan,upperSpan);
               Char_t type = 0;    // TODO: set this according to the database???

               StFgtStripCollection *stripCollectionPtr = mFgtCollectionPtr->getStripCollection( discIdx );
               if( stripCollectionPtr )
                  {
                     StSPtrVecFgtStrip &stripVec = stripCollectionPtr->getStripVec();
                     stripVec.push_back( new StFgtStrip( geoId,adc,type,timebin) );
                  }
               else
                  { LOG_WARN << "StFgtRawMaker::Make() -- Could not access disc " << discIdx << endm; }
            }
      }

   return kStOK;
};

Int_t StFgtRawMaker::Init()
{
   Int_t ierr = kStOk;

   LOG_INFO << "StFgtRawMaker::Init we are named "  << GetName() << endm;

   return ierr;
};

StFgtRawMaker::StFgtRawMaker(const Char_t* name) :
   StRTSBaseMaker( "adc", name ),
   mFgtCollectionPtr(0)
{
  fgtDb=0;
};


StFgtRawMaker::~StFgtRawMaker()
{
  // nothing to do
};

void StFgtRawMaker::Clear( Option_t *opts )
{
   if( mFgtCollectionPtr )
     mFgtCollectionPtr->Clear( opts );
};

ClassImp(StFgtRawMaker);
