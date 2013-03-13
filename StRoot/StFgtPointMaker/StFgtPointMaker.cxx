//\class StFgtPointMaker
//\author Anselm Vossen (avossen@indiana.edu)
//

#include "StFgtPointMaker.h"
#include "StRoot/StEvent/StFgtCollection.h"
#include "StRoot/StEvent/StEvent.h"
#include "StFgtSimplePointAlgo.h"

void StFgtPointMaker::Clear(Option_t *opts)
{

};

Int_t StFgtPointMaker::setPointAlgo(StFgtIPointAlgo* algo)
{
  mPointAlgoPtr=algo;
  return kStOk;
}


Int_t StFgtPointMaker::Make()
{
   Int_t ierr = kStOk;

   StEvent* eventPtr = 0;
   eventPtr = (StEvent*)GetInputDS("StEvent");

   if( !eventPtr ) {
      LOG_ERROR << "Error getting pointer to StEvent from '" << ClassName() << "'" << endm;
      ierr = kStErr;
   };

   StFgtCollection* fgtCollectionPtr = 0;

   if( eventPtr ) {
      fgtCollectionPtr=eventPtr->fgtCollection();
   };

   if( !fgtCollectionPtr) {
      LOG_ERROR << "Error getting pointer to StFgtCollection from '" << ClassName() << "'" << endm;
      ierr = kStErr;
   }

   if( !ierr ){
     StFgtPointCollection *pointCollectionPtr = fgtCollectionPtr->getPointCollection();
     ierr = mPointAlgoPtr->makePoints( *fgtCollectionPtr );
      }
      
   return ierr;
};



Int_t StFgtPointMaker::Init()
{

   Int_t ierr = kStOk;

   if( !mPointAlgoPtr ){
      LOG_WARN << "No fgt point algorithm specified" << endm;
      mPointAlgoPtr=new StFgtSimplePointAlgo();
      ///
      AddObj(mPointAlgoPtr,".data",0);
      ierr = kStOk;

   };

   if( !ierr )
      ierr = mPointAlgoPtr->Init();

   return ierr;
};
  
 
StFgtPointMaker::StFgtPointMaker( const Char_t* name ) : StMaker(name),mPointAlgoPtr(0)
{


   /* */
};

StFgtPointMaker::~StFgtPointMaker()
{
	
};

    
ClassImp(StFgtPointMaker);
    
/*
 * $Id: StFgtPointMaker.cxx,v 1.1 2013/03/13 20:36:28 jeromel Exp $ 
 * $Log: StFgtPointMaker.cxx,v $
 * Revision 1.1  2013/03/13 20:36:28  jeromel
 * Initial revision, Anselm Vossen
 *
 * Revision 1.6  2011/11/04 18:55:50  ckriley
 * minor bug fix
 *
 * Revision 1.5  2011/11/01 18:48:34  sgliske
 * Updated to correspond with StEvent containers, take 2.
 *
 * Revision 1.3  2011/10/28 14:41:27  sgliske
 * Changed to get StFgtEvent from StEvent rather than another maker.
 * Also pPointrAlgo changed to mPointerAlgoPtr to conform with STAR guidelines.
 *
 */
