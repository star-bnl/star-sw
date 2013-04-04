//\class StFgtPointMaker
//\author Anselm Vossen (avossen@indiana.edu)
//

#include "StFgtPointMaker.h"
#include "StEventTypes.h"
#include "StRoot/StEvent/StEvent.h"
#include "StRoot/StFgtDbMaker/StFgtDbMaker.h"
#include "StRoot/StFgtDbMaker/StFgtDb.h"
#include "StarClassLibrary/StThreeVectorF.hh"

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
     
     //now points are made... setting xyz, error, detectorId, etc
     int npoint[kFgtNumDiscs][kFgtNumQuads]; memset(npoint,0,sizeof(npoint));
     if( !ierr ){
       StSPtrVecFgtPoint &point = pointCollectionPtr->getPointVec();
       for(StSPtrVecFgtPointIterator it=point.begin(); it!=point.end(); it++) {
	 int idisc=(*it)->getDisc();
	 int iquad=(*it)->getQuad();
	 float phi = (*it)->getPositionPhi();
	 float r   = (*it)->getPositionR();
	 TVector3 xyz;
	 mDb->getStarXYZ(idisc,iquad,r,phi,xyz);
	 StThreeVectorF sxyz(xyz.X(),xyz.Y(),xyz.Z());
	 StThreeVectorF serr(0.01,0.01,0.1);
	 (*it)->setPosition(sxyz);
	 (*it)->setPositionError(sxyz);
	 (*it)->setHardwarePosition(kFgtId);
	 //printf("FgtPoint %3d D=%1d Q=%1d R=%6.3f P=%6.3f x=%6.3f y=%6.3f z=%6.3f\n",
	 //	npoint[idisc][iquad],idisc,iquad,r,phi,sxyz.x(),sxyz.y(),sxyz.z());
	 npoint[idisc][iquad]++;
       }
     }
     
     if(mSkipEvent>0){
       printf("FgtPoint : ");
       int max=0;
       for(int q=0; q<kFgtNumQuads; q++){
	 int n=0;
	 for(int d=0; d<kFgtNumDiscs; d++) {if(npoint[d][q]>0) n++;}
	 printf("Q%1d=%3d ",q,n);
	 if(max<n) max=n;
       }
       printf(" max=%d",max);
       if(max<mSkipEvent) {ierr=kStSKIP; printf(" SKIPPING Event!"); }
       printf("\n");
     }          
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
   
   StFgtDbMaker *fgtDbMkr = static_cast< StFgtDbMaker* >( GetMakerInheritsFrom( "StFgtDbMaker" ) );                              
   if( !fgtDbMkr ){
     LOG_FATAL << "StFgtDb not provided and error finding StFgtDbMaker" << endm;
     ierr = kStFatal;     
   } else {
     mDb = fgtDbMkr->getDbTables();
     if( !mDb ){
       LOG_FATAL << "StFgtDb not provided and error retrieving pointer from StFgtDbMaker '"
		 << fgtDbMkr->GetName() << endm;
       ierr = kStFatal;
     }
   }      
   return ierr;
}
 
StFgtPointMaker::StFgtPointMaker( const Char_t* name ) : StMaker(name),mPointAlgoPtr(0),mSkipEvent(0)
{
   /* noop */
};

StFgtPointMaker::~StFgtPointMaker()
{
  /* noop */	
  if (mPointAlgoPtr){
    LOG_INFO << "Cleaning up point Algo" << endm;
    delete mPointAlgoPtr;
  }
};

    
ClassImp(StFgtPointMaker);
    
/*
 * $Id: StFgtPointMaker.cxx,v 1.3 2013/04/04 20:24:49 akio Exp $ 
 * $Log: StFgtPointMaker.cxx,v $
 * Revision 1.3  2013/04/04 20:24:49  akio
 * - Filling StHit with xyz, error on xyz and detectorId
 * - Add option to return kStSkip if max number of disc hit per quad is less than setSkipEvent (default 0)
 *    This is for expert only, and not for production. Use it with SetAttr(".Privilege",1)
 *
 * Revision 1.2  2013/03/13 21:31:47  jeromel
 * Minor modif
 *
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
