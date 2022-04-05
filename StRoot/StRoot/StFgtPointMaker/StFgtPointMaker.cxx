//\class StFgtPointMaker
//\author Anselm Vossen (avossen@indiana.edu)
//

#include "StFgtPointMaker.h"
#include "StEventTypes.h"
#include "StRoot/StEvent/StEvent.h"
#include "StRoot/StFgtDbMaker/StFgtDbMaker.h"
#include "StRoot/StFgtDbMaker/StFgtDb.h"
#include "StarClassLibrary/StThreeVectorF.hh"
#include "StTpcDb/StTpcDb.h"
#include "TGeoManager.h"

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

   StThreeVectorF serr(0.01,0.01,0.1);
   if( !ierr ){
     StFgtPointCollection *pointCollectionPtr = fgtCollectionPtr->getPointCollection();

     if(mFakeData==0.0){
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
	   TVector3 xyz,gxyz;
	   mDb->getStarXYZ(idisc,iquad,r,phi,xyz);
	   if(gStTpcDb){
	     double local[3]={0,0,0}, global[3]={0,0,0};
	     xyz.GetXYZ(local);
	     //printf("FGT local  %9.6f %9.6f %9.6f %9.6f\n",local[0],local[1],local[2],local[3]);
	     TGeoHMatrix globalMatrix = gStTpcDb->Tpc2GlobalMatrix();
	     //globalMatrix.Print();
	     globalMatrix.LocalToMaster(local,global);
	     gxyz.SetXYZ(global[0],global[1],global[2]);	   
	   }else{
	     static int nmess=0;
	     if(nmess<100){
	       printf("StFgtPointMaker::Make could not get gStTpcDb... global xyz is same as fgt local xyz\n");
	       nmess++;
	     }
	     gxyz=xyz;
	   }
	   //printf("FGT Globl  %9.6f %9.6f %9.6f\n",gxyz.X(),gxyz.Y(),gxyz.Z());
	   StThreeVectorF sxyz(gxyz.X(),gxyz.Y(),gxyz.Z());
	   (*it)->setPosition(sxyz);
	   (*it)->setPositionError(serr);	   
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
     }else{ //fake FGT and TPC hits at eta=mFakeData, phi=0
       StSPtrVecFgtPoint& pointVec = pointCollectionPtr->getPointVec();
       int quad=0;
       float eta=mFakeData;
       float phi=0.0;
       float theta=2*atan(exp(-eta));
       for(short d=0; d<6; d++){
	 StFgtHitCollection *hitCollectionPtr = fgtCollectionPtr->getHitCollection(d);
	 StSPtrVecFgtHit &clustVec = hitCollectionPtr->getHitVec();
	 float z=StFgtGeom::getDiscZ(d);
	 float x=z*tan(theta);
	 float y=0;
	 float r=sqrt(x*x+y*y);
	 if(r<38.3){
	   printf("FakeData FGT eta=%6.3f phi=%6.3f z,x=%6.3f,%6.3f\n",eta,phi,z,x);
	   StFgtHit *phit = new StFgtHit(d*2,  11,1050,d,quad,'P',0,0.1,phi,0.1,z,0.1);  clustVec.push_back(phit);
	   StFgtHit *rhit = new StFgtHit(d*2+1,11,1000,d,quad,'R',r,0.1,0  ,0.1,z,0.1);  clustVec.push_back(rhit);
	   StFgtPoint *p  = new StFgtPoint(phit,rhit,d,1); pointVec.push_back(p);
	   StThreeVectorF xyz(x,y,z);
	   p->setPosition(xyz);
	   p->setPositionError(serr);
	 }
       }
       StTpcHitCollection* tpcHits = eventPtr->tpcHitCollection();
       if ( !tpcHits ) {
	 tpcHits = new StTpcHitCollection();
	 eventPtr->setTpcHitCollection(tpcHits);
       }
       float tpcr[45];
       for(int ipad= 0;ipad< 8; ipad++){tpcr[ipad]=60.0 + 4.8*ipad;}
       for(int ipad= 8;ipad<13; ipad++){tpcr[ipad]=60.0 + 4.8*7 + 5.2*(ipad-8);}
       for(int ipad=13;ipad<45; ipad++){tpcr[ipad]=127.950 + 2.0*(ipad-13);}
       for(int ipad=0; ipad<45; ipad++){
	 float r=tpcr[ipad];
	 float x=r;
	 float y=0;	 
	 float z=r/tan(theta);
	 if(z<210){
	   printf("FakeData TPC eta=%6.3f phi=%6.3f z,x=%6.3f,%6.3f\n",eta,phi,z,x);
	   StThreeVectorF xyz(x,y,z);
	   StThreeVectorF exyz(0.1,0.1,0.1);
	   int sector=3;
	   int row=ipad+1;
	   int hw = 1 + (sector<<4) + (row<<9);
	   StTpcHit* t=new StTpcHit(xyz,exyz,hw,10); 
	   tpcHits->addHit(t);
	 }
       }
     }
   }
   return ierr;
}
   


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
 
StFgtPointMaker::StFgtPointMaker( const Char_t* name ) : StMaker(name),mPointAlgoPtr(0),mSkipEvent(0),mFakeData(0)
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
 * $Id: StFgtPointMaker.cxx,v 1.4 2013/04/25 11:52:31 akio Exp $ 
 * $Log: StFgtPointMaker.cxx,v $
 * Revision 1.4  2013/04/25 11:52:31  akio
 * *** empty log message ***
 *
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
