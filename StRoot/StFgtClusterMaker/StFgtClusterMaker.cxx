//\class StFgtClusterMaker
//\author Anselm Vossen (avossen@indiana.edu)
//
// 
//   $Id: StFgtClusterMaker.cxx,v 1.18 2011/11/02 20:53:30 balewski Exp $

#include "StFgtClusterMaker.h"
#include "StRoot/StEvent/StEvent.h"
#include "StRoot/StEvent/StFgtCollection.h"
#include "StFgtIClusterAlgo.h"
#include "StEvent/StFgtHit.h"

void StFgtClusterMaker::Clear(Option_t *opts)
{

};


Int_t StFgtClusterMaker::Make()
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
   };

   if( !ierr ){
     for( UInt_t discIdx=0; discIdx<fgtCollectionPtr->getNumDiscs(); ++discIdx ){
       LOG_INFO << "disc: " << discIdx << " has strips: " << fgtCollectionPtr->getNumStrips(discIdx) << endm;
       
       StFgtStripCollection *stripCollectionPtr = fgtCollectionPtr->getStripCollection( discIdx );
       StFgtHitCollection *hitCollectionPtr = fgtCollectionPtr->getHitCollection( discIdx );
       
       if( stripCollectionPtr && hitCollectionPtr ){
	 Int_t loc_ierr = mClusterAlgoPtr->doClustering( *stripCollectionPtr, *hitCollectionPtr );
	 if(loc_ierr!=kStOk) {
	   LOG_WARN <<"StClusterMaker::Make(): clustering for disc " << discIdx << " returned " << loc_ierr <<endm;
	   if(loc_ierr>ierr)
	     ierr=loc_ierr;
	 }
       }
     }
   }
   

   printf("End of fgt-clust-maker, print all strips & clusters:\n");
   
   printf("  fgtCollnumDisc=%d, tot strip=%d  totClust=%d\n",fgtCollectionPtr->getNumDiscs(),fgtCollectionPtr->getNumStrips()  , fgtCollectionPtr->getNumHits());
  for(int iDisc=0; iDisc <(int)fgtCollectionPtr->getNumDiscs(); iDisc++) {
    printf("  content: iDisc=%d  # of : strips=%d  hits=%d\n" ,iDisc , fgtCollectionPtr->getNumStrips(iDisc)  , fgtCollectionPtr-> getNumHits( iDisc));

    // ..... print all strips ....
    StFgtStripCollection *stripPtr= fgtCollectionPtr->getStripCollection(iDisc);
    StSPtrVecFgtStrip &stripVec = stripPtr->getStripVec();    
    int ih=0;
    for( StSPtrVecFgtStripIterator it=stripVec.begin();it!=stripVec.end();++it, ih++)    {
      printf("iDisc=%d ih=%d  strip: geoId=%d ADC=%d\n",iDisc,ih,((*it))->getGeoId(),((*it))->getAdc());
    }
    
    // ..... print all 1D clusters (aka FGT HITs) ....
    StFgtHitCollection *clustPtr= fgtCollectionPtr->getHitCollection(iDisc);
    StSPtrVecFgtHit &clustVec = clustPtr->getHitVec();    
    ih=0;
    for( StSPtrVecFgtHitIterator it=clustVec.begin();it!=clustVec.end();++it, ih++)    {

      printf("iDisc=%d ih=%d  clust  quad=%d, layer=%c charge=%.2f  R/cm=%.3f +/- %.3f  Phi/rad=%f +/-%f   centStripId=%d totCharge=%.2f\n",iDisc,ih, ((*it))->getQuad(), ((*it))->getLayer(), ((*it))->charge(), ((*it))->getPositionR(), ((*it))->getErrorR(), ((*it))->getPositionPhi(), ((*it))->getErrorPhi(),((*it))->getCentralStripGeoId(),  ((*it))->charge());
    }
    
    
  }
  
  return ierr;

};


Int_t StFgtClusterMaker::setClusterAlgo(StFgtIClusterAlgo* algo)
{
  mClusterAlgoPtr=algo;
  return kStOk;
}

Int_t StFgtClusterMaker::Init()
{
  //  cout <<"cluster init " <<endl;
  Int_t ierr = kStOk;

  if( !mClusterAlgoPtr ){
     LOG_ERROR << "No fgt cluster algorithm specified" << endm;
     ierr = kStErr;
  };

  if( !ierr )
     ierr = mClusterAlgoPtr->Init();

  return ierr;
};
  
  
StFgtClusterMaker::StFgtClusterMaker( const Char_t* name ) : StMaker(name),mClusterAlgoPtr(0)
{
   /* */
};

StFgtClusterMaker::~StFgtClusterMaker()
{
	
};

    
ClassImp(StFgtClusterMaker);
    

//   $Log: StFgtClusterMaker.cxx,v $
//   Revision 1.18  2011/11/02 20:53:30  balewski
//   lot of printouts
//
//   Revision 1.17  2011/11/02 16:03:56  balewski
//   fix indexing of printout
//
//   Revision 1.16  2011/11/01 18:46:30  sgliske
//   Updated to correspond with StEvent containers, take 2.
//
//   Revision 1.15  2011/10/28 14:29:43  sgliske
//   fixed CVS tags
//
//   Revision 1.14  2011/10/28 14:28:26  sgliske
//   Cleaned up prepareEnvironment (no functional change).
//   Removed old methods of getting data pointer.
//   Also pClusterAlgo changed to mClusterAlgoPtr to conform with STAR guidelines.
//
//   Revision 1.13  2011/10/26 20:56:50  avossen
//   use geoIds to determine if two strips are adjacent
//
//   Revision 1.12  2011/10/26 19:32:31  balewski
//   now fgt-geom is owned by fgtDb-maker
//
//   Revision 1.11  2011/10/26 17:02:04  balewski
//   get fgt event the proper way
//
//   Revision 1.10  2011/10/20 17:30:37  balewski
//   revert
//
//   Revision 1.7  2011/10/17 21:42:02  balewski
//   added tmp interface to fgt-simu-maker
//
//   Revision 1.6  2011/10/10 20:35:08  avossen
//   fixed strip-cluster association in MaxCluster algo, made other files cvs compliant
//
//
//
