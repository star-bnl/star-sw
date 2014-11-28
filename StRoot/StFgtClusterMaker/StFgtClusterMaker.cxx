//\class StFgtClusterMaker
//\author Anselm Vossen (avossen@indiana.edu)
//
// 
//   $Id: StFgtClusterMaker.cxx,v 1.37 2013/02/20 23:33:28 avossen Exp $

#include "StFgtClusterMaker.h"
#include "StRoot/StEvent/StEvent.h"
#include "StRoot/StEvent/StFgtCollection.h"
#include "StFgtIClusterAlgo.h"
#include "StEvent/StFgtHit.h"
#include "StRoot/StFgtUtil/geometry/StFgtGeom.h"
#include "StFgtSeededClusterAlgo.h"
#include "StRoot/StFgtDbMaker/StFgtDbMaker.h"
#include "StRoot/StFgtDbMaker/StFgtDb.h"

/*void StFgtClusterMaker::Clear(Option_t *opts)
{

};*/


StFgtClusterMaker::StFgtClusterMaker( const Char_t* name ) : StMaker(name),mClusterAlgoPtr(0), mDb(0)
{
  // noop
};

StFgtClusterMaker::~StFgtClusterMaker()
{
  // noop
};


Int_t StFgtClusterMaker::Make()
{
    Int_t ierr = kStOk;
    //   cout <<"cluster maker " <<endl;
   StEvent* eventPtr = 0;
   eventPtr = (StEvent*)GetInputDS("StEvent");

   if( !eventPtr ) {
      LOG_ERROR << "Error getting pointer to StEvent from '" << ClassName() << "'" << endm;
      ierr = kStErr;
   }

   StFgtCollection* fgtCollectionPtr = 0;

   if( eventPtr ) {
      fgtCollectionPtr=eventPtr->fgtCollection();
   }

   if( !fgtCollectionPtr) {
      LOG_ERROR << "Error getting pointer to StFgtCollection from '" << ClassName() << "'" << endm;
      ierr = kStErr;
   }

   if( !ierr ){
     for( UInt_t discIdx=0; discIdx<fgtCollectionPtr->getNumDiscs(); ++discIdx ){
        //LOG_INFO << "disc: " << discIdx << " has strips: " << fgtCollectionPtr->getNumStrips(discIdx) << endm;
       
       StFgtStripCollection *stripCollectionPtr = fgtCollectionPtr->getStripCollection( discIdx );
       StFgtHitCollection *hitCollectionPtr = fgtCollectionPtr->getHitCollection( discIdx );
       
       if( stripCollectionPtr && hitCollectionPtr ){
	 //	 cout <<"doing clustering ..." <<endl;
	 Int_t loc_ierr = mClusterAlgoPtr->doClustering(*fgtCollectionPtr, *stripCollectionPtr, *hitCollectionPtr );
	 //	 cout <<"done ..." <<endl;
	 if(loc_ierr!=kStOk) {
	   LOG_WARN <<"StClusterMaker::Make(): clustering for disc " << discIdx << " returned " << loc_ierr <<endm;
	   if(loc_ierr>ierr)
	     ierr=loc_ierr;
	 }
       }
       ///after building the clusters (StFgtHits)
       for( StSPtrVecFgtHitIterator it=fgtCollectionPtr->getHitCollection(discIdx)->getHitVec().begin();it!=fgtCollectionPtr->getHitCollection(discIdx)->getHitVec().end();++it)
	 {
	   Int_t centralStripGeoId=(*it)->getCentralStripGeoId();
	   Short_t disc, quad;
	   Char_t layer;
	   Double_t ordinate, lowerSpan, upperSpan;
	   Float_t mR=0.0;
	   Float_t mPhi=0.0;
	   Float_t mErrR=0.0;
	   Float_t mErrPhi=0.0;

	   StFgtGeom::getPhysicalCoordinate( centralStripGeoId, disc, quad, layer, ordinate, lowerSpan, upperSpan );
	   if( layer == 'R' ){
	     mR = (*it)->getPositionR();

	     mErrR = (*it)->getErrorR();
	     // mid point of the strip
	     mPhi = 0.5*(upperSpan + lowerSpan);  
	     mErrPhi = upperSpan - lowerSpan;      // length of the strip
	   } else {
	     mPhi = (*it)->getPositionPhi();
	     mErrPhi = (*it)->getErrorPhi();
	     mR = 0.5*(upperSpan + lowerSpan);   // mid point of the strip
	     mErrR = upperSpan - lowerSpan;      // length of the strip
	   };
	   //	   cout <<"r pos is: " << mR << " phi : " << mPhi <<endl;
	   mPhi+=StFgtGeom::phiQuadXaxis(quad);
	   if(mPhi>TMath::Pi())
	     mPhi-=(2*TMath::Pi());
	   if(mPhi<((-1)*TMath::Pi()))
	     mPhi+=(2*TMath::Pi());

	   (*it)->setPositionR(mR);
	   (*it)->setErrorR(mErrR);
	   (*it)->setPositionPhi(mPhi);
	   (*it)->setErrorPhi(mErrPhi);

	   (*it)->setPositionZ(StFgtGeom::getDiscZ(disc));
	   (*it)->setErrorZ(0.2); // the thickens of sensitive volume (2mm), Jan

	   //printf("CLM:  ev= %6d centStrgeoId= %5d, disc=%d at phi=%f and Z=%f Z2=%f, phi1=%f  phi2=%f\n",GetEventNumber(), centralStripGeoId,disc,StFgtGeom::phiQuadXaxis(quad), StFgtGeom::getDiscZ(disc),(*it)->getPositionZ(),mPhi,(*it)->getPositionPhi());

	 }
       ///////

     }
   }
   LOG_DEBUG << "End of fgt-clust-maker, print all strips & clusters: " << endm;
   LOG_DEBUG <<"  fgtCollnumDisc=" << fgtCollectionPtr->getNumDiscs()<<", tot strip=" <<fgtCollectionPtr->getNumStrips()<<"  totClust=" <<  fgtCollectionPtr->getNumHits() <<endm;
   for(int iDisc=0; iDisc <(int)fgtCollectionPtr->getNumDiscs(); iDisc++) {

    LOG_DEBUG <<"  content: iDisc="<<iDisc<< " # of : strips="<<fgtCollectionPtr->getNumStrips(iDisc) <<"  hits=" <<fgtCollectionPtr-> getNumHits( iDisc)<<endm;
  
    // ..... print all strips ....
    StFgtStripCollection *stripPtr= fgtCollectionPtr->getStripCollection(iDisc);
    StSPtrVecFgtStrip &stripVec = stripPtr->getStripVec();    
    int ih=0;
    for( StSPtrVecFgtStripIterator it=stripVec.begin();it!=stripVec.end();++it, ih++)    {
     // details of strip localization, use output variables ending w/ X
      Short_t discX,  quadrantX,  stripX; Char_t  layerX;
      StFgtGeom::decodeGeoId(((*it))->getGeoId(),discX,quadrantX, layerX, stripX);
      int octX=1; if (stripX<300) octX=0;
      LOG_DEBUG << "iDisc="<<iDisc<<" ih=" <<ih <<"   strip: geoId=" <<((*it))->getGeoId()<< " ADC=" <<((*it))->getAdc()<<"  charge=" << ((*it))->getCharge()<< " 1f decode0: strip=" <<stripX << " quad="<<quadrantX << " oct=" <<octX << " plane="<<layerX<<" disc="<<discX<<endm;
    }
    
    // ..... print all 1D clusters (aka FGT HITs) ....
    StFgtHitCollection *clustPtr= fgtCollectionPtr->getHitCollection(iDisc);
    StSPtrVecFgtHit &clustVec = clustPtr->getHitVec();    
    ih=0;
    for( StSPtrVecFgtHitIterator it=clustVec.begin();it!=clustVec.end();++it, ih++)    {
      // details of central strip localization, use output variables ending w/ X
      Short_t discX,  quadrantX,  stripX; Char_t  layerX;
      StFgtGeom::decodeGeoId(((*it))->getCentralStripGeoId(),discX,quadrantX, layerX, stripX);
      int octX=1; if (stripX<300) octX=0;
      
      LOG_DEBUG <<"iDisc="<<iDisc<<" ih="<<ih<<"  clust  quad="<<((*it))->getQuad()<<", layer="<<((*it))->getLayer()<<" totCharge="<<(*it)->charge()<< " R/cm="<<(*it)->getPositionR() <<" +/-"<<(*it)->getErrorR() <<" Phi/rad="<<(*it)->getPositionPhi()<<" +/- " << (*it)->getErrorPhi()<< "  Z/cm= " << (*it)->getPositionZ() <<" +/-  "<<(*it)->getErrorZ()<<" centStripId=" << (*it)->getCentralStripGeoId() <<"decode0: strip= " << stripX <<" quad="<<quadrantX <<" oct="<<octX <<"plane="<< layerX<<"  disc=" <<discX <<endm;
    }
    
    
  }
  
  return ierr;

}


Int_t StFgtClusterMaker::setClusterAlgo(StFgtIClusterAlgo* algo)
{
  mClusterAlgoPtr=algo;
  return kStOk;
}

Int_t StFgtClusterMaker::InitRun(Int_t runnumber)
{
   Int_t ierr = kStOk;
  if( !mClusterAlgoPtr ){
     LOG_INFO << "No fgt cluster algorithm specified, using default seededAlgo" << endm;
     mClusterAlgoPtr=new StFgtSeededClusterAlgo();
  }

  if( !ierr )
    {
      LOG_INFO << "StFgtClusterMaker::InitRun for "  << runnumber << endm;
      if( !mDb ){
	LOG_INFO << "No fgtDb yet, trying to get a hold" << endm;
	//StFgtDbMaker *fgtDbMkr = static_cast< StFgtDbMaker* >( GetMakerInheritsFrom( "StFgtDbMaker" ) );
	StFgtDbMaker *fgtDbMkr = static_cast<StFgtDbMaker * >( GetMaker("fgtDb"));
	if( !fgtDbMkr ){
	  LOG_FATAL << "StFgtDb not provided and error finding StFgtDbMaker" << endm;
	  ierr = kStFatal;

	} else {
	  mDb = fgtDbMkr->getDbTables();

	  if( !mDb ){
            LOG_FATAL << "StFgtDb not provided and error retrieving pointer from StFgtDbMaker '"
                      << fgtDbMkr->GetName() << endm;
            ierr = kStFatal;
	  } else {
	    LOG_INFO << "Got on hold on fgtDb, all OK" << endm;
	  }
	}
      }
    }
  mClusterAlgoPtr->setDb(mDb);
  return ierr;
}
Int_t StFgtClusterMaker::Init()
{
  //  cout <<"cluster init " <<endl;
  Int_t ierr = kStOk;

  //
  // Please, conside the Maker's SetMode() method (setting m_Mode but you can get
  // the value using Getmode() as well ... to switch between cluster agos.
  // Extrenal setting a-la fgtClusMkr->setClusterAlgo( seededClusAlgo ); is
  // not appropriate for a chain mades maker.
  //
  if( !mClusterAlgoPtr ){
     LOG_INFO << "No fgt cluster algorithm specified, using default seededAlgo" << endm;
     mClusterAlgoPtr=new StFgtSeededClusterAlgo();
  }

  if( !ierr )
     ierr = mClusterAlgoPtr->Init();


  if( !ierr )
    {
      LOG_INFO << "StFgtClusterMaker::Init  "  << endm;
      if( !mDb ){
	LOG_INFO << "No fgtDb yet, trying to get a hold" << endm;
	//StFgtDbMaker *fgtDbMkr = static_cast< StFgtDbMaker* >( GetMakerInheritsFrom( "StFgtDbMaker" ) );
	StFgtDbMaker *fgtDbMkr = static_cast<StFgtDbMaker * >( GetMaker("fgtDb"));
	if( !fgtDbMkr ){
	  LOG_FATAL << "StFgtDb not provided and error finding StFgtDbMaker" << endm;
	  ierr = kStFatal;

	} else {
	  mDb = fgtDbMkr->getDbTables();

	  if( !mDb ){
            LOG_FATAL << "StFgtDb not provided and error retrieving pointer from StFgtDbMaker '"
                      << fgtDbMkr->GetName() << endm;
            ierr = kStFatal;
	  } else {
	    LOG_INFO << "Got on hold on fgtDb, all OK" << endm;
	  }
	}
      }
    }
  mClusterAlgoPtr->setDb(mDb);

  return ierr;
}
  

Int_t StFgtClusterMaker::Finish()
{
  cout <<"cluster maker finish!" << endl;
  Int_t ierr=kStOk;
  if( mClusterAlgoPtr ){
     ierr = mClusterAlgoPtr->Finish();
  }
  return ierr;
}

  

    
ClassImp(StFgtClusterMaker);
    

//   $Log: StFgtClusterMaker.cxx,v $
//   Revision 1.37  2013/02/20 23:33:28  avossen
//   add strips on both sides of the cluster
//
//   Revision 1.36  2013/02/20 01:32:27  avossen
//   added n strips before and after cluster
//
//   Revision 1.35  2013/02/19 18:24:04  avossen
//   *** empty log message ***
//
//   Revision 1.34  2012/12/10 23:18:00  avossen
//   merged cluster finder
//
//   Revision 1.33  2012/07/31 21:45:25  jeromel
//   Misc reshapes
//
//   Revision 1.32  2012/04/13 18:43:13  sgliske
//   Commented out a LOG_INFO that should have been a LOG_DEBUG
//   but isn't really needed
//
//   Revision 1.31  2012/03/08 17:43:40  avossen
//   added default cluster algo, made StFgtIClusterAlgo destructor =0
//
//   Revision 1.30  2012/03/07 22:08:15  avossen
//   added default cluster algo
//
//   Revision 1.29  2012/03/07 03:57:22  avossen
//   various updates
//
//   Revision 1.28  2012/02/28 19:32:25  avossen
//   many changes to enable new clustering algo: New strip fields, identification of seed strips, passing neighboring strips, new order in strip collections
//
//   Revision 1.27  2012/01/06 17:56:07  sgliske
//   oops--didn't intend to commit the last one.
//   this commit undoes that.
//
//   Revision 1.25  2011/11/09 17:50:10  balewski
//   more printout
//
//   Revision 1.24  2011/11/04 19:31:53  balewski
//   fixed Z problem, by circumventing the bug in the set methods
//
//   Revision 1.23  2011/11/04 19:00:22  avossen
//   changed phi from local to global phi
//
//   Revision 1.22  2011/11/04 17:38:12  balewski
//   more printouts
//
//   Revision 1.21  2011/11/04 17:01:19  balewski
//   *** empty log message ***
//
//   Revision 1.20  2011/11/03 21:18:28  balewski
//   more printout
//
//   Revision 1.19  2011/11/03 20:04:17  avossen
//   updated clustering makers and algos to reflect new containers
//
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
