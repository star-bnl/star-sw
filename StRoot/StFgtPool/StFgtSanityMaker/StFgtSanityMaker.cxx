/*!
 * \class StFgtTimeShapeMaker 
 * \author Jan Balewski , February 2012
 *
 ***************************************************************************
 *
 * Description: detects & counts bad APVs in FGT events
 *
 ***************************************************************************/

#include <string>
#include "StFgtSanityMaker.h"
#include "StRoot/StEvent/StFgtCollection.h"
#include "StRoot/StEvent/StFgtStrip.h"
#include "StRoot/StEvent/StEvent.h"
#include "StRoot/StFgtUtil/geometry/StFgtGeom.h"
#include "StRoot/StFgtDbMaker/StFgtDbMaker.h"

//========================================================
//========================================================
//========================================================

StFgtSanityMaker::StFgtSanityMaker( const Char_t* name , const Char_t* dbMkrName  ) : StMaker( name ), mDbMkrName( dbMkrName ), mFgtDbMkr(0) {
};


//========================================================
//========================================================
//========================================================
Int_t StFgtSanityMaker::Init(){


   GetEvtHddr()->SetEventNumber(1);

   StEvent* eventPtr=0;
   eventPtr= (StEvent*)GetInputDS("StEvent");
   
   mFgtCollectionPtr=NULL;
   if(eventPtr) {
     mFgtCollectionPtr=eventPtr->fgtCollection();
   } else {
     eventPtr=new StEvent();
     AddData(eventPtr);
     mFgtCollectionPtr=eventPtr->fgtCollection();
   };
   if(!mFgtCollectionPtr) {
     mFgtCollectionPtr=new StFgtCollection();
     eventPtr->setFgtCollection(mFgtCollectionPtr);
     LOG_DEBUG <<"::prepareEnvironment() has added a non existing StFgtCollection()"<<endm;
   } else {
     //this should be unncessary if the member clear function is called
     mFgtCollectionPtr->Clear();
   };
   
   mFgtDbMkr = static_cast< StFgtDbMaker* >( GetMaker( mDbMkrName.data() ) );
   
   if( !mFgtDbMkr ){
     LOG_FATAL << "Error finding mFgtDbMkr named '" << mDbMkrName << "'" << endm;
     assert(1==2);
   };

   
   LOG_INFO << "Using date and time " << mFgtDbMkr->GetDateTime().GetDate() << ", "
	    << mFgtDbMkr->GetDateTime().GetTime() << endm;
   
   iEvt=-1;
   hh=new TH1F("fgt1","Seen APVs per event; # APVs/event",150,-0.5,149.5);

   
   return kStOk;
};

//========================================================
//========================================================
//========================================================

Int_t StFgtSanityMaker::Make(){

   Int_t rdo;
   Int_t arm;
   Int_t apv;
   Int_t chn;
   Short_t disk;
   Short_t quad;
   Short_t strip;
   Short_t stat;
   Double_t ordinate;
   Double_t lowerSpan;
   Double_t upperSpan;
   Char_t layer;
   Double_t ped;
   Double_t pedSig;
   enum {Ntimebin=7};
   Int_t adcA[Ntimebin];
 
   iEvt++; 
   // cout << "iEvt = " << iEvt << endl;
   assert( mFgtDbMkr );
   StFgtDb *fgtTables = mFgtDbMkr->getDbTables();
   assert(fgtTables);  

   StEvent* eventPtr =  (StEvent*)GetInputDS("StEvent");
   assert(eventPtr);
   enum {kFgtMaxApvId=kFgtApvsPerAssembly*2, kFgtOctPerDisc=kFgtNumQuads*2};

   int chCntDet[kFgtNumDiscs][kFgtNumQuads][kFgtApvsPerAssembly*2]; 
   memset(chCntDet,0,sizeof(chCntDet));
   int chCntDaq[  kFgtNumRdos ][kFgtNumArms ][kFgtApvsPerAssembly*2];
   memset(chCntDaq,0,sizeof(chCntDaq));
 

   mFgtCollectionPtr = mFgtCollectionPtr=eventPtr->fgtCollection();
 
   assert( mFgtCollectionPtr);
   for( UInt_t discIdx=0; discIdx<mFgtCollectionPtr->getNumDiscs(); ++discIdx ){
     StFgtStripCollection *stripCollectionPtr = mFgtCollectionPtr->getStripCollection( discIdx );
     if( stripCollectionPtr ){
       StSPtrVecFgtStrip& stripVec = stripCollectionPtr->getStripVec();
       StSPtrVecFgtStripIterator stripIter;
       rdo=0;arm=-1;apv=-1;chn=-1;		 
       for( stripIter = stripVec.begin(); stripIter != stripVec.end(); ++stripIter ){	      
	 rdo=0;arm=apv=chn=stat=-1;	
	 disk=quad=strip=-1.;layer=' ';
	 ordinate=lowerSpan=upperSpan=-1.;
	 
	 (*stripIter)->getElecCoords( rdo, arm, apv, chn );      
	 stat=fgtTables->getStatusFromElecCoord(rdo,arm,apv,chn);
	 int geoId=fgtTables->getGeoIdFromElecCoord(rdo, arm, apv, chn);
	 //
	 assert (geoId>=0);
	 StFgtGeom::decodeGeoId(geoId,disk,quad,layer,strip);
	 StFgtGeom::getPhysicalCoordinate(geoId,disk,quad,layer,ordinate,lowerSpan,upperSpan);

	 chCntDet[disk][quad][apv]++;
	 chCntDaq[rdo-1][arm][apv]++;
	 
	 ped=99999.;pedSig=0.;
	 ped=fgtTables->getPedestalFromElecCoord(rdo,arm,apv,chn);
	 pedSig=fgtTables->getPedestalSigmaFromElecCoord(rdo,arm,apv,chn);
	 memset(adcA,0,sizeof(adcA));
	 int up=0;
	 if(rdo==2 && chn!=127)//tmp
	 for(Int_t is=0;is<Ntimebin;is++){
	   adcA[is]=(*stripIter)->getAdc(is);
	   adcA[is]-=ped;
	   if(adcA[is]<1000) continue;
	   if(adcA[is]>2500) continue;
	   up++;
	 }
	 if(up) {
	   printf("\nieve=%d rdo=%d arm=%d apv=%d ch=%d geoId=%d \n    adc-spd[0...6]=",iEvt,rdo,arm,apv,chn,geoId);
	   for(Int_t is=0;is<Ntimebin;is++) printf(" %d ",adcA[is]);
	   printf("\n");
	 }
       }
     }
   }

   if(iEvt==0) {
   printf("JJ eventID=%d FGT sanity (detector view)\n",eventPtr->id());
   for(int idisc=0;idisc<kFgtNumDiscs;idisc++)
     for(int iquad=0;iquad<kFgtNumQuads;iquad++){
       printf("disc=%d quad=%c   ",idisc+1,iquad+'A');
       for(int iapv=0;iapv<kFgtApvsPerAssembly*2;iapv++){
	 if(chCntDet[idisc][iquad][iapv]<=0) continue;
	 printf(" apv%02d:%3d, ",iapv,chCntDet[idisc][iquad][iapv]);
       }
       printf("\n");
     }

   printf("\nJJ eventID=%d FGT sanity (Daq view)\n",eventPtr->id());
   for(int irdo=0;irdo<kFgtNumRdos; irdo++)
     for(int iarm=0;iarm<kFgtNumArms;iarm++) {
       printf("RDO=%d arm=%d  apv:nCh  ",irdo+1,iarm);
       for(int iapv=0;iapv<kFgtApvsPerAssembly*2;iapv++){
	 if(chCntDaq[irdo][iarm][iapv]<=0) continue;
	 printf(" %2d:%3d, ",iapv,chCntDaq[irdo][iarm][iapv]);
       }
       printf("\n");
     }
   }// end of printout

   int nSeenApv=0;
   for(int irdo=0;irdo<kFgtNumRdos; irdo++)
     for(int iarm=0;iarm<kFgtNumArms;iarm++) 
       for(int iapv=0;iapv<kFgtApvsPerAssembly*2;iapv++)
	 if(chCntDaq[irdo][iarm][iapv]>64) nSeenApv++;
   hh->Fill(nSeenApv);    
   
   return kStOK;
};


Int_t StFgtSanityMaker::Finish(){
  
  cout << "StFgtSanityMaker::Finish()" << endl;
  hh->Draw();
  gPad->SetLogy();
  return StMaker::Finish();
};




ClassImp( StFgtSanityMaker );

/**************************************************************************
 *
 * $Log: StFgtSanityMaker.cxx,v $
 * Revision 1.1  2012/02/04 22:03:40  balewski
 * start
 *
 *
 **************************************************************************/
