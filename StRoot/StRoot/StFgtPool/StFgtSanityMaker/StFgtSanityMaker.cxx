/*!
 * \class StFgtSanityMaker 
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
   assert(eventPtr);
   
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

   enum {Ntimebin=7};
   Int_t adcA[Ntimebin];
   int chCntDet[kFgtNumDiscs][kFgtNumQuads][kFgtApvsPerAssembly*2]; 
   int chCntDaq[  kFgtNumRdos ][kFgtNumArms ][kFgtApvsPerAssembly*2];
   memset(chCntDet,0,sizeof(chCntDet));
   memset(chCntDaq,0,sizeof(chCntDaq));
 
   iEvt++; 
   // cout << "iEvt = " << iEvt << endl;
   assert( mFgtDbMkr );
   StFgtDb *fgtTables = mFgtDbMkr->getDbTables();
   assert(fgtTables);  

   StEvent* eventPtr =  (StEvent*)GetInputDS("StEvent");
   assert(eventPtr);
 
   mFgtCollectionPtr = mFgtCollectionPtr=eventPtr->fgtCollection();
   assert( mFgtCollectionPtr);
 
   int lastGeo=-999;
   for( UInt_t discIdx=0; discIdx<mFgtCollectionPtr->getNumDiscs(); ++discIdx ){
     StFgtStripCollection *stripCollectionPtr = mFgtCollectionPtr->getStripCollection( discIdx );
     if( stripCollectionPtr ){
       StSPtrVecFgtStrip& stripVec = stripCollectionPtr->getStripVec();
       StSPtrVecFgtStripIterator stripIter;

       for( stripIter = stripVec.begin(); stripIter != stripVec.end(); ++stripIter ){	      
       int rdo=0, arm=-1, apv=-1, chn=-1;		 
       Short_t disk=-1, quad=-1, strip=-1; char layer='x';
       double	 ordinate=-1,lowerSpan=-1,upperSpan=-1.;       
       (*stripIter)->getElecCoords( rdo, arm, apv, chn );      
       int stat=fgtTables->getStatusFromElecCoord(rdo,arm,apv,chn);
       if(stat) continue; // drop bad strips
       int geoId=fgtTables->getGeoIdFromElecCoord(rdo, arm, apv, chn);
	
       assert (geoId>=0);
       StFgtGeom::decodeGeoId(geoId,disk,quad,layer,strip);
       StFgtGeom::getPhysicalCoordinate(geoId,disk,quad,layer,ordinate,lowerSpan,upperSpan);

       chCntDet[disk][quad][apv]++;
       chCntDaq[rdo-1][arm][apv]++;

       // drop unstable or dead APVs - Jan's private list
       if(rdo==2 && arm==1 && apv==5) continue; 
       if(rdo==2 && arm==1 ) continue;
       if(rdo==2 && arm==2 && apv==18) continue;
       if(rdo==1 && arm==3 && apv==9) continue;
       if(rdo==2 && arm==3 ) continue;
       if(rdo==1 && arm==4 && apv==19) continue;
       if(rdo==1 && arm==4 && apv==21) continue;
       if(rdo==2 && arm==4 ) continue;
       
       double  ped=fgtTables->getPedestalFromElecCoord(rdo,arm,apv,chn);
       double  pedSig=fgtTables->getPedestalSigmaFromElecCoord(rdo,arm,apv,chn);

       memset(adcA,0,sizeof(adcA));
       float minAdc=9999, maxAdc=-9999, sum=0;
       int iMin=-1;
       for(Int_t is=0;is<Ntimebin;is++){
	 adcA[is]=(*stripIter)->getAdc(is);
	 if(adcA[is]<minAdc) { minAdc=adcA[is]; iMin=is;}
	 if(adcA[is]>maxAdc) maxAdc=adcA[is];
	 sum+=adcA[is];
       }

       //printf("mm %f %f\n",ped,maxAdc);
       maxAdc-=minAdc;
       sum-=minAdc*Ntimebin;
       if(maxAdc<600) continue;
       if(maxAdc>3000) continue;
       if(iMin>0) continue; // require ped is in time bin 0
       char star=' ';
       if(abs(lastGeo-geoId)==1) star='*';
       lastGeo=geoId;
       printf("\nieve=%d rdo=%d arm=%d apv=%d ch=%d geoId=%d     strip=%d%c%c%03d  %csumADC-ped=%.1f\n    adc-ped[0...6]=",iEvt,rdo,arm,apv,chn,geoId,disk+1,quad+'A',layer,strip,star,sum);
       for(Int_t is=0;is<Ntimebin;is++) printf(" %.0f ",adcA[is]-minAdc);
       printf("\n    dbPd=%.1f minAdc=%.1f  del=%.1f\n",ped,minAdc,ped-minAdc);       
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
 * Revision 1.5  2012/02/07 08:25:29  balewski
 * *** empty log message ***
 *
 * Revision 1.4  2012/02/07 06:14:42  balewski
 * *** empty log message ***
 *
 * Revision 1.3  2012/02/07 05:33:30  balewski
 * *** empty log message ***
 *
 * Revision 1.2  2012/02/06 04:17:36  balewski
 * added 2012 APV exclusions
 *
 * Revision 1.1  2012/02/04 22:03:40  balewski
 * start
 *
 *
 **************************************************************************/
