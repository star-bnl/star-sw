/*!
 * \class StFgtJanGainMaker 
 * \author Jan Balewski , February 2012
 *
 ***************************************************************************
 *
 * Description: detects & counts bad APVs in FGT events
 *
 ***************************************************************************/

#include <string>
#include "StFgtJanGainMaker.h"
#include "StRoot/StEvent/StFgtCollection.h"
#include "StRoot/StEvent/StFgtStrip.h"
#include "StRoot/StEvent/StEvent.h"
#include "StRoot/StFgtUtil/geometry/StFgtGeom.h"
#include "StRoot/StFgtDbMaker/StFgtDbMaker.h"

//========================================================
//========================================================
//========================================================

StFgtJanGainMaker::StFgtJanGainMaker(  const Char_t* dbMkrName ,const Char_t* name  ) : StMaker( name ), mDbMkrName( dbMkrName ), mFgtDbMkr(0) {
  HList=0;
};


//========================================================
//========================================================
//========================================================
Int_t StFgtJanGainMaker::Init(){
  assert(HList);

   GetEvtHddr()->SetEventNumber(1);

   StEvent* eventPtr=0;
   eventPtr= (StEvent*)GetInputDS("StEvent");
   assert(eventPtr);
   
   mFgtCollectionPtr=eventPtr->fgtCollection();
   assert(mFgtCollectionPtr);
   mFgtDbMkr = static_cast< StFgtDbMaker* >( GetMaker( mDbMkrName.data() ));   
   assert(mFgtDbMkr );
   
   LOG_INFO << "Using date and time " << mFgtDbMkr->GetDateTime().GetDate() << ", "
	    << mFgtDbMkr->GetDateTime().GetTime() << endm;
   
   iEvt=-1;
   initHistos();
   return kStOk;
};


//________________________________________________
//________________________________________________
void
StFgtJanGainMaker::initHistos(){
  //  const float PI=TMath::Pi();

  //...... data histograms
  memset(hA,0,sizeof(hA));
   hA[5]=new TH1F("fgt1","Seen APVs per event; # APVs/event",150,-0.5,149.5);

   
   // add histos to the list (if provided)
   for(int i=0;i<mxHA;i++) {
     if(  hA[i]==0) continue;
     HList->Add( hA[i]);
   }
   //  HList->ls();

}

//========================================================
//========================================================
//========================================================
Int_t StFgtJanGainMaker::Make(){

   enum {Ntimebin=7};
   Int_t adcA[Ntimebin];
   int nPulse=0;
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
	 adcA[is]=(*stripIter)->getAdc(is)-ped;
	 if(adcA[is]<minAdc) { minAdc=adcA[is]; iMin=is;}
	 if(adcA[is]>maxAdc) maxAdc=adcA[is];
	 sum+=adcA[is];
       }

       //printf("mm %f %f\n",ped,maxAdc);
       if(maxAdc<600) continue;
       if(maxAdc>3000) continue;
       if(iMin>1) continue; // require ped is in time bin 0 or 1
       
       // save histo with pulse-shape
       nPulse++;
       TH1F * hsp=new TH1F(Form("ps%d_%d",iEvt,nPulse), Form("ieve=%d rdo=%d arm=%d apv=%d ch=%d geoId=%d     strip=%d%c%c%03d; time bin",iEvt,rdo,arm,apv,chn,geoId,disk+1,quad+'A',layer,strip),Ntimebin+2,0,Ntimebin+2);
       for(Int_t is=0;is<Ntimebin;is++){
	 hsp->SetBinContent(is+1,adcA[is]-ped);
	 hsp->SetBinError(is+1,pedSig);
       }
       HList->Add(hsp);
       // end of histo-save

       char star=' ';
       if(abs(lastGeo-geoId)==1) star='*';
       lastGeo=geoId;
       printf("\nieve=%d rdo=%d arm=%d apv=%d ch=%d geoId=%d     strip=%d%c%c%03d  %csumADC-ped=%.1f\n    adc-ped[0...6]=",iEvt,rdo,arm,apv,chn,geoId,disk+1,quad+'A',layer,strip,star,sum);
       for(Int_t is=0;is<Ntimebin;is++) printf(" %.0f ",adcA[is]-minAdc);
       printf("\n    dbPd=%.1f minAdc=%.1f  del=%.1f\n",ped,minAdc,ped-minAdc);       
       }
     }
   }
   
   
   return kStOK;
};


Int_t StFgtJanGainMaker::Finish(){
  
  cout << "StFgtJanGainMaker::Finish()" << endl;
  return StMaker::Finish();
};




ClassImp( StFgtJanGainMaker );

/**************************************************************************
 *
 * $Log: StFgtJanGainMaker.cxx,v $
 * Revision 1.1  2012/02/07 08:25:29  balewski
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
