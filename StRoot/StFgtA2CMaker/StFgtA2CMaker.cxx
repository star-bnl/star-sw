/***************************************************************************
 *
 * $Id: StFgtA2CMaker.cxx,v 1.52 2013/03/14 01:45:43 akio Exp $
 *
 ***************************************************************************
 *
 * Description: See header.
 *
 ***************************************************************************
 *
 * $Log: StFgtA2CMaker.cxx,v $
 * Revision 1.52  2013/03/14 01:45:43  akio
 * fix some kStFgtNumTimebins -> dynamic local mMaxTimeBin from StFgtCollection
 * Seed Type 3 & 4 changed, and 5 goone
 * 3 = 3 timbins in row above thr, tbin0<peak/3, last tbin<peak
 * 4 = 3 timbins in row above thr, tbin0<peak/3
 *
 * Revision 1.51  2012/11/27 17:32:51  akio
 * Adding option to read ped & status from text file. Default is reading from DB.
 *
 * Revision 1.50  2012/11/08 18:28:15  akio
 * - Split seedTypes3 into raising (kFgtSeedTypes3) and falling (kFgtSeedTypes4)
 * - Adding new seed Type (kFgtSeedTypes5) with 3 timebins in row above 3 sigma, and not raising nor falling
 *      You can disable this by setLeastRestrictiveSeed(false)
 * - Charge uncertainty factor can be adjusted by setPedSigFactor4Charge() [default is 1.0]
 *
 * Revision 1.49  2012/08/02 05:42:05  avossen
 * removed printout, set accept long pulses as default
 *
 * Revision 1.48  2012/08/02 05:41:06  avossen
 * *** empty log message ***
 *
 * Revision 1.47  2012/07/31 20:40:01  jeromel
 * Agreed with Anselm, changed default param - future should be Db based
 *
 * Revision 1.46  2012/07/31 20:08:11  jeromel
 * Changes to make maker compatible with running in chain (was not)
 *
 * Revision 1.45  2012/07/10 21:47:24  avossen
 * random update
 *
 * Revision 1.44  2012/07/06 01:12:17  avossen
 * implemented scaled pulse finder
 *
 * Revision 1.43  2012/07/05 21:49:44  avossen
 * *** empty log message ***
 *
 * Revision 1.42  2012/07/05 21:39:47  avossen
 * added flag to allow long pulses
 *
 * Revision 1.41  2012/06/14 12:36:49  avossen
 * *** empty log message ***
 *
 * Revision 1.40  2012/06/12 19:28:44  avossen
 * *** empty log message ***
 *
 * Revision 1.39  2012/05/10 14:37:19  avossen
 * modified pulse finder
 *
 * Revision 1.38  2012/04/17 17:47:51  avossen
 * changed seedType3
 *
 * Revision 1.37  2012/04/13 18:56:56  sgliske
 * More adjustments based on the review:
 * - Lastest StEvents from Thomas U.
 * - StFgtA2CMaker can no longer remove strips other than bad status or bad ped
 * - other related updates
 *
 * Revision 1.36  2012/03/21 19:25:06  sgliske
 * fixed bug in changed from geoId to elecId for DB lookups
 *
 * Revision 1.35  2012/03/07 18:34:29  sgliske
 * Missing a few default value in the constructor
 *
 * Revision 1.34  2012/03/07 18:07:18  sgliske
 * StFgtStrip::getClusterSeed() -> StFgtStrip::getClusterSeedType
 * StFgtStrip::setClusterSeed() -> StFgtStrip::setClusterSeedType
 *
 * Revision 1.33  2012/03/07 17:46:55  sgliske
 * Added options for not removing strips
 *
 * Revision 1.32  2012/03/07 17:09:05  sgliske
 * code removed from compiling by #ifdef completely removed
 *
 * Revision 1.31  2012/03/07 15:32:41  sgliske
 * Last update was commenting out 'strip->setType( 1 );'
 * But final version should not commented out code,
 * either real used code or real explanatory comments.
 * Commented 'strip->setType' has been removed.
 *
 * Revision 1.30  2012/03/07 03:57:22  avossen
 * various updates
 *
 * Revision 1.29  2012/03/06 21:21:17  sgliske
 * Responces to reviewers incoorperated.
 * White space and comments cleaned up.
 * Few remaining items offset with #ifdef,
 * which may get removed before final move to DEV
 *
 * Revision 1.28  2012/03/05 16:13:13  avossen
 * changed maxAdc default to -9999
 *
 * Revision 1.27  2012/03/05 03:42:00  avossen
 * added reset of max adc, so that the max adc contains the max adc after ped substraction
 *
 * Revision 1.26  2012/03/01 16:38:13  avossen
 * implemented tweaks to clustering
 *
 * Revision 1.25  2012/02/29 20:29:08  avossen
 * changes to seed and cluster algo
 *
 * Revision 1.24  2012/02/28 19:32:25  avossen
 * many changes to enable new clustering algo: New strip fields, identification of seed strips, passing neighboring strips, new order in strip collections
 *
 * Revision 1.23  2012/02/06 17:18:05  avossen
 * fixed negative charge clusters
 *
 * Revision 1.22  2012/02/02 15:38:58  sgliske
 * bu fixed: getAdc not in timebin loop
 *
 * Revision 1.21  2012/02/01 18:21:51  avossen
 * changed error on the charge to pedRMS and replaced fit with sum over timebins
 *
 * Revision 1.20  2012/02/01 17:56:33  avossen
 * changed error on the charge to pedRMS and replaced fit with sum over timebins
 *
 * Revision 1.19  2012/01/31 11:23:02  sgliske
 * If no cut on ped, than skip fit.
 * Still cut based on status mask in either case
 *
 * Revision 1.18  2012/01/31 08:26:53  sgliske
 * cleaned up, and removed need to use setFgtDb.
 * Now, if not set, will try to find it using
 * GetMakerInheritsFrom
 *
 * Revision 1.17  2012/01/30 21:49:33  avossen
 * removed references to files
 *
 * Revision 1.16  2012/01/30 13:38:38  sgliske
 * made mistake in last update.  Now it is fixed
 *
 * Revision 1.15  2012/01/30 13:08:37  sgliske
 * updated charge uncertainty to include portion from
 * adc Poisson uncertainty.
 *
 * Revision 1.14  2012/01/30 11:40:04  sgliske
 * a2cMaker now fits the pulse shape,
 * strip containers updated
 *
 * Revision 1.13  2012/01/30 10:42:22  sgliske
 * strip containers now contain adc values for
 * all time bins.  Also fixed bug where setType modified the timebin
 * rather than the type.
 *
 * Revision 1.12  2012/01/28 20:10:12  avossen
 * addec cluster uncertainty
 *
 * Revision 1.11  2012/01/28 11:22:53  sgliske
 * changed status check to status map
 * changed setDb to setFgtDb
 * cleaned up few other minor things
 *
 * Revision 1.10  2012/01/27 13:38:29  sgliske
 * updated to correspond with new Status/Ped readers,
 * Now keyed by elecId
 *
 * Revision 1.9  2012/01/24 06:52:45  sgliske
 * made status cuts optional
 * and updated status to a fail condition--
 * i.e. status == 0x0 is good, otherwise is bad.
 * WARNING--this may be different than that used at first
 * in for the cosmic test stand.
 *
 * Revision 1.8  2012/01/24 05:52:13  sgliske
 * Surrounded printf's with #ifdef,
 * cleaned up a little whitespace,
 * added strip->SetType( 1 );
 *
 * Revision 1.7  2012/01/04 20:23:02  sgliske
 * fixed spelling of iDsic to iDisc
 *
 * Revision 1.6 2011/12/01 00:13:23 avossen
 * included use of db. Note: For DB use it hast to be set with setDb.
 * Instantiate StFgtDBMaker, get the StFgtDb from the getTables method
 * and give the pointer to the A2C maker
 *
 * Revision 1.5  2011/11/25 20:24:13  ckriley
 * added statusmaker functionality
 *
 * Revision 1.4  2011/11/17 18:40:40  sgliske
 * Bug fixed: need to always call stripCollectionPtr->removeFlagged();
 * Also implemented check to invalidate strip if weird ped. value
 *
 * Revision 1.3  2011/11/04 17:01:06  balewski
 * added printouts
 *
 * Revision 1.2  2011/11/01 18:46:14  sgliske
 * Updated to correspond with StEvent containers, take 2.
 *
 * Revision 1.1  2011/10/28 14:58:49  sgliske
 * replacement to StFgtCorAdcMaker
 *
 *
 **************************************************************************/

#include "StRoot/StEvent/StEvent.h"
#include "StRoot/StEvent/StFgtCollection.h"
#include "StRoot/StEvent/StFgtStripCollection.h"
#include "StRoot/StEvent/StFgtStrip.h"
#include "StRoot/StFgtDbMaker/StFgtDbMaker.h"
#include "StRoot/StFgtDbMaker/StFgtDb.h"
#include "StFgtA2CMaker.h"

/// Class constructors - does nothing else than setting name
StFgtA2CMaker::StFgtA2CMaker( const Char_t* name ) : StMaker( name ), mAcceptLongPulses(true), 
						     mStatusMask(0xff), mAbsThres(-10000), mRelThres(4.0),mClusterThreshold(1.0), 
						     mPedSigFactor4Charge(1.0), mUseLeastRestrictiveSeed(true) ,mReadPedFile(0), mReadStatusFile(0), mDb(0) {
  // do nothing
}

/// destructor - really does nothing
StFgtA2CMaker::~StFgtA2CMaker()
{
  // nothing to do
}


/// Does nothing else than printing "we are here"
Int_t StFgtA2CMaker::Init(){
   LOG_INFO << "StFgtA2CMaker::Init we are named "  << GetName() << endm;
   return kStOk;
}

/// Get pointer to fgtDb
Int_t StFgtA2CMaker::InitRun(Int_t runumber){
   Int_t ierr = kStOk;

   LOG_INFO << "StFgtA2CMaker::InitRun for "  << runumber << endm;
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
   //TString a("dbdump.txt");
   //mDb->printFgtDumpCSV1(a,1,1);
   return ierr;
}


Int_t StFgtA2CMaker::Make(){
   Int_t ierr = kStOk;

   if( !mDb ){
      // warning message already given in init,
      // so just silently skip the event
      return kStFatal;
   }
   //   cout <<"in a2cmaker " << endl;
   StEvent* eventPtr = 0;
   eventPtr = (StEvent*) GetInputDS("StEvent");

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
      mMaxTimeBin=fgtCollectionPtr->getNumTimeBins();
      if(mMaxTimeBin==0) return 0;
      for( UInt_t discIdx=0; discIdx<fgtCollectionPtr->getNumDiscs(); ++discIdx ){
	//cout <<"looking at disc: " << discIdx << endl;
         StFgtStripCollection *stripCollectionPtr = fgtCollectionPtr->getStripCollection( discIdx );
         if( stripCollectionPtr ){
	    //cout <<"got strip coll" <<endl;
            StSPtrVecFgtStrip& stripVec = stripCollectionPtr->getStripVec();
            StSPtrVecFgtStripIterator stripIter;
	    //cout <<stripVec.size() <<" strips " << endl;
            for( stripIter = stripVec.begin(); stripIter != stripVec.end(); ++stripIter ){
	      //cout <<" running over strips .. " <<endl;
               StFgtStrip *strip = *stripIter;
               Float_t ped = 0, pedErr = 0;
               if( strip ){
                  Int_t nTbAboveThres = 0;

                  //set max adc back so that the new max adc is set by the adc -ped
                  strip->setMaxAdc(-9999);

                  Int_t rdo, arm, apv, chan; 
                  strip->getElecCoords( rdo, arm, apv, chan ); 
                  Int_t elecId = StFgtGeom::encodeElectronicId( rdo, arm, apv, chan );

                  // sum of adc-ped values
                  Float_t sumC=0;

                  // get the pedestal
		  if(mPedFilename.empty()){
		    ped = mDb->getPedestalFromElecId( elecId );
		    pedErr = mDb->getPedestalSigmaFromElecId( elecId );
	          }else{
		    readPedFile(elecId,ped,pedErr);
		  }

                  strip->setPed(ped);
                  strip->setPedErr(pedErr);
		  //cout <<"we got ped: " << ped << " error: " << pedErr <<endl;

                  if( ped > kFgtMaxAdc || ped < 0 ){
                    strip->setGeoId( -1 );      // flag for removal
                  } else {
                     for( Int_t timebin = 0; timebin < mMaxTimeBin && strip->getGeoId() > -1; ++timebin ){
                        Int_t adc = strip->getAdc( timebin );

                        // subract the ped, and set
                        Int_t adcMinusPed = adc - ped;
                        strip->setAdc(adcMinusPed, timebin );

                        // sum over all (averages out fluctuations), but avoid invalid tb with large negative adc values
                        if( adcMinusPed > -1000)
                           sumC += adcMinusPed;

                        if( (mRelThres && adcMinusPed > mRelThres*pedErr) || (mAbsThres>-kFgtMaxAdc && adcMinusPed > mAbsThres)) {
                           // only add if it is above pedestal, otherwise negative values can be added...
                           ++nTbAboveThres;
                        }
                     }
                  }

		  //		    if(strip->getGeoId() >=13092 && strip->getGeoId()<=13105)
		  //		      cout <<"" <<endl;

                  // get gain
                  Double_t gain = mDb->getGainFromElecId( elecId );

                  // set the charge
                  strip->setCharge( sumC/gain );
		  int idebug=0;
		  //if(sumC/gain==4713.0 || sumC/gain==5250.0) {
		  // idebug=1;
		  // printf("charge=%f\n ",sumC/gain);
		  //}
                  // for seven timebins... change to some variable...., but does this actuall make sense for high nTB?? then the 
                  // error on the charge is higher than it should be.... (Anselm)
                  strip->setChargeUncert(gain ? mPedSigFactor4Charge*sqrt(7)*pedErr/gain : 10000);

                  // check if any signal here
                  if( !nTbAboveThres && (mRelThres || mAbsThres>-kFgtMaxAdc) ){
                     // No time bins above thresholds for this strip
                     // and thresholds are set, thus no signal
                     strip->setClusterSeedType(kFgtSeedTypeNo);

                  } else if( mRelThres || mAbsThres>-kFgtMaxAdc ){
                     // but if it is +/- n strips from valid pulse, keep it
		    //		    if(strip->getGeoId() >=13092 && strip->getGeoId()<=13105)
		    //		      cout <<"checking pulse for geoID:  " << strip->getGeoId() <<" adc : " << strip->getAdc(0)<<" " << strip->getAdc(1) <<" " << strip->getAdc(2)<<" " << strip->getAdc(3)<<" " << strip->getAdc(4)<<" " << strip->getAdc(5)<<" " << strip->getAdc(6)<<endl;
                     strip->setClusterSeedType(checkValidPulse(strip, pedErr));		    		 

                  } else {
                     strip->invalidateCharge();
                  };
		  
                  if( mStatusMask != 0x0 ){
		    UInt_t status;
		    if(mStatusFilename.empty()){
		      status=mDb->getStatusFromElecId( elecId );
		    }else{
		      readStatusFile(elecId,status);
		    }
		    if( status & mStatusMask )
		      strip->setClusterSeedType(kFgtDeadStrip);
                  }

		  //if(idebug==1){
		    //if(nTbAboveThres>2 && strip->getCharge()>500 && strip->getClusterSeedType()==0)
		    //printf("geoid=%5d %4.1f %4.1f %4.1f %4.1f %4.1f %4.1f %4.1f  gain=%3.1f sum=%6.1f pedrms=%6.1f nTbAboveThres=%1d  type=%5d\n",
		  //   strip->getGeoId(),
		  //   strip->getAdc(0)/pedErr,
		  //   strip->getAdc(1)/pedErr,
		  //   strip->getAdc(2)/pedErr,
		  //   strip->getAdc(3)/pedErr,
		  //   strip->getAdc(4)/pedErr,
		  //   strip->getAdc(5)/pedErr,
		  //   strip->getAdc(6)/pedErr,
		  //   gain,
		  //   strip->getCharge(),
		  //   pedErr,
		  //   nTbAboveThres,
		  //   strip->getClusterSeedType());
		  //}
		  
		  //	    if(strip->getGeoId() >=13092 && strip->getGeoId()<=13105)
		  //	      cout <<" seed type is: " << strip->getSeedType() <<endl;
               }

	    }


            // always check if any need removed, as it is possible
            // some ``bad'' strips may have abnormally large st. dev.
	    //this also removes dead strips
            stripCollectionPtr->removeFlagged();
         }
      }
   }

   return ierr;
}

/// Returns true for a valid pulse
Short_t StFgtA2CMaker::checkValidPulse( StFgtStrip* pStrip, Float_t ped ){

   if( ped <=0 )
      return false;

   Float_t peakAdc = -9999;
   Int_t leadEdgeBin = -9999;
   Float_t sumAdc = 0;
   Int_t numHighBins = 0;
   Int_t numAlmostHighBins = 0; //3 sigma
   Int_t numTailHighBins = 0; //2 sigma in the tails
   Int_t numHighBinsAfterLeadingEdge = 0; //3 sigma
   Int_t numPlateau = 0;
   Int_t numMaxPlateau = 0;
   Float_t prvAdc = -1;

   if(pStrip->getGeoId()<0) return kFgtSeedTypeNo;

   for( Int_t timebin = 0; timebin < mMaxTimeBin; ++timebin ) {
      Float_t adc=pStrip->getAdc(timebin);

      // to remove seeds where all tbs are high and close together
      if(prvAdc>0 && fabs(prvAdc-adc)<ped && adc>3*ped) {
         numPlateau++;
      }

      if(numPlateau>numMaxPlateau) {
         numMaxPlateau=numPlateau;
      } else {
         //end of plateau
         numPlateau=0;
      }

      prvAdc=adc;

      // this excludes the leading edge, don't count if there is a hole after the leading edge
      //      if(leadEdgeBin>=0 && adc>3*ped && (timebin-numHighBinsAfterLeadingEdge)>(leadEdgeBin+1))
      //         numHighBinsAfterLeadingEdge++;

      sumAdc += adc;

      if( leadEdgeBin<0 && adc>5*ped )
         leadEdgeBin=timebin;

      if(2<=timebin && timebin <=4 && peakAdc<adc)
         peakAdc = adc;
      if(2<=timebin && timebin <=4 && adc>5*ped)
         numHighBins++;
      if(2<=timebin && timebin <=4 && adc>3*ped)
         numAlmostHighBins++;
      if(5<=timebin && timebin <=6)
         numTailHighBins++;
   }

   //  deciding on max plateau
   if(!mAcceptLongPulses)
     {
       if(numMaxPlateau>=3) { //means basically 4 because we start counting after the first one
	 //cout <<"not accepting long pulses..." <<endl;
	 return kFgtSeedTypeNo;
       }
     }



   //most restrictive condition
   if(pStrip->getAdc(0) <3*ped && numHighBins==3 && peakAdc > pStrip->getAdc(6) && numAlmostHighBins>=3 && numHighBinsAfterLeadingEdge>=2) {
      return kFgtSeedType1;
   }

   if(pStrip->getAdc(0) <3*ped && numHighBins==2 && peakAdc > pStrip->getAdc(6)&& numHighBinsAfterLeadingEdge>=2)
      return kFgtSeedType2;

   //   if(pStrip->getAdc(0) <3*ped && numHighBins==1 && peakAdc > pStrip->getAdc(6)&& numHighBinsAfterLeadingEdge>=1&& numAlmostHighBins>=2)


   //Akio Adding a requirement that sum is above sqrt(3bins)*5*mClusterThreshold*pedrms
   if(pStrip->getCharge() < 1.732*mClusterThreshold*5*ped) return kFgtSeedTypeNo;

   float maxadc=0, adc[kFgtNumTimeBins]; 
   int maxt=-1;
   for( Int_t t=0; t < mMaxTimeBin; t++ ) {
     adc[t]=pStrip->getAdc(t);
     if(adc[t]>maxadc) {maxadc=adc[t]; maxt=t;}
   }
   if(maxt==0) return kFgtSeedTypeNo; //eaarly pulse
   if(maxt>=9) return kFgtSeedTypeNo; //late pulse
   int flag=0;
   for( Int_t t = 0; t < mMaxTimeBin-2; t++ ) {
     float thr=mClusterThreshold*5*ped;
     if(adc[t]>thr && adc[t+1]>thr && adc[t+2]>thr) {flag=1; break;}
     //found some sort of rising edge
     //if(adc1 < adc2 && adc2 < adc3) {iseed=10; break;}
     //falling edge for grossly out of time pulses
     //if(adc1 > adc2 && adc2 > adc3 && iseed<10) {iseed=9;}
     // Akio-- adding if charge sum is above x3 threshold
     //if(pStrip->getCharge()> mClusterThreshold*15*ped && mUseLeastRestrictiveSeed && iseed<9) {iseed=8;}   }
   }
   if(flag==0) return kFgtSeedTypeNo; //no 3 tbin in row above threshold
   if(adc[0]>=maxadc/3.0) return kFgtSeedTypeNo; //first timebin is not low enough
   //we may have a good pulse....
   if(adc[mMaxTimeBin-1]<maxadc) return kFgtSeedType3;  // saw falling edge as well
   return kFgtSeedType4;  //didn't see falling edge... but maybe ok?
   //switch(iseed){
   //case 10: return kFgtSeedType3;
   //case  9: return kFgtSeedType4;
   //case  8: return kFgtSeedType5;
   //default: return kFgtSeedTypeNo; 
   //}
   //   cout <<" no seed found! " << endl;
   //	      if(pStrip->getGeoId() >=13092 && pStrip->getGeoId()<=13105)
   //		cout <<"nope..." << endl;
}

void StFgtA2CMaker::readPedFile(Int_t elecid, Float_t &ped, Float_t &pedrms){
  if(mReadPedFile==0){
    LOG_INFO << "Reading pedestal from a text file ="<<mPedFilename.data()<<endm;
    int eid,tbin;
    float p,s;
    ifstream file;
    memset(mPed,0,sizeof(mPed));
    memset(mPedRMS,0,sizeof(mPedRMS));
    file.open(mPedFilename.data());
    if(file.is_open()){
      while(file.good()){
	file>>eid>>tbin>>p>>s;
	LOG_DEBUG<<"Reading Ped: "<<eid<<" "<<tbin<<" "<<p<<" "<<s<<endm;
	mPed[eid]=p;
	mPedRMS[eid]=s;
      }      
    }else{
      LOG_INFO<<"Reading pedestal from a text file failed"<<endm;
    }
    file.close();
    mReadPedFile=1;
  }
  ped=mPed[elecid];
  pedrms=mPedRMS[elecid];
}

void StFgtA2CMaker::readStatusFile(Int_t elecid, UInt_t &status){
  if(mReadStatusFile==0){
    LOG_INFO << "Reading status from a text file ="<<mStatusFilename.data()<<endm;
    unsigned int eid,stat;
    TString statread;
    ifstream file;
    memset(mStatus,1,sizeof(mStatus));
    file.open(mStatusFilename.data());
    if(file.is_open()){
      while(file.good()){
        file>>eid>>statread;
	statread.Remove(0,2);
	stat = statread.Atoi();
        LOG_DEBUG<<"Reading Status: "<<eid<<" "<<stat<<endm;
        mStatus[eid]=stat;
      }
    }else{
      LOG_INFO<<"Reading status from a text file failed"<<endm;
    }
    mReadStatusFile=1;
    file.close();
  }
  status=mStatus[elecid];
}

ClassImp(StFgtA2CMaker);
