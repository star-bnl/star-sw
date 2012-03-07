/***************************************************************************
 *
 * $Id: StFgtA2CMaker.cxx,v 1.35 2012/03/07 18:34:29 sgliske Exp $
 * Author: S. Gliske, Oct 2011
 *
 ***************************************************************************
 *
 * Description: See header.
 *
 ***************************************************************************
 *
 * $Log: StFgtA2CMaker.cxx,v $
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

// constructors
StFgtA2CMaker::StFgtA2CMaker( const Char_t* name )
   : StMaker( name ), mRemoveNonPulse(1), mRemoveNonSignal(1), mStatusMask(0xFF), mAbsThres(-10000), mRelThres(5), mDb(0) { /* */ };


Int_t StFgtA2CMaker::Init(){
   Int_t ierr = kStOk;

   if( !mDb ){
      StFgtDbMaker *fgtDbMkr = static_cast< StFgtDbMaker* >( GetMakerInheritsFrom( "StFgtDbMaker" ) );
      if( !fgtDbMkr ){
         LOG_FATAL << "StFgtDb not provided and error finding StFgtDbMaker" << endm;
         ierr = kStFatal;
      };

      if( !ierr ){
         mDb = fgtDbMkr->getDbTables();

         if( !mDb ){
            LOG_FATAL << "StFgtDb not provided and error retrieving pointer from StFgtDbMaker '"
                      << fgtDbMkr->GetName() << endm;
            ierr = kStFatal;
         };
      };
   };

   return ierr;
};


Int_t StFgtA2CMaker::Make(){
   Int_t ierr = kStOk;

   if( !mDb ){
      // warning message already given in init,
      // so just silently skip the event
      return kStFatal;
   };

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
         StFgtStripCollection *stripCollectionPtr = fgtCollectionPtr->getStripCollection( discIdx );
         if( stripCollectionPtr ){
            StSPtrVecFgtStrip& stripVec = stripCollectionPtr->getStripVec();
            StSPtrVecFgtStripIterator stripIter;

            for( stripIter = stripVec.begin(); stripIter != stripVec.end(); ++stripIter ){
               StFgtStrip *strip = *stripIter;
               Float_t ped = 0, pedErr = 0;
               if( strip ){
                  Int_t nTbAboveThres = 0;

                  //set max adc back so that the new max adc is set by the adc -ped
                  strip->setMaxAdc(-9999);

                  Int_t geoId = strip->getGeoId();
                  // Later, switch geoId to elecId lookups, since DB keyed by
                  // elecId, as soon as function made available.  Also
                  // clean up computations of elecId in this code at
                  // the same time.

                  // sum of adc-ped values
                  Float_t sumC=0;

                  // get the pedestal
                  ped = mDb->getPedestalFromElecId( geoId );
                  pedErr = mDb->getPedestalSigmaFromElecId( geoId );
                  strip->setPed(ped);
                  strip->setPedErr(pedErr);

                  if( ped > 4096 || ped < 0 ){
                     strip->setGeoId( -1 );
                  } else {
                     for( Int_t timebin = 0; timebin < kFgtNumTimeBins && strip->getGeoId() > -1; ++timebin ){
                        Int_t adc = strip->getAdc( timebin );

                        // subract the ped, and set
                        Int_t adcMinusPed = adc - ped;
                        strip->setAdc(adcMinusPed, timebin );

                        // sum over all (averages out fluctuations), but avoid invalid tb with large negative adc values
                        if( adcMinusPed > -1000)
                           sumC += adcMinusPed;

                        if( (mRelThres && adcMinusPed > mRelThres*pedErr) || (mAbsThres>-4096 && adcMinusPed > mAbsThres)) {
                           // only add if it is above pedestal, otherwise negative values can be added...
                           ++nTbAboveThres;
                        };
                     };
                  };

                  // get gain
                  Double_t gain = mDb->getGainFromElecId( geoId );

                  // set the charge
                  strip->setCharge( sumC/gain );

                  // for seven timebins... change to some variable...., but does this actuall make sense for high nTB?? then the 
                  // error on the charge is higher than it should be.... (Anselm)
                  strip->setChargeUncert(gain ? sqrt(7)*pedErr/gain : 10000);

                  // check if any signal here
                  if( !nTbAboveThres && (mRelThres || mAbsThres>-4096) ){
                     // no time bins above thresholds for this strip
                     // i.e. no signal
                     strip->setClusterSeedType(kFgtSeedTypeNo);

                     // If not removing strips far from pulses, check
                     // if supposed to remove non-signal.  Flag geoId
                     // if this is the case.
                     if( !mRemoveNonPulse && mRemoveNonSignal )
                        strip->setGeoId( -1 );

                  } else if( mRelThres || mAbsThres>-4096 ){
                     // but if it is +/- n strips from valid pulse, keep it
                     strip->setClusterSeedType(checkValidPulse(strip, pedErr));
                  } else {
                     strip->invalidateCharge();
                  };

                  if( mStatusMask != 0x0 ){
                     UInt_t status=mDb->getStatusFromElecId(geoId);

                     if( status & mStatusMask )
                        strip->setClusterSeedType(kFgtDeadStrip);
                  };
               };
            };

            // always check if any need removed, as it is possible
            // some ``bad'' strips may have abnormally large st. dev.
            stripCollectionPtr->removeFlagged( mRemoveNonPulse );
         };
      };
   };

   return ierr;
};

/// Implementation of Jan's seed finder.
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

   for( Int_t timebin = 0; timebin < kFgtNumTimeBins && pStrip->getGeoId() > -1; ++timebin ) {
      Float_t adc=pStrip->getAdc(timebin);

      // to remove seeds where all strips are high and close together
      if(prvAdc>0 && fabs(prvAdc-adc)<ped && adc>3*ped) {
         numPlateau++;
      };

      if(numPlateau>numMaxPlateau) {
         numMaxPlateau=numPlateau;
      } else {
         //end of plateau
         numPlateau=0;
      };

      prvAdc=adc;

      // this excludes the leading edge, don't count if there is a hole after the leading edge
      if(leadEdgeBin>=0 && adc>3*ped && (timebin-numHighBinsAfterLeadingEdge)>(leadEdgeBin+1))
         numHighBinsAfterLeadingEdge++;

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
   if(numMaxPlateau>=3) //means basically 4 because we start counting after the first one
      return kFgtSeedTypeNo;

   //most restrictive condition
   if(pStrip->getAdc(0) <3*ped && numHighBins==3 && peakAdc > pStrip->getAdc(6) && numAlmostHighBins>=3 && numHighBinsAfterLeadingEdge>=2) {
      return kFgtSeedType1;
   }

   if(pStrip->getAdc(0) <3*ped && numHighBins==2 && peakAdc > pStrip->getAdc(6)&& numHighBinsAfterLeadingEdge>=2)
      return kFgtSeedType2;

   if(pStrip->getAdc(0) <3*ped && numHighBins==1 && peakAdc > pStrip->getAdc(6)&& numHighBinsAfterLeadingEdge>=3&& numAlmostHighBins>=2)
      return kFgtSeedType3;

   return kFgtSeedTypeNo;
};

ClassImp(StFgtA2CMaker);
