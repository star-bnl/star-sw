/*!
 *
 * \class StSpinInfoMaker_t
 * \author Stephen Gliske <sgliske@anl.gov>
 *
 * See StSpinInfoMaker.h
 *
 */

#include "StRoot/StMuDSTMaker/COMMON/StMuDst.h"
#include "StRoot/StMuDSTMaker/COMMON/StMuEvent.h"
#include "StRoot/StSpinPool/StSpinDbMaker/StSpinDbMaker.h"

#include "StSpinInfoMaker.h"

Int_t StSpinInfoMaker_t::Init(){
   int ierr = kStOK;

   StMuDst *mudst = (StMuDst*)GetDataSet("MuDst");
   if(!mudst) {
      LOG_FATAL << "StSpinInfoMaker_t::Init(), no MuDst found!" << endm;
      ierr = kStFatal;
   };

   return ierr;
};

Int_t StSpinInfoMaker_t::Make(){
   //
   // COPY DETAILS FROM MuDst
   //

   StMuDst *mudst = (StMuDst*)GetDataSet("MuDst");
   if(!mudst) {
      LOG_FATAL << "StSpinInfoMaker_t::Make(), no MuDst found!" << endm;
      return kStFatal;
   };

   StMuEvent *event = mudst->event();
   if( !event ){
      LOG_FATAL << "Cannot get 'StEvent' pointer from MuDst" << endm;
      return kStFatal;
   };

   mSpinInfo.setDsmVertex( event->bbcTriggerDetector().onlineTimeDifference() );

   //
   // COPY DETAILS FROM StSpinDb
   //

   StSpinDbMaker *spinDB = dynamic_cast<StSpinDbMaker*>(GetMakerInheritsFrom("StSpinDbMaker"));
   if (!spinDB) {
      LOG_FATAL << "StSpinInfoMaker_t::Make(), no StSpinDbMaker found!" << endm;
      return kStFatal;
   };

   if( !spinDB->isValid()) {
      LOG_WARN << "StSpinInfoMaker_t::Make(), SpinDB reports invalid for run " << GetRunNumber() << ", " << GetEventNumber() << "." << endm;
   };

   StL0Trigger *trig  =&(event->l0Trigger());
   if( !trig ){
      LOG_FATAL << "Cannot get 'StL0Trigger' pointer from MuDst" << endm;
      return kStFatal;
   };
    
   UShort_t bx48   = static_cast< UShort_t >( trig->bunchCrossingId() );
   UShort_t bx7    = static_cast< UShort_t >( trig->bunchCrossingId7bit( event->runNumber() ));
   UShort_t bxStar = static_cast< UShort_t >( spinDB->BXyellowUsingBX48(bx48) );

   mSpinInfo.setbXingIsMaskedInSpinDB( spinDB->isMaskedUsingBX48(bx48) );
   mSpinInfo.setValidDB( spinDB->isValid() );
   mSpinInfo.setBunchCrossing7bit( bx7 );
   mSpinInfo.setBunchCrossing48bit( bx48 );
   mSpinInfo.setBunchCrossingStar( bxStar );

   //
   // SPIN PART
   //

   mSpinInfo.setSpin4( -1 );
   if( !spinDB->isMaskedUsingBX48(bx48) ){

      if( spinDB->offsetBX48minusBX7(bx48,bx7) != 0 ) {
         LOG_WARN << " ++++ spindb indicates 7bit and 48bit bunch crossings are inconsistent... event invalid ++++" << endm;
         return kStOK; // returns and leaves spin info in an "invalidated" state
      }

      // Finanlly, store the spin information
      Int_t spin4 = spinDB->spin4usingBX48(bx48);

      mSpinInfo.setSpin4( static_cast< UShort_t >( spin4 ) );
    
      if( spinDB->isPolDirLong() )
         mSpinInfo.setPolarizationType( StSpinInfo_t::LONG_LONG_POLARIZATION );

      if( spinDB->isPolDirTrans() )
         mSpinInfo.setPolarizationType( StSpinInfo_t::TRANS_TRANS_POLARIZATION );
   };

   return kStOK;
};

/// Clear for next event
void StSpinInfoMaker_t::Clear(Option_t *opts){
   mSpinInfo.clear();
};


ClassImp( StSpinInfoMaker_t );

/*
 * $Id: StSpinInfoMaker.cxx,v 1.2 2013/02/21 21:58:14 sgliske Exp $
 * $Log: StSpinInfoMaker.cxx,v $
 * Revision 1.2  2013/02/21 21:58:14  sgliske
 * added mask field, cleaned up comments, and adjusted the logic
 *
 * Revision 1.1  2012/11/26 19:06:11  sgliske
 * moved from offline/users/sgliske/StRoot/StEEmcPool/StEEmcTreeMaker to StRoot/StEEmcPool/StEEmcTreeMaker
 *
 * 
 */
