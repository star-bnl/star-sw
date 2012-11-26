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
   //LOG_INFO << "StSpinInfoMaker_t::Init()" << endm;

   int ierr = kStOK;

   StMuDst *mudst = (StMuDst*)GetDataSet("MuDst");
   if(!mudst) {
      LOG_FATAL << "StSpinInfoMaker_t::Init(), no MuDst found!" << endm;
      ierr = kStFatal;
   };

   //LOG_INFO << "\t DONE" << endm;

   return ierr;
};

Int_t StSpinInfoMaker_t::Make(){
   //LOG_INFO << "StSpinInfoMaker_t::Make()" << endm;

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

   //LOG_INFO << "A" << endm;

   mSpinInfo.setDsmVertex( event->bbcTriggerDetector().onlineTimeDifference() );

   //LOG_INFO << "B" << endm;


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

   // 
   // MASKS
   //

   StL0Trigger *trig  =&(event->l0Trigger());
   if( !trig ){
      LOG_FATAL << "Cannot get 'StL0Trigger' pointer from MuDst" << endm;
      return kStFatal;
   };
    
   //LOG_INFO << "C" << endm;


   //StMuTriggerIdCollection& tic = event->triggerIdCollection();
   //const StTriggerId& l1trig = tic.l1();
    
   UShort_t bx48   = static_cast< UShort_t >( trig->bunchCrossingId() );
   UShort_t bx7    = static_cast< UShort_t >( trig->bunchCrossingId7bit( event->runNumber() ));
   UShort_t bxStar = static_cast< UShort_t >( spinDB->BXyellowUsingBX48(bx48) );

   //LOG_INFO << "D" << endm;

   // If bunch crossing is masked out skip event
   if( spinDB->isMaskedUsingBX48(bx48) )
      return kStOK;

   if( spinDB->offsetBX48minusBX7(bx48,bx7) != 0 ) {
      LOG_WARN << " ++++ spindb indicates 7bit and 48bit bunch crossings are inconsistent... event invalid ++++" << endm;
      return kStOK; // returns and leaves spin info in an "invalidated" state
   }

   Int_t spin4 = spinDB->spin4usingBX48(bx48);

   // Finanlly, store the spin information

   //LOG_INFO << "E" << endm;

   mSpinInfo.setValidDB( spinDB->isValid() );
   mSpinInfo.setSpin4( static_cast< UShort_t >( spin4 ) );
   mSpinInfo.setBunchCrossing7bit( bx7 );
   mSpinInfo.setBunchCrossing48bit( bx48 );
   mSpinInfo.setBunchCrossingStar( bxStar );
    
   if( spinDB->isPolDirLong() )
      mSpinInfo.setPolarizationType( StSpinInfo_t::LONG_LONG_POLARIZATION );

   if( spinDB->isPolDirTrans() )
      mSpinInfo.setPolarizationType( StSpinInfo_t::TRANS_TRANS_POLARIZATION );
    
   //LOG_INFO << "\t DONE" << endm;

   return kStOK;
};

/// Clear for next event
void StSpinInfoMaker_t::Clear(Option_t *opts){
   //LOG_INFO << "StSpinInfoMaker_t::Clear()" << endm;

   mSpinInfo.clear();

   //LOG_INFO << "\t DONE" << endm;
};


ClassImp( StSpinInfoMaker_t );

/*
 * $Id: StSpinInfoMaker.cxx,v 1.1 2012/11/26 19:06:11 sgliske Exp $
 * $Log: StSpinInfoMaker.cxx,v $
 * Revision 1.1  2012/11/26 19:06:11  sgliske
 * moved from offline/users/sgliske/StRoot/StEEmcPool/StEEmcTreeMaker to StRoot/StEEmcPool/StEEmcTreeMaker
 *
 * 
 */
