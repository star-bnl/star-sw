/*!
 * \class  StMuScalerCutter
 * \brief  Apply cuts to events based on corrupt RICH scalers
 * \author G. Van Buren, BNL, Feb. 2009
 *
 * Use ratios of RICH scalers to determine corrupt values which
 * can cause erroneous application of luminosity dependent
 * TPC distortion corrections.
 *
 *
 * $Id: StMuScalerCutter.cxx,v 1.3 2010/01/28 18:15:10 perev Exp $
 * $Log: StMuScalerCutter.cxx,v $
 * Revision 1.3  2010/01/28 18:15:10  perev
 * WarningOff
 *
 * Revision 1.2  2009/02/18 19:50:57  genevb
 * Minor error in defining Run 8 dAu data
 *
 * Revision 1.1  2009/02/04 21:37:45  genevb
 * Introduction of class to determine events with bad RICH scalers
 *
 *
 * -------------------------------------------------------------------------
 */
//

#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMessMgr.h"
#include "StMuScalerCutter.h"

ClassImp(StMuScalerCutter)

// The constructor. Initialize data members here.
StMuScalerCutter::StMuScalerCutter(const Char_t *name) : StMaker(name) {}

Int_t StMuScalerCutter::Make()
{
    // Result is kStSkip if scalers are corrupt

    StMuDst* mu =  (StMuDst*) GetInputDS("MuDst"); 
    if (!mu){
	  LOG_WARN << "StMuScalerCutter::Make : No MuDst" << endm;
          return kStOK;        // if no event, we're done
    }

    return (accept(mu->event()) ? kStOK : kStSkip);
}

bool StMuScalerCutter::accept(StMuEvent* event)
{
    // Result is false if scalers are corrupt

    if (!event) return true;
    StRunInfo& runInfo = event->runInfo();
    int run = runInfo.runId();
    double zdce = runInfo.zdcEastRate();
    double zdcw = runInfo.zdcWestRate();
    double zdcx = runInfo.zdcCoincidenceRate();
    //double bbce = runInfo.bbcEastRate();
    double bbcw = runInfo.bbcWestRate();
    double bbcx = runInfo.bbcCoincidenceRate();
    double bbcbb = runInfo.bbcBlueBackgroundRate();
    //double bbcyb = runInfo.bbcYellowBackgroundRate();

    // only for Run 8 dAu
    if (run > 8330000 && run < 9029000) {

      // zdcx check
      if (zdcx/bbcx > 0.42 ||
          zdcx/zdcw > 0.35) return false;
    
      // zdce check
      if (run > 8361110 && run < 8363032) {
        if (zdce/bbcx > 2.9 ||
            zdce/zdcw > 1.8) return false;
      } else {
        if (zdce/bbcx > 1.9 ||
            zdce/zdcw > 1.8) return false;
      }

      // bbcbb check (for 1-second scalers)
      double R1 = 1.4923e-3*pow(zdcw,-2.57818)*pow(bbcw,3.6488);
      double R2 = bbcbb/R1;
      if (((R2 > 0.2) && (R2 < 0.72)) ||
          ((R2 > 1.4) && (R2 < 4.6) )) return false;

    } // Run 8 dAu cuts

    return true;
}
