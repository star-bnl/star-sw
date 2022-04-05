//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 10 Dec 2009
//

// C++ STL
#include <sstream>

// STAR
#include "StTriggerUtilities/StTriggerSimuMaker.h"
#include "StTriggerUtilities/Emc/StEmcTriggerSimu.h"
#include "StTriggerUtilities/Bemc/StBemcTriggerSimu.h"
#include "StTriggerUtilities/Eemc/StEemcTriggerSimu.h"
#include "StEmcTriggerMaker/StEmcTriggerMaker.h"

// Local
#include "StBfcTriggerFilterMaker.h"

ClassImp(StBfcTriggerFilterMaker);

int StBfcTriggerFilterMaker::Init()
{
  // Allow maker to abort events
  SetAttr(".Privilege",1);

  // Print mask
  LOG_INFO << Form("MASK: BHT0=%d BHT1=%d BHT2=%d BHT3=%d EHT0=%d EHT1=%d JP1=%d JP2=%d BJP1=%d BJP2=%d EJP1=%d EJP2=%d AJP=%d BAJP=%d EAJP=%d JP0=%d",
		   mMask.test(0),
		   mMask.test(1),
		   mMask.test(2),
		   mMask.test(3),
		   mMask.test(4),
		   mMask.test(5),
		   mMask.test(6),
		   mMask.test(7),
		   mMask.test(8),
		   mMask.test(9),
		   mMask.test(10),
		   mMask.test(11),
		   mMask.test(12),
		   mMask.test(13),
		   mMask.test(14),
		   mMask.test(15)) << endm;

  // Print triggers (year < 2009)
  ostringstream os;
  copy(mTriggers.begin(),mTriggers.end(),ostream_iterator<int>(os," "));
  LOG_INFO << "Triggers: " << os.str() << endm;

  return kStOk;
}

// Print format: trigger ID (trigger decision)
// where trigger decision is 1=yes, 0=no, -1=unknown
struct PrintTrigger {
  ostream& out;
  StEmcTriggerMaker* emcTrig;

  PrintTrigger(ostream& out, StEmcTriggerMaker* emcTrig) : out(out), emcTrig(emcTrig) {}

  ostream& operator()(int trigId) const { return out << trigId << "(" << emcTrig->isTrigger(trigId) << ") "; }
};

int StBfcTriggerFilterMaker::Make()
{
  if (mOkAllEvents  ) return kStOk  ;
  if (mSkipAllEvents) return kStSkip;

  const TDatime& datime = GetDBTime();

  LOG_INFO << "DB Time = " << datime.AsSQLString() << endm;

  if (datime.GetYear() < 2009) {
    StEmcTriggerMaker* emcTrig = (StEmcTriggerMaker*)GetMakerInheritsFrom("StEmcTriggerMaker");
    if (!emcTrig) {
      LOG_ERROR << "Missing StEmcTriggerMaker" << endm;
      return kStErr;
    }

    // Print trigger ID and decision for each trigger
    ostringstream os;
    for_each(mTriggers.begin(),mTriggers.end(),PrintTrigger(os,emcTrig));
    LOG_INFO << "Triggers: " << os.str() << endm;

    // If at least one trigger fired (trigger decision == 1), process event. Otherwise, skip.
    return os.str().find("(1)") != string::npos ? kStOk : kStSkip;
  }

  // Get trigger simulator
  StTriggerSimuMaker* trgsim = (StTriggerSimuMaker*)GetMakerInheritsFrom("StTriggerSimuMaker");
  if (!trgsim) {
    LOG_ERROR << "Missing StTriggerSimuMaker" << endm;
    return kStErr;
  }

  // Get output of EMC L2 DSM
  bitset<16> emc(trgsim->emc->EM201output());

  // Mask out any uninteresting bits
  emc &= mMask;

  // If any of the interesting bits are on, accept, else reject
  return emc.any() ? kStOk : kStSkip;
}

void StBfcTriggerFilterMaker::changeJPThresh(int dsm)
{
  StTriggerSimuMaker* trgsim = (StTriggerSimuMaker*)GetMakerInheritsFrom("StTriggerSimuMaker");
  trgsim->changeJPThresh(dsm);
}
