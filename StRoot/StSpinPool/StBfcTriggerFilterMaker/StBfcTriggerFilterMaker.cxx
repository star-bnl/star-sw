//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 10 Dec 2009
//

// STAR
#include "StTriggerUtilities/StTriggerSimuMaker.h"
#include "StTriggerUtilities/Emc/StEmcTriggerSimu.h"
#include "StTriggerUtilities/Bemc/StBemcTriggerSimu.h"
#include "StTriggerUtilities/Eemc/StEemcTriggerSimu.h"

// Local
#include "StBfcTriggerFilterMaker.h"

ClassImp(StBfcTriggerFilterMaker);

int StBfcTriggerFilterMaker::Init()
{
  // Allow maker to abort events
  SetAttr(".Privilege",1);

  // Print mask
  LOG_INFO << Form("MASK: BHT0=%d BHT1=%d BHT2=%d BHT3=%d EHT0=%d EHT1=%d JP1=%d JP2=%d BJP1=%d BJP2=%d EJP1=%d EJP2=%d AJP=%d BAJP=%d EAJP=%d",
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
		   mMask.test(14)) << endm;

  return kStOk;
}

int StBfcTriggerFilterMaker::Make()
{
  // Get trigger simulator
  StTriggerSimuMaker* trgsim = (StTriggerSimuMaker*)GetMakerInheritsFrom("StTriggerSimuMaker");
  assert(trgsim);

  // Get output of EMC L2 DSM
  bitset<16> emc(trgsim->emc->EM201output());

  LOG_INFO << Form("EM201: BHT0=%d BHT1=%d BHT2=%d BHT3=%d EHT0=%d EHT1=%d JP1=%d JP2=%d BJP1=%d BJP2=%d EJP1=%d EJP2=%d AJP=%d BAJP=%d EAJP=%d",
		   emc.test(0),
		   emc.test(1),
		   emc.test(2),
		   emc.test(3),
		   emc.test(4),
		   emc.test(5),
		   emc.test(6),
		   emc.test(7),
		   emc.test(8),
		   emc.test(9),
		   emc.test(10),
		   emc.test(11),
		   emc.test(12),
		   emc.test(13),
		   emc.test(14)) << endm;

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
