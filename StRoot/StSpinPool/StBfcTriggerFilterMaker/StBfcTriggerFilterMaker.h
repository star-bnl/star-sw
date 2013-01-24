// -*- mode:c++ -*-
//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 10 Dec 2009
//
// Trigger filter to accept events that fire selected bits of the
// output of the EMC layer 2 DSM, EM201. The output is a 16-bit integer
// that forms the basis of the trigger decision by the calorimeters.
// The definiton of the bits follows (as of Run 9):
//
//   (Bit 0:3) Barrel HT bits (4 bits)
//   (4:5) Endcap HT bits (2)
//   (6) JP1, unified over the BEMC+EEMC (1)
//   (7) JP2, unified over the BEMC+EEMC (1)
//   (8) BJP1 for the 18 BEMC-only patches (1)
//   (9) BJP2 for the 18 BEMC-only patches (1)
//   (10) EJP1 for the 6 EEMC-only patches (1)
//   (11) EJP2 for the 6 EEMC-only patches (1)
//   (12) AJP for BEMC and EEMC but NOT the boundary (1)
//   (13) BAJP for the BEMC-only patches (1)
//   (14) EAJP for the EEMC-only patches (1)
//   (15) JP0, unified over the BEMC+EEMC
//
// J.M. Engelage, C. Gagliardi, E.G. Judd: The Recabling Scheme and the New Algorithms for the EMC Trigger (April 5, 2010)
//
// http://www.star.bnl.gov/public/trg/run2009/emc_recabling.pdf
//
// An example usage is shown in the BFC macro bfcSpin.C that takes as input
// a fzd file then process each event through the EMC response.
// If the event doesn't fire any selected trigger from the EMC response
// alone further reconstruction is aborted at this stage. This is
// especially handy to decide whether or not to continue reconstruction of
// detectors like the TPC which can be computationally intensive.

#ifndef ST_BFC_TRIGGER_FILTER_MAKER_H
#define ST_BFC_TRIGGER_FILTER_MAKER_H

// C++ STL
#include <bitset>
#include <vector>

using namespace std;

// ROOT
#include "StMaker.h"

class StBfcTriggerFilterMaker : public StMaker {
public:
  StBfcTriggerFilterMaker(const char* name = "TriggerFilter")
    : StMaker(name)
    , mOkAllEvents(0)
    , mSkipAllEvents(0)
  {
  }

  int Init();
  int Make();

  void SetBHT0(int value = 1) { mMask.set(0,value);  }
  void SetBHT1(int value = 1) { mMask.set(1,value);  }
  void SetBHT2(int value = 1) { mMask.set(2,value);  }
  void SetBHT3(int value = 1) { mMask.set(3,value);  }
  void SetEHT0(int value = 1) { mMask.set(4,value);  }
  void SetEHT1(int value = 1) { mMask.set(5,value);  }
  void SetJP1 (int value = 1) { mMask.set(6,value);  }
  void SetJP2 (int value = 1) { mMask.set(7,value);  }
  void SetBJP1(int value = 1) { mMask.set(8,value);  }
  void SetBJP2(int value = 1) { mMask.set(9,value);  }
  void SetEJP1(int value = 1) { mMask.set(10,value); }
  void SetEJP2(int value = 1) { mMask.set(11,value); }
  void SetAJP (int value = 1) { mMask.set(12,value); }
  void SetBAJP(int value = 1) { mMask.set(13,value); }
  void SetEAJP(int value = 1) { mMask.set(14,value); }
  void SetJP0 (int value = 1) { mMask.set(15,value); }

  void changeJPThresh(int dsm);	/// Changes the JP Thresholds by dsm
  void SetOkAllEvents  (int ok   = 1) { mOkAllEvents   = ok  ; }
  void SetSkipAllEvents(int skip = 1) { mSkipAllEvents = skip; }
  void addTrigger(int triggerId) { mTriggers.push_back(triggerId); }

private:
  bitset<16> mMask;
  vector<int> mTriggers;
  int mOkAllEvents;
  int mSkipAllEvents;

  ClassDef(StBfcTriggerFilterMaker,0);
};

#endif	// ST_BFC_TRIGGER_FILTER_MAKER_H
