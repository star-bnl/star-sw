// -*- mode: C++ -*-
//
// Pibero Djawotho <pibero@comp.tamu.edu>
// Texas A&M University Cyclotron Institute
// 12 Jan 2009
//

#ifndef ST_EMC_TRIGGER_SIMU_H
#define ST_EMC_TRIGGER_SIMU_H

class StBemcTriggerSimu;
class StEemcTriggerSimu;
struct DSMLayer_EM201_2009;
struct DSMLayer_LD301_2009;
struct TriggerDefinition;
class TCU;

#include <set>

using namespace std;

#include "StTriggerUtilities/StVirtualTriggerSimu.h"
#include "StTriggerUtilities/StTriggerSimuResult.h"

class StEmcTriggerSimu : public StVirtualTriggerSimu {
public:
  StEmcTriggerSimu();
  virtual ~StEmcTriggerSimu();

  bool isTrigger(int trigId);
  set<int> triggerIds() const;
  StTriggerSimuDecision triggerDecision(int trigId);

  // 16-bit output of EM201 DSM
  int EM201output() const;

  // Access to individual bits of EM201 DSM ouput
  // See http://www.star.bnl.gov/public/trg/run2009/emc_recabling.pdf
  int  BHT() const;		// (Bit 0:3) Barrel HT bits (4 bits)
  int  EHT() const;		// (Bit 4:5) Endcap HT bits (2 bits)
  int  JP1() const;		// (Bit 6) JP1, unified over the BEMC+EEMC (1 bit)
  int  JP2() const;		// (Bit 7) JP2, unified over the BEMC+EEMC (1 bit)
  int BJP1() const;		// (Bit 8) BJP1 for the 18 BEMC-only patches (1 bit)
  int BJP2() const;		// (Bit 9) BJP2 for the 18 BEMC-only patches (1 bit)
  int EJP1() const;		// (Bit 10) EJP1 for the 6 EEMC-only patches (1 bit)
  int EJP2() const;		// (Bit 11) EJP2 for the 6 EEMC-only patches (1 bit)
  int  AJP() const;		// (Bit 12) AJP for BEMC and EEMC but NOT the boundary (1 bit)
  int BAJP() const;		// (Bit 13) BAJP for the BEMC-only patches (1 bit)
  int EAJP() const;		// (Bit 14) EAJP for the EEMC-only patches (1 bit)
  int  JP0() const;		// (Bit 15) JP0, unified over the BEMC+EEMC

  int BHT0() const;
  int BHT1() const;
  int BHT2() const;
  int BHT3() const;

  int EHT0() const;
  int EHT1() const;

  // 2013 change
  int EEMCdijet() const;  // Bit 9
  int JP1dijet() const;  // Bit 11
  int JP0dijet() const;  //Bit 12
  int DAQ10k() const;  // Bit 14
  // 2014 change
  int BHT4() const {return btest(EM201output(), 4);} // Bit 4
  int BHTUPC() const {return btest(EM201output(), 5);} //Bit 5
  int BTP() const {return btest(EM201output(), 6);} //Bit 6
  int BHTTP() const {return btest(EM201output(), 7);} //Bit 7
  int BTPtopo() const {return btest(EM201output(), 8);} //Bit 8
  int BHTTPtopo() const {return btest(EM201output(), 9);} //Bit 9
  int BHT4topo() const {return btest(EM201output(), 10);} //Bit 10
  int EHT0_2014() const {return btest(EM201output(), 13);}  //Bit 13
  int EHT1_2014() const {return btest(EM201output(), 14); } //Bit 14
  int DAQ10k_2014() const { return btest(EM201output(), 15); }//Bit 15
  //2015 change
  int HTTP() const {return btest(EM201output(), 3);} //Bit 3
  int EB2B() const {return btest(EM201output(), 14);} //Bit 14
  void setHeadMaker(StMaker*) { /* dummy */ }

  void InitRun(int runNumber);
  void Make();

  void setBemc(StBemcTriggerSimu* bemc);
  void setEemc(StEemcTriggerSimu* eemc);

  DSMLayer_EM201_2009* get2009_DSMLayer2_Result();
  DSMLayer_LD301_2009* get2009_DSMLayer3_Result();

  //  void defineTrigger(const TriggerDefinition& trigdef);
  void defineTrigger(TriggerDefinition& trigdef);

  void defineTrigger(int triggerIndex, const char* name, int triggerId, unsigned int onbits, unsigned int offbits, unsigned int onbits1, unsigned int onbits2, unsigned int onbits3, unsigned int offbits1, unsigned int offbits2, unsigned int offbits3);

  int  overlapJetPatchTh(int i) const;
  void getOverlapJetPatchAdc(int i, int& jp, int& adc) const;

private:
  // returns value of bit from x at position pos
  int btest(int x, int pos) const { return x >> pos & 1; }

  // returns n bits from x starting at position pos
  int getbits(int x, int pos, int n) const { return x >> pos & ~(~0 << n); }

  // OR x with value starting at position pos
  void setbits(int& x, int pos, int value) const { x |= value << pos; }

  TDatime mDBTime;
  StBemcTriggerSimu* mBemc;
  StEemcTriggerSimu* mEemc;
  DSMLayer_EM201_2009* mEM201;
  DSMLayer_LD301_2009* mLD301;
  TCU* mTcu;

  ClassDef(StEmcTriggerSimu,0);
};

inline DSMLayer_EM201_2009* StEmcTriggerSimu::get2009_DSMLayer2_Result() { return mEM201; }
inline DSMLayer_LD301_2009* StEmcTriggerSimu::get2009_DSMLayer3_Result() { return mLD301; }

inline int  StEmcTriggerSimu::BHT() const { return getbits(EM201output(),0,4); }
inline int  StEmcTriggerSimu::EHT() const { return getbits(EM201output(),4,2); }
inline int  StEmcTriggerSimu::JP1() const { return btest(EM201output(),6); }
inline int  StEmcTriggerSimu::JP2() const { return btest(EM201output(),7); }
inline int StEmcTriggerSimu::BJP1() const { return btest(EM201output(),8); }
inline int StEmcTriggerSimu::BJP2() const { return btest(EM201output(),9); }
inline int StEmcTriggerSimu::EJP1() const { return btest(EM201output(),10); }
inline int StEmcTriggerSimu::EJP2() const { return btest(EM201output(),11); }
inline int  StEmcTriggerSimu::AJP() const { return btest(EM201output(),12); }
inline int StEmcTriggerSimu::BAJP() const { return btest(EM201output(),13); }
inline int StEmcTriggerSimu::EAJP() const { return btest(EM201output(),14); }
inline int  StEmcTriggerSimu::JP0() const { return btest(EM201output(),15); }

inline int StEmcTriggerSimu::BHT0() const { return btest(EM201output(),0); }
inline int StEmcTriggerSimu::BHT1() const { return btest(EM201output(),1); }
inline int StEmcTriggerSimu::BHT2() const { return btest(EM201output(),2); }
inline int StEmcTriggerSimu::BHT3() const { return btest(EM201output(),3); }

inline int StEmcTriggerSimu::EHT0() const { return btest(EM201output(),4); }
inline int StEmcTriggerSimu::EHT1() const { return btest(EM201output(),5); }

// 2013 change
inline int StEmcTriggerSimu::EEMCdijet() const { return btest(EM201output(),9); }
inline int StEmcTriggerSimu::JP1dijet() const { return btest(EM201output(),11); }
inline int  StEmcTriggerSimu::JP0dijet() const { return btest(EM201output(),12); }
inline int StEmcTriggerSimu::DAQ10k() const { return btest(EM201output(),14); }

#endif // ST_EMC_TRIGGER_SIMU_H
