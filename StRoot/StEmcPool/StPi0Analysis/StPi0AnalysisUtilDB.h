//////////////////////////////////////////////////////////////////////
//                                                                  //
// author: Alexander Stolpovsky                                     //
// modifications: Oleksandr Grebenyuk                               //
//                                                                  //
//////////////////////////////////////////////////////////////////////

#include <TObject.h>

extern Int_t *triggerIDsMB;
extern Int_t *triggerIDsHT1;
extern Int_t *triggerIDsHT2;

extern const Char_t *DBConnectString;

Int_t getPrescalesFromDB(Int_t runNumber, Int_t &ps_minBias, Int_t &ps_ht1, Int_t &ps_ht2, Bool_t print = true, const Int_t *ourMinBiasTriggers = 0, const Int_t *ourHt1Triggers = 0, const Int_t *ourHt2Triggers = 0);

Int_t getEventNumbersFromDB(Int_t runNumber, Int_t &evNum_minBias, Int_t &evNum_ht1, Int_t &evNum_ht2, bool print = true, const Int_t *ourMinBiasTriggers = 0, const Int_t *ourHt1Triggers = 0, const Int_t *ourHt2Triggers = 0);

Int_t getRunTimesFromDB(Int_t runNumber, Int_t &date, Int_t &time, bool print = true);

class StPi0AnalysisUtilDB {public: Int_t i;}; // To make RootCint happy
