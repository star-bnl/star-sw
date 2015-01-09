#ifndef __IOSectorPar__
#define __IOSectorPar__
#include "Riostream.h"
#include "Rtypes.h"
#include "TString.h"
#include "TMath.h"
struct data_t {
  Int_t sector;
  Double_t x, Dx, y, Dy, z, Dz, alpha, Dalpha, beta, Dbeta, gamma, Dgamma;
  const Char_t *Comment;
  void Print() {
    cout << Form("%2i %8.2f %5.2f %8.2f %5.2f %8.2f %5.2f",sector, x, Dx, y, Dy, z, Dz)
	 << Form(" %8.2f %5.2f %8.2f %5.2f %8.2f %5.2f %s", alpha, Dalpha, beta, Dbeta, gamma, Dgamma, Comment) << endl;
  }
};
struct SurveyPass_t {
  Int_t date, time;
  const Char_t *PassName;
  data_t Data[24];
#if 0
  SurveyPass_t &operator=(Int_t d, Int_t t, const Char_t *Pass, data_t data[24]) {
    date = d; time = t; PassName = Pass;
    for (Int_t i = 0; i < 24; i++) Data[i] = data[i];
    return *this;
  }
#endif
  void Print() {
    cout << Form("%8i %6i %s",date,time,PassName) << endl;
    for (Int_t i = 0; i < 24; i++) {
      Data[i].Print();
    }
  }
};
const Int_t N = 24;
SurveyPass_t Passes[] = {
#if 0
#if defined(__K2011__)
#include "K2011.h"
#endif /* __K2011__ */

#if defined( __K2011__) && defined( __L2011__)
  ,
#endif

#if defined( __L2011__)
#include "L2011.h"
#endif /*  __L2011__ */

#if defined( __L2011__) && defined(__2014_FIRST_PASS__)
  ,
#endif 

#ifdef __2014_FIRST_PASS__
  #include "2014_FIRST_PASS.h"
#elif defined(__2014_SECOND_PASS__)  /* ! __2014_FIRST_PASS__ && __2014_SECOND_PASS__ */
  #include "2014_SECOND_PASS.h"
#else /*  ! __2014_FIRST_PASS__ && ! __2014_SECOND_PASS__ */
  #ifdef __y2014A_511 
    #include "2014A_511.h"
  #endif /* __y2014A_511 */
  #ifdef __y2014A_512 
    #include "2014A_512.h"
  #endif /* __y2014A_512 */
#endif /* __2014_FIRST_PASS__ || __2014_SECOND_PASS__ || __2014__THIRD_PASS */
#else /* new */
  //#include "y2014A_FF605.h"
  //#include "y2014A_FF607.h"
  //#include "y2014A_608.h"
  //#include "y2014A_609.h"
  //#include "y2014A_610.h"
  //#include "y2014A_611.h"
  //#include "y2014A_612.h"
  //#include "y2014A_613.h"
  //#include "y2014A_614.h"
  //#include "y2014A_615.h"
  //#include "y2014A_616.h"
  //#include "y2014A_617.h"
  //#include "y2014A_618.h"
  //#include "y2014A_619.h"
  //#include "y2014A_620.h"
  //#include "y2014A_621.h"
  //#include "y2014A_622.h"
  //#include "y2014A_623.h"
  //#include "y2014A_624.h"
  //#include "y2014A_625.h"
  //#include "y2014A_626.h"
  //#include "y2014A_627.h"
  //#include "y2014A_628.h"
  //#include "y2014A_629.h"
  //#include "y2014A_630.h"
  //#include "y2014A_631.h"
  //#include "y2014A_632.h"
  //#include "y2014A_632.h"
  //#include "y2014A_901.h"
  //#include "y2014A_902.h"
  //#include "y2014A_903.h"
  //#include "y2014A_904.h"
  //#include "y2014A_905.h"
  //#include "y2014A_906.h"
  //#include "y2014A_907.h"
  //#include "y2014A_908.h"
  //#include "y2014A_911.h"
  //#include "y2014A_912.h"
  //#include "y2014A_913.h"
  //#include "y2014A_914.h"
  //#include "y2014A_915.h"
  //#include "y2014A_922.h"
  //#include "y2014A_923.h"
  //#include "y2014A_924.h"
  //#include "y2014A_925.h"
  //#include "y2014A_926.h"
  //#include "y2014A_927.h"
  //#include "y2014A_928.h"
  //#include "y2014A_929.h"
  //#include "y2014A_930.h"
  //#include "y2014A_950.h"
  //#include "y2014A_951.h"
  //#include "y2014A_952.h"
  //#include "y2014A_953.h"
  //#include "y2014A_957.h"
  //#include "y2014A_1101.h"
  //#include "y2014A_1102.h"
  //#include "y2014A_1103.h"
  //#include "y2014A_1104.h"
  //#include "y2014A_1105.h"
  //#include "y2014A_1106.h"
  //#include "y2014A_1107.h"
  //#include "y2014A_1108.h"
  //#include "y2014A_1201.h"
  //#include "y2014A_1202.h"
  //#include "y2014A_1203.h"
  //#include "y2014A_1205.h"
  //#include "y2014A_1211.h"
#include "y2014A_1451.h"
#endif /* new */  
 };
const  Int_t NP = sizeof(Passes)/sizeof(SurveyPass_t);
#endif
  
