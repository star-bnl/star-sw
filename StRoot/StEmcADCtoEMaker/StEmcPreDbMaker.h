#ifndef StEmcPreDbMaker_hh
#define StEmcPreDbMaker_hh

#include "StMaker.h"
#include "StChain.h"

class StMuDstMaker;

/*!\class StEmcPreDbMaker
\author Alexandre A. P. Suaide

This maker just sets the correct timestamp for the St_db_Maker. It should
run before St_db_Maker when you are reading data directly from muDst files
WITHOUT converting them back to StEvent

*/
class StEmcPreDbMaker : public StMaker 
{
 public:
                    StEmcPreDbMaker(const char* self="EmcmuDst2StEventMaker", const char* muDstMakerName="muDstMaker");
                    ~StEmcPreDbMaker();
    int             Make();   ///< Time stamp gets written and set in StEvtHddr for database usage
 private:
    StMuDstMaker* mMuDstMaker;
    ClassDef(StEmcPreDbMaker, 1)
}; 


#endif

