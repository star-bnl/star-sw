#ifndef STAR_EmcChecker
#define STAR_EmcChecker

#include <QDate>
#include "StEmcUtil/database/StEmcDecoder.h"
#include "StBemcOnlData.h"

class EmcChecker : public StBemcOnlData {
   public:
   EmcChecker(char*name="emcChecker") : StBemcOnlData(name) 
   {
       QDate currentDate = QDate::currentDate(Qt::UTC);
       EventDate =  100*(currentDate.year()*100+currentDate.month()) + currentDate.day();
   
       QTime currentTime = QTime::currentTime(Qt::UTC);
       EventTime =  100*(currentTime.hour()*100+currentTime.minute()) + currentTime.second();      
      // fprintf(stderr," first  timestamp date=%d:%d time %d: month%d\n", EventDate,currentDate.year(),EventTime,currentDate.month());
   }

   inline void ResetEmcDecoder(int date, int time) {
      if (date != EventDate) {
         // fprintf(stderr," new timesatmp date=%d:%d time %d:%d\n", EventDate ,date,EventTime,time);
         EventDate = date;
         EventTime = time;
         delete mDecoder; mDecoder = 0;
      }
   }
   inline StEmcDecoder* EmcDecoder() { 
      if (!mDecoder) mDecoder = new StEmcDecoder(EventDate,EventTime);
      return mDecoder;
   }
   inline int  GetDaqIdFromTowerId(int softId,int &RDO)  
   {
     return EmcDecoder()->GetDaqIdFromTowerId(softId,RDO);
   }
   inline int GetTowerTDCChannel(int softId,int &TDC,int &channel) {
      int error = 0;
      int crate;
      if (EmcDecoder()->GetCrateFromTowerId(softId, crate, channel)) 
         error = EmcDecoder()->GetTowerTDCFromCrate(crate, TDC);
      return error;
   }
};
#endif
