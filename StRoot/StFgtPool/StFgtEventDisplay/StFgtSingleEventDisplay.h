/***************************************************************************
 *
 * $Id: StFgtSingleEventDisplay.h,v 1.1 2012/09/21 18:56:52 pnord Exp $
 * Author: S. Gliske, Jan 2012
 *
 ***************************************************************************
 *
 * Description: Make plots of single events.  
 *
 ***************************************************************************
 *
 * $Log: StFgtSingleEventDisplay.h,v $
 * Revision 1.1  2012/09/21 18:56:52  pnord
 * *** empty log message ***
 *
 * Revision 1.1  2012/01/31 09:26:17  sgliske
 * StFgtQaMakers moved to StFgtPool
 *
 * Revision 1.2  2012/01/24 05:45:31  sgliske
 * debugged--mostly :)
 *
 * Revision 1.1  2012/01/24 03:32:17  sgliske
 * creation
 *
 *
 **************************************************************************/

#ifndef _ST_FGT_SINGLE_EVENT_DISPLAY_H_
#define _ST_FGT_SINGLE_EVENT_DISPLAY_H_

#include <string>
#include <sstream>

#include "StMaker.h"
#include "StuDraw3DEvent.h"
#include "StFgtDbMaker/StFgtDbMaker.h"
#include "StFgtDbMaker/StFgtDb.h"

class StFgtCollection;
class TH2F;

struct FgtStripDbItem{
  int electId,geoId, rdo,arm,apv,chan;
  Short_t disc,strip;
  Char_t stat; // 0 is good
  float  r1, r2, phi1, phi2; // cm, rad
  float  phi1Deg, phi2Deg; // cm, rad
  float  x1, y1, x2, y2, Z;
  float ped, sigPed;
  Char_t layer;  // P,R
  Short_t quad; // A-D
  char name[10];
  int hit;  // Flag used to indicate strip is above some threshold
  short adc[kFgtNumTimeBins];
};

enum {mxFgtElect=30720, mxFgtApv=22, mxFgtPln=2, kFgtPlnP=0, kFgtPlnR=1 };


class StFgtSingleEventDisplay : public StMaker {
 public:
   // constructors
   StFgtSingleEventDisplay( const Char_t* name = "FgtSingleEvent", const Char_t* fname = "fgtMapDump.csv" );

   // default OK
 // StFgtSingleEventDisplay(const StFgtSingleEventDisplay&);

   // equals operator -- default OK
   // StFgtSingleEventDisplay& operator=(const StFgtSingleEventDisplay&);

   void readFgtStripDb();

   Int_t getNumFgtElect() { return numFgtElect; }

   FgtStripDbItem * getDbItem(Int_t num) { 
     if ( itemNumInRange(num)) {
       return &(stripDb[num]);
     } else {
       return NULL;
     }
   }

   void drawStrip(Int_t geoId, Color_t lineColor =0); 

   Int_t getElectId(Int_t num) { 
     if ( itemNumInRange(num)) {
       return (stripDb[num].electId);
     } else {
       return NULL;
     }
   }

   Int_t getGeoId(Int_t num) { 
     if ( itemNumInRange(num)) {
       return (stripDb[num].geoId);
     } else {
       return NULL;
     }
   }

   Int_t getLayer(Int_t num) { 
     if ( itemNumInRange(num)) {
       return (stripDb[num].layer);
     } else {
       return NULL;
     }
   }

   Double_t getR1(Int_t num) { 
     if ( itemNumInRange(num)) {
       return (stripDb[num].r1);
     } else {
       return NULL;
     }
   }

   Double_t getR2(Int_t num) { 
     if ( itemNumInRange(num)) {
       return (stripDb[num].r2);
     } else {
       return NULL;
     }
   }

   Double_t getPhi1(Int_t num) { 
     if ( itemNumInRange(num)) {
       return (stripDb[num].phi1);
     } else {
       return NULL;
     }
   }

   Double_t getPhi2(Int_t num) { 
     if ( itemNumInRange(num)) {
       return (stripDb[num].phi2);
     } else {
       return NULL;
     }
   }

   Int_t getHit(Int_t num) { 
     if ( itemNumInRange(num)) {
       return (stripDb[num].hit);
     } else {
       return NULL;
     }
   }

   Int_t getDisc(Int_t num) { 
     if ( itemNumInRange(num)) {
       return (stripDb[num].disc);
     } else {
       return NULL;
     }
   }

   Int_t getStat(Int_t num) { 
     if ( itemNumInRange(num)) {
       return (stripDb[num].stat);
     } else {
       return NULL;
     }
   }

   Int_t getPed(Int_t num) { 
     if ( itemNumInRange(num)) {
       return (stripDb[num].ped);
     } else {
       return NULL;
     }
   }

   Int_t getPedForGeoId(Int_t geoId) { 
     for(int i = 0; i < numFgtElect; i++) {
       if ( stripDb[i].geoId == geoId ) {
         return (stripDb[i].ped);
       }
     }
     return NULL;
   }

   Int_t getSigPed(Int_t num) { 
     if ( itemNumInRange(num)) {
       return (stripDb[num].sigPed);
     } else {
       return NULL;
     }
   }

   Int_t getAdc(Int_t num, Int_t tb) { 
     if ( itemNumInRange(num) && (tb >= 0) && (tb < kFgtNumTimeBins)) {
       return (stripDb[num].adc[tb]);
     } else {
       return NULL;
     }
   }


   Int_t getGeometry( Int_t num, Short_t & disc,
                      Short_t & quad, Char_t & layer, Int_t & strip,
                      Double_t & x1, Double_t & y1,
                      Double_t & x2, Double_t & y2,
                      Double_t & z ) {
     if ( itemNumInRange(num)) {
       disc  = (stripDb[num].disc);
       quad  = (stripDb[num].quad);
       layer = (stripDb[num].layer);
       strip = (stripDb[num].strip);
       x1    = (stripDb[num].x1);
       y1    = (stripDb[num].y1);
       x2    = (stripDb[num].x2);
       y2    = (stripDb[num].y2);
       z     = (stripDb[num].Z);
       return true;
     } else {
       return NULL;
     }
   }

   Int_t getEntryForGeoId(Int_t geoId) {
     if(geoId >= 0 && geoId < kFgtNumGeoIds) {
       return entryForGeoId[geoId];
     } else {
       return 0;
     }
   }

   // deconstructor
   virtual ~StFgtSingleEventDisplay();

   Int_t Init();
   Int_t Make();
   Int_t Finish();

   // modifiers

 protected: 
   int numFgtElect;
   StFgtDbMaker *fgtDbMkr;

   bool itemNumInRange (Int_t num) {
     return ( (num >= 0) && (num < numFgtElect)) ;
   }


   Int_t entryForGeoId[kFgtNumGeoIds];

   FgtStripDbItem stripDb[mxFgtElect];

   // for accessing the data
   StFgtCollection *mFgtCollectionPtr;

   // to get track of the event
   Int_t mEventNum;

   // and for accessing the DB
   // if name is "", then use naive cosmic setup
   StFgtDbMaker *mFgtDbMkr;
   StFgtDb *mDb;


   

 private:   
   ClassDef(StFgtSingleEventDisplay,1);

}; 

// inline functions

// modifiers

#endif
