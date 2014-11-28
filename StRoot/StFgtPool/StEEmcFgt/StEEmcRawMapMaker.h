/***************************************************************************
 *
 * $Id: StEEmcRawMapMaker.h,v 1.3 2012/05/09 21:11:58 sgliske Exp $
 * Author: S. Gliske, April 2012
 *
 ***************************************************************************
 *
 * Description: Make a lookup table of ADC values and pedestals for
 * the endcap preshowers, towers, and postshowers.  Much of the input
 * code is borrowed from StEEmcA2EMaker.
 *
 ***************************************************************************
 *
 * $Log: StEEmcRawMapMaker.h,v $
 * Revision 1.3  2012/05/09 21:11:58  sgliske
 * updates
 *
 * Revision 1.2  2012/04/13 15:08:43  sgliske
 * updates
 *
 * Revision 1.1  2012/04/12 17:11:16  sgliske
 * creation
 *
 *
 **************************************************************************/

#ifndef _StEEmcRawMapMaker_H_
#define _StEEmcRawMapMaker_H_

#include "StMaker.h"
#include <map>
#include <string>

class StEEmcDb;

struct StEEmcRawMapData {
   Int_t rawAdc, fail, stat;
   Float_t ped, pedSigma, gain;

   StEEmcRawMapData() : rawAdc(-1), fail(1), stat(1), ped(-1), pedSigma(1000), gain(-1) { /* */ };
};

typedef std::map< Int_t, StEEmcRawMapData > StEEmcRawMap;

class StEEmcRawMapMaker : public StMaker {
 public:
   // constructors
   StEEmcRawMapMaker( const Char_t* name = "EEmcRawMapMaker" );

   // deconstructor
   virtual ~StEEmcRawMapMaker();

   // default equals operator and copy constructor OK
   virtual Int_t Init();
   virtual Int_t Make();
   virtual void Clear( Option_t *opt = "");

   Int_t setInput( const Char_t *name, Int_t type );   // type = 0 is StEvent, type == 1 is MuDst

   const StEEmcRawMap& getMap( Int_t layer );

 protected:
   enum layer_t { TOWER, PRE1, PRE2, POST, ESMD };

   Int_t mInputType;
   std::string mInputName, mDbName;
   const StEEmcDb *mEEmcDb;

   Int_t loadFromMuDst();
   Int_t loadFromStEvent();
   void addHitTower( Int_t sec, Int_t sub, Int_t eta, Int_t adc, Int_t layer );
   void addHitStrip( Int_t sec, Bool_t layerIsV, Int_t strip, Int_t adc );

   StEEmcRawMap mMap[5];  // one for each layer+SMD

 private:   
   ClassDef(StEEmcRawMapMaker,1);

}; 

// inline
inline const StEEmcRawMap& StEEmcRawMapMaker::getMap( Int_t layer ){ return mMap[ (layer>0&&layer<5) ? layer : 0 ]; };


#endif
