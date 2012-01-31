/*!
 * \class StFgtQaClusterChargePerAPV 
 * \author S. Gliske, Oct 2011
 */

/***************************************************************************
 *
 * $Id: StFgtQaClusterChargePerAPV.h,v 1.2 2012/01/31 09:31:37 sgliske Exp $
 * Author: S. Gliske, Oct 2011
 *
 ***************************************************************************
 *
 * Description: Maker to make a histogram of the charge per cluster,
 * combined for all r (or phi) strips connected to single APV.
 *
 ***************************************************************************
 *
 * $Log: StFgtQaClusterChargePerAPV.h,v $
 * Revision 1.2  2012/01/31 09:31:37  sgliske
 * includes updated for things moved to StFgtPooll
 *
 * Revision 1.1  2012/01/31 09:26:17  sgliske
 * StFgtQaMakers moved to StFgtPool
 *
 * Revision 1.2  2011/11/01 19:07:24  sgliske
 * Updated to correspond with StEvent containers, take 2.
 *
 * Revision 1.1  2011/10/10 17:40:36  sgliske
 * creation
 *
 *
 **************************************************************************/

#ifndef _ST_FGT_QA_CLUSTER_CHARGE_PER_APV_
#define _ST_FGT_QA_CLUSTER_CHARGE_PER_APV_

#include <string>
#include <TH2F.h>

#include "StFgtQaMaker.h"
#include "StRoot/StFgtPool/StFgtPedMaker/StFgtPedReader.h"
#include "StRoot/StFgtPool/StFgtPedMaker/StFgtPedReader.h"


class StFgtQaClusterChargePerAPV : public StFgtQaMaker {
 public:
   // constructors
   StFgtQaClusterChargePerAPV( const Char_t* name = "FGT_QA_Adc_vs_Channel",
                               Short_t discId = 0,
                               Short_t quadId = 0,
                               Short_t apvID = 0,
                               const Char_t* quadName = "000" );
   StFgtQaClusterChargePerAPV(const StFgtQaClusterChargePerAPV&);

   // deconstructor
   ~StFgtQaClusterChargePerAPV();

   // equals operator
   StFgtQaClusterChargePerAPV& operator=(const StFgtQaClusterChargePerAPV&);

   Int_t Init();
   Int_t Make();
   Int_t Finish();

   // accessor/modifers
   TH1F* getHistR(Int_t i);
   TH1F* getHistPhi(Int_t i);

   // modifiers
   void setNbins( Int_t nbins );
   void setRange( Float_t min, Float_t max );
   void setUnits( const Char_t *units );

 protected:
   // the histograms
   TH1F *mHistR[10], *mHistPhi[10];

   // histo parameters
   Int_t mNbins;
   Float_t mChargeMin, mChargeMax;
   std::string mUnits;

 private:   
   ClassDef(StFgtQaClusterChargePerAPV,1);
}; 

// inline functions

// accessors
inline TH1F* StFgtQaClusterChargePerAPV::getHistR(Int_t i){
   return (i<10 && i > -1) ? mHistR[i] : 0;
};
inline TH1F* StFgtQaClusterChargePerAPV::getHistPhi(Int_t i){
   return (i<10 && i > -1) ? mHistPhi[i] : 0;
};

inline void StFgtQaClusterChargePerAPV::setNbins( Int_t nbins ){ mNbins = nbins; };
inline void StFgtQaClusterChargePerAPV::setRange( Float_t min, Float_t max ){ mChargeMin = min; mChargeMax = max; };
inline void StFgtQaClusterChargePerAPV::setUnits( const Char_t* units ){ mUnits = units; };

#endif
