/***************************************************************************
 *
 * $Id: StFgtQaAdcVsChannel.h,v 1.2 2012/01/31 09:31:37 sgliske Exp $
 * Author: S. Gliske, Sept 2011
 *
 ***************************************************************************
 *
 * Description: Maker to make a plot for ADC vs channel.  Optionally,
 * can plot before or after pedistal subtraction.  Can also optionally
 * plot vs. strip number rather than vs channel.
 *
 ***************************************************************************
 *
 * $Log: StFgtQaAdcVsChannel.h,v $
 * Revision 1.2  2012/01/31 09:31:37  sgliske
 * includes updated for things moved to StFgtPooll
 *
 * Revision 1.1  2012/01/31 09:26:17  sgliske
 * StFgtQaMakers moved to StFgtPool
 *
 * Revision 1.6  2011/11/01 19:07:24  sgliske
 * Updated to correspond with StEvent containers, take 2.
 *
 * Revision 1.5  2011/09/27 15:28:17  sgliske
 * added common StFgtQaMaker parent
 *
 * Revision 1.4  2011/09/27 00:49:01  sgliske
 * cosmic QA update
 *
 * Revision 1.3  2011/09/24 02:14:10  sgliske
 * updated FGT cosmic QA
 *
 * Revision 1.2  2011/09/22 21:21:37  sgliske
 * working on adding in ped. subtr.
 *
 * Revision 1.1  2011/09/21 20:26:46  sgliske
 * creation
 *
 *
 *
 **************************************************************************/

#ifndef _ST_FGT_QA_ADC_VS_CHANNEL_
#define _ST_FGT_QA_ADC_VS_CHANNEL_

#include <string>
#include <TH2F.h>

#include "StFgtQaMaker.h"
#include "StRoot/StFgtPool/StFgtPedMaker/StFgtPedReader.h"
#include "StRoot/StFgtPool/StFgtPedMaker/StFgtPedReader.h"

class StFgtQaAdcVsChannel : public StFgtQaMaker {
 public:
   // constructors
   StFgtQaAdcVsChannel( const Char_t* name = "FGT_QA_Adc_vs_Channel",
                        Short_t discId = 0,
                        Short_t quadId = 0,
                        const Char_t* quadName = "000" );
   StFgtQaAdcVsChannel(const StFgtQaAdcVsChannel&);

   // deconstructor
   ~StFgtQaAdcVsChannel();

   // equals operator
   StFgtQaAdcVsChannel& operator=(const StFgtQaAdcVsChannel&);

   Int_t Init();
   Int_t Make();
   Int_t Finish();

   // modifiers
   void setFilenameBase( const Char_t* filenameBase );
   void setFilenameKey( const Char_t* filenameKey );
   void setPath( const Char_t* path );

   // accessor
   TH2F* getHist();

 protected:
   // for saving the plot
   std::string mPath, mFileNameBase, mFileNameKey;

   // the plot
   TH2F *mHist;

 private:   
   ClassDef(StFgtQaAdcVsChannel,1);
}; 

// inline functions

// modifiers
inline void StFgtQaAdcVsChannel::setFilenameBase( const Char_t* filenameBase ){ mFileNameBase = filenameBase; };
inline void StFgtQaAdcVsChannel::setFilenameKey( const Char_t* filenameKey ){ mFileNameKey = filenameKey; };
inline void StFgtQaAdcVsChannel::setPath( const Char_t* path ){ mPath = path; };

// accessor
inline TH2F* StFgtQaAdcVsChannel::getHist(){
   return mHist;
};

#endif
