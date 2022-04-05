/*!
 * \class StFgtQaMaker 
 * \author S. Gliske, Sept 2011
 */

/***************************************************************************
 *
 * $Id: StFgtQaMaker.h,v 1.2 2012/01/31 09:31:37 sgliske Exp $
 * Author: S. Gliske, Sept 2011
 *
 ***************************************************************************
 *
 * Description: Common parent for QA makers.  Defines certain member
 * functions commonly used by QA makers.
 *
 ***************************************************************************
 *
 * $Log: StFgtQaMaker.h,v $
 * Revision 1.2  2012/01/31 09:31:37  sgliske
 * includes updated for things moved to StFgtPooll
 *
 * Revision 1.1  2012/01/31 09:26:17  sgliske
 * StFgtQaMakers moved to StFgtPool
 *
 * Revision 1.4  2011/11/01 19:07:24  sgliske
 * Updated to correspond with StEvent containers, take 2.
 *
 * Revision 1.3  2011/09/29 18:39:21  sgliske
 * Update for geoId->elecCoord function now in StFgtCosmicTestStandGeom
 *
 * Revision 1.2  2011/09/28 17:48:50  sgliske
 * minor updates
 *
 * Revision 1.1  2011/09/27 15:28:17  sgliske
 * added common StFgtQaMaker parent
 *
 *
 **************************************************************************/

#ifndef _ST_FGT_QA_MAKER_H_
#define _ST_FGT_QA_MAKER_H_

#include <string>
#include "StMaker.h"

#include "StRoot/StFgtPool/StFgtPedMaker/StFgtPedReader.h"
#include "StRoot/StFgtPool/StFgtPedMaker/StFgtPedReader.h"
class StFgtCollection;

class StFgtQaMaker : public StMaker {
 public:
   // constructors
   StFgtQaMaker( const Char_t* name = "FGT_QA_Adc_vs_Channel",
                        Short_t discId = 0,
                        Short_t quadId = 0,
                        const Char_t* quadName = "000" );
   StFgtQaMaker(const StFgtQaMaker&);

   // deconstructor
   virtual ~StFgtQaMaker();

   // equals operator
   StFgtQaMaker& operator=(const StFgtQaMaker&);

   virtual Int_t Init();
   virtual Int_t Make();

   // modifiers
   void setDisc( Short_t discId );
   void setQuad( Short_t quadId );
   void setTimeBin( Short_t timeBin );
   void setQuadName( const Char_t* quadName );
   void setToPlotVsStrip( Char_t strip = 'R' );   // set anything besides 'r', 'R', or 'P' to do vs channel
                                                  // r is chamber 1 radial, 'R' is chamber 2 radial
   void setToSubtrPeds( Bool_t doIt );
   void setPedReaderFile( const Char_t* filename );
   void setPedThres( Float_t thres );
   void setXbins( Int_t nbins );
   void setXrange( Float_t low, Float_t high );
   void setYbins( Int_t nbins );
   void setYrange( Float_t low, Float_t high );
   void setBinFactors( Int_t factorX, Int_t factorY );

 protected:
   // for accessing the data
   StFgtCollection *mFgtCollectionPtr;

   // for knowing what & how to plot
   Short_t mDiscId, mQuadId, mTimeBin;
   Char_t mDoVsStrip;
   Bool_t mDoSubtrPeds;
   Int_t mXbins, mYbins;
   Float_t mXmin, mXmax, mYmin, mYmax;
   Int_t mBinFactorX, mBinFactorY;

   // for labeling the plot
   std::string mQuadName;

   // for the ped reader
   StFgtPedReader *mPedReader;
   std::string mPedFile;

   // threshold, in units of # sigma above average
   Float_t mPedThres;

 private:   
   ClassDef(StFgtQaMaker,1);

}; 

// inline functions

// modifiers
inline void StFgtQaMaker::setDisc( Short_t discId ){ mDiscId = discId; };
inline void StFgtQaMaker::setQuad( Short_t quadId ){ mQuadId = quadId; };
inline void StFgtQaMaker::setTimeBin( Short_t timeBin ){ mTimeBin = timeBin; };
inline void StFgtQaMaker::setQuadName( const Char_t* quadName ){ mQuadName = quadName; };
inline void StFgtQaMaker::setToSubtrPeds( Bool_t doIt ){ mDoSubtrPeds = doIt; };
inline void StFgtQaMaker::setPedReaderFile( const Char_t* filename ){ mPedFile = filename; };
inline void StFgtQaMaker::setPedThres( Float_t thres ){ mPedThres = thres; };

inline void StFgtQaMaker::setXbins( Int_t nbins ){ mXbins = nbins; };
inline void StFgtQaMaker::setXrange( Float_t low, Float_t high ){ mXmin = low; mXmax = high; };
inline void StFgtQaMaker::setYbins( Int_t nbins ){ mYbins = nbins; };
inline void StFgtQaMaker::setYrange( Float_t low, Float_t high ){ mYmin = low; mYmax = high; };

inline void StFgtQaMaker::setBinFactors( Int_t factorX, Int_t factorY ){
   mBinFactorX = factorX; 
   mBinFactorY = factorY; 
};

inline void StFgtQaMaker::setToPlotVsStrip( Char_t strip ){
   mDoVsStrip = ( (strip == 'R' || strip == 'P' || strip == 'r' ) ? strip : 'c' );
};

#endif
