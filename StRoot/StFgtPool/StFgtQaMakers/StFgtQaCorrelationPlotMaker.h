/*!
 * \class StFgtCorrelationPlotMaker 
 * \author S. Gliske, Sept 2011
 */

/***************************************************************************
 *
 * $Id: StFgtQaCorrelationPlotMaker.h,v 1.1 2012/01/31 09:26:17 sgliske Exp $
 * Author: S. Gliske, Sept 2011
 *
 ***************************************************************************
 *
 * Description: Plots either the covariance or correlation
 * coefficients between channels or groups of channels.
 *
 ***************************************************************************
 *
 * $Log: StFgtQaCorrelationPlotMaker.h,v $
 * Revision 1.1  2012/01/31 09:26:17  sgliske
 * StFgtQaMakers moved to StFgtPool
 *
 * Revision 1.2  2011/11/01 19:07:24  sgliske
 * Updated to correspond with StEvent containers, take 2.
 *
 * Revision 1.1  2011/09/28 17:48:50  sgliske
 * minor updates
 *
 *
 **************************************************************************/

#ifndef _ST_FGT_QA_CORRELATION_PLOT_MAKER_H_
#define _ST_FGT_QA_CORRELATION_PLOT_MAKER_H_

#include <string>
#include <Rtypes.h>
#include <TH2F.h>

#include "StFgtQaMaker.h"

class StFgtQaCorrelationPlotMaker : public StFgtQaMaker {
 public:
   // constructors
   StFgtQaCorrelationPlotMaker( const Char_t* name = "FGT_QA_Adc_vs_Channel",
                              Short_t discId = 0,
                              Short_t quadId = 0,
                              const Char_t* quadName = "000" );

   // copy constr.
   StFgtQaCorrelationPlotMaker(const StFgtQaCorrelationPlotMaker&);

   // equals operator
   StFgtQaCorrelationPlotMaker& operator=(const StFgtQaCorrelationPlotMaker&);

   // deconstructor
   virtual ~StFgtQaCorrelationPlotMaker();

   Int_t Init();
   Int_t Make();
   Int_t Finish();

   // accessor
   const TH2F* getCovarianceHist() const;
   const TH2F* getCorrelationHist() const;

   // modifiers
   void setComputeCorrelations( Bool_t doCor = 1 );

 protected:
   // what to plot
   Bool_t mComputeCor;

   // the histograms
   TH2F *mCovHist;
   TH2F *mCorHist;

   // need a struct for after ped subtraction
   struct data_t {
      Int_t count;
      Float_t val;

      data_t( Int_t i, Float_t v );
   };

   // to set title
   void getTitle( Bool_t isCor, std::string& title );

   // to hold sums
   Int_t mNbins;
   Float_t **mMatrix;
   Float_t *mSum;
   Int_t *mN;

 private:
   // forbid this from being changed
   void setToSubtrPeds( Bool_t doIt );

   ClassDef(StFgtQaCorrelationPlotMaker,1);

}; 

// inline functions

// accessor
inline const TH2F* StFgtQaCorrelationPlotMaker::getCovarianceHist() const { return mCovHist; };
inline const TH2F* StFgtQaCorrelationPlotMaker::getCorrelationHist() const { return mCorHist; };

// modifiers
inline void StFgtQaCorrelationPlotMaker::setComputeCorrelations( Bool_t doCor ){ mComputeCor = doCor; };

// for the struct
inline StFgtQaCorrelationPlotMaker::data_t::data_t( Int_t i, Float_t v ) : count(i), val(v) {
   //cout << "y " << count << ' ' << val << endl;
};

#endif
