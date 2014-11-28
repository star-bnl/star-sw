/*!
 * \class  StMuScalerCutter
 * \brief  Apply cuts to events based on corrupt RICH scalers
 * \author G. Van Buren, BNL, Feb. 2009
 *
 * Use ratios of RICH scalers to determine corrupt values which
 * can cause erroneous application of luminosity dependent
 * TPC distortion corrections.
 *
 *
 * $Id: StMuScalerCutter.h,v 1.2 2014/08/06 11:43:31 jeromel Exp $
 * $Log: StMuScalerCutter.h,v $
 * Revision 1.2  2014/08/06 11:43:31  jeromel
 * Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
 *
 * Revision 1.1  2009/02/04 21:37:45  genevb
 * Introduction of class to determine events with bad RICH scalers
 *
 *
 * -------------------------------------------------------------------------
 */
#ifndef StMuScalerCutter_hh     
#define StMuScalerCutter_hh

#include "StMaker.h"

class StMuEvent;

class StMuScalerCutter : public StMaker {
public:

    StMuScalerCutter(const Char_t *name="muScalerCutter");
    ~StMuScalerCutter() {}
    Int_t  Make();

    virtual const char *GetCVS() const {
      static const char cvs[]="Tag $Name:  $ $Id: StMuScalerCutter.h,v 1.2 2014/08/06 11:43:31 jeromel Exp $ built " __DATE__ " " __TIME__ ; 
      return cvs;
    }

    static bool accept(StMuEvent*);

    ClassDef(StMuScalerCutter,0)
};
#endif
