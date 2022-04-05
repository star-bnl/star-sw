/*!
 * \class  StMtdEvtFilterApplyMaker
 * \brief  Skip events using MTD criteria
 * \author G. Van Buren, R. Ma
 * \date   Apr 2015
 *
 * Skip production of events using MTD criteria stored in tags.root
 * under the MtdTrackFilterTag structure, which is filled in the
 * StMtdEvtFilterMaker class
 *
 */


#ifndef StMtdEvtFilterApplyMaker_hh     
#define StMtdEvtFilterApplyMaker_hh

#include "StTagFilterMaker.h"

class StMtdEvtFilterApplyMaker : public StTagFilterMaker {
 public:
  
  StMtdEvtFilterApplyMaker(const Char_t *name="MtdEvtFilterApply");
  ~StMtdEvtFilterApplyMaker() {}
  
  bool SkipEvent();
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StMtdEvtFilterApplyMaker.h,v 1.1 2015/05/01 21:25:58 jeromel Exp $ built " __DATE__ " " __TIME__ ; 
    return cvs;
  }
  

  ClassDef(StMtdEvtFilterApplyMaker,0)    
};
#endif

/* -------------------------------------------------------------------------
 * $Id: StMtdEvtFilterApplyMaker.h,v 1.1 2015/05/01 21:25:58 jeromel Exp $
 * $Log: StMtdEvtFilterApplyMaker.h,v $
 * Revision 1.1  2015/05/01 21:25:58  jeromel
 * First version of the DataFiler + one imp: MTD. Code from GVB reviewed & closed (VP+JL))
 *
 *
 * -------------------------------------------------------------------------
 */

