/*!
  \class StPrimaryMaker
  
  StPrimaryMaker finds primary tracks

*/

#ifndef STAR_StPrimaryMaker
#define STAR_StPrimaryMaker

#ifndef StMaker_H
#include "StMaker.h"
#endif

class St_egr_propagate;
class St_egr_egrpar;


class StPrimaryMaker : public StMaker {
 
 private:
  Int_t             m_flag;        //
  St_egr_propagate *m_tp_param;    //!
  St_egr_egrpar    *m_egr_egrpar;  //!
  St_egr_egrpar    *m_egr2_egrpar; //!

 protected:

 public: 
  StPrimaryMaker(const char *name="primary");
  virtual       ~StPrimaryMaker();
  virtual Int_t  Init();
  virtual Int_t  Make();

  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StPrimaryMaker.h,v 1.19 2002/02/18 19:48:20 genevb Exp $ built "__DATE__" "__TIME__ ; return cvs;}

  ClassDef(StPrimaryMaker, 0)   //StAF chain virtual base class for Makers
    };
    
#endif

//_____________________________________________________________________________
// $Id: StPrimaryMaker.h,v 1.19 2002/02/18 19:48:20 genevb Exp $
// $Log: StPrimaryMaker.h,v $
// Revision 1.19  2002/02/18 19:48:20  genevb
// Separation of primary vertex and track finding, other minor changes
//
//
// For older cvs log information, examine (via CVS browser or checkout)
// version 1.18
//
