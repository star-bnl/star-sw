/***************************************************************************
 *
 * $Id: StMuFgtOccTxtMkr.h,v 1.1 2012/03/06 01:32:35 sgliske Exp $
 * Author: S. Gliske, March 2012
 *
 ***************************************************************************
 *
 * Description: Writes out the total number of strips as well as the
 * number of clusters (i.e.  occupancy) per octant per event
 *
 ***************************************************************************
 *
 * $Log: StMuFgtOccTxtMkr.h,v $
 * Revision 1.1  2012/03/06 01:32:35  sgliske
 * creation
 *
 *
 **************************************************************************/

#ifndef _ST_MU_FGT_OCC_TXT_MKR_H_
#define _ST_MU_FGT_OCC_TXT_MKR_H_

#include "StMaker.h"

class StMuFgtOccTxtMkr : public StMaker {
 public:
   // constructors
   StMuFgtOccTxtMkr( const Char_t* name = "muDstOccTxtMkr" );

   // deconstructor
   virtual ~StMuFgtOccTxtMkr();

   // default equals operator and copy constructor OK

   virtual Int_t Make();

 protected:
 private:   
   ClassDef(StMuFgtOccTxtMkr,1);

}; 

#endif
