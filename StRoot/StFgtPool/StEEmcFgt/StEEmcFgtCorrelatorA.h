/***************************************************************************
 *
 * $Id: StEEmcFgtCorrelatorA.h,v 1.1 2012/05/09 17:26:26 sgliske Exp $
 * Author: S. Gliske, May 2012
 *
 ***************************************************************************
 *
 * Description: looks for MIP response in the ESMD, forms a line
 * between the ESMD position and the primary vertex, and then looks at
 * the FGT response near the line.
 *
 ***************************************************************************
 *
 * $Log: StEEmcFgtCorrelatorA.h,v $
 * Revision 1.1  2012/05/09 17:26:26  sgliske
 * creation
 *
 *
 **************************************************************************/

#ifndef _StEEmcFgtCorrelatorA_H_
#define _StEEmcFgtCorrelatorA_H_

#include "StMaker.h"
#include "TVector3.h"
#include <string>

class StEEmcRawMapMaker;

class StEEmcFgtCorrelatorA : public StMaker {
 public:
   // constructors
   StEEmcFgtCorrelatorA( const Char_t* name = "EEmcFgtCorA", const Char_t* rawMapMkrName = "EEmcRawMapMaker" );

   // deconstructor
   virtual ~StEEmcFgtCorrelatorA();

   // default equals operator and copy constructor OK
   virtual Int_t Init();
   virtual Int_t Make();
   virtual void Clear( Option_t *opt = "" );

   Int_t setInput( const Char_t *name, Int_t type );   // type = 0 is StEvent, type == 1 is MuDst

 protected:
   Int_t mInputType;
   std::string mInputName;
   TVector3 mVertex;

   StEEmcRawMapMaker *mEEmcRawMapMkr;

   Float_t mMipMin, mMipMax, mSigThres;

   Int_t loadVertex();

 private:   
   ClassDef(StEEmcFgtCorrelatorA,1);

}; 

#endif
