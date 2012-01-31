/***************************************************************************
 *
 * $Id: StFgtA2CMakerNoDB.h,v 1.1 2012/01/31 08:41:45 sgliske Exp $
 * Author: S. Gliske, Jan 2012
 *
 ***************************************************************************
 *
 * Description: Converts the ADC value to charge and optionally
 * removes strips with status not passing the mask.  Computing the
 * charge currently involves
 *
 * 1) pedestal subtraction
 * 2) applying minimum threshold (both fixed and multiples of the pedistal st. err.)
 * 3) fitting the pulse shape
 * 3) applying gain
 *
 * The status map is applied as follows: status 0x00 is good, all else
 * is bad.  Strips are removed if the status bit anded with the mask
 * is non-zero. To remove all strips with any status bit set
 * set the mask to 0xFF.  To ignore status, set the mask to 0x00.  To remove only
 * strips with bit 3 set, set the mask to 0x04, etc.  Status is
 * currently only a uchar.
 *
 * This maker was broken off of the StFgtA2CMaker Jan 31st, 2012.
 * Before then, these two makers were the same maker.
 *
 ***************************************************************************
 *
 * $Log: StFgtA2CMakerNoDB.h,v $
 * Revision 1.1  2012/01/31 08:41:45  sgliske
 * split from the StFgtA2CMaker
 *
 *
 **************************************************************************/

#ifndef _ST_FGT_A2C_MAKER_NODB_H
#define _ST_FGT_A2C_MAKER_NODB_H

#include <string>
#include "StMaker.h"

class StFgtPedReader;
class StFgtStatusReader;

class StFgtA2CMakerNoDB : public StMaker {
 public:
   // constructors
   StFgtA2CMakerNoDB( const Char_t* name = "fgtA2CMaker" );

   // default OK
   // StFgtA2CMakerNoDB(const StFgtA2CMakerNoDB&);

   // deconstructor
   virtual ~StFgtA2CMakerNoDB();

   // equals operator -- default OK
   // StFgtA2CMakerNoDB& operator=(const StFgtA2CMakerNoDB&);

   virtual Int_t Init();
   virtual Int_t Make();

   // modifiers
   void setPedReaderFile( const Char_t* filename );
   void setStatusReaderFile( const Char_t* filename );
   void setAbsThres( Float_t thres );  // set to below -4096 to skip cut
   void setRelThres( Float_t thres );  // set to zero to skip cut
   void doCutBadStatus( Bool_t doIt );
   void setStatusMask( UChar_t mask );

   virtual const char *GetCVS() const
   {static const char cvs[]="Tag $Name:  $ $Id: StFgtA2CMakerNoDB.h,v 1.1 2012/01/31 08:41:45 sgliske Exp $ built "__DATE__" "__TIME__ ; return cvs;}

 protected:
   // for the ped reader
   StFgtPedReader *mPedReader;
   std::string mPedFile;

   // for the strip status reader
   StFgtStatusReader *mStatusReader;
   std::string mStatusFile;

   // other parameters
   Int_t mStatusMask;
   Float_t mAbsThres, mRelThres;

   // for fitting
   TF1 *mPulseShapePtr;
   TH1F *mHistPtr;
 
 private:   
   ClassDef(StFgtA2CMakerNoDB,1);

}; 

// inline functions

// deconstructor
inline StFgtA2CMakerNoDB::~StFgtA2CMakerNoDB(){ /* */ };

// modifiers
inline void StFgtA2CMakerNoDB::setPedReaderFile( const Char_t* filename ){ mPedFile = filename; };
inline void StFgtA2CMakerNoDB::setStatusReaderFile( const Char_t* filename ){ mStatusFile = filename; };
inline void StFgtA2CMakerNoDB::setAbsThres( Float_t thres ){ mAbsThres = thres; };
inline void StFgtA2CMakerNoDB::setRelThres( Float_t thres ){ mRelThres = thres; };
inline void StFgtA2CMakerNoDB::doCutBadStatus(  Bool_t doIt ){ mStatusMask = 0xFF; };
inline void StFgtA2CMakerNoDB::setStatusMask( UChar_t mask ){ mStatusMask = mask; };

#endif
