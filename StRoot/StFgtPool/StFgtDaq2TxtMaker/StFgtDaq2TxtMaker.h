/*!
 * \class StFgtDaq2TxtMaker 
 * \author S. Gliske, Sept 2011
 */

/***************************************************************************
 *
 * $Id: StFgtDaq2TxtMaker.h,v 1.1 2011/10/07 19:55:37 sgliske Exp $
 * Author: S. Gliske, Sept 2011
 *
 ***************************************************************************
 *
 * Description: Converts a sfs file to a txt file, similar to that
 * which was done by an older code with the name of 'HALF_HACK' in the
 * title, although this is for several, complete quadrants, not just
 * half of a single quadrant.
 *
 * Output has a 'block' for each event, blocks being seperated by 3
 * empty lines.  There are 32 columns: channel (0-128), timebin (0-7),
 * and 30 columns of ADC values, 10 for each quadrant, one for each
 * APV chip.
 *
 ***************************************************************************
 *
 * $Log: StFgtDaq2TxtMaker.h,v $
 * Revision 1.1  2011/10/07 19:55:37  sgliske
 * creation
 *
 *
 **************************************************************************/

#ifndef _ST_FGT_DAQ_2_TXT_MAKER_
#define _ST_FGT_DAQ_2_TXT_MAKER_

#include <string>
#include <fstream>

#include "StMaker.h"
#include "StRoot/StEvent/StFgtEvent/StFgtEvent.h"


class StFgtDaq2TxtMaker : public StMaker {
 public:
   // constructors
   StFgtDaq2TxtMaker( const Char_t* name = "fgtDaq2Txt",
                      const Char_t* fgtRawBaseName = "FgtCosmicMaker",
                      const Char_t* outputfile = "testout.txt",
                      Short_t quadId = 0 );
   // default OK
   // StFgtDaq2TxtMaker(const StFgtDaq2TxtMaker&);

   // deconstructor
   ~StFgtDaq2TxtMaker();

   // equals operator -- default OK
   // StFgtDaq2TxtMaker& operator=(const StFgtDaq2TxtMaker&);

   void Clear(const Option_t* opts = "");
   Int_t Init();
   Int_t Make();
   Int_t Finish();

   void setIsCosmic( Bool_t itIs );

 protected:
   // for input
   std::string mInputName;
   StFgtEvent *mFgtEventPtr;

   // for output
   std::string mFileName;
   std::ofstream mFout;

   // for cuts
   Short_t mQuad;

   // temp storage
   Short_t *mData[7*128]; //!
   static const Int_t mDataSize1;
   Int_t mDataSize2;

   // whether is cosmic data or not
   Bool_t mIsCosmic;

 private:   
   ClassDef(StFgtDaq2TxtMaker,1);
}; 

// inline functions
inline void StFgtDaq2TxtMaker::setIsCosmic( Bool_t itIs ){ mIsCosmic = itIs; };


#endif
