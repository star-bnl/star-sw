/*!
  \class StRedoTracks
  
  StRedoTracks redoes the space charge correction for tracks

*/

#ifndef STAR_StRedoTracks
#define STAR_StRedoTracks

#ifndef StMaker_H
#include "StMaker.h"
#endif

class StMagUtilities;
class StTpcDbMaker;

class StRedoTracks : public StMaker {
 
 private:

 protected:
  StMagUtilities* m_ExB;    //!
  StTpcDbMaker* tpcDbMaker; //!
  Bool_t redo;

 public: 
  StRedoTracks(const char *name="redux", StTpcDbMaker* mkr=0);
  virtual       ~StRedoTracks();
  virtual Int_t  Init();
  virtual Int_t  Make();
          void   Do()     { redo = kTRUE; }
          void   DontDo() { redo = kFALSE; }

  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StRedoTracks.h,v 1.4 2014/08/06 11:43:08 jeromel Exp $ built " __DATE__ " " __TIME__ ; return cvs;}

  ClassDef(StRedoTracks, 0)   //StAF chain virtual base class for Makers
};
    
#endif

//_____________________________________________________________________________
// $Id: StRedoTracks.h,v 1.4 2014/08/06 11:43:08 jeromel Exp $
// $Log: StRedoTracks.h,v $
// Revision 1.4  2014/08/06 11:43:08  jeromel
// Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
//
// Revision 1.3  2003/07/02 15:32:38  genevb
// Remove StTpcDb.so dependence
//
// Revision 1.1  2003/06/28 00:46:15  genevb
// Introduction of StRedoTracks class
//
//
