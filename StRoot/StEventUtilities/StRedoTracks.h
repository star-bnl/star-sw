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

class StRedoTracks : public StMaker {
 
 private:

 protected:
  StMagUtilities* m_ExB; //!
  Bool_t redo;

 public: 
  StRedoTracks(const char *name="redux");
  virtual       ~StRedoTracks();
  virtual Int_t  Init();
  virtual Int_t  Make();
          void   Do()     { redo = kTRUE; }
          void   DontDo() { redo = kFALSE; }

  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StRedoTracks.h,v 1.1 2003/06/28 00:46:15 genevb Exp $ built "__DATE__" "__TIME__ ; return cvs;}

  ClassDef(StRedoTracks, 0)   //StAF chain virtual base class for Makers
};
    
#endif

//_____________________________________________________________________________
// $Id: StRedoTracks.h,v 1.1 2003/06/28 00:46:15 genevb Exp $
// $Log: StRedoTracks.h,v $
// Revision 1.1  2003/06/28 00:46:15  genevb
// Introduction of StRedoTracks class
//
//
