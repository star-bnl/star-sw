/********************************************************
 *
 * $Id: StTriggerDataMother.h,v 1.1 2004/11/29 17:28:31 mvl Exp $
 *
 * Author: Jan Balewski
 ********************************************************
 *
 * Description: abstract class for trigger data from all years
 *
 *******************************************************/

#ifndef StTriggerDataMother_hh
#define StTriggerDataMother_hh

#include "StTriggerData.h"

class EztTrigBlob;
class StTriggerDataMother : public StTriggerData {
  private:
   StTriggerData *fCurrent;
  public:

  ~StTriggerDataMother();
  StTriggerDataMother(EztTrigBlob*);
  StTriggerData * data() const { return fCurrent;}
  // .............. supply missing  virtual functions:  
  void dump() const {fCurrent->dump();}
  unsigned int version() const {return fCurrent->version();}
  unsigned int numberOfPreXing() const {return fCurrent->numberOfPreXing();}
  unsigned int numberOfPostXing() const {return fCurrent->numberOfPostXing();}
  unsigned int triggerWord() const {return fCurrent->triggerWord();}
  unsigned int actionWord() const {return fCurrent->actionWord();}
  short unsigned int tcuBits() const {return fCurrent->tcuBits();}
  char* getTriggerStructure() {return fCurrent->getTriggerStructure();}
  int getRawSize()  const {return fCurrent->getRawSize();}
  unsigned int token()  const {return fCurrent->token();} 

  ClassDef(StTriggerDataMother,1) 
};
    
#endif

/*
 * $Log: StTriggerDataMother.h,v $
 * Revision 1.1  2004/11/29 17:28:31  mvl
 * New class for trigger versioning (by Jan)
 *
 */
