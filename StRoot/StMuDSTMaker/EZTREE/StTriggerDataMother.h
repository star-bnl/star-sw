/********************************************************
 *
 * $Id: StTriggerDataMother.h,v 1.2 2004/12/09 03:06:03 mvl Exp $
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

  unsigned  char * getDsm0_EEMC(int prepost=0) const {return fCurrent->getDsm0_EEMC( prepost) ; }
  unsigned short int * getDsm1_EEMC(int prepost=0) const  {return fCurrent->getDsm1_EEMC( prepost);}
  unsigned short int * getDsm2_EMC()  const  {return fCurrent->getDsm2_EMC();}
  unsigned short int * getDsm3()      const  {return fCurrent->getDsm3() ;}


  ClassDef(StTriggerDataMother,1) 
};
    
#endif

/*
 * $Log: StTriggerDataMother.h,v $
 * Revision 1.2  2004/12/09 03:06:03  mvl
 * Added access functions for intermediate DSM levels (Jan B)
 *
 * Revision 1.1  2004/11/29 17:28:31  mvl
 * New class for trigger versioning (by Jan)
 *
 */
