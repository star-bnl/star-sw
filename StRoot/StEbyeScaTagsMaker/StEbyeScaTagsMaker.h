#ifndef StEbyeScaTagsMaker_HH
#define StEbyeScaTagsMaker_HH

///////////////////////////////////////////////////////////////////////////////
//
// $Id: StEbyeScaTagsMaker.h,v 1.6 2000/02/04 22:44:30 jgreid Exp $
//
// StEbyeScaTagsMaker
//
// Description: 
//  Sample maker to access and analyze StEvent
//
// Environment:
//  Software developed for the STAR Detector at Brookhaven National Laboratory
//
// Author List: 
//  Jeff Reid, UW, 2/99
//
// History:
//
// $Log: StEbyeScaTagsMaker.h,v $
// Revision 1.6  2000/02/04 22:44:30  jgreid
// added functionality for ScaTags to be picked up by the TagDB after filling
//
//
///////////////////////////////////////////////////////////////////////////////
#include <iostream.h>
#include <fstream.h>
#include "StMaker.h"
#include "ScaTag.h"
#include "tables/St_ScaTag_Table.h"

class StEvent;
class StRun;

class StEbyeScaTagsMaker : public StMaker {

private:

  // define tag-based member objects
  St_ScaTag* mTagHeader; //!
  ScaTag_st* mTag; //!

protected:
  void fillTag(StEvent& event);    // does the actual work;
  float mtInverseSlope(double *mthisto, int ibegin, int istop); 

public:

  StEbyeScaTagsMaker(const Char_t *name="phi", const Char_t *title="phi");
  ~StEbyeScaTagsMaker();
  void Clear(Option_t *option="");
  Int_t Init();
  Int_t  Make();
  Int_t  Finish();

  // Tag accessor
  ScaTag_st* tag();

  // output Tag info to screen
  void printTag(ostream& = cout);

  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StEbyeScaTagsMaker.h,v 1.6 2000/02/04 22:44:30 jgreid Exp $ built "__DATE__" "__TIME__ ; return cvs;}

  ClassDef(StEbyeScaTagsMaker, 1)
};

#endif
