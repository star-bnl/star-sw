/**********************************************************************
 *
 * $Id: StEStructEventReader.h,v 1.2 2006/04/04 22:05:05 porter Exp $
 *
 * Author: Jeff Porter 
 *
 **********************************************************************
 *
 * Description:  Abstract event reader class
 *
 ***********************************************************************/
#ifndef __STEBYEEVENTREADER__H
#define __STEBYEEVENTREADER__H

// -> for rootcint preprocessing
#include "TROOT.h"

//-> forward declaration
class StEStructEvent;

class StEStructEventReader {


public:

  virtual ~StEStructEventReader();
  virtual StEStructEvent* next()  = 0;
  virtual bool done()          = 0;

  // now expanding the class to return quantities that may
  // be available only to an event generator. Here we'll not make
  // pure virtual but return nominal values...

  virtual double getImpact();       // typically impact parameter
  virtual double getBinary();       // typically N binary collisions
  virtual double getParticipants(); // typically N participants
  virtual double getNPartonic();    // charactorisation of N-partons (pythia)


  ClassDef(StEStructEventReader,1)

};

inline double StEStructEventReader::getImpact(){ return 0.; };
inline double StEStructEventReader::getBinary(){ return 1.; };
inline double StEStructEventReader::getParticipants(){ return 2.; };
inline double StEStructEventReader::getNPartonic(){ return 0.; };

#endif

/***********************************************************************
 *
 * $Log: StEStructEventReader.h,v $
 * Revision 1.2  2006/04/04 22:05:05  porter
 * a handful of changes:
 *  - changed the StEStructAnalysisMaker to contain 1 reader not a list of readers
 *  - added StEStructQAHists object to contain histograms that did exist in macros or elsewhere
 *  - made centrality event cut taken from StEStructCentrality singleton
 *  - put in  ability to get any max,min val from the cut class - one must call setRange in class
 *
 * Revision 1.1  2003/10/15 18:20:32  porter
 * initial check in of Estruct Analysis maker codes.
 *
 *
 *********************************************************************/




