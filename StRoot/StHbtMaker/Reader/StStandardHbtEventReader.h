/***************************************************************************
 *
 * $Id: StStandardHbtEventReader.h,v 1.13 2000/02/18 22:01:56 laue Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *       This is the HbtEventReader class to be used when running
 *  root4star with StEventReaderMaker.
 *  It inherits from StHbtReaderMaker
 *
 *  Since this StHbtEventReader class gets its input from StEvent in root4star,
 *  it needs to know what chain has the StEventReaderMaker on it.  So you have
 *  to initialize (thru SetTheChain()).
 *  Other StHbtEventReader classes (that might read ASCII files for example)
 *  would need other information, like the filename I guess, and so would
 *  have other private data members that they access.
 *
 ***************************************************************************
 *
 * $Log: StStandardHbtEventReader.h,v $
 * Revision 1.13  2000/02/18 22:01:56  laue
 * Implementation of a collections of StHbtEventWriters.
 * We now can write multiple microDsts at a time.
 *
 * All readers can have front-loaded cuts now. For that reason some
 * functionality was moved from the specific readers to the base class
 *
 * Revision 1.12  2000/01/25 17:35:27  laue
 * I. In order to run the stand alone version of the StHbtMaker the following
 * changes have been done:
 * a) all ClassDefs and ClassImps have been put into #ifdef __ROOT__ statements
 * b) unnecessary includes of StMaker.h have been removed
 * c) the subdirectory StHbtMaker/doc/Make has been created including everything
 * needed for the stand alone version
 *
 * II. To reduce the amount of compiler warning
 * a) some variables have been type casted
 * b) some destructors have been declared as virtual
 *
 * Revision 1.11  1999/12/03 22:24:37  lisa
 * (1) make Cuts and CorrFctns point back to parent Analysis (as well as other way). (2) Accommodate new PidTraits mechanism
 *
 * Revision 1.10  1999/11/24 22:01:41  laue
 * reader adopted to the new StEvent 2.x
 *
 * Revision 1.9  1999/10/15 01:57:37  lisa
 * Important enhancement of StHbtMaker - implement Franks CutMonitors
 * ----------------------------------------------------------
 * This means 3 new files in Infrastructure area (CutMonitor),
 * several specific CutMonitor classes in the Cut area
 * and a new base class in the Base area (StHbtCutMonitor).
 * This means also changing all Cut Base class header files from .hh to .h
 * so we have access to CutMonitor methods from Cint command line.
 * This last means
 * 1) files which include these header files are slightly modified
 * 2) a side benefit: the TrackCuts and V0Cuts no longer need
 * a SetMass() implementation in each Cut class, which was stupid.
 * Also:
 * -----
 * Include Franks StHbtAssociationReader
 * ** None of these changes should affect any user **
 *
 * Revision 1.8  1999/09/24 01:23:14  fisyak
 * Reduced Include Path
 *
 * Revision 1.7  1999/09/17 22:38:03  lisa
 * first full integration of V0s into StHbt framework
 *
 * Revision 1.6  1999/09/16 18:48:01  lisa
 * replace placeholder HbtV0Track stuff with Helens StHbtV0 classes
 *
 * Revision 1.5  1999/09/09 02:59:55  lisa
 * fix Randys factor of 2 in CoulombCorrection AND add SetCut methods to StStandardHbtEventReader which were forgotten last commit
 *
 * Revision 1.4  1999/09/08 04:15:53  lisa
 * persistent microDST implementation tweaked to please fickle solaris details
 *
 * Revision 1.3  1999/09/03 22:39:17  lisa
 * Readers now MUST have Report() methods and MAY have WriteHbtEvent() methods
 *
 * Revision 1.2  1999/07/06 22:33:24  lisa
 * Adjusted all to work in pro and new - dev itself is broken
 *
 * Revision 1.1.1.1  1999/06/29 16:02:57  lisa
 * Installation of StHbtMaker
 *
 * With the .DEV->DEV revolution of June 1999, must change
 * the way that this thing gets StEvent object
 * no more going through the chain.  Now, we have to have
 * a pointer to the object (of type StEventMaker, which is
 * derived from StMaker) which has a method to get the StEvent.
 **************************************************************************/

#ifndef StStandardHbtEventReader_hh
#define StStandardHbtEventReader_hh

#include "StHbtMaker/Base/StHbtEventReader.hh"
//#include "StChain.h"
#include "StMaker.h"
#include "StV0MiniDstMaker/StV0MiniDstMaker.h"

class StPionPlus;
class StKaonPlus;
class StProton;
class StTpcDedxPidAlgorithm;
class StParticleDefinition;

class TOrdCollection;
class StStandardHbtEventReader : public StHbtEventReader{

private:

  StMaker* mTheEventMaker;      //! this is the chain where the StEventReaderMaker is
  StV0MiniDstMaker* mTheV0Maker; //! this is the chain where the StV0MiniDstMaker is
  long              mV0;        //! Number of v0s looked at to date

 protected:
 TOrdCollection *mCollection; //!

public:
  StStandardHbtEventReader();
  ~StStandardHbtEventReader();

  StHbtEvent* ReturnHbtEvent();
  StHbtString Report();

  void SetTheEventMaker(StMaker*);
  StMaker* TheEventMaker();
  void SetTheV0Maker(StV0MiniDstMaker*);
  StV0MiniDstMaker* TheV0Maker();

#ifdef __ROOT__
  ClassDef(StStandardHbtEventReader, 1)
#endif
};

inline void StStandardHbtEventReader::SetTheEventMaker(StMaker* maker){mTheEventMaker=maker;}
inline StMaker* StStandardHbtEventReader::TheEventMaker(){return mTheEventMaker;}
inline void StStandardHbtEventReader::SetTheV0Maker(StV0MiniDstMaker* maker){mTheV0Maker=maker;}
inline StV0MiniDstMaker* StStandardHbtEventReader::TheV0Maker(){return mTheV0Maker;}

#endif

