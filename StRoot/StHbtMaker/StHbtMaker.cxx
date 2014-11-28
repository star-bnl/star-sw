
/***************************************************************************
 *
 * $Id: StHbtMaker.cxx,v 1.14 2007/04/28 17:56:14 perev Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *              Maker class is the interface with root4star/Maker framework
 *
 ***************************************************************************
 *
 * $Log: StHbtMaker.cxx,v $
 * Revision 1.14  2007/04/28 17:56:14  perev
 * Redundant StChain.h removed
 *
 * Revision 1.13  2003/09/02 17:58:19  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.12  2003/01/31 20:29:37  magestro
 * Small change to eliminate compiler warning
 *
 * Revision 1.11  2001/09/05 20:40:42  laue
 * Updates of the hbtMuDstTree microDSTs
 *
 * Revision 1.9  2000/04/03 16:21:12  laue
 * some include files changed
 *
 * Revision 1.8  2000/01/25 17:33:38  laue
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
 * Revision 1.7  1999/09/24 01:23:08  fisyak
 * Reduced Include Path
 *
 * Revision 1.6  1999/09/08 04:15:51  lisa
 * persistent microDST implementation tweaked to please fickle solaris details
 *
 * Revision 1.5  1999/09/05 02:58:10  lisa
 * add ASCII microDST reader/writer AND franksParticle cuts
 *
 * Revision 1.4  1999/07/26 16:21:25  lisa
 * always convert string to char when output - needed on solaris
 *
 * Revision 1.3  1999/07/15 13:57:10  perev
 * cleanup
 *
 * Revision 1.2  1999/07/06 22:33:18  lisa
 * Adjusted all to work in pro and new - dev itself is broken
 *
 * Revision 1.1.1.1  1999/06/29 16:02:56  lisa
 * Installation of StHbtMaker
 *
 **************************************************************************/

#include <Stiostream.h>
#include <stdlib.h>

#ifdef __ROOT__
#endif

#include "StHbtMaker.h"

#ifdef __ROOT__
ClassImp(StHbtMaker)
#endif

//_____________________________________________________________________________

    
StHbtMaker::StHbtMaker(const char*name, const char * title) : 
#ifdef __ROOT__
  StMaker(name,title) ,
#endif
mDebug(0) 
{
  // StHbtMaker - constructor
  mHbtManager = new StHbtManager;
  cout << string::npos << endl;
}
//_____________________________________________________________________________
StHbtMaker::~StHbtMaker()
{
  // StHbtMaker - destructor
    cout << "Inside ReaderMaker Destructor" << endl;
#ifdef __ROOT__
    SafeDelete(mHbtManager);  //
#else
  delete mHbtManager;
#endif
}
//_____________________________________________________________________________
void StHbtMaker::Clear(const char*)
{
  /* no-op - do not delete manager! */
#ifdef __ROOT__
  StMaker::Clear();
#endif
}
//_____________________________________________________________________________
//_____________________________________________________________________________
Int_t StHbtMaker::Init()
{
  if (mHbtManager->Init()){
    cout << "StHbtMaker::Init() - Manager init failed " << endl;
    return (2);
  }
  cout << "StHbtMaker::Init() - requesting a Report " << endl;
  StHbtString tempString = mHbtManager->Report();
  cout << "Got the report, now let me try to put it to screen" << endl;
  cout << tempString.c_str() << endl; //!
#ifdef __ROOT__
  return StMaker::Init();
#else
  return 0;
#endif
}
//_____________________________________________________________________________
Int_t StHbtMaker::Finish()
{
  cout << mHbtManager->Report().c_str() << endl; //!
  mHbtManager->Finish();
#ifdef __ROOT__
  return StMaker::Finish();
#else
  return 0;
#endif
}
//_____________________________________________________________________________
Int_t StHbtMaker::Make()
{
  if (mDebug>1) cout << "\nStHbtMaker::Make -- processing event" << endl;
#ifdef __ROOT__
  if (mHbtManager->ProcessEvent()){
    return kStEOF;    // non-zero value returned --> end of file action
  }
  else{
    return kStOK;
  }
#else
  return  mHbtManager->ProcessEvent();
#endif
}
