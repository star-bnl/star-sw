
/***************************************************************************
 *
 * $Id: StHbtMaker.cxx,v 1.7 1999/09/24 01:23:08 fisyak Exp $
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

#include <iostream.h>
#include <stdlib.h>
//#include <string>
#include <vector>
#include "StChain.h"
#include "StHbtMaker.h"


ClassImp(StHbtMaker)


//_____________________________________________________________________________

    
StHbtMaker::StHbtMaker(const char*name, const char * title):StMaker(name,title)
{
  // StHbtMaker - constructor
  mHbtManager = new StHbtManager;
}
//_____________________________________________________________________________
StHbtMaker::~StHbtMaker()
{
  // StHbtMaker - destructor
  cout << "Inside ReaderMaker Destructor" << endl;
  SafeDelete(mHbtManager);  //
}
//_____________________________________________________________________________
void StHbtMaker::Clear(const char*)
{
  /* no-op - do not delete manager! */
  StMaker::Clear();
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

  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StHbtMaker::Finish()
{
  cout << mHbtManager->Report().c_str() << endl; //!
  mHbtManager->Finish();
  return StMaker::Finish();
}
//_____________________________________________________________________________
Int_t StHbtMaker::Make()
{
  cout << "\nStHbtMaker::Make -- processing event" << endl;
  if (mHbtManager->ProcessEvent()){
    return kStEOF;    // non-zero value returned --> end of file action
  }
  else{
    return kStOK;
  }
}
