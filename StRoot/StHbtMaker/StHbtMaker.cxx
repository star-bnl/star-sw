
/***************************************************************************
 *
 * $Id: StHbtMaker.cxx,v 1.3 1999/07/15 13:57:10 perev Exp $
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
#include "StChain/StChain.h"
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

  cout << "StHbtMaker::Init() - requesting a Report " << endl;
  cout << mHbtManager->Report() << endl;

  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StHbtMaker::Finish()
{
  cout << mHbtManager->Report() << endl;
  mHbtManager->Finish();
  return StMaker::Finish();
}
//_____________________________________________________________________________
Int_t StHbtMaker::Make()
{
  cout << "\nStHbtMaker::Make -- processing event" << endl;
  mHbtManager->ProcessEvent();
  return kStOK;
}
