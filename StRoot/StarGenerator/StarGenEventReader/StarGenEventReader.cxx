#include "StarGenEventReader.h"
#include "TTree.h"

Int_t StarGenEventReader::Init()
{
  if ( SAttr("input") ) {
    SetInputFile( SAttr("input"),"genevents","primaryEvent" );
  }
  return kStOK;
};

Int_t StarGenEventReader::Generate()
{
  if ( mInputTree ) mInputTree -> GetEntry( mEntry++ );
  if ( IAttr("debug") ) mEvent->Print();
  return kStOK;
};
