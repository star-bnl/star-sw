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
  auto result = kStOK;

  if ( mEntry < mTreeEntries ) {

    if ( mInputTree ) {
      mInputTree -> GetEntry( mEntry++ );
    }
    if ( IAttr("debug") ) {
      mEvent->Print();
    }
  }
  else {

    result = kStEOF;

  }
  return result;
};
