// $Id: StjBEMCTree.cxx,v 1.5 2008/08/11 03:50:41 tai Exp $
#include "StjBEMCTree.h"

#include "StjTowerEnergyListReader.h"

ClassImp(StjBEMCTree)

StjTowerEnergyList StjBEMCTree::getEnergyList()
{
  return _reader->getEnergyList();
}
