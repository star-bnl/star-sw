// $Id: StjBEMCTree.cxx,v 1.1 2008/11/27 07:35:22 tai Exp $
#include "StjBEMCTree.h"

#include "StjTowerEnergyListReader.h"

ClassImp(StjBEMCTree)

StjTowerEnergyList StjBEMCTree::getEnergyList()
{
  return _reader->getEnergyList();
}
