// $Id: StjFMSTree.cxx,v 1.1 2017/05/22 19:36:06 zchang Exp $
#include "StjFMSTree.h"

#include "StjTowerEnergyListReader.h"

ClassImp(StjFMSTree)

StjTowerEnergyList StjFMSTree::getEnergyList()
{
  return _reader->getEnergyList();
}
