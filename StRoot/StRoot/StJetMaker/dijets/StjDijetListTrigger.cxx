// $Id: StjDijetListTrigger.cxx,v 1.1 2008/09/11 23:34:53 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjDijetListTrigger.h"

ClassImp(StjDijetListTrigger)

using namespace std;

StjDijetList StjDijetListTrigger::operator()(const StjDijetList &inList)
{
  StjDijetList retList;
  for(StjDijetList::const_iterator inIter = inList.begin(); inIter != inList.end(); ++inIter)
    {
      StjDijetList outListForOneInput = processOneItem(*inIter);
      copy(outListForOneInput.begin(), outListForOneInput.end(), back_inserter(retList));
    }
  return retList;
}
