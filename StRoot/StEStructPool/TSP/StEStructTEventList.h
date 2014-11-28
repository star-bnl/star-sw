/*********************************************************************
 *
 *  $Id: StEStructTEventList.h,v 1.1 2010/09/02 21:55:43 prindle Exp $
 *
 *  Author: Duncan Prindle
 *
 *********************************************************************
 *
 *  Discription:
 *    StMuDstReader has ability to read arbitray events which are given by a TEventList
 *    TEventList doesn't preserve order though, so add an append method and hope list doesn't 
 *    get triggered to sort itself somehow.
 *
 ********************************************************************/

#ifndef StEStructTEventList_h
#define StEStructTEventList_h

#include <TROOT.h>
#include <TEventList.h>

class StEStructTEventList : public TEventList {
  private:
  public:
    StEStructTEventList();
    virtual ~StEStructTEventList();

    void Append(Long64_t entry);

    ClassDef(StEStructTEventList,1)
};


#endif
