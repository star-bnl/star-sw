 /**********************************************
 *
 * $Id: StPmdCollection.cxx,v 1.5 2010/08/27 16:54:07 perev Exp $
 *
 * Author: Subhasis Chattopadhyay, July 2002.
 ***********************************************
 *
 * Description: Base class for PMD collection
 *
 ***********************************************
 * $Log: StPmdCollection.cxx,v $
 * Revision 1.5  2010/08/27 16:54:07  perev
 * WarnOff
 *
 * Revision 1.4  2004/11/15 23:27:16  subhasis
 * if() removed in ctor to stop valgrind error
 *
 * Revision 1.3  2003/10/14 10:16:31  subhasis
 * zeroed before delete
 *
 * Revision 1.2  2003/05/12 12:07:13  subhasis
 * Mapping added
 *
 ***********************************************/
#include "StPmdCollection.h"
#include "StPmdDetector.h"

ClassImp(StPmdCollection)

StPmdCollection::StPmdCollection(const Char_t * name):TDataSet(name) {
  for(int i=0; i<2; i++){
      mDetector[i]=0;
      StPmdDetector * det = new StPmdDetector(i,12);
      this->setDetector(det,i);
  }
}

StPmdCollection::~StPmdCollection(){
  for(int i=0; i<2; i++){
    if(mDetector[i]) delete mDetector[i];
  }
}
    
StPmdDetector*
StPmdCollection::detector(Int_t id)
{
    if(id >= 0 && id <= 1)
        return mDetector[id];
    else
        return 0;
}

void
StPmdCollection::setDetector(StPmdDetector* val, Int_t id)
{
    if (val) {
        if (id >= 0 && id <= 1) {

	    if (mDetector[id]) mDetector[id]=0;
            if (mDetector[id]) delete mDetector[id];
            mDetector[id] = val;
        }
    }
}



