/*!
 * \class StPmdCollection
 * \author  Subhasis Chattopadhyay
 */
/************************************************
 *
 * $Id: StPmdCollection.h,v 1.1 2002/08/27 12:18:32 subhasis Exp $
 *
 *Author: Subhasis Chattopadhyay
 ************************************************
 *
 * Description: Base class for PMD Collection which
 *              includes both PMD and CPV
 *
 * $Log: StPmdCollection.h,v $
 * Revision 1.1  2002/08/27 12:18:32  subhasis
 * First version
 *
 *
 **************************************************/
#ifndef StPmdCollection_hh
#define StPmdCollection_hh

#include "StObject.h"
#include "StPmdHit.h"

class StPmdDetector;

class StPmdCollection : public TDataSet {
public:
    StPmdCollection(Char_t *);
    ~StPmdCollection();
    
    StPmdDetector*    detector(Int_t);
  
    void setDetector(StPmdDetector*, Int_t);
    
private:
    StPmdDetector*            mDetector[2];   //!pointer for detector

    ClassDef(StPmdCollection,1)
};
#endif








