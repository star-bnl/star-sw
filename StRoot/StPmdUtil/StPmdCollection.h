/*!
 * \class StPmdCollection
 * \author  Subhasis Chattopadhyay
 */
/************************************************
 *
 * $Id: StPmdCollection.h,v 1.3 2010/08/27 16:54:20 perev Exp $
 *
 *Author: Subhasis Chattopadhyay
 ************************************************
 *
 * Description: Base class for PMD Collection which
 *              includes both PMD and CPV
 *
 * $Log: StPmdCollection.h,v $
 * Revision 1.3  2010/08/27 16:54:20  perev
 * WarnOff
 *
 * Revision 1.2  2003/05/12 12:07:13  subhasis
 * Mapping added
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
  StPmdCollection(const Char_t *); //! constructor keeps the information for bothCPV/PMD
  ~StPmdCollection(); //!destructor
    
  StPmdDetector*    detector(Int_t); //! detector id
  
  void setDetector(StPmdDetector*, Int_t);  //!fills the detector id
    
private:
    StPmdDetector*            mDetector[2];   //!pointer for detector

    ClassDef(StPmdCollection,1)
};
#endif








