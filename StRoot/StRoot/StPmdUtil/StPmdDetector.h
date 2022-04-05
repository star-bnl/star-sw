/*!
 * \class StPmdDetector
 * \author Subhasis Chattopadhyay
 */
/********************************************
 *
 * $Id: StPmdDetector.h,v 1.2 2003/05/12 12:07:13 subhasis Exp $
 *
 * Author:  Subhasis Chattopadhyay
 ********************************************
 *
 * Description: Base class for PMD detector
 *
 ********************************************
 * $Log: StPmdDetector.h,v $
 * Revision 1.2  2003/05/12 12:07:13  subhasis
 * Mapping added
 *
 ********************************************/
#ifndef StPmdDetector_hh
#define StPmdDetector_hh

#include "StObject.h"
class StPmdHit;
class StPmdModule;
class StPmdClusterCollection;

class StPmdDetector : public StObject {
public:
  StPmdDetector(); //! constructor
  StPmdDetector(Int_t, unsigned int);  //!constructor
  ~StPmdDetector();  //!destructor
    
  unsigned int  numberOfModules() const; //! no of modules
    
  bool          addHit(StPmdHit*);   //! for adding hits to detector
  unsigned int  numberOfHits() const;  //! number of hits
    
  StPmdModule*       module(unsigned int);  //! module number
  Int_t       module_hit(Int_t);  //! number of hits in the module

  void setModule(StPmdModule*,int);   //

  StPmdClusterCollection*         cluster();  //! number of clusters
  void setCluster(StPmdClusterCollection*);   //! function for clusters
        
private:
    Int_t            mDetectorId;
    UInt_t                  mNumberOfModules;
    
    Int_t             mModules_NHit[12];
    StPmdModule             *mModules[12];  //! pointer for hits
    StPmdClusterCollection  *mClusters;     //! pointer for clusters
    
    ClassDef(StPmdDetector,1)
};
#endif
