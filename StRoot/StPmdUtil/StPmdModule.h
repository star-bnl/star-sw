/*!
 * \class StPmdModule
 * \author Subhasis Chattopadhyay
 */
/*****************************************************
 *                                                
 * $Id: StPmdModule.h,v 1.2 2003/05/12 12:07:13 subhasis Exp $                             
 *                                                
 * Author: Subhasis Chattopadhyay                  
 *****************************************************
 *                                                
 * Description: This is the class for each supermodule 
 *  having the hit information.
 *                           
 ******************************************************
 * $Log: StPmdModule.h,v $
 * Revision 1.2  2003/05/12 12:07:13  subhasis
 * Mapping added
 *
 *                                                
 *******************************************************/
#ifndef StPmdModule_hh
#define StPmdModule_hh

#include "StObject.h"
#include "StPmdHit.h"

class StPmdModule : public StObject {

public:

  StPmdModule();           //! A constructor
  ~StPmdModule();          //! A destructor
    unsigned int numberOfHits() const;
    TObjArray* Hits();      
    
private:
    TObjArray mHits;
    ClassDef(StPmdModule,1)
 };
#endif

inline TObjArray* StPmdModule::Hits() {return &mHits;} 











