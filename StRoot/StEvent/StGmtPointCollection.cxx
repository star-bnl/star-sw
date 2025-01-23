/**
 * \class StGmtColection
 * \brief Holds collections of GMT points
 * 
 * Collection of GMT points for StEvent. Basically a wrapper 
 * for an StSPtrVecGmtPoint (based on StFgtPointCollection)
 *
 * \author K.S. Engle, Jan. 2013
 * \author Richard Witt (witt@usna.edu), Jan. 2013
 * \author Grigory Nigmatkulov (nigmatkulov@gmail.com), Dec. 2020
 */

// STAR headers
#include "StContainers.h"
#include "StGmtPoint.h"
#include "StGmtPointCollection.h"

//________________
StGmtPointCollection::StGmtPointCollection() : StObject() {
  /* empty */
};

//________________
StGmtPointCollection::~StGmtPointCollection() {
  /* empty */
}


