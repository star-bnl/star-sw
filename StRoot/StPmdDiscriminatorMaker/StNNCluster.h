/*!
 * \class StNNCluster
 * \author
 */
/**************************************************************
 *
 * $Id: StNNCluster.h,v 1.2 2003/09/02 17:58:48 perev Exp $
 *
 * Author:
 **************************************************************
 *
 * Description: class for PMD NN cluster
 *
 **************************************************************
*
* $Log: StNNCluster.h,v $
* Revision 1.2  2003/09/02 17:58:48  perev
* gcc 3.2 updates + WarnOff
*
* Revision 1.1  2003/05/29 13:20:07  subhasis
*  NN Input cluster
*
*
***************************************************************/
#ifndef STAR_StNNCluster
#define STAR_StNNCluster

#include <math.h>
#include <Stiostream.h>
#include "TArrayI.h"
#include "StObject.h"
#include "StPmdUtil/StPmdGeom.h"
#include "StPmdUtil/StPmdHit.h"
//#include "StEventTypes.h"

class StPhmdCluster;

class StNNCluster : public StObject {

private:
  //! cluster objects
 StPhmdCluster * mpmdcluster; 
 StPhmdCluster * mcpvcluster; 
public: 
  StNNCluster();      //! constructor     
  StNNCluster(StPhmdCluster*,StPhmdCluster*);   //! constructor
  ~StNNCluster();          //! destructor

  /*! member functions*/

  StPhmdCluster*    PmdCluster();
  StPhmdCluster*    CpvCluster();

  void            setPmdCluster(StPhmdCluster*);
  void            setCpvCluster(StPhmdCluster*);

  ClassDef(StNNCluster,1)// Base class for PMD cluster
};


inline              StNNCluster::~StNNCluster(){ /* Nobody */ }

inline void StNNCluster::setPmdCluster(StPhmdCluster* var){mpmdcluster=var;}
inline void StNNCluster::setCpvCluster(StPhmdCluster* var){mcpvcluster=var;}
inline StPhmdCluster* StNNCluster::PmdCluster(){return mpmdcluster;}
inline StPhmdCluster* StNNCluster::CpvCluster(){return mcpvcluster;}
#endif







