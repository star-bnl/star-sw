/***************************************************************************
 *
 * $Id: 
 *
 * Author: Laurent Conin, Fabrice Retiere, Subatech, France
 ***************************************************************************
 *
 * Description : Base class for the Weight calculator
 *
 ***************************************************************************
 *
 * $Log: 
 *
 ***************************************************************************/

#include "StHbtMaker/Base/StHbtFsiWeight.hh"


#ifdef __ROOT__
ClassImp(StHbtFsiWeight)
#endif

StHbtFsiWeight::StHbtFsiWeight()
{ mWeightDen=1.; }

StHbtFsiWeight::~StHbtFsiWeight()
{/* no-op */};   

inline double StHbtFsiWeight::GetWeightDen() {return mWeightDen;}
inline StHbtString  StHbtFsiWeight::Report() {return StHbtString("StHbtFsiWeight Default Report");}
