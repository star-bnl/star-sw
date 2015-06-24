//$Id: StSstDynamicControl.cxx,v 1.2 2015/06/24 17:37:21 smirnovd Exp $
//
//$Log: StSstDynamicControl.cxx,v $
//Revision 1.2  2015/06/24 17:37:21  smirnovd
//StSstUtil: Prepend included headers with path to submodule
//
//Revision 1.1  2015/06/23 16:26:20  jeromel
//First version created from the SSD code and reshaped
//
//Revision 1.1  2015/04/19 17:30:32  bouchet
//initial commit ; SST codes
//

//fork from the SSD code, move along - see history therein

#include "St_base/Stiostream.h"
#include "StSstDynamicControl.h"
#include "StMessMgr.h"
#include "tables/St_sstSlsCtrl_Table.h"
#include "StDbLib/StDbManager.hh"                      // Database Libraries
#include "StDbLib/StDbConfigNode.hh"                   //
#include "StDbLib/StDbTable.h"       
/*!
Basic Constructor
 */
StSstDynamicControl::StSstDynamicControl()
{
}
/*!
Constructor loading the parameters from the Db table
 */
StSstDynamicControl::StSstDynamicControl(St_sstSlsCtrl *sstCtrl)
{
  sstSlsCtrl_st *control = sstCtrl->GetTable();
  if (!control) {LOG_WARN << "No sstSlsCtrl_st table available" << endm;}
  else
    {
      mnElectronInAMip      = control[0].nElectronInAMip;
      madcDynamic           = control[0].adcDynamic;
      ma128Dynamic          = control[0].a128Dynamic;
      mnbitEncoding         = control[0].nbitEncoding;
      mnstripInACluster     = control[0].nstripInACluster;
      mpairCreationEnergy   = control[0].pairCreationEnergy;
      mparDiffP             = control[0].parDiffP;
      mparDiffN             = control[0].parDiffN;
      mparIndRightP         = control[0].parIndRightP;
      mparIndRightN         = control[0].parIndRightN;
      mparIndLeftP	    = control[0].parIndLeftP;
      mparIndLeftN          = control[0].parIndLeftN;
      mdaqCutValue          = control[0].daqCutValue;
    }
}
/*!
The destructor deletes nothing
 */
StSstDynamicControl::~StSstDynamicControl()
{
}

/*!
Printing the major parameters
 */

void  StSstDynamicControl::printParameters(){
  LOG_INFO<<"**** **** SST Dynamic Control Parameters **** ****"<<endm;
  LOG_INFO<<"**** nElectronInAMip     = "<<this->getnElectronInAMip()<<" ****"<<endm;
  LOG_INFO<<"**** adcDynamic          = "<<this->getadcDynamic()<<"      ****"<<endm;
  LOG_INFO<<"**** a128Dynamic         = "<<this->geta128Dynamic()<<"     ****"<<endm;
  LOG_INFO<<"**** nbitEncoding        = "<<this->getnbitEncoding()<<"    ****"<<endm;
  LOG_INFO<<"**** pairCreationEnergy  = "<<this->getpairCreationEnergy()<<"  ****"<<endm;
  LOG_INFO<<"**** daqCutValue         = "<<this->getdaqCutValue()<<"  ****"<<endm;
  LOG_INFO<<"**************************************"<<endm;
}


