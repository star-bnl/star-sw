// $Id: StSsdDynamicControl.cxx,v 1.1 2006/10/16 16:43:29 bouchet Exp $
//
// $Log: StSsdDynamicControl.cxx,v $
// Revision 1.1  2006/10/16 16:43:29  bouchet
// StSsdUtil regroups now methods for the classes StSsdStrip, StSsdCluster and StSsdPoint
//
// Revision 1.5  2006/09/15 21:03:14  bouchet
// id_mctrack is using for setIdTruth and propagated to the hit
//
// Revision 1.4  2005/03/18 14:22:40  lmartin
// missing CVS header added
//

#include <Stiostream.h>
#include "StSsdDynamicControl.h"
#include "StMessMgr.h"
#include "tables/St_slsCtrl_Table.h"
#include "StDbLib/StDbManager.hh"                      // Database Libraries
#include "StDbLib/StDbConfigNode.hh"                   //
#include "StDbLib/StDbTable.h"       
/*! 
Basic constructor. The members are filled in the code
*/

StSsdDynamicControl::StSsdDynamicControl()
{
//   mnElectronInAMip      =   22500; 
//   madcDynamic           =      20;
//   ma128Dynamic          =      12;    
//   mnbitEncoding         =      10; 
//   mnstripInACluster     =       4;
//   mpairCreationEnergy   = 3.6e-09;
//   mparDiffP             = 0.00123;
//   mparDiffN             = 0.00094; 
//   mparIndRightP         =   0.021; 
//   mparIndRightN         =   0.026; 
//   mparIndLeftP	        =   0.013; 
//   mparIndLeftN	        =    0.01; 
//   mdaqCutValue          =       4;
}
/*!
Constructor loading the parameters from the Db table
 */
StSsdDynamicControl::StSsdDynamicControl(St_slsCtrl * slsCtrl)
{
  slsCtrl_st *control = slsCtrl->GetTable();
  if (!control) gMessMgr->Error() << "No slsCtrl_st table available" << endm;
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
StSsdDynamicControl::~StSsdDynamicControl()
{
}

/*!
Printing the major parameters
 */
void  StSsdDynamicControl::printParameters(){
  cout<<"**** **** SSD Dynamic Control Parameters **** ****"<<endl;
  cout<<"**** nElectronInAMip     = "<<this->getnElectronInAMip()<<" ****"<<endl;
  cout<<"**** adcDynamic          = "<<this->getadcDynamic()<<"      ****"<<endl;
  cout<<"**** a128Dynamic         = "<<this->geta128Dynamic()<<"     ****"<<endl;
  cout<<"**** nbitEncoding        = "<<this->getnbitEncoding()<<"    ****"<<endl;
  cout<<"**** pairCreationEnergy  = "<<this->getpairCreationEnergy()<<"  ****"<<endl;
  cout<<"**** daqCutValue         = "<<this->getdaqCutValue()<<"  ****"<<endl;
  cout<<"**************************************"<<endl;
}


