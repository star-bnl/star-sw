#include <Stiostream.h>
#include "StSsdDynamicControl.h"
#include "StMessMgr.h"
#include "tables/St_slsCtrl_Table.h"
/*! 
Basic constructor. The members are filled in the code
*/
StSsdDynamicControl::StSsdDynamicControl()
{
  mNElectronInAMip      =   22500; 
  mADCDynamic           =      20;
  mA128Dynamic          =      12;    
  mNBitEncoding         =      10; 
  mNStripInACluster     =       4;
  mPairCreationEnergy   = 3.6e-09;
  mparDiffP             = 0.00123;
  mparDiffN             = 0.00094; 
  mparIndRightP         =   0.021; 
  mparIndRightN         =   0.026; 
  mparIndLeftP	        =   0.013; 
  mparIndLeftN	        =    0.01; 
  mDAQCutValue          =       4;
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
      mNElectronInAMip      = control[0].nElectronInAMip;
      mADCDynamic           = control[0].adcDynamic;
      mA128Dynamic          = control[0].a128Dynamic;
      mNBitEncoding         = control[0].nbitEncoding;
      mNStripInACluster     = control[0].nstripInACluster;
      mPairCreationEnergy   = control[0].pairCreationEnergy;
      mparDiffP             = control[0].parDiffP;
      mparDiffN             = control[0].parDiffN;
      mparIndRightP         = control[0].parIndRightP;
      mparIndRightN         = control[0].parIndRightN;
      mparIndLeftP	    = control[0].parIndLeftP;
      mparIndLeftN          = control[0].parIndLeftN;
      mDAQCutValue          = control[0].daqCutValue;
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
  cout<<"**** NElectronInAMip = "<<this->getNElectronInAMip()<<"  ****"<<endl;
  cout<<"****mADCDynamic = "<<this->getADCDynamic()<<"  ****"<<endl;
  cout<<"****mA128Dynamic = "<<this->getA128Dynamic()<<"  ****"<<endl;
  cout<<"****mNBitEncoding = "<<this->getNBitEncoding()<<"  ****"<<endl;
  cout<<"****mPairCreationEnergy = "<<this->getPairCreationEnergy()<<"  ****"<<endl;
  cout<<"****mDAQCutValue = "<<this->getDAQCutValue()<<"  ****"<<endl;
  cout<<"**************************************"<<endl;
}
