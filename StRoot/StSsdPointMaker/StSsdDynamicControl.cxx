#include <Stiostream.h>
#include "StSsdDynamicControl.h"

StSsdDynamicControl::StSsdDynamicControl()
{
  // Former sls_ctrl table
  mNElectronInAMip      =   22500; 
  mADCDynamic           =      20;
  mA128Dynamic          =      12;    
  mNBitEncoding         =      10; 
  mPairCreationEnergy   = 3.6e-09;
  mDAQCutValue          =       4;
}
//_____________________________________________________________________________
StSsdDynamicControl::~StSsdDynamicControl()
{
}
//_____________________________________________________________________________
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
