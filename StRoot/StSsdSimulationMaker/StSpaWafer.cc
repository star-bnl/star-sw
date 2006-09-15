// $Id: StSpaWafer.cc,v 1.3 2006/09/15 21:09:52 bouchet Exp $
//
// $Log: StSpaWafer.cc,v $
// Revision 1.3  2006/09/15 21:09:52  bouchet
// read the noise and pedestal from ssdStripCalib
//
// Revision 1.2  2005/05/13 08:39:33  lmartin
// CVS tags added
//

#include "StSpaWafer.hh"

StSpaWafer::StSpaWafer(int nid)
{
  mId      = nid;
  mStripP = new StSpaListStrip();
  mStripN = new StSpaListStrip();
  mNoiseP = new StSpaListNoise();
  mNoiseN = new StSpaListNoise();
}
StSpaWafer::~StSpaWafer()
{
  delete    mStripP;
  delete    mStripN;
  delete    mNoiseP;
  delete    mNoiseN;
}

StSpaListStrip* StSpaWafer::getStripP()
{ return mStripP; }   

StSpaListStrip* StSpaWafer::getStripN()
{ return mStripN; }   

void StSpaWafer::addStrip(StSpaStrip *ptr, int iSide)
{
  if (iSide)
    { (this->mStripN)->addNewStrip(ptr); }
  else
    { (this->mStripP)->addNewStrip(ptr); }
}

void StSpaWafer::addNoise(StSpaNoise *ptr, int iSide)
{
  if (iSide)
    { (this->mNoiseN)->addNewNoise(ptr); }
  else
    { (this->mNoiseP)->addNewNoise(ptr); }
}

void StSpaWafer::setIsActive(int rIsActive, int iSide, int rNStrip)
{
  if (iSide)
    { (this->mNoiseN)->setIsActive(rIsActive, rNStrip); }
  else
    { (this->mNoiseP)->setIsActive(rIsActive, rNStrip); }
}

 
void StSpaWafer::sortNoise()
{
  (this->mNoiseP)->sortStrip();
  (this->mNoiseN)->sortStrip();
}

void StSpaWafer::sortStrip()
{
  (this->mStripP)->sortStrip();
  (this->mStripN)->sortStrip();
}

void StSpaWafer::addNoiseToStripSignal(long nElectronInAMip,long a128Dynamic)
{
  (this->mNoiseP)->addSignal(this->mStripP, 
			     nElectronInAMip, a128Dynamic);
  (this->mNoiseN)->addSignal(this->mStripN, 
			     nElectronInAMip, a128Dynamic);
}

void StSpaWafer::pedestalSubstraction()
{
  (this->mNoiseP)->substractPedestal();
  (this->mNoiseN)->substractPedestal();
}

void StSpaWafer::zeroSubstraction()
{
  (this->mNoiseP)->zeroSubstraction();
  (this->mNoiseN)->zeroSubstraction();
}

void StSpaWafer::convertAnalogToDigit(long nElectronInAMip,long adcDynamic,
				      long nbitEncoding, float daqCutValue)
{
  (this->mNoiseP)->convertAnalogToDigit(nElectronInAMip, adcDynamic,
					nbitEncoding,daqCutValue);
  (this->mNoiseN)->convertAnalogToDigit(nElectronInAMip, adcDynamic,
					nbitEncoding,daqCutValue);
}

void StSpaWafer::updateListStrip()
{
  (this->mStripP)->updateListStrip(this->mNoiseP); 
  (this->mStripN)->updateListStrip(this->mNoiseN); 
}

