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

void StSpaWafer::addNoiseToStripSignal(long NElectronInAMip,long A128Dynamic)
{
  (this->mNoiseP)->addSignal(this->mStripP, 
			     NElectronInAMip, A128Dynamic);
  (this->mNoiseN)->addSignal(this->mStripN, 
			     NElectronInAMip, A128Dynamic);
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

void StSpaWafer::convertAnalogToDigit(long NElectronInAMip,long ADCDynamic,
				      long NBitEncoding, float DAQCutValue)
{
  (this->mNoiseP)->convertAnalogToDigit(NElectronInAMip, ADCDynamic,
					NBitEncoding,DAQCutValue);
  (this->mNoiseN)->convertAnalogToDigit(NElectronInAMip, ADCDynamic,
					NBitEncoding,DAQCutValue);
}

void StSpaWafer::updateListStrip()
{
  (this->mStripP)->updateListStrip(this->mNoiseP); 
  (this->mStripN)->updateListStrip(this->mNoiseN); 
}
