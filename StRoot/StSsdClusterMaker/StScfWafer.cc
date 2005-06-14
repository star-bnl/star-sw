// $Id: StScfWafer.cc,v 1.4 2005/06/14 12:20:25 bouchet Exp $
//
// $Log: StScfWafer.cc,v $
// Revision 1.4  2005/06/14 12:20:25  bouchet
// cleaner version
//
// Revision 1.3  2005/06/13 16:01:00  reinnart
// Jonathan and Joerg changed the update function
//
// Revision 1.2  2005/05/17 14:16:34  lmartin
// CVS tags added
//
#include "StScfWafer.hh"
StScfWafer::StScfWafer(int nid)
{
  mId      = nid;
  mStripP = new StScfListStrip();
  mStripN = new StScfListStrip();
  mClusterP = new StScfListCluster();
  mClusterN = new StScfListCluster();
}
StScfWafer::~StScfWafer()
{

  delete    mStripP;
  delete    mStripN;
  delete    mClusterP;
  delete    mClusterN;
}



StScfListCluster* StScfWafer::getClusterP()
{ return mClusterP; }   

StScfListCluster* StScfWafer::getClusterN()
{ return mClusterN; }   

StScfListStrip* StScfWafer::getStripP()
{ return mStripP; }   

StScfListStrip* StScfWafer::getStripN()
{ return mStripN; }   

void StScfWafer::addStrip(StScfStrip *ptr, int iSide)
{
  if (iSide)
    { (this->mStripN)->addNewStrip(ptr); }
  else
    { (this->mStripP)->addNewStrip(ptr); }
}

void StScfWafer::setSigmaStrip(int iStrip, int iSide, int iSigma, sls_ctrl_st *sls_ctrl)
{
  if (iSide)
    { (this->mStripN)->setSigma(iStrip, iSigma, sls_ctrl); }
  else
    { (this->mStripP)->setSigma(iStrip, iSigma, sls_ctrl); }
}

 
void StScfWafer::sortCluster()
{
  (this->mClusterP)->sortCluster();
  (this->mClusterN)->sortCluster();
}

void StScfWafer::sortStrip()
{
  (this->mStripP)->sortStrip();
  (this->mStripN)->sortStrip();
}

void StScfWafer::doClusterisation(int *NClusterPerSide,St_sls_ctrl *my_sls_ctrl,St_scf_ctrl *my_scf_ctrl)
{
  int iSide = 0;
  this->doFindCluster(my_sls_ctrl, my_scf_ctrl, iSide);
  NClusterPerSide[0] = this->doClusterSplitting(my_scf_ctrl, iSide); 
  iSide = 1;
  this->doFindCluster(my_sls_ctrl, my_scf_ctrl, iSide); 
  NClusterPerSide[1] = this->doClusterSplitting(my_scf_ctrl, iSide);
}
int StScfWafer::doFindCluster(St_sls_ctrl *my_sls_ctrl,St_scf_ctrl *my_scf_ctrl, int iSide)
{
  StScfListStrip   *CurrentListStrip   =  0;
  StScfListCluster *CurrentListCluster =  0;


  switch (iSide)
    {
    case 0:
     CurrentListStrip   =  this->mStripP;
     CurrentListCluster =  this->mClusterP;
     break;

    case 1:
     CurrentListStrip   =  this->mStripN;
     CurrentListCluster =  this->mClusterN;
     break;
    }
  
  if(!CurrentListStrip->getSize()) return 0;
  
  int nCluster = 0;
  int atTheEnd = 0;
  StScfStrip *CurrentStrip = CurrentListStrip->first();
  StScfStrip *ScanStrip  = 0;
  StScfStrip *LastScanStrip = 0;
  StScfStrip *tmpStrip = CurrentListStrip->first();
  for(int i = 0; i<CurrentListStrip->getSize(); i++)
    { tmpStrip = CurrentListStrip->next(tmpStrip); }
  
  while(CurrentStrip) 
    {
      scf_ctrl_st *ctrl = my_scf_ctrl->GetTable();
      // if(CurrentStrip->getDigitSig()>(scf_ctrl[0].high_cut*CurrentStrip->getSigma()))
// 	{
	if(CurrentStrip->getDigitSig()>(ctrl->high_cut*CurrentStrip->getSigma()))
	{ 
      LastScanStrip = 0;
	  StScfCluster *newCluster = new StScfCluster(CurrentListCluster->getSize());
	  nCluster++;
	  newCluster->update(CurrentStrip,1.,iSide);
	  ScanStrip = CurrentListStrip->prev(CurrentStrip);  
	  while(ScanStrip)
	    {
	      if(((ScanStrip->getNStrip())-((CurrentListStrip->next(ScanStrip))->getNStrip()))==-1)
		{
		newCluster->update(ScanStrip,1.,iSide);
		ScanStrip = CurrentListStrip->prev(ScanStrip);
		}
	      else
	      {	ScanStrip = 0; }
	    }
	  ScanStrip = CurrentListStrip->next(CurrentStrip);  	  
	  while(ScanStrip)
	    {
	      if(((ScanStrip->getNStrip())-((CurrentListStrip->prev(ScanStrip))->getNStrip()))==1)
		{
		  newCluster->update(ScanStrip,1.,iSide);
		  ScanStrip = CurrentListStrip->next(ScanStrip);
		  if (!ScanStrip) atTheEnd = 1;
		}
	      else
		{
		  LastScanStrip = ScanStrip;
		  ScanStrip     = 0;
		}
	    }
	  CurrentListCluster->addNewCluster(newCluster);
	  if(LastScanStrip)
	    { CurrentStrip = LastScanStrip; }
	  else
	    {
	      if (atTheEnd)
		{ CurrentStrip = 0; }
	      else
		{ CurrentStrip = CurrentListStrip->next(CurrentStrip); }
	    }
	}
      else
	{ CurrentStrip = CurrentListStrip->next(CurrentStrip); }
    }
  return nCluster;
}

int StScfWafer::doClusterSplitting(St_scf_ctrl *my_scf_ctrl,int iSide)
{
  StScfListStrip   *CurrentListStrip   =  0;
  StScfListCluster *CurrentListCluster =  0;

  switch (iSide)
    {
    case 0:
      CurrentListStrip   =  this->mStripP;
      CurrentListCluster =  this->mClusterP;
      
      break;
    case 1:
      CurrentListStrip   =  this->mStripN;
      CurrentListCluster =  this->mClusterN;
      break;
    }

  int ClusterListSize = CurrentListCluster->getSize();
  if(!ClusterListSize) return 0;
  
  int iCluster = 0;
  StScfCluster *CurrentCluster = CurrentListCluster->first();
  
  for(iCluster = 0 ; iCluster < ClusterListSize ; iCluster++)
    {   
      
      int *ListAdc = CurrentListStrip->getListAdc(CurrentCluster->getFirstStrip(),CurrentCluster->getClusterSize());
      int toBeDeleted = CurrentListCluster->splitCluster(my_scf_ctrl,CurrentCluster,ListAdc,CurrentListStrip);
      if(toBeDeleted)
	{
	  StScfCluster *TempCluster = CurrentCluster;
	  CurrentCluster = CurrentListCluster->next(CurrentCluster);
	  CurrentListCluster->removeCluster(TempCluster);
	}
      else
	{
	  CurrentCluster = CurrentListCluster->next(CurrentCluster);
	} 

      delete [] ListAdc;
    }
  CurrentListCluster->renumCluster();
  return CurrentListCluster->getSize();
}
