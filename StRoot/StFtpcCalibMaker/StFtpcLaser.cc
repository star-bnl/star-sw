// $Id: StFtpcLaser.cc,v 1.6 2007/07/12 19:36:11 fisyak Exp $
//
// $Log: StFtpcLaser.cc,v $
// Revision 1.6  2007/07/12 19:36:11  fisyak
// Add includes for ROOT 5.16
//
// Revision 1.5  2007/01/22 13:08:15  jcs
// replace cout with LOG
//
// Revision 1.4  2007/01/19 08:22:58  jcs
// replace true, false with kTRUE, kFALSE
//
// Revision 1.3  2006/04/04 10:57:04  jcs
// Fix memory leak
//
// Revision 1.2  2006/03/15 15:13:56  jcs
// add lines for listing CVS update info
//

#include "StFtpcLaser.hh"
#include "TMath.h"
#include "StMessMgr.h"

//---------------------------------------------------------------

StFtpcLaser::StFtpcLaser()
{
  TStyle *plain  = new TStyle("Plain","Plain Style (no colors/fill areas)");
  plain->SetTitleOffset(1.25);
  plain->SetCanvasBorderMode(0);
  plain->SetPadBorderMode(0);
  plain->SetPadColor(0);
  plain->SetCanvasColor(0);
  plain->SetTitleColor(0);
  plain->SetStatColor(0);
  plain->SetPalette(1);
  plain->SetOptStat(11);
  plain->SetOptFit(0);
}

//---------------------------------------------------------------

//StFtpcLaser::StFtpcLaser(TString filename)
//{
//
//f=new TFile(filename);
//LOG_DEBUG<<"StFtpcLaser constructed"<<endm;
//readtree(f);
//LOG_DEBUG<<"File : "<<filename<<" eingelesen !"<<endm;  
//}

//---------------------------------------------------------------

void StFtpcLaser::Init(TString filename)
{

  f=new TFile(filename+".root");
  LOG_INFO<<"StFtpcLaser::Init..."<<endm;
  readtree(f);
  LOG_INFO<<" "<<endm;
  LOG_INFO<<"Read file   : "<<filename<<".root"<<endm;
  LOG_INFO<<"--------------"<<endm;
}

//______________________________________________________________________________                                                                                
void StFtpcLaser::GetTreeEntry(int k)
{
   btcluster->GetEntry(k);
   bthit->GetEntry(k);
   btevent->GetEntry(k);
                                                                                
   bRun->GetEntry(k);    //JCS
}

//---------------------------------------------------------------

void StFtpcLaser::GetClusterTreeEntry(int k)
{
   bcluster->GetEntry(k);
   bhit->GetEntry(k);
   bevent->GetEntry(k);
}

//---------------------------------------------------------------

StFtpcLaser::~StFtpcLaser()
{
  f->Close();
  //LOG_DEBUG<<"StFtpcLaser deconstructed"<<endm; 
}
//---------------------------------------------------------------


Float_t StFtpcLaser::zyltrafo(Float_t x,Float_t y, Float_t z)
{
  Float_t erg=0;
  if (x<0) erg=atan(y/x)+TMath::Pi();
  if (x>0) erg=atan(y/x);
  if (x==0 && y>0) erg=TMath::Pi()/2;
  if (x==0 && y<0) erg=-TMath::Pi()/2;
  return erg;
}

//---------------------------------------------------------------

bool StFtpcLaser::laser_sector(int whichftpc,int whichsec,int sec)
{
  if (whichftpc==2)
      {
	switch(whichsec)
	  {

	  case 0:
	    // all sectors
	    return kTRUE;
	    break;
	  case 1:
	    if (sec==32 || sec==38 || sec==44 || sec==50 || sec==56)
	      return kTRUE;
	    else
	      return kFALSE;
	    break;
	  case 2:
	    if (sec==34 || sec==40 || sec==46 || sec==52 || sec==58)
	      return kTRUE;
	    else
	      return kFALSE;
	    break;
	  case 3:
	    if (sec==36 || sec==42 || sec==48 || sec==54 || sec==60)
	      return kTRUE;
	    else
	      return kFALSE;
	    break;
	  default :
	    {
	      LOG_ERROR<<"ERROR : No valid Laser sector for Ftpc East!"<<endm;
	      return kFALSE;
	    }
	  }
      }
  else if (whichftpc==1)
    {
      switch(whichsec)
	{
	case 0:
	  // all sectors
	  return kTRUE;
	  break;
	 case 1:
	    if (sec==2 || sec==8 || sec==14 || sec==20 || sec==26)
	      return kTRUE;
	    else
	      return kFALSE;
	    break;
	  case 2:
	    if (sec==4 || sec==10 || sec==16 || sec==22 || sec==28)
	      return kTRUE;
	    else
	      return kFALSE;
	    break;
	  case 3:
	    if (sec==6 || sec==12 || sec==18 || sec==24 || sec==30)
	      return kTRUE;
	    else
	      return kFALSE;
	    break; 
	default :
	  {
	    LOG_ERROR<<"ERROR : No valid Laser sector for Ftpc West!"<<endm;
	    return kFALSE;
	  }
	}
    }
  else if (whichftpc==0)
    {
      // take all tracks !!!
      return kTRUE;
    }
      LOG_ERROR<<"ERROR : No FTPC chosen !"<<endm;

  return kFALSE;
}

//---------------------------------------------------------------

int StFtpcLaser::laser_straight(float *rad,int max)
{
  //if (fabs(rad[0]-rad[max-1])>5)
  if ((rad[0]-rad[max-1])>5)
    return 0;
  //else if ((rad[max-1]-rad[0])>0) // nur einen schraegen !
  else if (fabs(rad[0]-rad[max-1])>5)
    return 2;
  else
    return 1;
  //else
  //return kFALSE;
} 


void StFtpcLaser::readtree(TFile *f)
{
  drtree=(TTree*) f->Get("rinfo");
  bRun=drtree->GetBranch("Run");
  bRun->SetAddress(&Run);
  dtree=(TTree*) f->Get("cl");
  bhit=dtree->GetBranch("hit");
  bhit->SetAddress(&hit);
  bcluster=dtree->GetBranch("cluster");
  bcluster->SetAddress(&cluster);
  bevent=dtree->GetBranch("event");
  bevent->SetAddress(&event);
 
  dtrtree=(TTree*) f->Get("tr");
  btrevent=dtrtree->GetBranch("event");
  btrevent->SetAddress(&trevent);
  btrack=dtrtree->GetBranch("track");
  btrack->SetAddress(&track);
  btrvertex=dtrtree->GetBranch("vertex");
  btrvertex->SetAddress(&mvertex);

  dttree=(TTree*) f->Get("clot");
  btcluster=dttree->GetBranch("cluster");
  btcluster->SetAddress(&tcluster);
  bthit=dttree->GetBranch("hit");
  bthit->SetAddress(&thit);
  btevent=dttree->GetBranch("event");
  btevent->SetAddress(&tevent);
}

