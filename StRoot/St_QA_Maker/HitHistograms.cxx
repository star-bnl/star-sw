///////////////////////////////////////////////////////////////////////////
// $Id: HitHistograms.cxx,v 1.3 2000/08/25 16:04:09 genevb Exp $
//
// Author: M.L. Miller, Yale
//
///////////////////////////////////////////////////////////////////////////
//
// Description: TPC sector gains histograms class
//
///////////////////////////////////////////////////////////////////////////
//
// $Log: HitHistograms.cxx,v $
// Revision 1.3  2000/08/25 16:04:09  genevb
// Introduction of files
//
// Revision 1.2  2000/08/09 18:57:43  lansdell
// improvements in TPC gains code reduces CPU time per event by factor of 2
//
//
///////////////////////////////////////////////////////////////////////////

#include "StMultiH1F.h"
#include "HitHistograms.h"

//Root
#include "TH1.h"
#include "TF1.h"

// StEvent
#include "StEventTypes.h"

HitHistograms::HitHistograms()
{
  /* noop */
}

HitHistograms::HitHistograms(const char *name,const char *title,
			     Int_t nbinsx,Axis_t xlow,Axis_t xup, Int_t nbinsy)
{
  // if you change these constants, be sure to change them in StQABookHist
  // for the "dE/dx for all TPC sectors" histogram (m_dedx_all_sectors)
  double xmin = 0.;
  //double xmax = 1.e-4;
  double xmax = 1.e-5;
  int xbins = 100;
  char* name1 = "QaInnerSectorDeDx";
  char* name2 = "QaOuterSectorDeDx";
  char* title1 = "Inner Sector De/Dx Distribution";
  char* title2 = "Outer Sector De/Dx Distribution";

  m_innerSectorDeDxHist = new TH1F(name1,title1,xbins,xmin,xmax);
  m_outerSectorDeDxHist = new TH1F(name2,title2,xbins,xmin,xmax);
  m_allSectorsDeDxHist = new StMultiH1F(name,title,nbinsx,xlow,xup,nbinsy);
  if (nbinsy == 2) {
    m_allSectorsDeDxHist->Rebin(0,"Outer");
    m_allSectorsDeDxHist->Rebin(1,"Inner");
  }
}

HitHistograms::~HitHistograms()
{
    delete m_innerSectorDeDxHist;
    delete m_outerSectorDeDxHist;
    delete m_allSectorsDeDxHist;
    //for (int i = 1; i<24; i++) {
    //delete m_InnerSectorDeDxHistMap[i];
    //delete m_OuterSectorDeDxHistMap[i];
    //}
}

void HitHistograms::buildHistMaps()
{
    //I'm assuming that the number of sectors is fixed
    //double xmin = 0.;
    //double xmax = 1.e-4;
    //int xbins = 1000;
    
    for (int i = 1; i<=24; i++) {
	char* name1 = new char[100];
	char* name2 = new char[100];
	sprintf(name1,"InnerSector%iDeDxHist",i);
	sprintf(name2,"OuterSector%iDeDxHist",i);
	char* title1 = new char[100];
	char* title2 = new char[100];
	sprintf(title1,"Inner Sector %i dE/dx Distribution",i);
	sprintf(title2,"Outer Sector %i dE/dx Distribution",i);

	//m_InnerSectorDeDxHistMap[i] = new TH1F(name1, title1, xbins, xmin, xmax);
	//m_OuterSectorDeDxHistMap[i] = new TH1F(name2, title2, xbins, xmin, xmax);

	delete name1;
	delete name2;
	delete title1;
	delete title2;
    }
    
    return;
}
void HitHistograms::fillHistograms()
{
    for (vector<StTpcHit*>::const_iterator it=m_tpcHitVec.begin(); it!=m_tpcHitVec.end(); it++) {
	double ds = dx(*it);
	//Keep only hit.flag()==0 points
	if ( (ds != 0.) && (keepHit(*it)) ) {
	  //int sector = (*it)->sector();
	    if ((*it)->padrow() <= 13){
		//m_InnerSectorDeDxHistMap[sector]->Fill( (*it)->charge()/ds );
	        m_innerSectorDeDxHist->Fill( (*it)->charge()/ds );
		m_allSectorsDeDxHist->Fill((*it)->charge()/ds,1.);
	    }
	    if ((*it)->padrow() > 13) {
		//m_OuterSectorDeDxHistMap[sector]->Fill( (*it)->charge()/ds);
		m_outerSectorDeDxHist->Fill( (*it)->charge()/ds );
		m_allSectorsDeDxHist->Fill((*it)->charge()/ds,0.);
	    }
	}
    }
    return;
}

//TH1F* HitHistograms::innerSectorDeDxHist(int sector) const
//{
//    return m_InnerSectorDeDxHistMap[sector];
//}

//TH1F* HitHistograms::outerSectorDeDxHist(int sector) const
//{
//    return m_OuterSectorDeDxHistMap[sector];
//}

TH1F* HitHistograms::innerSectorDeDxHist() const {
  return m_innerSectorDeDxHist;
}

TH1F* HitHistograms::outerSectorDeDxHist() const {
  return m_outerSectorDeDxHist;
}
