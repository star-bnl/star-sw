/*!
  \class StEventQAMaker
  \author M.L. Miller, Yale
                                                                     
  Description: TPC sector gains histograms class                                          
 
*/

#include "StMultiH1F.h"
#include "HitHistograms.h"
#include "StMaker.h"

// StEvent
#include "StEventTypes.h"

//_____________________________________________________________________________
HitHistograms::HitHistograms() :
  m_innerSectorDeDxHist(0),
  m_outerSectorDeDxHist(0),
  m_allSectorsDeDxHist(0)
{
  /* noop */
}
//_____________________________________________________________________________
HitHistograms::HitHistograms(const char *name,const char *title,
			     Int_t nbinsx,Axis_t xlow,Axis_t xup, Int_t nbinsy,
			     StMaker* mk)
{
  // if you change these constants, be sure to change them in StQABookHist
  // for the "dE/dx for all TPC sectors" histogram (m_dedx_all_sectors)
  double xmin = 0.;
  //double xmax = 1.e-4;
  double xmax = 1.e-5;
  int xbins = 100;
  const char* name1 = "StEQaInnerSectorDeDx";
  const char* name2 = "StEQaOuterSectorDeDx";
  const char* title1 = "Inner Sector De/Dx Distribution";
  const char* title2 = "Outer Sector De/Dx Distribution";

  m_innerSectorDeDxHist = new TH1F(name1,title1,xbins,xmin,xmax);
  m_outerSectorDeDxHist = new TH1F(name2,title2,xbins,xmin,xmax);
  m_allSectorsDeDxHist = new StMultiH1F(name,title,nbinsx,xlow,xup,nbinsy);
  m_allSectorsDeDxHist->SetStats(kFALSE);
  if (nbinsy == 2) {
    m_allSectorsDeDxHist->Rebin(0,"Outer");
    m_allSectorsDeDxHist->Rebin(1,"Inner");
  }
  if (mk) {
    mk->AddHist(m_innerSectorDeDxHist);
    mk->AddHist(m_outerSectorDeDxHist);
    mk->AddHist(m_allSectorsDeDxHist);
  }
}
//_____________________________________________________________________________
HitHistograms::~HitHistograms()
{
    if (m_innerSectorDeDxHist) delete m_innerSectorDeDxHist;
    if (m_outerSectorDeDxHist) delete m_outerSectorDeDxHist;
    if (m_allSectorsDeDxHist) delete m_allSectorsDeDxHist;
}
//_____________________________________________________________________________
void HitHistograms::buildHistMaps()
{
    /* depracated */
    return;
}
//_____________________________________________________________________________
void HitHistograms::fillHistograms()
{
    for (vector<StTpcHit*>::const_iterator it=m_tpcHitVec.begin(); it!=m_tpcHitVec.end(); it++) {
	double ds = dx(*it);
	//Keep only hit.flag()==0 points
	if ( (ds != 0.) && (keepHit(*it)) ) {
	    if ((*it)->padrow() <= 13){
	        m_innerSectorDeDxHist->Fill( (*it)->charge()/ds );
		m_allSectorsDeDxHist->Fill((*it)->charge()/ds,1.);
	    }
	    if ((*it)->padrow() > 13) {
		m_outerSectorDeDxHist->Fill( (*it)->charge()/ds );
		m_allSectorsDeDxHist->Fill((*it)->charge()/ds,0.);
	    }
	}
    }
    return;
}
//_____________________________________________________________________________
TH1F* HitHistograms::innerSectorDeDxHist() const {
  return m_innerSectorDeDxHist;
}
//_____________________________________________________________________________
TH1F* HitHistograms::outerSectorDeDxHist() const {
  return m_outerSectorDeDxHist;
}

///////////////////////////////////////////////////////////////////////////
// $Id: HitHistograms.cxx,v 1.11 2009/11/19 20:12:10 genevb Exp $
// $Log: HitHistograms.cxx,v $
// Revision 1.11  2009/11/19 20:12:10  genevb
// Clean up compiler warnings
//
// Revision 1.10  2007/03/13 18:43:43  genevb
// Use StE prefix
//
// Revision 1.9  2006/05/22 18:27:34  genevb
// Remove patch to observe fast offline issues
//
// Revision 1.8  2006/05/18 03:27:41  genevb
// Patch to observe fast offline issues
//
// Revision 1.7  2003/09/19 22:58:10  genevb
// Initialize pointers to zero, some doxygenization
//
// Revision 1.6  2002/02/12 22:17:11  genevb
// Forgot to remove some print statements
//
// Revision 1.5  2002/02/12 18:41:59  genevb
// Additional FTPC histograms
//
// Revision 1.4  2001/05/16 20:57:02  lansdell
// new histograms added for qa_shift printlist; some histogram ranges changed; StMcEvent now used in StEventQA
//
// Revision 1.3  2000/08/25 16:04:09  genevb
// Introduction of files
//
// Revision 1.2  2000/08/09 18:57:43  lansdell
// improvements in TPC gains code reduces CPU time per event by factor of 2
//

