///////////////////////////////////////////////////////////////////////////
// $Id: HitHistograms.h,v 1.3 2000/08/25 16:04:09 genevb Exp $
//
// Author: M.L. Miller, Yale
//
///////////////////////////////////////////////////////////////////////////
//
// Description: TPC sector gains histograms class
//
///////////////////////////////////////////////////////////////////////////
//
// $Log: HitHistograms.h,v $
// Revision 1.3  2000/08/25 16:04:09  genevb
// Introduction of files
//
// Revision 1.2  2000/08/09 18:57:44  lansdell
// improvements in TPC gains code reduces CPU time per event by factor of 2
//
//
///////////////////////////////////////////////////////////////////////////

#ifndef HitHistograms_H
#define HitHistograms_H

#include <map>
#include "TpcHitUtilities.h"

class TH1F;
class TH2F;

class HitHistograms : public TpcHitUtilities {
public:
    HitHistograms();
    HitHistograms(const char *name,const char *title,
		  Int_t nbinsx,Axis_t xlow,Axis_t xup, Int_t nbinxy);
    virtual ~HitHistograms();

    //Access---------------------------------------
    //const map<const int, TH1F*>& innerSectorDeDxHistMap() const; //!
    //const map<const int, TH1F*>& outerSectorDeDxHistMap() const; //!
    //TH1F* innerSectorDeDxHist(int sector) const; // Return a histogram for sect.
    //TH1F* outerSectorDeDxHist(int sector) const; // Return a histogram for sect.

    TH1F* innerSectorDeDxHist() const; // Return a histogram for inner
    TH1F* outerSectorDeDxHist() const; 

    //Methods------------------------------------
    void clearHistograms();  //Zero all histograms
    void fillHistograms();  //Fill Histograms from inheried TpcHitVec
    void fitGainHistograms(); //Fit Gain Histograms with a Landau
    void buildHistMaps();
    
protected:
    TH1F* m_innerSectorDeDxHist;
    TH1F* m_outerSectorDeDxHist;
    TH2F *m_allSectorsDeDxHist;
    //map<const int, TH1F*> m_InnerSectorDeDxHistMap; // ! Map of Inner Sector dE/dx Histograms
    //map<const int, TH1F*> m_OuterSectorDeDxHistMap; //! Map of Outer Sector dE/dx Histograms
    
};

#endif
