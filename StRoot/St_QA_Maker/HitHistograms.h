/*!
  \class StEventQAMaker
  \author M.L. Miller, Yale
                                                                     
  Description: TPC sector gains histograms class                                          
 
*/

#ifndef HitHistograms_H
#define HitHistograms_H

#include "TpcHitUtilities.h"

class TH1F;
class TH2F;
class StMaker;

class HitHistograms : public TpcHitUtilities {
public:
    HitHistograms();
    HitHistograms(const char *name,const char *title,
		  Int_t nbinsx,Axis_t xlow,Axis_t xup, Int_t nbinxy,
		  StMaker* mk=0);
    virtual ~HitHistograms();

    //Access---------------------------------------
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
    
};

#endif
///////////////////////////////////////////////////////////////////////////
// $Id: HitHistograms.h,v 1.5 2003/09/19 22:58:11 genevb Exp $
// $Log: HitHistograms.h,v $
// Revision 1.5  2003/09/19 22:58:11  genevb
// Initialize pointers to zero, some doxygenization
//
// Revision 1.4  2002/02/12 18:41:59  genevb
// Additional FTPC histograms
//
// Revision 1.3  2000/08/25 16:04:09  genevb
// Introduction of files
//
// Revision 1.2  2000/08/09 18:57:44  lansdell
// improvements in TPC gains code reduces CPU time per event by factor of 2
//
