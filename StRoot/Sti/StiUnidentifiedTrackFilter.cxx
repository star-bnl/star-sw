#include "StiTrack.h"
#include "StiUnidentifiedTrackFilter.h"

StiUnidentifiedTrackFilter::StiUnidentifiedTrackFilter()
{
    filters.push_back(new StiFilter(0.,20.,string("Chi2")));
    filters.push_back(new StiFilter(0.,20.,string("Phi")));
    filters.push_back(new StiFilter(0.1,20.,"Pt"));
    filters.push_back(new StiFilter(-0.75,0.75,string("PseudoRapidity")));
    filters.push_back(new StiFilter(-0.75,0.75,string("Rapidity"),false));
    filters.push_back(new StiFilter(15.,60.,string("NPts")));
    filters.push_back(new StiFilter(15.,60.,string("NFitPts")));
    filters.push_back(new StiFilter(0.,2.,string("NGaps"),false));
    filters.push_back(new StiFilter(0.5,2.,string("FitToTotalPts"),false));
    filters.push_back(new StiFilter(0.,20.,string("PrimaryDca"),false));
    filters.push_back(new StiFilter(0.,20.,string("NTpcPts"),false));
    filters.push_back(new StiFilter(0.,20.,string("NSvtPts"),false));
    filters.push_back(new StiFilter(0.,20.,string("TpcDedx"),false));
    filters.push_back(new StiFilter(0.,20.,string("SvtDedx"),false));
    filters.push_back(new StiFilter(0,string("TrackType"),false));
    filters.push_back(new StiFilter(0,string("Charged"),false));
}

void StiUnidentifiedTrackFilter::setDefaults()
{
    /** 
     * Set default values of this filters
     */ 
    /*
      usePhi = false;
      useEta = false;
      usePt  = false;
      useTpcDedx = false;
      useSvtDedx = false;
      useDca  = false;
      usePtsCount = false;
      useTpcPtsCount = false;
      useSvtPtsCount = false;
      useChi2          = false;
      useCharged       = true;
      useFitToTotalPtsRatio = false;

      setEtaRange(-0.5, 0.5);
      setPtRange(0.1,1.5);
      setPointCountRange(15, 50);
      //setTpcPointCountRange(5, 50);
      //setSvtPointCountRange(5, 50);
      setTpcDedxRange(0.25,50.);
      setDcaRange(0.,3.);            
    */
}

bool StiUnidentifiedTrackFilter::accept(StiTrack * t)
{
    StiFilter * f;

    f = filters[kChi2];
    if (f&&f->isUsed()&&!f->accept(t->getChi2())) {
	return false;
    }
    
    f = filters[kPt];
    if (f&&f->isUsed()&&!f->accept(t->getPt())) {
	return false;
    }
    
    f = filters[kPseudoRapidity];
    if (f&&f->isUsed()&&!f->accept(t->getPseudoRapidity())) {
	return false;
    }
    
    f = filters[kRapidity];
    if (f&&f->isUsed()&&!f->accept(t->getRapidity())) {
	return false;
    }
    
    f = filters[kNPts];
    if (f&&f->isUsed()&&!f->accept(t->getPointCount())) {
	return false;
    }
    
    f = filters[kNFitPts];
    if (f&&f->isUsed()&&!f->accept(t->getFitPointCount())) {
	return false;
    }
    
    f = filters[kNGaps];
    if (f&&f->isUsed()&&!f->accept(t->getGapCount())) {
	return false;
    }
    
    f = filters[kFitToTotalPts];  
    double pts,fitPts,ratio;
    if (f && f->isUsed()) {
	pts    = t->getPointCount();
	fitPts = t->getFitPointCount();			
	if (pts>0) {
	    ratio  = fitPts/pts;						
	    if (f->accept(ratio)) {
		return false;
	    }
	}
	else {
	    return false;
	}
    }
    
    f = filters[kPrimaryDca];
    if (f&&f->isUsed()&&!f->accept(t->getDca())) {
	return false;
    }

    //If we made it here we're done!
    return true;
}

/********
	 bool StiUnidentifiedTrackFilter::accept(StiTrack * t)
	 {
	 if (t)
	 {
	 analyzedTrackCount++;
	 float phi = t->getPhi(); 
	 float eta = t->getPseudoRapidity();
	 float pt  = t->getPt();
	 float dca = t->getDca();
	 //float tpcDedx = 0.;//1.e6*t->getTpcDedx();
	 //float svtDedx = 0.;//1.e6*t->getSvtDedx();
	 float chi2 = t->getChi2();
	 int   pts    = t->getPointCount();
	 //int   tpcPts = 0;//t->getTpcPointCount();
	 //int   svtPts = 0;//t->getSvtPointCount();
	 int   fitPts = t->getFitPointCount();
	 float ptsRatio = float(fitPts)/float(pts);
	 if ( 
	 ((!usePhi)           || (phi>=minPhi          && phi<maxPhi) ) &&
	 ((!useEta)           || (eta>=minEta          && eta<maxEta) ) &&
	 ((!usePt)            || (pt>=minPt            && pt<maxPt  ) ) && 
	 ((!useDca)           || (dca>=minDca          && dca<maxDca) ) &&
	 // ((!useTpcDedx)       || (tpcDedx>=minTpcDedx     && tpcDedx<maxTpcDedx      ) ) &&
	 // ((!useSvtDedx)       || (svtDedx>=minSvtDedx     && svtDedx<maxSvtDedx      ) ) &&
	 ((!usePtsCount)      || (pts>=minPtsCount         &&  pts<maxPtsCount) )        &&
	 //((!useTpcPtsCount)   || (tpcPts>=minTpcPtsCount   &&  tpcPts<maxTpcPtsCount)   ) &&
	 //((!useSvtPtsCount)   || (svtPts>=minSvtPtsCount   &&  svtPts<maxSvtPtsCount)   ) &&
	 ((!useFitPtsCount)   || (fitPts>=minFitPtsCount   &&  fitPts<maxFitPtsCount)   ) &&
	 ((!useFitToTotalPtsRatio)|| (ptsRatio>=minFitToTotalPtsRatio &&  ptsRatio<maxFitToTotalPtsRatio)) &&
	 ((!useChi2)                || (chi2>=minChi2              &&  chi2<maxChi2)              ) &&
	 ((!useCharged)             || (t->getCharge()!=0)) 
	 )
	 {
	 acceptedTrackCount++;
	 return true;
	 }
	 else
	 {
	 return false;	
	 }
	 }
	 else
	 return false;
	 }


	 void StiUnidentifiedTrackFilter::setEtaRange(double min, double max)
	 {
	 if (min<max)
	 {
	 useEta = true;
	 minEta = min;
	 maxEta = max;
	 }
	 else
	 useEta = false;
	 }

	 void StiUnidentifiedTrackFilter::setPtRange(double min, double max)
	 {
	 if (min<max)
	 {
	 usePt = true;
	 minPt = min;
	 maxPt = max;
	 }
	 else
	 usePt = false;
	 }

	 void StiUnidentifiedTrackFilter::setPhiRange(double min, double max)
	 {
	 if (min<max)
	 {
	 usePhi = true;
	 minPhi = min;
	 maxPhi = max;
	 }
	 else
	 usePhi = false;
	 }

	 void StiUnidentifiedTrackFilter::setTpcDedxRange(double min, double max)
	 {
	 if (min<max)
	 {
	 useTpcDedx = true;
	 minTpcDedx = min;
	 maxTpcDedx = max;
	 }
	 else
	 useTpcDedx = false;
	 }

	 void StiUnidentifiedTrackFilter::setSvtDedxRange(double min, double max)
	 {
	 if (min<max)
	 {
	 useSvtDedx = true;
	 minSvtDedx = min;
	 maxSvtDedx = max;
	 }
	 else
	 useSvtDedx = false;
	 }

	 void StiUnidentifiedTrackFilter::setDcaRange(double min, double max)
	 {
	 if (min<max)
	 {
	 useDca = true;
	 minDca = min;
	 maxDca = max;
	 }
	 else
	 useDca = false;
	 }


	 void StiUnidentifiedTrackFilter::setPointCountRange(int min, int max)
	 {
	 if (min<max)
	 {
	 usePtsCount = true;
	 minPtsCount = min;
	 maxPtsCount = max;
	 }
	 else
	 usePtsCount = false;
	 }

	 void StiUnidentifiedTrackFilter::setTpcPointCountRange(int min, int max)
	 {
	 if (min<max)
	 {
	 useTpcPtsCount = true;
	 minTpcPtsCount = min;
	 maxTpcPtsCount = max;
	 }
	 else
	 useTpcPtsCount = false;
	 }
	 void StiUnidentifiedTrackFilter::setSvtPointCountRange(int min, int max)
	 {
	 if (min<max)
	 {
	 useSvtPtsCount = true;
	 minSvtPtsCount = min;
	 maxSvtPtsCount = max;
	 }
	 else
	 useSvtPtsCount = false;
	 }

	 void StiUnidentifiedTrackFilter::setFitPointCountRange(int min, int max)
	 {
	 if (min<max)
	 {
	 useFitPtsCount = true;
	 minFitPtsCount = min;
	 maxFitPtsCount = max;
	 }
	 else
	 useFitPtsCount = false;
	 }

	 void StiUnidentifiedTrackFilter::setFitToTotalPointRatioRange(float min, float max)
	 {
	 if (min<max)
	 {
	 useFitToTotalPtsRatio = true;
	 minFitToTotalPtsRatio = min;
	 maxFitToTotalPtsRatio = max;
	 }
	 else
	 useFitToTotalPtsRatio = false;
	 }


	 void StiUnidentifiedTrackFilter::setChi2Range(double min, double max)
	 {
	 if (min<max)
	 {
	 useChi2 = true;
	 minChi2 = min;
	 maxChi2 = max;
	 }
	 else
	 useChi2 = false;
	 }

*/
