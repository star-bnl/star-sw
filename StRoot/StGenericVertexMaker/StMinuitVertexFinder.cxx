/***************************************************************************
 *
 * $Id: StMinuitVertexFinder.cxx,v 1.7 2004/03/23 16:15:04 lbarnby Exp $
 *
 * Author: Thomas Ullrich, Feb 2002
 ***************************************************************************
 *
 * Description: 
 *
 ***************************************************************************
 *
 * $Log: StMinuitVertexFinder.cxx,v $
 * Revision 1.7  2004/03/23 16:15:04  lbarnby
 * Extra protection for non-finite track length. User function to not use ITTF tracks
 *
 * Revision 1.6  2003/10/09 16:40:12  perev
 * delete helix object added
 *
 * Revision 1.5  2003/10/06 04:37:58  perev
 * delete helix
 *
 * Revision 1.4  2003/09/02 17:58:19  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.3  2003/05/12 21:10:06  lbarnby
 * Made destructor virtual
 *
 * Revision 1.2  2003/05/09 22:20:00  lbarnby
 * Now also calculates and reports error on vertex. Corrected filter to use ITTF tracks. Some temporary protections against inf/Nan. Skip delete of TMinuit class since causing seg. fault.
 *
 * Revision 1.1  2002/12/05 23:42:46  hardtke
 * Initial Version for development and integration
 *
 **************************************************************************/
#include "StMinuitVertexFinder.h"
#include "StEventTypes.h"
#include "StEnumerations.h"
#include "TMinuit.h"
#include "StGlobals.hh"
#include "SystemOfUnits.h"
#include "StCtbMatcher.h"
#include "StMessMgr.h"
#include <cmath>

vector<StPhysicalHelixD> StMinuitVertexFinder::mHelices;
vector<double>           StMinuitVertexFinder::mSigma;
vector<bool>             StMinuitVertexFinder::mCTB;
double                   StMinuitVertexFinder::mWidthScale = 1;
double                   StMinuitVertexFinder::mX0;
double                   StMinuitVertexFinder::mY0;
double                   StMinuitVertexFinder::mdxdz;
double                   StMinuitVertexFinder::mdydz;
bool                     StMinuitVertexFinder::requireCTB;
int                      StMinuitVertexFinder::nCTBHits;

StMinuitVertexFinder::StMinuitVertexFinder() {
    mBeamHelix=0;
    mStatus = 0;
    mMinNumberOfFitPointsOnTrack = 10; 
    mMinuit = new TMinuit(3);         
    mMinuit->SetFCN(&StMinuitVertexFinder::fcn);
    mMinuit->SetPrintLevel(1);
    mMinuit->SetMaxIterations(1000);
    mExternalSeedPresent = false;
    mVertexConstrain = false;
    requireCTB = false;
    use_ITTF = false;
}


 StMinuitVertexFinder::~StMinuitVertexFinder()
 {
     delete mBeamHelix;mBeamHelix=0;
     gMessMgr->Info() << "Skipping delete Minuit in StMinuitVertexFinder::~StMinuitVertexFinder()" << endm;
   //delete mMinuit;
 }

bool
StMinuitVertexFinder::fit(StEvent* event)
{
    double arglist[4];

    //
    //  Reset vertex
    //
    mFitError = mFitResult = StThreeVectorD(0,0,0);

    // get CTB info
    StCtbTriggerDetector* ctbDet = 0;
    vector<ctbHit>* ctbHits = new vector<ctbHit>();

	StTriggerDetectorCollection* trigCol = event->triggerDetectorCollection();
	if(trigCol){
	    ctbDet = &(trigCol->ctb());

	    float ctbSum = 0;
	    	    
	    for (UInt_t slat = 0; slat < ctbDet->numberOfSlats(); slat++) {
		for (UInt_t tray = 0; tray < ctbDet->numberOfTrays(); tray++) {
		    ctbHit curHit;
		    curHit.adc = ctbDet->mips(tray,slat,0);
		    if(curHit.adc > 0){
			ctbSum += curHit.adc;
			ctb_get_slat_from_data(slat, tray, curHit.phi, curHit.eta);
			ctbHits->push_back(curHit);
			//			ctbHits_eta->push_back(ctb_eta);
			//cout << "CTB Hit at: eta = " << ctb_eta << " phi= " << ctb_phi << endl;
		    }
		}
	    }
	}



    //
    //  Loop all global tracks (TPC) and store the
    //  refering helices and their estimated DCA
    //  resolution in  vectors.
    //  Quality cuts are applied (see accept()).
    //  The helices and the sigma are used in
    //  fcn to calculate the fit potential which
    //  gets minimized by Minuit.
    //
    mHelices.clear();
    mSigma.clear();
    mCTB.clear();
    double sigma;
    bool ctb_match;

    StSPtrVecTrackNode& nodes = event->trackNodes();
    for (unsigned int k=0; k<nodes.size(); k++) {
	StTrack* g = nodes[k]->track(global);
	if (accept(g)&&
	    ((use_ITTF&&g->fittingMethod()==kITKalmanFitId)||
	     (!use_ITTF&&g->fittingMethod()!=kITKalmanFitId))) 
	  {
	    ///LSB This should not be necessary and could be removed in future
	    if (isinf(g->geometry()->helix().curvature()) ){
	      gMessMgr->Warning() << "INFINITE curvature in track !!" << endm;
	      continue;
	    }
	    mHelices.push_back(g->geometry()->helix());
	    // sigma = 0.45+0.0093*::sqrt(g->length())/abs(g->geometry()->momentum()); HIJING + TRS
	    sigma = 0.6+0.0086*::sqrt(g->length())/abs(g->geometry()->momentum());
	    mSigma.push_back(sigma);         
            bool shouldHitCTB = false;
            double etaInCTBFrame = -999;
            ctb_match =  EtaAndPhiToOrriginAtCTB(g,ctbHits,shouldHitCTB,etaInCTBFrame);
	    mCTB.push_back(ctb_match);				
	}
    }
    //
    //  In case there are no tracks left we better quit
    //
    if (mHelices.empty()) {
	cout << "StMinuitVertexFinder::fit: no tracks to fit." << endl;
	mStatus = -1;
	return false;
    }
    cout << "StMinuitVertexFinder::fit size of helix vector: " << mHelices.size() << endl;


    //
    //  Reset and clear Minuit parameters
    //
    mMinuit->mnexcm("CLEar", 0, 0, mStatus);
    
    //
    //  Set parameters and start values. We do
    //  constrain the parameters since it harms
    //  the fit quality (see Minuit documentation).
    //
    static double seed[3] = {0, 0, 0};
    static double step[3] = {0.03, 0.03, 0.03};
    if(mExternalSeedPresent) {
	seed[0] = mExternalSeed.x();
	seed[1] = mExternalSeed.y();
	seed[2] = mExternalSeed.z();
    }
    if (!mVertexConstrain){ 
     mMinuit->mnparm(0, "x", seed[0], step[0], 0, 0, mStatus);
     mMinuit->mnparm(1, "y", seed[1], step[1], 0, 0, mStatus);
     mMinuit->mnparm(2, "z", seed[2], step[2], 0, 0, mStatus);
    }
    else {
     mMinuit->mnparm(0, "z", seed[0], step[0], 0, 0, mStatus);
    }
    //
    //  In case of one usable track only we set x = y = 0;
    //  Note that in calls to mnexcm() we need to follow
    //  F77 syntax, i.e. the first variable is 1 not 0 as
    //  in mnparm() above. This is pretty confusing.
    //
    if (mHelices.size() == 1) {
	arglist[0] = 1; 
	arglist[1] = 2;
        if (!mVertexConstrain) 	mMinuit->mnexcm("FIX", arglist, 2, mStatus);
    }

    //
    //  Scan z to find best seed for the actual fit.
    //  Skip this step if an external seed is given.
    //
    if (!mExternalSeedPresent) {
      if (!mVertexConstrain){ 
	arglist[0] = 3;
      }
      else {
        arglist[0]=1;
      }
	arglist[1] = 100;
	arglist[2] = -200;
	arglist[3] = 200;
	mWidthScale = 2;
        if (mRequireCTB) requireCTB = true;
	cout << "StMinuitVertexFinder::fit : Starting initial SCAN (no external z seed)" << endl;
	mMinuit->mnexcm("SCAn", arglist, 4, mStatus);
	cout << "StMinuitVertexFinder::fit : Done with initial SCAN (no external z seed)" << endl;
    }

    //
    //  Final scan with smaller steps, performed in any case
    //  Note: GetParameter() uses C++ syntax, i.e. parameter
    //  2 means the third variable z.
    //
    double z, foo;
    if (!mVertexConstrain) {
     mMinuit->GetParameter(2, z, foo); 
    }
    else{ 
     mMinuit->GetParameter(0, z, foo); 
    }
    cout << "Vertex seed = " << z << endl;

    ///Give up when bad seed
    ///LSB again, this a temporary protection which could be removed in future 
    ///since it rejects vertices very close to zero in z
    if(fabs(z)<1E-10){
      cout << "Vertex seed not found ?? (very close to zero)" << endl;
      mStatus=-1;
      return false;
    }

      if (!mVertexConstrain){ 
	arglist[0] = 3;
      }
      else {
        arglist[0]=1;
      }
    arglist[1] = 10;
    arglist[2] = z-5;
    arglist[3] = z+5;
    mWidthScale = 1;
    if (mRequireCTB) requireCTB = true;
    cout << "StMinuitVertexFinder::fit : Starting second SCAN" << endl;
    mMinuit->mnexcm("SCAn", arglist, 4, mStatus);
    cout << "StMinuitVertexFinder::fit : Done with second SCAN" << endl;

    //
    //  Reset the flag which tells us about external
    //  seeds. This needs to be provided for every fit.
    //
    mExternalSeedPresent = false;
    
    //
    //  Perform the actual fit (MINI = MIGRAD + SIMPLEX)
    //
    mWidthScale = 1;
    requireCTB = false;
    cout << "StMinuitVertexFinder::fit : Starting minimization" << endl;
    mMinuit->mnexcm("MINImize", 0, 0, mStatus);
    cout << "StMinuitVertexFinder::fit : Done minimization" << endl;
    if (mStatus) {
	cout << "StMinuitVertexFinder::fit: error in Minuit::mnexcm(), check status flag." << endl;
	return false;
    }

    //
    //  Save and check fit result
    //
    double val, verr;
    mMinuit->mnstat(mFmin, mFedm, mErrdef, mNpari, mNparx, mStatus);
    if (!mVertexConstrain) {
      mMinuit->GetParameter(0, val, verr); 
      mFitResult.setX(val); mFitError.setX(verr);
      mMinuit->GetParameter(1, val, verr);
      mFitResult.setY(val); mFitError.setY(verr);
      mMinuit->GetParameter(2, val, verr);
      mFitResult.setZ(val); mFitError.setZ(verr);
    }
    else {
     mMinuit->GetParameter(0, val, verr); 
     mFitResult.setZ(val); mFitError.setZ(verr);
     // LSB Really error in x and y should come from error on constraint
     // At least this way it is clear that those were fixed paramters
     mFitResult.setX(beamX(val)); mFitError.setX(0);
     mFitResult.setY(beamY(val)); mFitError.setY(0);
    }
    delete ctbHits;
    ctbHits = 0;
    return true;
} 

void StMinuitVertexFinder::fcn1D(int& npar, double* gin, double& f, double* par, int iflag)
{
    nCTBHits = 0;
    f = 0;
    double e, s;
    //    StThreeVectorD vtx(par);
    double z = par[0];
    double x = beamX(z);
    double y = beamY(z);
    StThreeVectorD vtx(x,y,z);
    for (unsigned int i=0; i<mHelices.size(); i++) {
      if (!requireCTB||mCTB[i]) {
	s = mWidthScale*mSigma[i];
	e = mHelices[i].distance(vtx);
	f -= exp(-(e*e)/(2*s*s));  // robust potential
        if(mCTB[i]&&e<3.0) nCTBHits++;
//          if (abs(s)>0.01) {
//    	f -= exp(-(e*e)/(2*s*s));  // robust potential
//          }
//          else {
//  	  f += (e*e)/(2*s*s);
//          }
      }
    }
}

void StMinuitVertexFinder::fcn(int& npar, double* gin, double& f, double* par, int iflag)
{
  f = 0;
  double e, s;
  StThreeVectorD vtx(par);
  for (unsigned int i=0; i<mHelices.size(); i++) {
    if (!requireCTB||mCTB[i]) {
      s = mWidthScale*mSigma[i];
      e = mHelices[i].distance(vtx);
      f -= exp(-(e*e)/(2*s*s));  // robust potential
      //          if (abs(s)>0.01) {
      //    	f -= exp(-(e*e)/(2*s*s));  // robust potential
      //          }
      //          else {
      //  	  f += (e*e)/(2*s*s);
      //          }
    }
  }
}

StThreeVectorD
StMinuitVertexFinder::result() const {return mFitResult;}

StThreeVectorD
StMinuitVertexFinder::error() const {return mFitError;}

int
StMinuitVertexFinder::status() const {return mStatus;}

bool
StMinuitVertexFinder::accept(StTrack* track) const
{
    //
    //   Accept only tracks which fulfill certain
    //   quality criteria.
    //
    return (track &&
	    track->flag() >= 0 &&
	    track->fitTraits().numberOfFitPoints() >= mMinNumberOfFitPointsOnTrack &&
	    !track->topologyMap().trackFtpc() &&
            isfinite(track->length()) ); //LSB another temporary check
}

void
StMinuitVertexFinder::setExternalSeed(const StThreeVectorD& s)
{
    mExternalSeedPresent = true;
    mExternalSeed = s;
}

void
StMinuitVertexFinder::setPrintLevel(int level) {mMinuit->SetPrintLevel(level);}

void
StMinuitVertexFinder::printInfo(ostream& os) const
{
    os << "StMinuitVertexFinder - Fit Statistics:" << endl;
    os << "fitted vertex ........................ " << mFitResult << endl;
    os << "position errors ...................... " << mFitError << endl;
    os << "# of used tracks ..................... " << mHelices.size() << endl;
    os << "minimum found (FMIN) ................. " << mFmin << endl;
    os << "estimated distance to minimum (FEDM) . " << mFedm << endl;
    os << "parameter uncertainties (ERRDEF) ..... " << mErrdef << endl;
    os << "# of parameters (NPARI) .............. " << mNpari << endl;
    os << "# of max parameters (NPARX) .......... " << mNparx << endl;
    os << "goodness of covariance matrix (ISTAT)  " << mStatus << endl;
    os << "min # of fit points for tracks ....... " << mMinNumberOfFitPointsOnTrack << endl;
    os << "final potential width scale .......... " << mWidthScale << endl;
}

void StMinuitVertexFinder::UseVertexConstraint(double x0, double y0, double dxdz, double dydz, double weight) {
  mVertexConstrain = true;
  mX0 = x0;
  mY0 = y0;
  mdxdz = dxdz;
  mdydz = dydz;
  mWeight = weight;
  cout << "StMinuitVertexFinder::Using Constrained Vertex" << endl;
  cout << "x origin = " << mX0 << endl;
  cout << "y origin = " << mY0 << endl;
  cout << "slope dxdz = " << mdxdz << endl;
  cout << "slope dydz = " << mdydz << endl;
  cout << "weight in fit = " << weight <<  endl;
  StThreeVectorD origin(mX0,mY0,0.0);
  double pt  = 88889999;   
  double nxy=::sqrt(mdxdz*mdxdz +  mdydz*mdydz);
    if(nxy<1.e-5){ // beam line _MUST_ be tilted
      cout << "StMinuitVertexFinder:: Beam line must be tilted!" << endl;
      nxy=mdxdz=1.e-5; 
    }
    double p0=pt/nxy;  
    double px   = p0*mdxdz;
    double py   = p0*mdydz;
    double pz   = p0; // approximation: nx,ny<<0
    StThreeVectorD MomFstPt(px*GeV, py*GeV, pz*GeV);
    delete mBeamHelix;
    mBeamHelix = new StPhysicalHelixD(MomFstPt,origin,0.5*tesla,1.);

    //re-initilize minuit for 1D fitting
    mMinuit = new TMinuit(1);         
    mMinuit->SetFCN(&StMinuitVertexFinder::fcn1D);
    mMinuit->SetPrintLevel(1);
    mMinuit->SetMaxIterations(1000);
    mExternalSeedPresent = false;


}

void StMinuitVertexFinder::NoVertexConstraint() {mVertexConstrain = false; cout << "StMinuitVertexFinder::No Vertex Constraint" << endl;}

double StMinuitVertexFinder::beamX(double z) {
  float x = mX0 + mdxdz*z;
  return x;
}

double StMinuitVertexFinder::beamY(double z) {
  float y = mY0 + mdydz*z;
  return y;
}

void StMinuitVertexFinder::CTBforSeed() { mRequireCTB = true; return; }
void StMinuitVertexFinder::NoCTBforSeed() { mRequireCTB = false; return; }
int  StMinuitVertexFinder::NCtbMatches() { return nCTBHits;}
void StMinuitVertexFinder::SetFitPointsCut(int fitpoints) {mMinNumberOfFitPointsOnTrack = fitpoints;return;}


