//////////////////////////////////////////////////////////////////////////
///                                                                    ///
/// StV0FinderMaker class (finds V0 secondary vertices)                ///
///                                                                    ///
//////////////////////////////////////////////////////////////////////////
//
//  Cuts can be found in the code by comments beginning with "Cut:"
//
//


#include "StV0FinderMaker.h"
#include "StMessMgr.h"
#include "StEvent/StEventTypes.h"
#include "TMath.h"
#include "TVector2.h"
#include "tables/St_V0FinderParameters_Table.h"

///Begin Betty
///#include "StEstMaker/StEstTracker.h"
#include "StEvent/StTrack.h"
#include "StMuDSTMaker/COMMON/StMuTypes.hh"
///End Betty

#include "math_constants.h"
#include "phys_constants.h"
#include "SystemOfUnits.h"

static const int BLOCK=1024;


StV0FinderMaker* StV0FinderMaker::mInstance = 0;

 
//_____________________________________________________________________________
  StV0FinderMaker::StV0FinderMaker(const char *name):StMaker(name),
         v0pars(0),pars(0),pars2(0),event(0),v0Vertex(0),
         prepared(kFALSE),useExistingV0s(kFALSE),dontZapV0s(kFALSE),
         useTracker(kTrackerUseBOTH),useSVT(kNoSVT),useEventModel(kUseStEvent),
         useV0Language(kV0LanguageUseCpp),useXiLanguage(kXiLanguageUseCppOnCppV0),
         useLanguage(kLanguageUseRun),useLikesign(kLikesignUseStandard),
         useRotating(kRotatingUseStandard)
{
  // Initializes everything that wasn't yet :
  trks = 0;
  det_id_v0 = 0;
  ITTFflag = 0;
  TPTflag = 0;
  mainv.setX(0.);
  mainv.setY(0.);
  mainv.setZ(0.);

  trkcnt = 0;
  trkmax = BLOCK;
  trkNodeRatio = 1.;
  trkNodeRatioCnt = 0.;
  
  // Check for multiple instances
  if (mInstance != 0)
    gMessMgr->Warning() << "(" << name <<
      ") : MORE THAN ONE INSTANCE!" << endm;
  else mInstance = this;

  Bfield = 1.e-10; //Random value for initialisation.
                   //If it isn't changed to correct value in
                   // Prepare() then something has gone wrong!
}










//_____________________________________________________________________________
StV0FinderMaker::~StV0FinderMaker() {
}












//_____________________________________________________________________________
void StV0FinderMaker::GetPars()
{
  TDataSet* dbDataSet = GetDataBase("Calibrations/tracker");
  if (!dbDataSet) {
    gMessMgr->Error(
      "Init() : could not find Calibrations/tracker database.");
    return; 
  }
  v0pars = (St_V0FinderParameters*) (dbDataSet->FindObject("V0FinderParameters"));
  if (!v0pars) {
    gMessMgr->Error(
      "Init() : could not find V0FinderParameters in database.");
    return;
  }
  ///AddRunCont(v0pars);
}













//_____________________________________________________________________________
Int_t StV0FinderMaker::Init()
{bool a,b,c;
 
 if ((useTracker!=kTrackerUseTPT) && (useTracker!=kTrackerUseITTF) && (useTracker!=kTrackerUseBOTH))
    {gMessMgr->Error("Init() : wrong TrackerUsage parameter set.");
     return kStErr;
     }
 if ((useSVT!=kNoSVT) && (useSVT!=kUseSVT))
    {gMessMgr->Error("Init() : wrong SVTUsage parameter set.");
     return kStErr;
     }
 if ((useEventModel!=kUseStEvent) && (useEventModel!=kUseMuDst))
    {gMessMgr->Error("Init() : wrong EventModelUsage parameter set.");
     return kStErr;
     }
 if ((useLikesign!=kLikesignUseStandard) && (useLikesign!=kLikesignUseLikesign))
    {gMessMgr->Error("Init() : wrong LikesignUsage parameter set.");
     return kStErr;
     }
 if ((useRotating!=kRotatingUseStandard) && (useRotating!=kRotatingUseRotating) && (useRotating!=kRotatingUseSymmetry) && (useRotating!=kRotatingUseRotatingAndSymmetry))
    {gMessMgr->Error("Init() : wrong RotatingUsage parameter set.");
     return kStErr;
     }
 
 if (useTracker == kTrackerUseTPT) gMessMgr->Info()<<"use TPT tracks."<<endm;
 if (useTracker == kTrackerUseITTF) gMessMgr->Info()<<"use ITTF tracks."<<endm;
 if (useTracker == kTrackerUseBOTH) gMessMgr->Info()<<"use TPT *and* ITTF tracks."<<endm;
 if (useSVT == kUseSVT) gMessMgr->Info()<<"use SVT points if possible."<<endm;
 if (useSVT == kNoSVT) gMessMgr->Info()<<"do not use SVT points."<<endm;
 if (useEventModel == kUseStEvent) gMessMgr->Info()<<"expect StEvent files in input."<<endm;
 if (useEventModel == kUseMuDst)  gMessMgr->Info()<<"expect MuDst files in input."<<endm;
 if (useLikesign == kLikesignUseLikesign) gMessMgr->Info()<<"does like-sign finding."<<endm;
 if (useRotating == kRotatingUseRotating) gMessMgr->Info()<<"does rotating finding."<<endm;
 if (useRotating == kRotatingUseSymmetry) gMessMgr->Info()<<"does symmetry finding."<<endm;
 if (useRotating == kRotatingUseRotatingAndSymmetry) gMessMgr->Info()<<"does rotating + symmetry finding."<<endm;

 if (useLanguage != kLanguageUseSpecial)
    {a=(bool)(1&(useLanguage>>2));
     b=(bool)(1&(useLanguage>>1));
     c=(bool)(1&useLanguage);
     useV0Language=2*(!(a^c))+(a|c);
     useXiLanguage=4*(b&(!(a^c)))+2*(a&b&(!c))+(a|c);
     }
 switch (useLanguage)
    {case kLanguageUseOldRun : gMessMgr->Info()<<"Fortran run."<<endm;
                               break;
     case kLanguageUseRun : gMessMgr->Info()<<"C++ run."<<endm;
                            gMessMgr->Info()<<"BE CAREFUL : you are NOT running the XiFinder !"<<endm;
                            break;
     case kLanguageUseTestV0Finder : gMessMgr->Info()<<"Test V0Finder."<<endm;
                                     break;
     case kLanguageUseTestXiFinder : gMessMgr->Info()<<"Test XiFinder."<<endm;
                                     gMessMgr->Info()<<"BE CAREFUL : you are NOT running the XiFinder !"<<endm;
                                     break;
     case kLanguageUseTestBothFinders : gMessMgr->Info()<<"Test V0Finder and XiFinder."<<endm;
                                        gMessMgr->Info()<<"BE CAREFUL : you are NOT running the XiFinder !"<<endm;
                                        break;
     case kLanguageUseSpecial : break;
     default : gMessMgr->Error("Init() : wrong LanguageUsage parameter set.");
               return kStErr;
     }
 if ((useV0Language!=kV0LanguageUseFortran) && (useV0Language!=kV0LanguageUseCpp) && (useV0Language!=kV0LanguageUseBoth))
    {gMessMgr->Error("Init() : wrong V0LanguageUsage parameter set.");
     return kStErr;
     }
 if (1&useV0Language) gMessMgr->Info()<<"   Will store Fortran V0."<<endm;
 if (2&useV0Language) gMessMgr->Info()<<"   Will store C++ V0."<<endm;
 if (1&useXiLanguage) gMessMgr->Info()<<"   Will store Fortran Xi."<<endm;
 if (2&useXiLanguage) gMessMgr->Info()<<"   BE CAREFUL : will NOT store C++ Xi, although asked."<<endm;
 if (4&useXiLanguage) gMessMgr->Info()<<"   BE CAREFUL : will NOT store C++ Xi, although asked."<<endm;

 if (useEventModel)  { //initialize mMuDstMaker
   mMuDstMaker = (StMuDstMaker*)GetMaker("myMuDstMaker");
   if(!mMuDstMaker) gMessMgr->Warning("Init can't find a valid MuDst");
 }
 
 return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StV0FinderMaker::Prepare() {

  if (prepared) return kStOk;

  unsigned short i,j,nNodes,nTrksEstimate;
  StThreeVectorD p;

  // Get pars
  GetPars();
  ITTFflag=kITKalmanFitId;
  TPTflag=kHelix3DIdentifier;

  // Get event 

  ///Betty method one:
  if(GetEventUsage()==kUseStEvent){
    event = (StEvent*) GetInputDS("StEvent");
  }
  ///Betty method two:
  else if(GetEventUsage()==kUseMuDst){ 
    StMuDst* mu = mMuDstMaker->muDst();
    if(mu) cout<<"V0Finder :: found a MuDst"<<endl;
    if(mu->event())cout<<"see a muEvent ... "<<endl;
    if(mu) event = mu->createStEvent();
    
    if(event) AddData(event);
    if(event)cout<<"see a recreated StEvent!"<<endl;
#if 0
    Int_t nV0s = mu->GetNV0();
    cout<<"the number of existing v0's is "<<nV0s<<endl;
#endif
  }
  ///end of Betty
  
  if (!event)
    {gMessMgr->Warning("no StEvent ; skipping event.");
    return kStWarn;
    }
  
  // Get Primary Vertex Position
  StPrimaryVertex* pvert = event->primaryVertex();
  if (!pvert)
     {gMessMgr->Warning("no primary vertex ; skipping event.");
      return kStWarn;
      }
  mainv = pvert->position();

  StSPtrVecTrackNode& theNodes = event->trackNodes();
  nNodes = theNodes.size();

  // Initial estimate of needed vector sizes
  nTrksEstimate = (unsigned int) (nNodes*trkNodeRatio);
  if (nTrksEstimate > trk.size()) ExpandVectors(nTrksEstimate);
  
  // Find which global tracks to use
  trks=0;
  double BfieldRunning = 0;
  double nBfieldRunning = 0;
  for (i=0; i<nNodes; i++) {
    int nj = theNodes[i]->entries(global);
    for (j=0; j<nj; j++) {

      StTrack* tri = theNodes[i]->track(global,j);
      ///Begin Betty
      //if there is a track that uses an SVT point, set the track to estGlobal
      if(useSVT){
        StTrack* svtTrack = theNodes[i]->track(estGlobal,j);
        if (svtTrack){
          tri=svtTrack;
        }
      }
      ///End Betty

      //Cut: track type
      //cout << "i,j" << i << "," << j << " fittingMethod = " << tri->fittingMethod() << endl;

      if (
	  //(tri->fittingMethod() == TPTflag && (GetTrackerUsage() == kTrackerUseITTF)) ||
	  (tri->fittingMethod() != ITTFflag && (GetTrackerUsage() == kTrackerUseITTF)) ||
          (tri->fittingMethod() == ITTFflag && (GetTrackerUsage() == kTrackerUseTPT))) continue;

      //Cut: track flag
      if (tri->flag() <= 0) continue;
      
      // Expand vectors if needed
      if (trks >= trk.size()) ExpandVectors(trks+1);

      // Determine detector id of track i
      const StTrackTopologyMap& map = tri->topologyMap();
      Bool_t tpcHit = map.hasHitInDetector(kTpcId);
      Bool_t silHit = map.hasHitInDetector(kSvtId) ||
        map.hasHitInDetector(kSsdId);
      if (tpcHit) {
        if (silHit)
          detId[trks] = 3; //SVT+TPC
        else
          detId[trks] = 1; //TPC-only
      } else if (silHit)
        detId[trks] = 2; //SVT-only
      else
        //ignore this track
        continue;
      
      trk[trks] = tri;
      
      StTrackGeometry* triGeom = tri->geometry();
      if(!triGeom) continue; //ignore the track if it has no geometry 
      
      heli[trks] = triGeom->helix();
      
      p = triGeom->momentum();
      
      pt[trks] = p.perp();
      ptot[trks] = p.mag();
      trkID[trks]=tri->key();
      
      // Determine number of hits (in SVT+TPC)
      hits[trks] = map.numberOfHits(kTpcId) +
        map.numberOfHits(kSvtId) +
        map.numberOfHits(kSsdId);
      //Cut: number of hits
      pars2 = v0pars->GetTable(detId[trks]-1);
      if (hits[trks] < pars2->n_point) continue;
      
      //if (!trks)
      if (nBfieldRunning<1e2) {
        StThreeVectorD p1 = triGeom->momentum();
        StThreeVectorD p2 = heli[trks].momentum(Bfield);

        if      (fabs(p2.x()) > fabs(p2.y())) Bfield *= p1.x()/p2.x();
        else if (fabs(p2.y()) > 1.e-20) Bfield *= p1.y()/p2.y();
	else continue;
        // This method appears to only be accurate to about 1 part in 1000 for
        //  single tracks. We can do better by using information from multiple
        //  tracks. But Bfield is only used in momentum determination, which
        //  is not likely to be that accurate anyhow, so no need to push too far.
        //  (Bfield is not used in helix extrapolation for now.)

        if (fabs(Bfield)<1.e-20) return kStWarn;
        Bfield = TMath::Sign(Bfield,-1.0*triGeom->charge()*triGeom->helicity());

	BfieldRunning += Bfield;
        nBfieldRunning += 1;
      }
      if (triGeom->charge() > 0) ptrk.push_back(trks);
      else if (triGeom->charge() < 0) ntrk.push_back(trks);
      trks++;
    }
  }
  if (nBfieldRunning>0) Bfield = BfieldRunning/nBfieldRunning;

  // Manage vector memory usage
  //   If maximum needed is less than half allocated for 10 events, resize
  if (trks < (trk.size()/2)) {
    if (trks > trkmax) trkmax = trks;
    trkcnt++;
    if (trkcnt >= 10) {
      ExpandVectors(trkmax);
      trkcnt = 0;
      trkmax = BLOCK;
    }
  } else {
    trkcnt = 0;
    trkmax = BLOCK;
  }
  trkNodeRatio = ((trkNodeRatio*trkNodeRatioCnt)+((float) trks)/((float) nNodes)) /
                   (trkNodeRatioCnt + 1.);
  trkNodeRatioCnt++;
  
  gMessMgr->Info() << "No. of nodes is : "
                   << nNodes << endm;
  gMessMgr->Info() << "No. of tracks is : "
                   << trks << endm;

  prepared = kTRUE;
  return kStOk;
}











//_____________________________________________________________________________
Int_t StV0FinderMaker::Make() {

  // Variables:
  StThreeVectorD xi,xj,pi,pj,xpp,pp,impact;      // 3D vectors of the V0
  StThreeVectorD xi1,xj1,pi1,pj1,tmp3V;          // temporary 3D vectors
  TVector2 ri,rj,xci,xcj,tmp2V;                  // 2D vectors of the tracks
  double rad_i,rad_j,separation,solution,dxc;    // helix circle params
  double dca_ij,dca_ij1,rmin,dlen,pperpsq,ppmag; // V0 params
  double alpha,ptArm_sq,pPosAlongV0,pNegAlongV0; // Armenteros params
  double cosij,sin2ij,t1,t2;                     // 3D dca calculation vars
  unsigned short i,j,ii,jj;                      // track iteration vars
  pairD paths,paths1,path2;                      // helix pathLength vars
  double temp;
  Bool_t doSecond, isPrimaryV0, usedV0;
  Int_t iRes;
  long  keepTrack; ///Betty

  if (! (2&useV0Language)) return kStOk;

  gMessMgr->Info("Make() : Starting...");
  
  // Prepare event and track variables
  iRes = Prepare();
  if (iRes != kStOk) return iRes;


  StSPtrVecV0Vertex& v0Vertices = event->v0Vertices();
  gMessMgr->Info()<<"coming in I have "<<v0Vertices.size()<<" V0s."<<endm;

  if (!(1&useV0Language)) {
    //Erase existing V0s and Xis
    // (must do Xis too as they point to the V0s!)
    gMessMgr->Info()<<"pre-existing V0s and Xis deleted."<<endm; 
    StSPtrVecV0Vertex v0Vertices2;
    v0Vertices = v0Vertices2;
    StSPtrVecXiVertex& xiVertices   = event->xiVertices();
    StSPtrVecXiVertex xiVertices2;
    xiVertices = xiVertices2;
  }


  // Loop over track pairs to find V0s

  //i track is positive
    int nii = ptrk.size();
    for (ii=0; ii<nii; ii++) {
    i = ptrk[ii];

    xci.Set(heli[i].xcenter(),heli[i].ycenter());
    ri.Set(heli[i].origin().x(),heli[i].origin().y());
    ri -= xci;
    rad_i = ri.Mod();

    //j track is negative
    int njj = ntrk.size();
    for (jj=0; jj<njj; jj++) {
      j = ntrk[jj];

      if (GetTrackerUsage() == kTrackerUseBOTH)
         {
	   //if ((trk[i]->fittingMethod() == ITTFflag) && (trk[j]->fittingMethod() == TPTflag)) continue;
	   if ((trk[i]->fittingMethod() == ITTFflag) && (trk[j]->fittingMethod() != ITTFflag)) continue;
	   //if ((trk[i]->fittingMethod() == TPTflag) && (trk[j]->fittingMethod() == ITTFflag)) continue;
	   if ((trk[i]->fittingMethod() != ITTFflag) && (trk[j]->fittingMethod() == ITTFflag)) continue;
	 }

      // Determine detector id of V0 for pars
      det_id_v0 = TMath::Max(detId[i],detId[j]);

      // Primary   V0 cut parameters
      pars  = v0pars->GetTable(det_id_v0+2);
      // Primary and secondary V0 cut parameters
      pars2 = v0pars->GetTable(det_id_v0-1);

      //Cut: number of hits
      //Now cut directly when filling the table of tracks in Prepare().
//      if ((hits[i] < pars2->n_point) ||
//          (hits[j] < pars2->n_point)) continue;

      //Cut: Initial cut on dca of tracks to primary vertex
      // (perform as early as possible)
      // V0 can't have pt larger than sum of pts of daughters
      temp = pt[i] + pt[j];
      if ((temp < 0.99 * pars2->dcapn_pt) &&
          ((trk[i]->impactParameter() <= pars2->dcapnmin) ||
           (trk[j]->impactParameter() <= pars2->dcapnmin))) continue;

      xcj.Set(heli[j].xcenter(),heli[j].ycenter());
      rj.Set(heli[j].origin().x(),heli[j].origin().y());
      rj -= xcj;
      rad_j = rj.Mod();

      tmp2V = xci - xcj;
      dxc = tmp2V.Mod();
      separation = dxc - (rad_i + rad_j);
      doSecond = kFALSE;
      dca_ij1 = -9999;


      // ********************* START OF DETERMINATION OF V0 GEOMETRY
      if (separation < 0)
         {// Check for one helix circle completely inside the other
          if (dxc < TMath::Abs(rad_i - rad_j)) continue;
          // Helix circles are overlapping
          //FULL 3D ITERATIVE METHOD (VERY SLOW, ONE SOLUTION)
          //      paths = heli[i].pathLengths(heli[j]);
          //2D+ METHOD (GETS 3D APPROXIMATION AFTER TWO 2D SOLUTIONS)
          path2 = heli[i].pathLength(rad_j,xcj.X(),xcj.Y());
          // Two possible solutions: process ones that aren't nans
          if (!std::isnan(path2.first))
             {solution = path2.first;
              if ((!std::isnan(path2.second)) && (path2.second != path2.first))
                 {doSecond = kTRUE;
                  }
              goto ProcessSolution;
              }
             else if (std::isnan(path2.second))
             {// no solutions
              continue;
              }
              //else run only with the second solution
          SecondSolution:
          solution = path2.second;
          doSecond = kFALSE;
          ProcessSolution:
          // paths contains the pathlengths for this solution with
          // that for track i stored in first, and track j stored
          // in second.
          paths.first = solution;
          xi = heli[i].at(paths.first );
          paths.second = heli[j].pathLength(xi.x(),xi.y());
          xj = heli[j].at(paths.second);
          }
          else if (separation < pars2->dca)
         {// Helix circles are close, but not overlapping,
          // find dca to point halfway between circle centers
          tmp2V = (xci + xcj) * 0.5;
          paths.first  = heli[i].pathLength(tmp2V.X(),tmp2V.Y());
          paths.second = heli[j].pathLength(tmp2V.X(),tmp2V.Y());
          xi = heli[i].at(paths.first );
          xj = heli[j].at(paths.second);
          }
          else
         {// Helix circles are too far apart
          continue;
          }
      
      dca_ij = xi.z() - xj.z();
      if (doSecond) {
        // If we have two solutions, save this one and compare
        dca_ij1 = dca_ij;
        xi1=xi;
        xj1=xj;
        paths1=paths;
        goto SecondSolution;
        }
      if ((dca_ij1 != -9999) &&
          (TMath::Abs(dca_ij1) < TMath::Abs(dca_ij))) {
        // First solution was better
        dca_ij = dca_ij1;
        xi=xi1;
        xj=xj1;
        paths=paths1;
        }
      // At this point, dca_ij is *signed* for use in 3D calc
      // *********************  END  OF DETERMINATION OF V0 GEOMETRY

      pi = heli[i].momentumAt(paths.first ,Bfield);
      pj = heli[j].momentumAt(paths.second,Bfield);
      
      //Cut: check if tracks points away from prim vtx
      if ((pi.dot(xi-mainv) < 0.0) ||
          (pj.dot(xj-mainv) < 0.0)) continue;
						
      ///Begin Betty
      //Cut: check if the first point of either track is after v0vertex
      // Commented out by helen 4/13/04 because it hurts SVT Xis try
      // to find a more elegant soultion later
      // if ((pi.dot(heli[i].origin() - xi) < 0.0) ||  //if pV0 * r <0, cut
      //   (pj.dot(heli[j].origin() - xj) < 0.0)) continue;
      ///End Betty


      // ********************* START OF DETERMINATION OF 3D DCA
      // dca_ij will be an approximation of the 3D dca
      pi1 = pi/ptot[i];
      pj1 = pj/ptot[j];

      cosij = pi1.dot(pj1);
      sin2ij = 1.0 - cosij*cosij;
      if (sin2ij) {
        temp = dca_ij/sin2ij;
        t1 = (-pi1.z()+pj1.z()*cosij)*temp;
        t2 = ( pj1.z()-pi1.z()*cosij)*temp;
 
        temp = rad_i*(ptot[i]/pt[i]);
        temp *= sin(t1/temp);
        xi1 = xi + pi1.pseudoProduct(temp,temp,t1);

        temp = rad_j*(ptot[j]/pt[j]);
        temp *= sin(t2/temp);
        xj1 = xj + pj1.pseudoProduct(temp,temp,t2);

        dca_ij1 = (xi1 - xj1).mag2();
        dca_ij *= dca_ij; /// dca_ij no longer signed (squared)

        if (dca_ij1 < dca_ij) {
          paths.first  = heli[i].pathLength(xi1.x(),xi1.y());
          paths.second = heli[j].pathLength(xj1.x(),xj1.y());
          ///paths.first  = heli[i].pathLength(xnix,xniy);
          ///paths.second = heli[j].pathLength(xnjx,xnjy);
          xi1 = heli[i].at(paths.first);
          xj1 = heli[j].at(paths.second);
          dca_ij1 = (xi1 - xj1).mag2();
          if (dca_ij1 < dca_ij) {
            xi = xi1;
            xj = xj1;
            pi = heli[i].momentumAt(paths.first ,Bfield);
            pj = heli[j].momentumAt(paths.second,Bfield);
            dca_ij = dca_ij1;
          }
        }
        //This code is the new one.

      /*if (dca_ij1 < dca_ij) {
          paths.first  = heli[i].pathLength(xi1.x(),xi1.y());
          paths.second = heli[j].pathLength(xj1.x(),xj1.y());
          xi = xi1;
          xj = xj1;
          pi = heli[i].momentumAt(paths.first ,Bfield);
          pj = heli[j].momentumAt(paths.second,Bfield);
          dca_ij = dca_ij1;
          }*/
        //And this one is the old one (comparison with Fortran).

 
      }
      // *********************  END  OF DETERMINATION OF 3D DCA


      // Now we have the positions and momenta of our tracks
      // at the V0 vertex determined. Ready to make cuts on
      // the V0 itself. 

      //Cut: dca between tracks      
      if (dca_ij >= (pars2->dca*pars2->dca)) continue;

      pp = pi + pj;
      pperpsq = pp.perp2();
                  
      //Cut: dca of tracks to primary vertex (early as possible)
      if ((pperpsq < pars2->dcapn_pt * pars2->dcapn_pt) && 
          ((trk[i]->impactParameter() <= pars2->dcapnmin) ||
           (trk[j]->impactParameter() <= pars2->dcapnmin))) continue;

      xpp = (xi + xj) * 0.5;
      impact = xpp - mainv;
      dlen = impact.mag2();

      //Cut: decay length from prim vtx
      if (dlen <= (pars2->dlen*pars2->dlen)) continue;

      //Cut: V0 momentum should be away from prim vtx 
      if (pp.dot(impact) < 0.0) continue;

      ppmag = pperpsq + pp.z()*pp.z();
      rmin = impact.cross(pp).mag2()/ppmag;

      //Cut: dca of V0 to prim vtx
      if (rmin >= (pars2->dcav0*pars2->dcav0)) continue;

      tmp3V = pp/::sqrt(ppmag);
      pPosAlongV0 = pi.dot(tmp3V);
      pNegAlongV0 = pj.dot(tmp3V);
      alpha = (pPosAlongV0-pNegAlongV0) /
              (pPosAlongV0+pNegAlongV0);

      //Cut: Armenteros alpha
      if (TMath::Abs(alpha) > pars2->alpha_max) continue;

      ptArm_sq = ptot[i]*ptot[i] - pPosAlongV0*pPosAlongV0;

      //Cut: Armenteros pt
      if (ptArm_sq > (pars2->ptarm_max*pars2->ptarm_max)) continue;

      rmin   = ::sqrt(rmin);
      dca_ij = ::sqrt(dca_ij);
      if (trk[i]->fittingMethod() == ITTFflag) dca_ij=-dca_ij;
      
      // Fill an StV0Vertex
      v0Vertex = new StV0Vertex();
      v0Vertex->setPosition(xpp);
      v0Vertex->addDaughter(trk[i]);
      v0Vertex->addDaughter(trk[j]);
      v0Vertex->setDcaDaughterToPrimaryVertex(positive,trk[i]->impactParameter());
      v0Vertex->setDcaDaughterToPrimaryVertex(negative,trk[j]->impactParameter());
      ///3VectorF vs 3VectorD???
      v0Vertex->setMomentumOfDaughter(positive,pi);
      v0Vertex->setMomentumOfDaughter(negative,pj);
      v0Vertex->setDcaDaughters(dca_ij);
      v0Vertex->setDcaParentToPrimaryVertex(rmin);
      
      ///Begin Betty
      /*Set bits 0000 0000 -> 1001 0000 if C++ used
      if SVT ran in tracking, 1001 1000;
                   SVT and +: 1001 1100
                SVT, + and -: 1001 1110; 
                 SVT, - only: 1001 1010*/
      
      ///variable keepTrack (32 bits)
      keepTrack=0;
      keepTrack |=((long)1 << 4); // c++ v0 finder ran
      if (useSVT)
         {keepTrack |=((long)1 << 3);  //sets to one the fourth digit if SVT was used
	 }
      if (detId[i]==2 || detId[i]==3){
	keepTrack |=((long)1 << 2); //sets 3rd bit to 1 if + track came from SVT  
      }
      if(detId[j]==2 || detId[j]==3){
	keepTrack |=((long)1 << 1); //sets 2nd bit to 1 if - track came from SVT
      }
    
      keepTrack *= -1; //sets to negative the last digit 
      v0Vertex->setChiSquared((float)keepTrack);
      ///End Betty
      
      // Use primary V0 cut parameters
      isPrimaryV0 =
        (rmin < pars->dcav0) &&
        ((pperpsq >= pars->dcapn_pt * pars->dcapn_pt) ||
         ((trk[i]->impactParameter() > pars->dcapnmin) &&
          (trk[j]->impactParameter() > pars->dcapnmin)));
      
      // Call secondary usage of V0 (such as for Xis)
      usedV0 = UseV0();
      
      // Tag used V0s to indicate if they aren't primary
      if (usedV0 && !isPrimaryV0)
        v0Vertex->setDcaParentToPrimaryVertex(-rmin);

      // If used or primary, keep it
      if (usedV0 || isPrimaryV0) {
        v0Vertices.push_back(v0Vertex);
      } else {
        delete v0Vertex;
        v0Vertex = 0;
      }

    } // j-Loop
  } // i-Loop

  gMessMgr->Info()<<"now I have "<<v0Vertices.size()<<" V0s."<<endm;
  gMessMgr->Info()<<"using magnetic field : "<<Bfield/tesla<<" T."<<endm;

  // Any cleanup involved for using KeepV0()
  
  return kStOk;
}










//____________________________________________________________________________

void StV0FinderMaker::Clear(Option_t *option){
  prepared = kFALSE;
  ptrk.clear();
  ntrk.clear();
  if(useEventModel){
    if(mMuDstMaker){
      if(event){
        delete event;
        event=0;
      }
    }
  }
}












//_____________________________________________________________________________
void StV0FinderMaker::Trim() {
  // Loop over V0s and remove those that don't satisfy the tight V0 cuts

  gMessMgr->Info() << "Trim() : Starting..." << endm;

  event = (StEvent*) GetInputDS("StEvent");
  pars = v0pars->GetTable(3);
  StSPtrVecV0Vertex& v0Vertices = event->v0Vertices();
  int iV0s = v0Vertices.size();
  for (int i=iV0s-1; i>=0; i--) {

    v0Vertex = v0Vertices[i];
    if ((v0Vertex) &&
        // Is it not a seconday V0?
        (v0Vertex->dcaParentToPrimaryVertex() >= 0) &&
        // Is it not a primary V0?
        ! ((v0Vertex->dcaParentToPrimaryVertex() < pars->dcav0) &&
           ((v0Vertex->momentum().perp2() >= pars->dcapn_pt * pars->dcapn_pt) ||
            ((v0Vertex->dcaDaughterToPrimaryVertex(positive) > pars->dcapnmin) &&
             (v0Vertex->dcaDaughterToPrimaryVertex(negative) > pars->dcapnmin))))) {
      v0Vertex->makeZombie();
      iV0s--;
    }
  } // V0 loop

  gMessMgr->Info() << "Trim() : saving " << iV0s <<
                      " V0 candidates" << endm;
}
//_____________________________________________________________________________
void StV0FinderMaker::ExpandVectors(unsigned short size) {
  unsigned int oldsize = trk.size();
  unsigned int newsize = oldsize;
  if (newsize > size) newsize = BLOCK;
  while (newsize <= size) newsize += BLOCK;
  if (newsize == oldsize) return;
  gMessMgr->Info() << IsA()->GetName() << "::ExpandVectors(" << newsize
                   << ") for " << GetName() << endm;
  trk.resize(newsize);
  hits.resize(newsize);
  detId.resize(newsize);
  pt.resize(newsize);
  ptot.resize(newsize);
  heli.resize(newsize);
  trkID.resize(newsize);
}
//_____________________________________________________________________________
// $Id: StV0FinderMaker.cxx,v 1.36 2016/12/12 17:18:04 smirnovd Exp $
// $Log: StV0FinderMaker.cxx,v $
// Revision 1.36  2016/12/12 17:18:04  smirnovd
// Removed outdated ClassImp ROOT macro
//
// Revision 1.35  2015/07/20 18:03:15  genevb
// isnan => std::isnan
//
// Revision 1.34  2013/02/22 18:37:39  perev
// Cleanu
//
// Revision 1.33  2013/02/22 17:06:10  fisyak
// Remove reference to gufld, which is not used
//
// Revision 1.32  2008/03/05 04:20:18  genevb
// Change to DB table of V0FinderParameters, reduce logger output, improve Bfield calc
//
// Revision 1.31  2006/06/12 15:17:48  caines
// Fix chisq flagging so chisq set for SVT even when sti and v02 flags are used
//
// Revision 1.30  2005/02/10 02:51:09  jeromel
// Correct Zero field protection (broke V0/kink)
//
// Revision 1.29  2005/02/09 21:10:01  perev
// test for zero field fixed
//
// Revision 1.28  2005/02/05 01:10:16  perev
// Zero field check
//
// Revision 1.27  2004/09/17 03:14:06  perev
// LeakOff+cleanup
//
// Revision 1.26  2004/08/27 05:37:28  genevb
// Slightly modify parameters of vector memory control
//
// Revision 1.25  2004/08/26 03:00:46  genevb
// Improved vector size management
//
// Revision 1.24  2004/08/23 23:14:53  genevb
// Use resize() for vectors, and allow downsizing.
//
// Revision 1.23  2004/08/11 21:26:38  genevb
// Trade static arrays for vectors
//
// Revision 1.22  2004/04/15 19:41:35  jeromel
// Mainly undo recent patch which is logically right (but over-estimate actual coding standards)
//
// Revision 1.21  2004/04/13 19:50:38  caines
// Remove cut on v0 being before hit as thsi hurts vos from xi in svt
//
// Revision 1.20  2004/04/06 14:05:16  faivre
// Change default options to : do not use SVT.
//
// Revision 1.19  2004/04/02 08:57:23  faivre
// Use actual TPT flag rather than "not ITTF" for TPT tracks. Minor changes.
//
// Revision 1.18  2004/03/04 18:31:03  faivre
// Add cut number of hits for Xi's bachelors.
//
// Revision 1.17  2004/02/16 16:18:39  betya
// added a check on (!triGeom) in V0Finder::Prepare()
//
// Revision 1.16  2004/02/03 09:52:45  faivre
// Update user-friendliness ; default options now use SVT and ITTF+TPT ; remove warning.
//
// Revision 1.15  2004/01/27 17:56:05  betya
//
// added EventModelUsage so that the V0Finder and XiFinder can no run on
// MuDst as well as on StEvent.  Note that the output is still in the StEvent
// format.  Added Clear() in StV0FinderMaker.cxx to accomodate this addition.
//
// Revision 1.14  2003/11/08 18:25:54  faivre
// Bfield + consistency int/short
//
// Revision 1.13  2003/09/17 12:00:31  faivre
// RH8 : initialize everything in constructor.
//
// Revision 1.12  2003/09/07 03:49:04  perev
// gcc 3.2 + WarnOff
//
// Revision 1.11  2003/09/02 17:58:59  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.10  2003/08/22 17:47:14  caines
// Get sign AND magnitude of mag field correctly for Xi and V0 finder
//
// Revision 1.9  2003/07/17 17:01:10  faivre
// Add one causality check. Exhaustive listing of cuts. Tab->spaces.
//
// Revision 1.8  2003/07/15 17:40:36  faivre
// Fixed charge of Bfield (used for print, and XiFinder since now).
//
// Revision 1.7  2003/07/04 17:52:54  faivre
// Use SVT cuts if any dg has a SVT hit.
//
// Revision 1.6  2003/06/24 16:20:01  faivre
// Uses SVT tracks. Fixed bool calculations. Exits when bad param. Reshaping.
//
// Revision 1.5  2003/05/14 19:15:03  faivre
// Fancy choices Fortran/C++ V0's and Xi's. SVT tracks.
//
// Revision 1.4  2003/05/02 21:21:08  lbarnby
// Now identify ITTF tracks by fittingMethod() equal to  kITKalmanFitId
//
// Revision 1.3  2003/04/30 20:38:22  perev
// Warnings cleanup. Modified lines marked VP
//
// Revision 1.2  2003/04/30 19:14:27  faivre
// ITTF vs TPT V0s
//
//
