#include "StiEvaluator/ResolutionPlots.h"
#include "StMiniMcEvent/StMiniMcEvent.h"
#include "StMiniMcEvent/StMiniMcPair.h"

ClassImp(ResolutionPlots)

ResolutionPlots::ResolutionPlots(const char* name, 
				 const char* description, 
				 double minMult,
				 double maxMult,
				 double minZ,
				 double maxZ,
				 int    id,
				 double  minNHits,
				 double  minNFitHits,
				 double  maxDca,
				 double  minEta,
				 double  maxEta,
				 int     trackType,
				 int     pullType)
  :  StiEvaluatorHistograms(name, 
			    description,
			    minMult, maxMult,
			    minZ, maxZ,
			    id,
			    minNHits,
			    minNFitHits,
			    maxDca,
			    minEta,
			    maxEta,
			    trackType),
     _pullType(pullType),
    
    _h_p_mc(0),
    _h_pt_mc(0),
    _h_eta_mc(0),
    _h_phi_mc(0),
    
    _h_p_rec(0),
    _h_pt_rec(0),
    _h_eta_rec(0),
    _h_phi_rec(0),
    
    _h_dpVsP(0),
    _h_dptVsP(0),
    _h_dptVsPt(0),
    _h_detaVsEta(0),
    _h_dphiVsPt(0),
    
    _h_dpByPVsP(0),
    _h_dptByPtVsPt(0),
    _h_dptByPtVsP(0),
    
    _p_dpVsP(0),
    _p_dptVsP(0),
    _p_dptVsPt(0),
    _p_detaVsEta(0),
    _p_dphiVsPt(0),
    
    _p_dpByPVsP(0),
    _p_dptByPtVsPt(0),
    _p_dptByPtVsP(0),
    
    _p_dpByP2VsP(0),
    _p_dptByPt2VsPt(0),
    _p_deta2VsEta(0),
    _p_dphi2VsPt(0),
    
    _h_sigdpByPVsP(0),
    _h_sigdptByPtVsPt(0),
    _h_sigdptByPtVsP(0),
    _h_sigdetaVsEta(0),
    _h_sigdphiVsPt(0),
    
    _h_curvPull(0),
    _h_ptPull(0),
     _h_tanPull(0),
  _h_curvPullVsPt(0),
  _h_ptPullVsPt(0),
  _h_tanPullVsPt(0),

  _h_chi2(0),       
  _h_rChi2(0),      
  _h_rChi2VsPt(0),  
  _h_rChi2VsEta(0), 
  _h_rChi2VsNpts(0)
{
  initialize();
}


ResolutionPlots::~ResolutionPlots() 
{}


void ResolutionPlots::initialize()
{
  cout << "ResolutionPlots::createHistograms " << getName()<< endl;
  
  _h_p_mc    = book("_h_p_mc",    "momentum of mc tracks",100,0., 10.);
  _h_pt_mc   = book("_h_pt_mc",   "transverse momentum of mc tracks", 100,0., 10.);
  _h_eta_mc  = book("_h_eta_mc",  "eta of mc tracks", 80, -2., 2.);
  _h_phi_mc  = book("_h_phi_mc",  "phi of mc tracks", 120, -180., 180.);
  
  _h_p_rec    = book("_h_p_rec",    "momentum of rec tracks",100,0., 10.);
  _h_pt_rec   = book("_h_pt_rec",   "transverse momentum of rec tracks", 100,0., 10.);
  _h_eta_rec  = book("_h_eta_rec",  "eta of rec tracks", 80, -2., 2.);
  _h_phi_rec  = book("_h_phi_rec",  "phi of rec tracks", 120, -180., 180.);
  
  _h_dpVsP     = book("_h_dpVsP",     "p diff vs p",  100,0., 10., 100, -0.5, 0.5);
  _h_dptVsP    = book("_h_dptVsP",    "pt diff vs p", 100,0., 10., 100, -0.5, 0.5);
  //_h_dptVsPt   = book("_h_dptVsPt"  , "pt diff vs pt", 100,0., 10., 100, -0.5, 0.5);
  _h_detaVsEta = book("_h_detaVsEta", "eta diff vs eta", 80, -2., 2., 100, -0.5, 0.5);
  _h_dphiVsPt  = book("_h_dphiVsPt",  "phi diff vs pt", 100,0., 10., 100, -0.5, 0.5);

  _h_dpByPVsP     = book("_h_dpByPVsP",    "p rel diff vs p",  100,0., 10., 100, -0.2, 0.2);
  //_h_dptByPtVsPt  = book("_h_dptByPtVsPt", "pt rel diff vs p", 100,0., 10., 100, -0.2, 0.2);
  _h_dptByPtVsP   = book("_h_dptByPtVsP",  "pt rel diff vs p", 100,0., 10., 100, -0.2, 0.2);

  _p_dpVsP     = bookProfile("_p_dpVsP",     "p diff vs p (prof)",  100,0., 10.);
  _p_dptVsP    = bookProfile("_p_dptVsP",    "pt diff vs p (prof)", 100,0., 10.);
  //_p_dptVsPt   = bookProfile("_p_dptVsPt"  , "pt diff vs pt (prof)", 100,0., 10.);
  _p_detaVsEta = bookProfile("_p_detaVsEta", "eta diff vs eta (prof)", 80, -2., 2.);
  _p_dphiVsPt  = bookProfile("_p_dphiVsPt",  "phi diff vs pt (prof)", 100,0., 10.);

  _p_dpByPVsP     = bookProfile("_p_dpByPVsP",    "p rel diff vs p (prof)",  100,0., 10.);
  //_p_dptByPtVsPt  = bookProfile("_p_dptByPtVsPt", "pt rel diff vs pt (prof)", 100,0., 10.);
  _p_dptByPtVsP   = bookProfile("_p_dptByPtVsP",  "pt rel diff vs p (prof)", 100,0., 10.);

  //_p_dpByP2VsP     = bookProfile("_p_dpByP2VsP",    "(p rel diff)^2 vs p (prof)",  100,0., 10.);
  //_p_dptByPt2VsPt  = bookProfile("_p_dptByPt2VsPt", "(pt rel diff)^2 vs pt (prof)", 100,0., 10.);
  //_p_dptByPt2VsP   = bookProfile("_p_dptByPt2VsP",  "(pt rel diff)^2 vs p (prof)", 100,0., 10.);
  //_p_deta2VsEta    = bookProfile("_p_deta2VsEta",   "(eta diff)^2 vs eta (prof)", 80, -2., 2.);
  //_p_dphi2VsPt     = bookProfile("_p_dphi2VsPt",    "(phi diff)^2 vs pt (prof)", 100,0., 10.);

  //_h_sigdpByPVsP     = book("_h_sigdpByPVsP",    "sig(p rel diff) vs p",  100,0., 10.);
  //_h_sigdptByPtVsPt  = book("_h_sigdptByPtVsPt", "sig(pt rel diff) vs pt", 100,0., 10.);
  //_h_sigdptByPtVsP   = book("_h_sigdptByPtVsP",  "sig(pt rel diff) vs p", 100,0., 10.);
  //_h_sigdetaVsEta    = book("_h_sigdetaVsEta",   "sig(eta) vs eta", 80, -2., 2.);
  //_h_sigdphiVsPt     = book("_h_sigdphiVsPt",    "sig(phi) vs pt", 100,0., 10.);

  _h_curvPull = book("curvPull","Curvature Pull",           250, -5., 5.);
  _h_ptPull   = book("ptPull",  "Transverse Momentum Pull", 250, -5., 5.);
  _h_tanPull  = book("tanPull", "Tan(Lambda) Pull",         250, -5., 5.);
  
  _h_curvPullVsPt = book("curvPullVsPt","Curvature Pull vs Pt",           25,0., 5., 250, -5., 5.);
  _h_ptPullVsPt   = book("ptPullVsPt",  "Transverse Momentum Pull vs Pt", 25,0., 5., 250, -5., 5.);
  _h_tanPullVsPt  = book("tanPullVsPt", "Tan(Lambda) Pull vs Pt",         25,0., 5., 250, -5., 5.);

  _h_chi2       = book("chi2","chi2",200,0., 200.);
  _h_rChi2      = book("rChi2","rChi2",200,0., 20.);
  _h_rChi2VsPt  = book("rChi2VsPt",  "rChi2 vs pt",   25, 0., 5., 200,0., 20.);
  _h_rChi2VsEta = book("rChi2VsEta", "rChi2 vs eta",  20, -2., 2., 200,0., 20.);
  _h_rChi2VsNpts= book("rChi2VsNpts","rChi2 vs npts", 50, 0., 50., 200,0., 20.);


}

void ResolutionPlots::fill(StMiniMcEvent* minimcevent) 
{
  //cout << "ResolutionPlots::fillHistograms(...) -I- Started Name:"<<getName()<<endl;
  int nMcTracks = minimcevent->nMcTrack();
  //cout << "No. MC Tracks " << nMcTracks <<endl;
  // mMcTracks loop
  for (int j=0; j<nMcTracks; ++j) 
    {
      StTinyMcTrack* tinymctrack = (StTinyMcTrack*) minimcevent->tracks(MC)->At(j);
      if (!tinymctrack) continue;
      int id = tinymctrack->geantId(); 
      // only look at pi-, k-, pbar = 9, 12, 15 and pi+, k+, prot = 8, 11, 14
      if (!acceptGeantId(id)) continue; 
      if (!acceptEtaCut(tinymctrack->etaMc())) continue;
      double p      = tinymctrack->pMc();
      double pt     = tinymctrack->ptMc();
      double px     = tinymctrack->pxMc();
      double py     = tinymctrack->pyMc();
      double pz     = tinymctrack->pzMc();
      double eta    = tinymctrack->etaMc();
      double phi    = (180./3.1415927)*tinymctrack->phiMc();
      int   nHits  = tinymctrack->nHitMc();
      if (nHits>=10)
	{
	  _h_p_mc->Fill(p);  
	  _h_pt_mc->Fill(pt); 
	  _h_eta_mc->Fill(eta);
	  _h_phi_mc->Fill(phi);
	}
    }
  
  double B = 0.49795819;
  double c = 0.299792458;
  int nMatchedPiKP=0;
  int nMatchedPairs = minimcevent->nMatchedPair();
  //cout << "# MatchedPairs  " << nMatchedPairs << ":" << minimcevent->tracks(MATCHED)->GetEntries() << endl;
  for (int k=0; k<nMatchedPairs; ++k) 
    {
      StMiniMcPair* minimcpair = (StMiniMcPair*) minimcevent->tracks(MATCHED)->At(k);
      if (!minimcpair) continue;
      int id = minimcpair->geantId();
      if (!acceptGeantId(id)) continue;
      if (!acceptPair(minimcpair)) continue; 
      double pt_mc    = minimcpair->ptMc();
      double px_mc    = minimcpair->pxMc();
      double py_mc    = minimcpair->pyMc();
      double pz_mc    = minimcpair->pzMc();
      double p_mc     = minimcpair->pMc();
      double eta_mc   = minimcpair->etaMc();
      double phi_mc   = (180./3.1415927)*minimcpair->phiMc();
      int    nHits    = minimcpair->nHitMc();
      double tan_mc   = pz_mc/pt_mc;
      double curv_mc  = B*c/100/pt_mc;

      double pt_rec, px_rec, py_rec, pz_rec, p_rec, eta_rec, phi_rec;
      double nFitPts, chi2;
      double tan_rec, curv_rec;
      double tanError, curvError, ptError;
      
      // _trackType == 0 primary
      //              1 global
      if (_trackType==0) // primary
	{
	  pt_rec   = minimcpair->ptPr();
	  px_rec   = minimcpair->pxPr();
	  py_rec   = minimcpair->pyPr();
	  pz_rec   = minimcpair->pzPr();
	  p_rec    = minimcpair->pPr();
	  eta_rec  = minimcpair->etaPr();
	  phi_rec  = (180./3.1415927)*minimcpair->phiPr();
	    
	  nFitPts  = minimcpair->fitPts(); 
	  chi2     = minimcpair->chi2Pr();

	  tan_rec   = minimcpair->tanLPr();
	  curv_rec  = minimcpair->curvPr();
	  // only for pull plots: if 0 then ITTF ; if 1 then TPT
	  if (_pullType==0) 
	    {
	      tanError  = sqrt(minimcpair->errPr(4));
	      curvError = sqrt(minimcpair->errPr(3));
	    }
	  else if (_pullType==1)
	    {
	      tanError  = sqrt(minimcpair->errPr(2));
	      curvError = B*c*sqrt(minimcpair->errPr(4))/100.;
	    } 
	}
      else // globals
	{
	  tan_rec     = minimcpair->tanLGl();
	  curv_rec    = minimcpair->curvGl();
	  // only for pull plots: if 0 then ITTF ; if 1 then TPT
	  if (_pullType==0) 
	    {
	      tanError = sqrt(minimcpair->errGl(4));
	      curvError= sqrt(minimcpair->errGl(3));
	    }
	  else if (_pullType==1)
	    {
	      tanError = sqrt(minimcpair->errGl(2));
	      curvError= B*c*sqrt(minimcpair->errGl(4))/100.;
	    }
	}
      ptError = curvError* pt_rec/curv_rec;

      double nFree= nFitPts - 5; 
      double rChi2;
      if (nFree>0)
	rChi2 = chi2/nFree;
      else
	rChi2 = -1.; // non sensical value indicates nFree==0
      double dp   = p_rec-p_mc;
      double dpt  = pt_rec-pt_mc;
      double deta = eta_rec-eta_mc;
      double dphi = phi_rec-phi_mc;
      double dcurv = curv_rec - curv_mc;
      double dtan  = tan_rec - tan_mc;

      _h_p_rec->Fill(p_rec);   
      _h_pt_rec->Fill(pt_rec); 
      _h_eta_rec->Fill(eta_rec);  
      _h_phi_rec->Fill(phi_rec);  
	
      _h_dpVsP->Fill(p_mc,       dp);   
      _h_dptVsP->Fill(p_mc,      dpt);  
      //_h_dptVsPt->Fill(pt_mc,    dpt);
      _h_detaVsEta->Fill(eta_mc, deta);  
      _h_dphiVsPt->Fill(pt_mc,   dphi);   
      _p_dpVsP->Fill(p_mc,dp);       
      _p_dptVsP->Fill(p_mc,dpt);      
      //_p_dptVsPt->Fill(pt_mc,dpt);     
      _p_detaVsEta->Fill(eta_mc,deta);   
      _p_dphiVsPt->Fill(phi_mc, dphi);    
	
      if (pt_mc>0)
	{
	  _h_dpByPVsP->Fill(p_mc,     dp/p_mc);   
	  //_h_dptByPtVsPt->Fill(pt_mc, dpt/pt_mc);  
	  _h_dptByPtVsP->Fill(p_mc,   dpt/pt_mc);   
	
	  _p_dpByPVsP->Fill(p_mc,dp/p_mc);    
	  //_p_dptByPtVsPt->Fill(pt_mc, dpt/pt_mc); 
	  _p_dptByPtVsP->Fill(p_mc, dpt/pt_mc);
	
	  //_p_dpByP2VsP->Fill(p_mc,dp*dp);    
	  //_p_dptByPt2VsPt->Fill(pt_mc, dpt*dpt/(pt_mc*pt_mc)); 
	  //_p_dptByPt2VsP->Fill(p_mc, dpt*dpt/(pt_mc*pt_mc));
	  //_p_deta2VsEta->Fill(eta_mc, deta*deta);
	  //_p_dphiVsPt->Fill(pt_mc, deta*deta);
	}

      if (curvError) 
	{
	  _h_curvPull->Fill( dcurv/curvError );
	  _h_curvPullVsPt->Fill( pt_mc, dcurv/curvError );
	}

      if (ptError)   
	{
	  _h_ptPull->Fill(  dpt/ptError );
	  _h_ptPullVsPt->Fill(pt_mc,  dpt/ptError );
	}
      if (tanError)  
	{
	  _h_tanPull->Fill( dtan/tanError );
	  _h_tanPullVsPt->Fill(pt_mc, dtan/tanError );
	}

      _h_chi2->Fill(chi2);       
      _h_rChi2->Fill(rChi2);    
      _h_rChi2VsPt->Fill(pt_mc,rChi2);  
      _h_rChi2VsEta->Fill(eta_mc,rChi2);  
      _h_rChi2VsNpts->Fill(nFitPts,rChi2); 
  
    }
}


void ResolutionPlots::finish()
{
  //calculateSTD(_p_dpByPVsP,   _p_dpByP2VsP,    _h_sigdpByPVsP);
  //calculateSTD(_p_dptByPtVsPt,_p_dptByPt2VsPt, _h_sigdptByPtVsPt);
  //calculateSTD(_p_dptByPtVsP, _p_dptByPt2VsP,  _h_sigdptByPtVsP);
  //calculateSTD(_p_detaVsEta,  _p_deta2VsEta,   _h_sigdetaVsEta);
  //calculateSTD(_p_dphiVsPt,   _p_dphi2VsPt,    _h_sigdphiVsPt);

  _p_dpVsP->SetMinimum(-1.);      _p_dpVsP->SetMaximum(1.);
  _p_dptVsP->SetMinimum(-1.);     _p_dptVsP->SetMinimum(1.);
  //_p_dptVsPt->SetMinimum(-1.);    _p_dptVsPt->SetMinimum(1.);
  _p_detaVsEta->SetMinimum(-0.2); _p_detaVsEta->SetMaximum(0.2);
  _p_dphiVsPt->SetMinimum(-1.);   _p_dphiVsPt->SetMaximum(1.);
  _p_dpByPVsP->SetMinimum(-0.1); _p_dpByPVsP->SetMaximum(0.1);
  //_p_dptByPtVsPt->SetMinimum(-0.1); _p_dptByPtVsPt->SetMaximum(0.1);
  _p_dptByPtVsP->SetMinimum(-0.1);  _p_dptByPtVsP->SetMaximum(0.1);

  //_h_sigdpByPVsP->SetMinimum(0.);    _h_sigdpByPVsP->SetMaximum(0.1);
  //_h_sigdptByPtVsPt->SetMinimum(0.); _h_sigdptByPtVsPt->SetMaximum(0.1);
  //_h_sigdptByPtVsP->SetMinimum(0.);  _h_sigdptByPtVsP->SetMaximum(0.1);
  //_h_sigdetaVsEta->SetMinimum(0.);   _h_sigdetaVsEta->SetMaximum(0.1);
  //_h_sigdphiVsPt->SetMinimum(0.);    _h_sigdphiVsPt->SetMaximum(0.1);

  //slice(_h_dptVsPt);   
  slice(_h_detaVsEta,-0.1, 0.1, 0., 0.1);
  slice(_h_dphiVsPt, -1., 1., 0., 2.);
  slice(_h_dptByPtVsP, -0.05, 0.05, 0., 0.05);
  _h_curvPull->Fit("gaus","","",-2.,2.);
  _h_ptPull->Fit("gaus","","",-2.,2.);
  _h_tanPull->Fit("gaus","","",-2.,2.);
  slice(_h_curvPullVsPt, -3., 3., 0., 5.);
  slice(_h_ptPullVsPt, -3., 3., 0., 5.);
  slice(_h_tanPullVsPt, -3., 3., 0., 5.);
}

