#include "Stiostream.h"
#include "StiEvaluator/EfficiencyPlots.h"
#include "StMiniMcEvent/StMiniMcEvent.h"
#include "StMiniMcEvent/StMiniMcPair.h"

ClassImp(EfficiencyPlots)
    
EfficiencyPlots::EfficiencyPlots(const char * name, 
				 const char * description, 
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
				 int     trackType)
  : StiEvaluatorHistograms(name, 
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
    _eventCount(0),
    _h_pt_mc(0),
    _h_eta_mc(0),
    _h_phi_mc(0),
    _h_ptVsEta_mc(0),
    _h_phiVsEta_mc(0),

    _h_pt_rec(0),
    _h_eta_rec(0),
    _h_phi_rec(0),
    _h_ptVsEta_rec(0),
    _h_phiVsEta_rec(0),

    _h_pt_ratio(0),
    _h_eta_ratio(0),
    _h_phi_ratio(0),
    _h_ptVsEta_ratio(0),
    _h_phiVsEta_ratio(0),

    _p_pt(0),
    _p_eta(0),
    _p_phi(0),
    _p_ptVsEta_mc(0),
    _p_phiVsEta_mc(0),

    _h_dca10(0),
    _h_dca20(0),
    _h_dca30(0),
    _h_dca40(0)
{
  initialize();
}


EfficiencyPlots::~EfficiencyPlots() 
{}


void EfficiencyPlots::initialize()
{
  cout << "EfficiencyPlots::createHistograms " << getName()<< endl;
  
  _h_pt_mc   = book("_h_pt_mc",   "transverse momentum of mc tracks", 30,0., 3.);
  _h_eta_mc  = book("_h_eta_mc",  "eta of mc tracks", 40, -2., 2.);
  _h_phi_mc  = book("_h_phi_mc",  "phi of mc tracks", 40, -180., 180.);
  //_h_ptVsEta_mc  = book("_h_ptVsEta_mc", "transverse momentum vs eta of mc tracks", 40, -2., 2.,  50, 0., 3.);
  //_h_phiVsEta_mc = book("_h_phiVsEta_mc","azituh angle(deg) vs eta of mc tracks",   40, -2., 2.,  40, -180., 180.);

  _h_pt_rec   = book("_h_pt_rec",   "transverse momentum of rec tracks", 30,0., 3.);
  _h_eta_rec  = book("_h_eta_rec",  "eta of rec tracks", 40, -2., 2.);
  _h_phi_rec  = book("_h_phi_rec",  "phi of rec tracks", 40, -180., 180.);
  //_h_ptVsEta_rec  = book("_h_ptVsEta_rec", "transverse momentum vs eta of rec tracks", 40, -2., 2.,  50,    0.,  3.);
  //_h_phiVsEta_rec = book("_h_phiVsEta_rec","azituh angle(deg) vs eta of rec tracks",   40, -2., 2.,  40, -180., 180.);

  _h_pt_ratio   = book("_h_pt_ratio",   "efficiency vs transverse momentum of rec tracks", 30,0., 3.);
  _h_eta_ratio  = book("_h_eta_ratio",  "efficiency vs eta of rec tracks", 40,   -2., 2.);
  _h_phi_ratio  = book("_h_phi_ratio",  "efficiency vs phi of rec tracks", 40, -180., 180.);
  //_h_ptVsEta_ratio = book("_h_ptVsEta_ratio", "efficiency vs transverse momentum vs eta of rec tracks",40,-2.,2., 50,0., 3.);
  //_h_phiVsEta_ratio= book("_h_phiVsEta_ratio","efficiency vs azimuth angle(deg) vs eta of rec tracks", 40,-2.,2., 40,-180., 180.);

  _h_dca10 = book("_h_dca10", "dca Npts>10", 100, 0., 10.);
  _h_dca20 = book("_h_dca20", "dca Npts>20", 100, 0., 10.);
  _h_dca30 = book("_h_dca30", "dca Npts>30", 100, 0., 10.);
  _h_dca40 = book("_h_dca40", "dca Npts>40", 100, 0., 10.);
  
}

void EfficiencyPlots::fill(StMiniMcEvent* minimcevent) 
{
  int nMcTracks = minimcevent->nMcTrack();
  if (nMcTracks<_minMult || nMcTracks>=_maxMult) return;
  ++_eventCount;
  for (int j=0; j<nMcTracks; ++j) 
    {
	StTinyMcTrack* tinymctrack = (StTinyMcTrack*) minimcevent->tracks(MC)->At(j);
	if (!tinymctrack) continue;
	int id = tinymctrack->geantId(); 
	// only look at pi-, k-, pbar = 9, 12, 15 and pi+, k+, prot = 8, 11, 14
	if (id==8 || id==9)
	  {
	    double p      = tinymctrack->pMc();	if(p){}
	    double pt     = tinymctrack->ptMc();
	    double eta    = tinymctrack->etaMc();
	    double phi    = (180./3.1415927)*tinymctrack->phiMc();
	    int    nHits  = tinymctrack->nHitMc();
	    if (nHits>=_minNHits)
	      {
		if (eta>=_minEta && eta<=_maxEta)
		  {
		    _h_pt_mc->Fill(pt); 
		    _h_phi_mc->Fill(phi);
		  }
		_h_eta_mc->Fill(eta);
		//_h_ptVsEta_mc->Fill(eta,pt);;
		//_h_phiVsEta_mc->Fill(eta,phi);;
	      }
	  }
    }
    int nMatchedPiKP=0;		if(nMatchedPiKP){}
    int nMatchedPairs = minimcevent->nMatchedPair();
    //cout << "# MatchedPairs  " << nMatchedPairs << ":" << minimcevent->tracks(MATCHED)->GetEntries() << endl;
    for (int k=0; k<nMatchedPairs; ++k) 
      {
	StMiniMcPair* minimcpair = (StMiniMcPair*) minimcevent->tracks(MATCHED)->At(k);
	if (!minimcpair) continue;
	int id = minimcpair->geantId();
	//if (!acceptGeantId(id)) continue;
	//if (!acceptPair(minimcpair)) continue; 
	if (id==8 || id==9)
	  {
	    double pt_mc    = minimcpair->ptMc();
	    double p_mc     = minimcpair->pMc();	if(p_mc){}
	    double eta_mc   = minimcpair->etaMc();
	    double phi_mc   = (180./3.1415927)*minimcpair->phiMc();
	    int nHits       = minimcpair->nHitMc();
	    
	    //double pt_rec   = minimcpair->ptPr();
	    //double p_rec    = minimcpair->pPr();
	    //double eta_rec  = minimcpair->etaPr();
	    //double phi_rec  = (180./3.1415927)*minimcpair->phiPr();
	    int hits        = minimcpair->nHitMc();
	    double dca_rec  = minimcpair->dcaGl();
	    
	    if (nHits>=_minNHits && hits>_minNFitHits && dca_rec<3.)
	      {
		if (eta_mc>=_minEta && eta_mc<=_maxEta)
		  {
		    _h_pt_rec->Fill(pt_mc); 
		    _h_phi_rec->Fill(phi_mc);  
		  }
		_h_eta_rec->Fill(eta_mc);  
		//_h_ptVsEta_rec->Fill(  eta_mc,pt_mc);;
		//_h_phiVsEta_rec->Fill( eta_mc,phi_mc);;
	      }
	    if (nHits>=_minNHits && hits>_minNFitHits)
	      {
		if (hits>=10) _h_dca10->Fill(dca_rec);
		if (hits>=20) _h_dca20->Fill(dca_rec);
		if (hits>=30) _h_dca30->Fill(dca_rec);
		if (hits>=40) _h_dca40->Fill(dca_rec);
	      }
	  }
      }
}

void EfficiencyPlots::finish()
{
  _h_pt_rec->Sumw2();
  _h_pt_mc->Sumw2();
  _h_eta_rec->Sumw2();
  _h_eta_mc->Sumw2();
  _h_phi_rec->Sumw2();
  _h_phi_mc->Sumw2();
  _h_pt_ratio->Divide(_h_pt_rec,_h_pt_mc,1.,1.,"b");
  _h_eta_ratio->Divide(_h_eta_rec,_h_eta_mc,1.,1.,"b");
  _h_phi_ratio->Divide(_h_phi_rec,_h_phi_mc,1.,1.,"b");

  _h_pt_ratio->SetMinimum(0.);  _h_pt_ratio->SetMaximum(1.1);
  _h_eta_ratio->SetMinimum(0.); _h_eta_ratio->SetMaximum(1.1);
  _h_phi_ratio->SetMinimum(0.); _h_phi_ratio->SetMaximum(1.1);

  if (_eventCount>0)
    {
      double scale = 1./_eventCount;
      _h_pt_rec->Scale(scale);
      _h_pt_mc->Scale(scale);
      _h_eta_rec->Scale(scale);
      _h_eta_mc->Scale(scale);
      _h_phi_rec->Scale(scale);
      _h_phi_mc->Scale(scale);
    }
  else
    {
      cout << "EfficiencyPlots::finish() -I- _eventCount = 0"<<endl;
    }

  //divide(_h_ptVsEta_rec,_h_ptVsEta_mc,_h_ptVsEta_ratio);
  //divide(_h_phiVsEta_rec,_h_phiVsEta_mc,_h_phiVsEta_ratio);
}

