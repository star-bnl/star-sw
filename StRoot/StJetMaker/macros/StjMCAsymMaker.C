#include "StMaker.h"


#include <StMuDSTMaker/COMMON/StMuDstMaker.h>
#include <StMuDSTMaker/COMMON/StMuDst.h>
#include <StMuDSTMaker/COMMON/StMuEvent.h>

#include <StTriggerUtilities/StTriggerSimuMaker.h>
#include <StTriggerUtilities/StTriggerSimuResult.h>
#include <StTriggerUtilities/Bemc/StBemcTriggerSimu.h>

#include <StSpinPool/StMCAsymMaker/StMCAsymMaker.h>

#include <TChain.h>
#include <TDirectory.h>

#include "StjMCMuDst.h"
#include "StjMCKinMuDst.h"
#include "StjMCParticleList.h"
#include "StjMCParticlePrint.h"

#include <libgen.h>

#include <string>
#include <cstring>

using namespace std;

class StjMCAsymMaker : public StMaker {

public:

  StjMCAsymMaker(const Char_t *name, TDirectory* file, StMuDstMaker* uDstMaker)
  : StMaker(name), _file(file), _uDstMaker(uDstMaker)
  { }

  virtual ~StjMCAsymMaker() { }

  const char* GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StjMCAsymMaker.C,v 1.3 2014/08/06 11:43:24 jeromel Exp $ built " __DATE__ " " __TIME__; return cvs;}

private:

  TDirectory* _file;
  TTree* _tree;

  StMuDstMaker* _uDstMaker;
  StjMCMuDst* _mc;

  Int_t _runNumber;
  Int_t _eventId;
  Double_t _vertexZ;

  Double_t _s;
  Double_t _t;
  Double_t _u;
  Double_t _pt;
  Double_t _costh;
  Double_t _x1;
  Double_t _x2;
  Int_t    _pid;

  Int_t    _pdg1;
  Int_t    _pdg2;
  Int_t    _pdg3;
  Int_t    _pdg4;

  Double_t _a_ll;

  Double_t _f1_cteq5m1;
  Double_t _f2_cteq5m1;
  Double_t _f1_cteq5l;
  Double_t _f2_cteq5l;

  Double_t _df1_gsa_nlo;
  Double_t _df2_gsa_nlo;
  Double_t _df1_gsb_nlo;
  Double_t _df2_gsb_nlo;
  Double_t _df1_gsc_nlo;
  Double_t _df2_gsc_nlo;

  Double_t _df1_grsv_lo;
  Double_t _df2_grsv_lo;
  Double_t _df1_grsv_nlo;
  Double_t _df2_grsv_nlo;
  Double_t _df1_grsv_g0;
  Double_t _df2_grsv_g0;
  Double_t _df1_grsv_gmax;
  Double_t _df2_grsv_gmax;
  Double_t _df1_grsv_gmin;
  Double_t _df2_grsv_gmin;

  Double_t _df1_grsv_m015;
  Double_t _df2_grsv_m015;
  Double_t _df1_grsv_m030;
  Double_t _df2_grsv_m030;
  Double_t _df1_grsv_m045;
  Double_t _df2_grsv_m045;
  Double_t _df1_grsv_m060;
  Double_t _df2_grsv_m060;
  Double_t _df1_grsv_m075;
  Double_t _df2_grsv_m075;
  Double_t _df1_grsv_m090;
  Double_t _df2_grsv_m090;
  Double_t _df1_grsv_m105;
  Double_t _df2_grsv_m105;
  Double_t _df1_grsv_p030;
  Double_t _df2_grsv_p030;
  Double_t _df1_grsv_p045;
  Double_t _df2_grsv_p045;
  Double_t _df1_grsv_p060;
  Double_t _df2_grsv_p060;
  Double_t _df1_grsv_p070;
  Double_t _df2_grsv_p070;

  Double_t _df1_dssv_nlo;
  Double_t _df2_dssv_nlo;

  StjMCParticle getMcParticleForId(const StjMCParticleList& list, int id)
  {
    for(StjMCParticleList::const_iterator it = list.begin(); it != list.end(); ++it)
      {
	if((*it).mcparticleId == id) return (*it);
      }
    return StjMCParticle();
  }

public:

  Int_t Init()
  {
    _mc = new StjMCMuDst(_uDstMaker);

    _file->cd();
    _tree = new TTree("mcasym", "mcasym");
    _tree->Branch("runNumber"  , &_runNumber  , "runNumber/I"  );
    _tree->Branch("eventId"    , &_eventId    , "eventId/I"    );
    _tree->Branch("vertexZ"    , &_vertexZ    , "vertexZ/D"    );    

    _tree->Branch("s"             , &_s                 , "s/D");		 
    _tree->Branch("t"             , &_t                 , "t/D");		 
    _tree->Branch("u"             , &_u                 , "u/D");		 
    _tree->Branch("pt"            , &_pt                , "pt/D");		 
    _tree->Branch("costh"         , &_costh             , "costh/D");	 
    _tree->Branch("x1"            , &_x1                , "x1/D");		 
    _tree->Branch("x2"            , &_x2                , "x2/D");		 
    _tree->Branch("pid"           , &_pid               , "pid/I");		 
    _tree->Branch("pdg1"          , &_pdg1              , "pdg1/I");	 
    _tree->Branch("pdg2"          , &_pdg2              , "pdg2/I");	 
    _tree->Branch("pdg3"          , &_pdg3              , "pdg3/I");	 
    _tree->Branch("pdg4"          , &_pdg4              , "pdg4/I");	 
    _tree->Branch("a_ll"          , &_a_ll              , "a_ll/D");	 
    _tree->Branch("f1_cteq5m1"    , &_f1_cteq5m1        , "f1_cteq5m1/D");	 
    _tree->Branch("f2_cteq5m1"    , &_f2_cteq5m1        , "f2_cteq5m1/D");	 
    _tree->Branch("f1_cteq5l"     , &_f1_cteq5l	        , "f1_cteq5l/D");	 
    _tree->Branch("f2_cteq5l"     , &_f2_cteq5l	        , "f2_cteq5l/D");	 
    _tree->Branch("df1_gsa_nlo"   , &_df1_gsa_nlo       , "df1_gsa_nlo/D");	 
    _tree->Branch("df2_gsa_nlo"   , &_df2_gsa_nlo       , "df2_gsa_nlo/D");	 
    _tree->Branch("df1_gsb_nlo"   , &_df1_gsb_nlo       , "df1_gsb_nlo/D");	 
    _tree->Branch("df2_gsb_nlo"   , &_df2_gsb_nlo       , "df2_gsb_nlo/D");	 
    _tree->Branch("df1_gsc_nlo"   , &_df1_gsc_nlo       , "df1_gsc_nlo/D");	 
    _tree->Branch("df2_gsc_nlo"   , &_df2_gsc_nlo       , "df2_gsc_nlo/D");	 
    _tree->Branch("df1_grsv_lo"   , &_df1_grsv_lo       , "df1_grsv_lo/D");	 
    _tree->Branch("df2_grsv_lo"   , &_df2_grsv_lo       , "df2_grsv_lo/D");	 
    _tree->Branch("df1_grsv_nlo"  , &_df1_grsv_nlo      , "df1_grsv_nlo/D"); 
    _tree->Branch("df2_grsv_nlo"  , &_df2_grsv_nlo      , "df2_grsv_nlo/D"); 
    _tree->Branch("df1_grsv_g0"   , &_df1_grsv_g0       , "df1_grsv_g0/D");	 
    _tree->Branch("df2_grsv_g0"   , &_df2_grsv_g0       , "df2_grsv_g0/D");	 
    _tree->Branch("df1_grsv_gmax" , &_df1_grsv_gmax     , "df1_grsv_gmax/D");
    _tree->Branch("df2_grsv_gmax" , &_df2_grsv_gmax     , "df2_grsv_gmax/D");
    _tree->Branch("df1_grsv_gmin" , &_df1_grsv_gmin     , "df1_grsv_gmin/D");
    _tree->Branch("df2_grsv_gmin" , &_df2_grsv_gmin     , "df2_grsv_gmin/D");
    _tree->Branch("df1_grsv_m015" , &_df1_grsv_m015     , "df1_grsv_m015/D");
    _tree->Branch("df2_grsv_m015" , &_df2_grsv_m015     , "df2_grsv_m015/D");
    _tree->Branch("df1_grsv_m030" , &_df1_grsv_m030     , "df1_grsv_m030/D");
    _tree->Branch("df2_grsv_m030" , &_df2_grsv_m030     , "df2_grsv_m030/D");
    _tree->Branch("df1_grsv_m045" , &_df1_grsv_m045     , "df1_grsv_m045/D");
    _tree->Branch("df2_grsv_m045" , &_df2_grsv_m045     , "df2_grsv_m045/D");
    _tree->Branch("df1_grsv_m060" , &_df1_grsv_m060     , "df1_grsv_m060/D");
    _tree->Branch("df2_grsv_m060" , &_df2_grsv_m060     , "df2_grsv_m060/D");
    _tree->Branch("df1_grsv_m075" , &_df1_grsv_m075     , "df1_grsv_m075/D");
    _tree->Branch("df2_grsv_m075" , &_df2_grsv_m075     , "df2_grsv_m075/D");
    _tree->Branch("df1_grsv_m090" , &_df1_grsv_m090     , "df1_grsv_m090/D");
    _tree->Branch("df2_grsv_m090" , &_df2_grsv_m090     , "df2_grsv_m090/D");
    _tree->Branch("df1_grsv_m105" , &_df1_grsv_m105     , "df1_grsv_m105/D");
    _tree->Branch("df2_grsv_m105" , &_df2_grsv_m105     , "df2_grsv_m105/D");
    _tree->Branch("df1_grsv_p030" , &_df1_grsv_p030     , "df1_grsv_p030/D");
    _tree->Branch("df2_grsv_p030" , &_df2_grsv_p030     , "df2_grsv_p030/D");
    _tree->Branch("df1_grsv_p045" , &_df1_grsv_p045     , "df1_grsv_p045/D");
    _tree->Branch("df2_grsv_p045" , &_df2_grsv_p045     , "df2_grsv_p045/D");
    _tree->Branch("df1_grsv_p060" , &_df1_grsv_p060     , "df1_grsv_p060/D");
    _tree->Branch("df2_grsv_p060" , &_df2_grsv_p060     , "df2_grsv_p060/D");
    _tree->Branch("df1_grsv_p070" , &_df1_grsv_p070     , "df1_grsv_p070/D");
    _tree->Branch("df2_grsv_p070" , &_df2_grsv_p070     , "df2_grsv_p070/D");
    _tree->Branch("df1_dssv_nlo"  , &_df1_dssv_nlo      , "df1_dssv_nlo/D"); 
    _tree->Branch("df2_dssv_nlo"  , &_df2_dssv_nlo      , "df2_dssv_nlo/D"); 

    return kStOk;
  }

  Int_t Make()
  {
    _runNumber = _uDstMaker->muDst()->event()->runId();
    _eventId = _uDstMaker->muDst()->event()->eventId();
    _vertexZ = _uDstMaker->muDst()->event()->primaryVertexPosition().z();

    StTriggerSimuMaker* trigSimu = dynamic_cast<StTriggerSimuMaker*>(GetMaker("StarTrigSimu"));
    StTriggerSimuResult trigResult = trigSimu->detailedResult(117001);

    StMCAsymMaker *asym =  dynamic_cast<StMCAsymMaker*>(GetMaker("MCAsym"));

    StjMCKinMuDst* mckin = new StjMCKinMuDst(_uDstMaker);
    _s = mckin->s();
    _t = mckin->t();
    _u = mckin->u();
    _pt = mckin->pt();
    _costh = mckin->costh();
    _x1 = mckin->x1();  
    _x2 = mckin->x2();
    _pid = mckin->pid();


    StjMCParticleList list = _mc->getMCParticleList();
    StjMCParticle part1 = getMcParticleForId(list, 5);
    StjMCParticle part2 = getMcParticleForId(list, 6);
    StjMCParticle part3 = getMcParticleForId(list, 7);
    StjMCParticle part4 = getMcParticleForId(list, 8);

    _pdg1 = part1.pdg;
    _pdg2 = part2.pdg;
    _pdg3 = part3.pdg;
    _pdg4 = part4.pdg;

    _a_ll = asym->getPartonicALL(_s, _t, _u, _pid, _pdg1, _pdg2, _pdg3, _pdg4);

    Double_t Q2 = _pt*_pt;

    _f1_cteq5m1 = asym->get_unpolPDF_NLO(_pdg1, _x1, Q2);
    _f2_cteq5m1 = asym->get_unpolPDF_NLO(_pdg2, _x2, Q2);
    _f1_cteq5l = asym->get_unpolPDF_LO(_pdg1, _x1, Q2);
    _f2_cteq5l = asym->get_unpolPDF_LO(_pdg2, _x2, Q2);

    _df1_gsa_nlo = asym->get_polPDF_NLO_GSA(_pdg1, _x1, Q2);
    _df2_gsa_nlo = asym->get_polPDF_NLO_GSA(_pdg2, _x2, Q2);
    _df1_gsb_nlo = asym->get_polPDF_NLO_GSB(_pdg1, _x1, Q2);
    _df2_gsb_nlo = asym->get_polPDF_NLO_GSB(_pdg2, _x2, Q2);
    _df1_gsc_nlo = asym->get_polPDF_NLO_GSC(_pdg1, _x1, Q2);
    _df2_gsc_nlo = asym->get_polPDF_NLO_GSC(_pdg2, _x2, Q2);

    _df1_grsv_lo = asym->get_polPDF_LO(_pdg1, _x1, Q2);
    _df2_grsv_lo = asym->get_polPDF_LO(_pdg2, _x2, Q2);
    _df1_grsv_nlo = asym->get_polPDF_NLO(_pdg1, _x1, Q2);
    _df2_grsv_nlo = asym->get_polPDF_NLO(_pdg2, _x2, Q2);
    _df1_grsv_g0 = asym->get_polPDF_NLO_g0(_pdg1, _x1, Q2);
    _df2_grsv_g0 = asym->get_polPDF_NLO_g0(_pdg2, _x2, Q2);
    _df1_grsv_gmax = asym->get_polPDF_NLO_gmax(_pdg1, _x1, Q2);
    _df2_grsv_gmax = asym->get_polPDF_NLO_gmax(_pdg2, _x2, Q2);
    _df1_grsv_gmin = asym->get_polPDF_NLO_gmin(_pdg1, _x1, Q2);
    _df2_grsv_gmin = asym->get_polPDF_NLO_gmin(_pdg2, _x2, Q2);

    _df1_grsv_m015 = asym->get_polPDF_NLO_m015(_pdg1, _x1, Q2);
    _df2_grsv_m015 = asym->get_polPDF_NLO_m015(_pdg2, _x2, Q2);
    _df1_grsv_m030 = asym->get_polPDF_NLO_m030(_pdg1, _x1, Q2);
    _df2_grsv_m030 = asym->get_polPDF_NLO_m030(_pdg2, _x2, Q2);
    _df1_grsv_m045 = asym->get_polPDF_NLO_m045(_pdg1, _x1, Q2);
    _df2_grsv_m045 = asym->get_polPDF_NLO_m045(_pdg2, _x2, Q2);
    _df1_grsv_m060 = asym->get_polPDF_NLO_m060(_pdg1, _x1, Q2);
    _df2_grsv_m060 = asym->get_polPDF_NLO_m060(_pdg2, _x2, Q2);
    _df1_grsv_m075 = asym->get_polPDF_NLO_m075(_pdg1, _x1, Q2);
    _df2_grsv_m075 = asym->get_polPDF_NLO_m075(_pdg2, _x2, Q2);
    _df1_grsv_m090 = asym->get_polPDF_NLO_m090(_pdg1, _x1, Q2);
    _df2_grsv_m090 = asym->get_polPDF_NLO_m090(_pdg2, _x2, Q2);
    _df1_grsv_m105 = asym->get_polPDF_NLO_m105(_pdg1, _x1, Q2);
    _df2_grsv_m105 = asym->get_polPDF_NLO_m105(_pdg2, _x2, Q2);
    _df1_grsv_p030 = asym->get_polPDF_NLO_p030(_pdg1, _x1, Q2);
    _df2_grsv_p030 = asym->get_polPDF_NLO_p030(_pdg2, _x2, Q2);
    _df1_grsv_p045 = asym->get_polPDF_NLO_p045(_pdg1, _x1, Q2);
    _df2_grsv_p045 = asym->get_polPDF_NLO_p045(_pdg2, _x2, Q2);
    _df1_grsv_p060 = asym->get_polPDF_NLO_p060(_pdg1, _x1, Q2);
    _df2_grsv_p060 = asym->get_polPDF_NLO_p060(_pdg2, _x2, Q2);
    _df1_grsv_p070 = asym->get_polPDF_NLO_p070(_pdg1, _x1, Q2);
    _df2_grsv_p070 = asym->get_polPDF_NLO_p070(_pdg2, _x2, Q2);

    _df1_dssv_nlo = asym->get_polPDF_NLO_DSSV(_pdg1, _x1, Q2);
    _df2_dssv_nlo = asym->get_polPDF_NLO_DSSV(_pdg2, _x2, Q2);

    _tree->Fill();
    return kStOk;
  }

  Int_t Finish()
  {
    _tree->BuildIndex("runNumber", "eventId");
    return kStOk;
  }
  ClassDef(StjMCAsymMaker, 0)
};
