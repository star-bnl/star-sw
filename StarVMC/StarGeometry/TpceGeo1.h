#ifndef __TpceGeo1__ 
#define __TpceGeo1__ 
 
#include "StarVMC/StarAgmlLib/AgModule.h" 
 
namespace TPCEGEO1 // $NMSPC 
{ 
   class Tpcg_t : public AgStructure 
   { 
      ClassDef(Tpcg_t,1); 
      public: 
      Float_t version; 
      Float_t rmin; 
      Float_t rmax; 
      Float_t length; 
      Float_t wheelir; 
      Float_t wheelor; 
      Float_t wheelthk; 
      Float_t sengasor; 
      Float_t tpeathk; 
      Float_t membthk; 
      Float_t tiaddr; 
      Float_t tinxdr; 
      Float_t tikadr; 
      Float_t tialdr; 
      Float_t tocsdr; 
      Float_t tokadr; 
      Float_t tonxdr; 
      Float_t toaddr; 
      Float_t toigdr; 
      Float_t toaldr; 
      Float_t tohadr; 
      Float_t mwcread; 
      Float_t gascorr; 
      Tpcg_t() : AgStructure("Tpcg_t","User-defined AgML structure") 
      { 
         version=0; 
         rmin=0; 
         rmax=0; 
         length=0; 
         wheelir=0; 
         wheelor=0; 
         wheelthk=0; 
         sengasor=0; 
         tpeathk=0; 
         membthk=0; 
         tiaddr=0; 
         tinxdr=0; 
         tikadr=0; 
         tialdr=0; 
         tocsdr=0; 
         tokadr=0; 
         tonxdr=0; 
         toaddr=0; 
         toigdr=0; 
         toaldr=0; 
         tohadr=0; 
         mwcread=0; 
         gascorr=0; 
         _index=0; 
      } 
      ~ Tpcg_t(){ /* nada */ }; 
   }; 
   class Tecw_t : public AgStructure 
   { 
      ClassDef(Tecw_t,1); 
      public: 
      Float_t sec; 
      Float_t gaprad; 
      Float_t gapheit; 
      Float_t gapwidi; 
      Float_t gapwido; 
      Float_t gapshft; 
      Float_t inwidth; 
      Float_t ouwidth; 
      Float_t height; 
      Float_t ppdepth; 
      Float_t asdepth; 
      Float_t ggdepth; 
      Float_t mwcdepth; 
      Float_t boundary; 
      Float_t rcenter; 
      Float_t mwcinn; 
      Float_t mwcout; 
      Float_t mwchei; 
      Float_t mwccent; 
      Float_t mwcnwir; 
      Float_t n; 
      Float_t nex; 
      Array_t<Float_t> z; 
      Array_t<Float_t> dz; 
      Array_t<Float_t> xex; 
      Array_t<Float_t> zex; 
      Array_t<Float_t> dxex; 
      Array_t<Float_t> dzex; 
      Array_t<Float_t> nhplane; 
      Float_t cardw; 
      Float_t cardth; 
      Float_t coolw; 
      Float_t coolth; 
      Float_t cardoff; 
      Float_t cooloff; 
      Float_t slotw; 
      Float_t slotrad; 
      Float_t pipethk; 
      Float_t pipeht; 
      Float_t manithk; 
      Float_t maniwid; 
      Float_t tan15; 
      Float_t clearance; 
      Float_t whlipthk; 
      Float_t whlipwid; 
      Float_t whblklen; 
      Float_t whblkpos; 
      Float_t whblkin; 
      Tecw_t() : AgStructure("Tecw_t","User-defined AgML structure") 
      { 
         sec=0; 
         gaprad=0; 
         gapheit=0; 
         gapwidi=0; 
         gapwido=0; 
         gapshft=0; 
         inwidth=0; 
         ouwidth=0; 
         height=0; 
         ppdepth=0; 
         asdepth=0; 
         ggdepth=0; 
         mwcdepth=0; 
         boundary=0; 
         rcenter=0; 
         mwcinn=0; 
         mwcout=0; 
         mwchei=0; 
         mwccent=0; 
         mwcnwir=0; 
         n=0; 
         nex=0; 
         z = Array_t<Float_t>(8); 
         dz = Array_t<Float_t>(8); 
         xex = Array_t<Float_t>(5); 
         zex = Array_t<Float_t>(5); 
         dxex = Array_t<Float_t>(5); 
         dzex = Array_t<Float_t>(5); 
         nhplane = Array_t<Float_t>(8); 
         cardw=0; 
         cardth=0; 
         coolw=0; 
         coolth=0; 
         cardoff=0; 
         cooloff=0; 
         slotw=0; 
         slotrad=0; 
         pipethk=0; 
         pipeht=0; 
         manithk=0; 
         maniwid=0; 
         tan15=0; 
         clearance=0; 
         whlipthk=0; 
         whlipwid=0; 
         whblklen=0; 
         whblkpos=0; 
         whblkin=0; 
         _index=0; 
      } 
      ~ Tecw_t(){ /* nada */ }; 
   }; 
   class Trov_t : public AgStructure 
   { 
      ClassDef(Trov_t,1); 
      public: 
      Float_t sec; 
      Array_t<Float_t> nhp; 
      Array_t<Float_t> hx; 
      Float_t offcardinout; 
      Trov_t() : AgStructure("Trov_t","User-defined AgML structure") 
      { 
         sec=0; 
         nhp = Array_t<Float_t>(16); 
         hx = Array_t<Float_t>(9,16); 
         offcardinout=0; 
         _index=0; 
      } 
      ~ Trov_t(){ /* nada */ }; 
   }; 
   class Tprs_t : public AgStructure 
   { 
      ClassDef(Tprs_t,1); 
      public: 
      Float_t sec; 
      Float_t nrow; 
      Float_t pitch; 
      Float_t width; 
      Float_t super; 
      Array_t<Float_t> rpads; 
      Array_t<Float_t> npads; 
      Tprs_t() : AgStructure("Tprs_t","User-defined AgML structure") 
      { 
         sec=0; 
         nrow=0; 
         pitch=0; 
         width=0; 
         super=0; 
         rpads = Array_t<Float_t>(40); 
         npads = Array_t<Float_t>(40); 
         _index=0; 
      } 
      ~ Tprs_t(){ /* nada */ }; 
   }; 
   class Trdo_t : public AgStructure 
   { 
      ClassDef(Trdo_t,1); 
      public: 
      Float_t rdovthk; 
      Float_t rdothk; 
      Float_t rdolen; 
      Float_t nrdobrd; 
      Array_t<Float_t> rdoht; 
      Trdo_t() : AgStructure("Trdo_t","User-defined AgML structure") 
      { 
         rdovthk=0; 
         rdothk=0; 
         rdolen=0; 
         nrdobrd=0; 
         rdoht = Array_t<Float_t>(9); 
         _index=0; 
      } 
      ~ Trdo_t(){ /* nada */ }; 
   }; 
   // ---------------------------------------------------------------------- TPCE -- 
   ///@defgroup TPCE_doc 
   ///@class TPCE 
   ///@brief is the TPC envelope 
   class TPCE : public AgBlock 
   {  public: 
      TPCE() : AgBlock("TPCE","is the TPC envelope"){ }; 
      ~TPCE(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TPCE,1); 
   }; 
   // ---------------------------------------------------------------------- TPGV -- 
   ///@defgroup TPGV_doc 
   ///@class TPGV 
   ///@brief is the Gas Volume placed in TPC 
   class TPGV : public AgBlock 
   {  public: 
      TPGV() : AgBlock("TPGV","is the Gas Volume placed in TPC"){ }; 
      ~TPGV(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TPGV,1); 
   }; 
   // ---------------------------------------------------------------------- TPSS -- 
   ///@defgroup TPSS_doc 
   ///@class TPSS 
   ///@brief is a division of gas volume corresponding to a supersectors 
   class TPSS : public AgBlock 
   {  public: 
      TPSS() : AgBlock("TPSS","is a division of gas volume corresponding to a supersectors"){ }; 
      ~TPSS(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TPSS,1); 
   }; 
   // ---------------------------------------------------------------------- TPAD -- 
   ///@defgroup TPAD_doc 
   ///@class TPAD 
   ///@brief is a real padrow with dimensions defined at positioning time 
   class TPAD : public AgBlock 
   {  public: 
      TPAD() : AgBlock("TPAD","is a real padrow with dimensions defined at positioning time"){ }; 
      ~TPAD(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TPAD,1); 
   }; 
   // ---------------------------------------------------------------------- TIFC -- 
   ///@defgroup TIFC_doc 
   ///@class TIFC 
   ///@brief defines the Inner Field Cage placed in TPC 
   class TIFC : public AgBlock 
   {  public: 
      TIFC() : AgBlock("TIFC","defines the Inner Field Cage placed in TPC"){ }; 
      ~TIFC(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TIFC,1); 
   }; 
   // ---------------------------------------------------------------------- TINX -- 
   ///@defgroup TINX_doc 
   ///@class TINX 
   ///@brief is the inner nomex structure 
   class TINX : public AgBlock 
   {  public: 
      TINX() : AgBlock("TINX","is the inner nomex structure"){ }; 
      ~TINX(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TINX,1); 
   }; 
   // ---------------------------------------------------------------------- TIKA -- 
   ///@defgroup TIKA_doc 
   ///@class TIKA 
   ///@brief is the kapton film of the inner field cage 
   class TIKA : public AgBlock 
   {  public: 
      TIKA() : AgBlock("TIKA","is the kapton film of the inner field cage"){ }; 
      ~TIKA(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TIKA,1); 
   }; 
   // ---------------------------------------------------------------------- TIAL -- 
   ///@defgroup TIAL_doc 
   ///@class TIAL 
   ///@brief is the inner Aluminum cylinder 
   class TIAL : public AgBlock 
   {  public: 
      TIAL() : AgBlock("TIAL","is the inner Aluminum cylinder"){ }; 
      ~TIAL(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TIAL,1); 
   }; 
   // ---------------------------------------------------------------------- TOFC -- 
   ///@defgroup TOFC_doc 
   ///@class TOFC 
   ///@brief defines outer field cage - fill it with insulating gas already 
   class TOFC : public AgBlock 
   {  public: 
      TOFC() : AgBlock("TOFC","defines outer field cage - fill it with insulating gas already"){ }; 
      ~TOFC(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TOFC,1); 
   }; 
   // ---------------------------------------------------------------------- TOFS -- 
   ///@defgroup TOFS_doc 
   ///@class TOFS 
   ///@brief is the Outer Field Cage structure 
   class TOFS : public AgBlock 
   {  public: 
      TOFS() : AgBlock("TOFS","is the Outer Field Cage structure"){ }; 
      ~TOFS(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TOFS,1); 
   }; 
   // ---------------------------------------------------------------------- TOKA -- 
   ///@defgroup TOKA_doc 
   ///@class TOKA 
   ///@brief is KAPTON layer 
   class TOKA : public AgBlock 
   {  public: 
      TOKA() : AgBlock("TOKA","is KAPTON layer"){ }; 
      ~TOKA(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TOKA,1); 
   }; 
   // ---------------------------------------------------------------------- TONX -- 
   ///@defgroup TONX_doc 
   ///@class TONX 
   ///@brief is Nomex support 
   class TONX : public AgBlock 
   {  public: 
      TONX() : AgBlock("TONX","is Nomex support"){ }; 
      ~TONX(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TONX,1); 
   }; 
   // ---------------------------------------------------------------------- TOAD -- 
   ///@defgroup TOAD_doc 
   ///@class TOAD 
   ///@brief is Adhesive layer 
   class TOAD : public AgBlock 
   {  public: 
      TOAD() : AgBlock("TOAD","is Adhesive layer"){ }; 
      ~TOAD(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TOAD,1); 
   }; 
   // ---------------------------------------------------------------------- TOST -- 
   ///@defgroup TOST_doc 
   ///@class TOST 
   ///@brief is the Outer Field Cage Support 
   class TOST : public AgBlock 
   {  public: 
      TOST() : AgBlock("TOST","is the Outer Field Cage Support"){ }; 
      ~TOST(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TOST,1); 
   }; 
   // ---------------------------------------------------------------------- TOHA -- 
   ///@defgroup TOHA_doc 
   ///@class TOHA 
   ///@brief is Honeycomb/Adhesive mixture 
   class TOHA : public AgBlock 
   {  public: 
      TOHA() : AgBlock("TOHA","is Honeycomb/Adhesive mixture"){ }; 
      ~TOHA(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TOHA,1); 
   }; 
   // ---------------------------------------------------------------------- TPCM -- 
   ///@defgroup TPCM_doc 
   ///@class TPCM 
   ///@brief is the Central Membrane placed in TPC 
   class TPCM : public AgBlock 
   {  public: 
      TPCM() : AgBlock("TPCM","is the Central Membrane placed in TPC"){ }; 
      ~TPCM(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TPCM,1); 
   }; 
   // ---------------------------------------------------------------------- TPEA -- 
   ///@defgroup TPEA_doc 
   ///@class TPEA 
   ///@brief is one endcap placed in TPC 
   class TPEA : public AgBlock 
   {  public: 
      TPEA() : AgBlock("TPEA","is one endcap placed in TPC"){ }; 
      ~TPEA(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TPEA,1); 
   }; 
   // ---------------------------------------------------------------------- TESS -- 
   ///@defgroup TESS_doc 
   ///@class TESS 
   ///@brief is a division of endcap volume corresponding to one supersector 
   class TESS : public AgBlock 
   {  public: 
      TESS() : AgBlock("TESS","is a division of endcap volume corresponding to one supersector"){ }; 
      ~TESS(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TESS,1); 
   }; 
   // ---------------------------------------------------------------------- TSEC -- 
   ///@defgroup TSEC_doc 
   ///@class TSEC 
   ///@brief is a supersector containing Al sector, PCB and MWC volume 
   class TSEC : public AgBlock 
   {  public: 
      TSEC() : AgBlock("TSEC","is a supersector containing Al sector, PCB and MWC volume"){ }; 
      ~TSEC(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TSEC,1); 
   }; 
   // ---------------------------------------------------------------------- TMWC -- 
   ///@defgroup TMWC_doc 
   ///@class TMWC 
   ///@brief is a wire chamber volume with both gated and sensitive volumes 
   class TMWC : public AgBlock 
   {  public: 
      TMWC() : AgBlock("TMWC","is a wire chamber volume with both gated and sensitive volumes"){ }; 
      ~TMWC(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TMWC,1); 
   }; 
   // ---------------------------------------------------------------------- TMEA -- 
   ///@defgroup TMEA_doc 
   ///@class TMEA 
   ///@brief is a double sensitive layer around gating grid 
   class TMEA : public AgBlock 
   {  public: 
      TMEA() : AgBlock("TMEA","is a double sensitive layer around gating grid"){ }; 
      ~TMEA(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TMEA,1); 
   }; 
   // ---------------------------------------------------------------------- TMSE -- 
   ///@defgroup TMSE_doc 
   ///@class TMSE 
   ///@brief is a single sensitive volume 
   class TMSE : public AgBlock 
   {  public: 
      TMSE() : AgBlock("TMSE","is a single sensitive volume"){ }; 
      ~TMSE(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TMSE,1); 
   }; 
   // ---------------------------------------------------------------------- THOL -- 
   ///@defgroup THOL_doc 
   ///@class THOL 
   ///@brief is part of a hole for PC boards 
   class THOL : public AgBlock 
   {  public: 
      THOL() : AgBlock("THOL","is part of a hole for PC boards"){ }; 
      ~THOL(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(THOL,1); 
   }; 
   // ---------------------------------------------------------------------- THRA -- 
   ///@defgroup THRA_doc 
   ///@class THRA 
   ///@brief is part of a hole for PC boards 
   class THRA : public AgBlock 
   {  public: 
      THRA() : AgBlock("THRA","is part of a hole for PC boards"){ }; 
      ~THRA(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(THRA,1); 
   }; 
   // ---------------------------------------------------------------------- THLA -- 
   ///@defgroup THLA_doc 
   ///@class THLA 
   ///@brief is part of a hole for PC boards 
   class THLA : public AgBlock 
   {  public: 
      THLA() : AgBlock("THLA","is part of a hole for PC boards"){ }; 
      ~THLA(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(THLA,1); 
   }; 
   // ---------------------------------------------------------------------- TIAG -- 
   ///@defgroup TIAG_doc 
   ///@class TIAG 
   ///@brief is an air gap in inner sector aluminum structure 
   class TIAG : public AgBlock 
   {  public: 
      TIAG() : AgBlock("TIAG","is an air gap in inner sector aluminum structure"){ }; 
      ~TIAG(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TIAG,1); 
   }; 
   // ---------------------------------------------------------------------- TOAE -- 
   ///@defgroup TOAE_doc 
   ///@class TOAE 
   ///@brief is extra aluminum supports in the air opennings 
   class TOAE : public AgBlock 
   {  public: 
      TOAE() : AgBlock("TOAE","is extra aluminum supports in the air opennings"){ }; 
      ~TOAE(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TOAE,1); 
   }; 
   // ---------------------------------------------------------------------- TCEX -- 
   ///@defgroup TCEX_doc 
   ///@class TCEX 
   ///@brief is part of the G10 for the PC boards 
   class TCEX : public AgBlock 
   {  public: 
      TCEX() : AgBlock("TCEX","is part of the G10 for the PC boards"){ }; 
      ~TCEX(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TCEX,1); 
   }; 
   // ---------------------------------------------------------------------- TAEC -- 
   ///@defgroup TAEC_doc 
   ///@class TAEC 
   ///@brief is part of the heat shield for the PC boards 
   class TAEC : public AgBlock 
   {  public: 
      TAEC() : AgBlock("TAEC","is part of the heat shield for the PC boards"){ }; 
      ~TAEC(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TAEC,1); 
   }; 
   // ---------------------------------------------------------------------- TPCW -- 
   ///@defgroup TPCW_doc 
   ///@class TPCW 
   ///@brief is the TPC supporting endcap Wheel 
   class TPCW : public AgBlock 
   {  public: 
      TPCW() : AgBlock("TPCW","is the TPC supporting endcap Wheel"){ }; 
      ~TPCW(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TPCW,1); 
   }; 
   // ---------------------------------------------------------------------- TWSS -- 
   ///@defgroup TWSS_doc 
   ///@class TWSS 
   ///@brief is a division of wheel corresponding to supersectors 
   class TWSS : public AgBlock 
   {  public: 
      TWSS() : AgBlock("TWSS","is a division of wheel corresponding to supersectors"){ }; 
      ~TWSS(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TWSS,1); 
   }; 
   // ---------------------------------------------------------------------- TWGI -- 
   ///@defgroup TWGI_doc 
   ///@class TWGI 
   ///@brief is the Inner air gap in Wheel 
   class TWGI : public AgBlock 
   {  public: 
      TWGI() : AgBlock("TWGI","is the Inner air gap in Wheel"){ }; 
      ~TWGI(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TWGI,1); 
   }; 
   // ---------------------------------------------------------------------- TWGC -- 
   ///@defgroup TWGC_doc 
   ///@class TWGC 
   ///@brief is the larger Inner air gap in Wheel 
   class TWGC : public AgBlock 
   {  public: 
      TWGC() : AgBlock("TWGC","is the larger Inner air gap in Wheel"){ }; 
      ~TWGC(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TWGC,1); 
   }; 
   // ---------------------------------------------------------------------- TWGB -- 
   ///@defgroup TWGB_doc 
   ///@class TWGB 
   ///@brief is added alum blocksin Wheel 
   class TWGB : public AgBlock 
   {  public: 
      TWGB() : AgBlock("TWGB","is added alum blocksin Wheel"){ }; 
      ~TWGB(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TWGB,1); 
   }; 
   // ---------------------------------------------------------------------- TMAN -- 
   ///@defgroup TMAN_doc 
   ///@class TMAN 
   ///@brief is aluminum water manifold 
   class TMAN : public AgBlock 
   {  public: 
      TMAN() : AgBlock("TMAN","is aluminum water manifold"){ }; 
      ~TMAN(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TMAN,1); 
   }; 
   // ---------------------------------------------------------------------- TPIP -- 
   ///@defgroup TPIP_doc 
   ///@class TPIP 
   ///@brief is a water pipe there are lots per sect 
   class TPIP : public AgBlock 
   {  public: 
      TPIP() : AgBlock("TPIP","is a water pipe there are lots per sect"){ }; 
      ~TPIP(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TPIP,1); 
   }; 
   // ---------------------------------------------------------------------- TCRX -- 
   ///@defgroup TCRX_doc 
   ///@class TCRX 
   ///@brief is part of the G10 for the PC boards 
   class TCRX : public AgBlock 
   {  public: 
      TCRX() : AgBlock("TCRX","is part of the G10 for the PC boards"){ }; 
      ~TCRX(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TCRX,1); 
   }; 
   // ---------------------------------------------------------------------- TALC -- 
   ///@defgroup TALC_doc 
   ///@class TALC 
   ///@brief is part of the heat shield for the PC boards 
   class TALC : public AgBlock 
   {  public: 
      TALC() : AgBlock("TALC","is part of the heat shield for the PC boards"){ }; 
      ~TALC(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TALC,1); 
   }; 
   // ---------------------------------------------------------------------- TRDV -- 
   ///@defgroup TRDV_doc 
   ///@class TRDV 
   ///@brief is the RDO board volume 
   class TRDV : public AgBlock 
   {  public: 
      TRDV() : AgBlock("TRDV","is the RDO board volume"){ }; 
      ~TRDV(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TRDV,1); 
   }; 
   // ---------------------------------------------------------------------- TRDS -- 
   ///@defgroup TRDS_doc 
   ///@class TRDS 
   ///@brief is a division of rdo board volume corresponding to one supersector 
   class TRDS : public AgBlock 
   {  public: 
      TRDS() : AgBlock("TRDS","is a division of rdo board volume corresponding to one supersector"){ }; 
      ~TRDS(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TRDS,1); 
   }; 
   // ---------------------------------------------------------------------- TRDC -- 
   ///@defgroup TRDC_doc 
   ///@class TRDC 
   ///@brief is an RDO Card 
   class TRDC : public AgBlock 
   {  public: 
      TRDC() : AgBlock("TRDC","is an RDO Card"){ }; 
      ~TRDC(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TRDC,1); 
   }; 
   /// \class TpceGeo1 
   /// \brief   is the TPC system in GSTAR  
   class TpceGeo1 : public AgModule 
   { 
      public: 
      TpceGeo1(); 
      virtual void ConstructGeometry( const Char_t *dummy="" ); 
      ~TpceGeo1(){ }; 
      ClassDef(TpceGeo1,1); 
      public: 
   }; 
   // endElement in class Module 
}; // namespace TpceGeo1 
#endif // __TpceGeo1__ 
