#ifndef __SisdGeo1__ 
#define __SisdGeo1__ 
 
#include "StarVMC/StarAgmlLib/AgModule.h" 
 
namespace SISDGEO1 // $NMSPC 
{ 
   class Ssdp_t : public AgStructure 
   { 
      ClassDef(Ssdp_t,1); 
      public: 
      Float_t version; 
      Float_t config; 
      Int_t placement; 
      Ssdp_t() : AgStructure("Ssdp_t","User-defined AgML structure") 
      { 
         version=0; 
         config=0; 
         placement=0; 
         _index=0; 
      } 
      ~ Ssdp_t(){ /* nada */ }; 
   }; 
   class Sfpb_t : public AgStructure 
   { 
      ClassDef(Sfpb_t,1); 
      public: 
      Float_t hhight; 
      Float_t khight; 
      Float_t hbase; 
      Float_t kbase; 
      Float_t fsize; 
      Float_t zcoor; 
      Sfpb_t() : AgStructure("Sfpb_t","User-defined AgML structure") 
      { 
         hhight=0; 
         khight=0; 
         hbase=0; 
         kbase=0; 
         fsize=0; 
         zcoor=0; 
         _index=0; 
      } 
      ~ Sfpb_t(){ /* nada */ }; 
   }; 
   class Sfpa_t : public AgStructure 
   { 
      ClassDef(Sfpa_t,1); 
      public: 
      Float_t version; 
      Float_t rmin; 
      Float_t rmax; 
      Float_t len; 
      Float_t rad; 
      Float_t nssd; 
      Float_t dmwid; 
      Float_t dmthk; 
      Float_t dmlen; 
      Float_t smwid; 
      Float_t smthk; 
      Float_t smlen; 
      Float_t sslen; 
      Float_t wplen; 
      Float_t sdlen; 
      Float_t tilt; 
      Float_t cprad; 
      Float_t cpral; 
      Float_t cfrad; 
      Float_t gpthk; 
      Array_t<Int_t> laddermap; 
      Array_t<Float_t> ladderangle; 
      Array_t<Float_t> laddertilt; 
      Array_t<Float_t> ladderradius; 
      Sfpa_t() : AgStructure("Sfpa_t","User-defined AgML structure") 
      { 
         version=0; 
         rmin=0; 
         rmax=0; 
         len=0; 
         rad=0; 
         nssd=0; 
         dmwid=0; 
         dmthk=0; 
         dmlen=0; 
         smwid=0; 
         smthk=0; 
         smlen=0; 
         sslen=0; 
         wplen=0; 
         sdlen=0; 
         tilt=0; 
         cprad=0; 
         cpral=0; 
         cfrad=0; 
         gpthk=0; 
         laddermap = Array_t<Int_t>(20); 
         ladderangle = Array_t<Float_t>(20); 
         laddertilt = Array_t<Float_t>(20); 
         ladderradius = Array_t<Float_t>(20); 
         _index=0; 
      } 
      ~ Sfpa_t(){ /* nada */ }; 
   }; 
   // ---------------------------------------------------------------------- SFMO -- 
   ///@defgroup SFMO_doc 
   ///@class SFMO 
   ///@brief is the mother of all Silicon Strip Detector volumes 
   class SFMO : public AgBlock 
   {  public: 
      SFMO() : AgBlock("SFMO","is the mother of all Silicon Strip Detector volumes"){ }; 
      ~SFMO(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SFMO,1); 
   }; 
   // ---------------------------------------------------------------------- SFLM -- 
   ///@defgroup SFLM_doc 
   ///@class SFLM 
   ///@brief is the mother of the ladder 
   class SFLM : public AgBlock 
   {  public: 
      SFLM() : AgBlock("SFLM","is the mother of the ladder"){ }; 
      ~SFLM(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SFLM,1); 
   }; 
   // ---------------------------------------------------------------------- SFDM -- 
   ///@defgroup SFDM_doc 
   ///@class SFDM 
   ///@brief is the mother of the detectors 
   class SFDM : public AgBlock 
   {  public: 
      SFDM() : AgBlock("SFDM","is the mother of the detectors"){ }; 
      ~SFDM(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SFDM,1); 
   }; 
   // ---------------------------------------------------------------------- SFSW -- 
   ///@defgroup SFSW_doc 
   ///@class SFSW 
   ///@brief is a single wafer container 
   class SFSW : public AgBlock 
   {  public: 
      SFSW() : AgBlock("SFSW","is a single wafer container"){ }; 
      ~SFSW(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SFSW,1); 
   }; 
   // ---------------------------------------------------------------------- SFRA -- 
   ///@defgroup SFRA_doc 
   ///@class SFRA 
   ///@brief is the hybrid stiffneer 
   class SFRA : public AgBlock 
   {  public: 
      SFRA() : AgBlock("SFRA","is the hybrid stiffneer"){ }; 
      ~SFRA(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SFRA,1); 
   }; 
   // ---------------------------------------------------------------------- SFRS -- 
   ///@defgroup SFRS_doc 
   ///@class SFRS 
   ///@brief two supports of the hybrid stiffneer (piece of it) 
   class SFRS : public AgBlock 
   {  public: 
      SFRS() : AgBlock("SFRS","two supports of the hybrid stiffneer (piece of it)"){ }; 
      ~SFRS(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SFRS,1); 
   }; 
   // ---------------------------------------------------------------------- SFFX -- 
   ///@defgroup SFFX_doc 
   ///@class SFFX 
   ///@brief is the flex 
   class SFFX : public AgBlock 
   {  public: 
      SFFX() : AgBlock("SFFX","is the flex"){ }; 
      ~SFFX(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SFFX,1); 
   }; 
   // ---------------------------------------------------------------------- SFAA -- 
   ///@defgroup SFAA_doc 
   ///@class SFAA 
   ///@brief is the A128C chip 
   class SFAA : public AgBlock 
   {  public: 
      SFAA() : AgBlock("SFAA","is the A128C chip"){ }; 
      ~SFAA(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SFAA,1); 
   }; 
   // ---------------------------------------------------------------------- SFPI -- 
   ///@defgroup SFPI_doc 
   ///@class SFPI 
   ///@brief are the pions 
   class SFPI : public AgBlock 
   {  public: 
      SFPI() : AgBlock("SFPI","are the pions"){ }; 
      ~SFPI(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SFPI,1); 
   }; 
   // ---------------------------------------------------------------------- SFPJ -- 
   ///@defgroup SFPJ_doc 
   ///@class SFPJ 
   ///@brief is the base of the pions 
   class SFPJ : public AgBlock 
   {  public: 
      SFPJ() : AgBlock("SFPJ","is the base of the pions"){ }; 
      ~SFPJ(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SFPJ,1); 
   }; 
   // ---------------------------------------------------------------------- SFSD -- 
   ///@defgroup SFSD_doc 
   ///@class SFSD 
   ///@brief is the strip detector 
   class SFSD : public AgBlock 
   {  public: 
      SFSD() : AgBlock("SFSD","is the strip detector"){ }; 
      ~SFSD(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SFSD,1); 
   }; 
   // ---------------------------------------------------------------------- SFSM -- 
   ///@defgroup SFSM_doc 
   ///@class SFSM 
   ///@brief is the mother volume of the ladder (mechanic structure) 
   class SFSM : public AgBlock 
   {  public: 
      SFSM() : AgBlock("SFSM","is the mother volume of the ladder (mechanic structure)"){ }; 
      ~SFSM(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SFSM,1); 
   }; 
   // ---------------------------------------------------------------------- SFLT -- 
   ///@defgroup SFLT_doc 
   ///@class SFLT 
   ///@brief is (half) the top corner of the triangular ladder skeleton 
   class SFLT : public AgBlock 
   {  public: 
      SFLT() : AgBlock("SFLT","is (half) the top corner of the triangular ladder skeleton"){ }; 
      ~SFLT(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SFLT,1); 
   }; 
   // ---------------------------------------------------------------------- SFLU -- 
   ///@defgroup SFLU_doc 
   ///@class SFLU 
   ///@brief is (half) the side corner of the triangular ladder skeleton 
   class SFLU : public AgBlock 
   {  public: 
      SFLU() : AgBlock("SFLU","is (half) the side corner of the triangular ladder skeleton"){ }; 
      ~SFLU(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SFLU,1); 
   }; 
   // ---------------------------------------------------------------------- SFFK -- 
   ///@defgroup SFFK_doc 
   ///@class SFFK 
   ///@brief horizontal part of the ladder skeleton carbon base 
   class SFFK : public AgBlock 
   {  public: 
      SFFK() : AgBlock("SFFK","horizontal part of the ladder skeleton carbon base"){ }; 
      ~SFFK(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SFFK,1); 
   }; 
   // ---------------------------------------------------------------------- SFFL -- 
   ///@defgroup SFFL_doc 
   ///@class SFFL 
   ///@brief titled part of the ladder skeleton carbon base 
   class SFFL : public AgBlock 
   {  public: 
      SFFL() : AgBlock("SFFL","titled part of the ladder skeleton carbon base"){ }; 
      ~SFFL(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SFFL,1); 
   }; 
   // ---------------------------------------------------------------------- SFKK -- 
   ///@defgroup SFKK_doc 
   ///@class SFKK 
   ///@brief horizontal part of the kapton film under the ladder base 
   class SFKK : public AgBlock 
   {  public: 
      SFKK() : AgBlock("SFKK","horizontal part of the kapton film under the ladder base"){ }; 
      ~SFKK(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SFKK,1); 
   }; 
   // ---------------------------------------------------------------------- SFKL -- 
   ///@defgroup SFKL_doc 
   ///@class SFKL 
   ///@brief titled part of the kpaton film under the ladder base 
   class SFKL : public AgBlock 
   {  public: 
      SFKL() : AgBlock("SFKL","titled part of the kpaton film under the ladder base"){ }; 
      ~SFKL(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SFKL,1); 
   }; 
   // ---------------------------------------------------------------------- SFLA -- 
   ///@defgroup SFLA_doc 
   ///@class SFLA 
   ///@brief is the long part of the bus cable linking the modules to the connection board 
   class SFLA : public AgBlock 
   {  public: 
      SFLA() : AgBlock("SFLA","is the long part of the bus cable linking the modules to the connection board"){ }; 
      ~SFLA(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SFLA,1); 
   }; 
   // ---------------------------------------------------------------------- SFLB -- 
   ///@defgroup SFLB_doc 
   ///@class SFLB 
   ///@brief is the part of the long bus cable on the connection board 
   class SFLB : public AgBlock 
   {  public: 
      SFLB() : AgBlock("SFLB","is the part of the long bus cable on the connection board"){ }; 
      ~SFLB(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SFLB,1); 
   }; 
   // ---------------------------------------------------------------------- SFLC -- 
   ///@defgroup SFLC_doc 
   ///@class SFLC 
   ///@brief is the part of the long bus cable on the connection board up to the connector 
   class SFLC : public AgBlock 
   {  public: 
      SFLC() : AgBlock("SFLC","is the part of the long bus cable on the connection board up to the connector"){ }; 
      ~SFLC(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SFLC,1); 
   }; 
   // ---------------------------------------------------------------------- SFEB -- 
   ///@defgroup SFEB_doc 
   ///@class SFEB 
   ///@brief is the big bus elbow 
   class SFEB : public AgBlock 
   {  public: 
      SFEB() : AgBlock("SFEB","is the big bus elbow"){ }; 
      ~SFEB(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SFEB,1); 
   }; 
   // ---------------------------------------------------------------------- SFES -- 
   ///@defgroup SFES_doc 
   ///@class SFES 
   ///@brief is the small bus elbow 
   class SFES : public AgBlock 
   {  public: 
      SFES() : AgBlock("SFES","is the small bus elbow"){ }; 
      ~SFES(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SFES,1); 
   }; 
   // ---------------------------------------------------------------------- SFAM -- 
   ///@defgroup SFAM_doc 
   ///@class SFAM 
   ///@brief is the mother volume of the adc board 
   class SFAM : public AgBlock 
   {  public: 
      SFAM() : AgBlock("SFAM","is the mother volume of the adc board"){ }; 
      ~SFAM(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SFAM,1); 
   }; 
   // ---------------------------------------------------------------------- SFAB -- 
   ///@defgroup SFAB_doc 
   ///@class SFAB 
   ///@brief is the big volume of the adc board 
   class SFAB : public AgBlock 
   {  public: 
      SFAB() : AgBlock("SFAB","is the big volume of the adc board"){ }; 
      ~SFAB(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SFAB,1); 
   }; 
   // ---------------------------------------------------------------------- SFAS -- 
   ///@defgroup SFAS_doc 
   ///@class SFAS 
   ///@brief is the small volume of the adc board 
   class SFAS : public AgBlock 
   {  public: 
      SFAS() : AgBlock("SFAS","is the small volume of the adc board"){ }; 
      ~SFAS(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SFAS,1); 
   }; 
   // ---------------------------------------------------------------------- SAPP -- 
   ///@defgroup SAPP_doc 
   ///@class SAPP 
   ///@brief is the mother volume of the adc board appendice 
   class SAPP : public AgBlock 
   {  public: 
      SAPP() : AgBlock("SAPP","is the mother volume of the adc board appendice"){ }; 
      ~SAPP(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SAPP,1); 
   }; 
   // ---------------------------------------------------------------------- SAPC -- 
   ///@defgroup SAPC_doc 
   ///@class SAPC 
   ///@brief is the core (Epoxy) of the adc board appendice 
   class SAPC : public AgBlock 
   {  public: 
      SAPC() : AgBlock("SAPC","is the core (Epoxy) of the adc board appendice"){ }; 
      ~SAPC(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SAPC,1); 
   }; 
   // ---------------------------------------------------------------------- SAPS -- 
   ///@defgroup SAPS_doc 
   ///@class SAPS 
   ///@brief is the side shell (Carbon) of the adc board appendice 
   class SAPS : public AgBlock 
   {  public: 
      SAPS() : AgBlock("SAPS","is the side shell (Carbon) of the adc board appendice"){ }; 
      ~SAPS(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SAPS,1); 
   }; 
   // ---------------------------------------------------------------------- SAPT -- 
   ///@defgroup SAPT_doc 
   ///@class SAPT 
   ///@brief is the top-bottom shell (Carbon) of the adc board appendice 
   class SAPT : public AgBlock 
   {  public: 
      SAPT() : AgBlock("SAPT","is the top-bottom shell (Carbon) of the adc board appendice"){ }; 
      ~SAPT(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SAPT,1); 
   }; 
   // ---------------------------------------------------------------------- SFCO -- 
   ///@defgroup SFCO_doc 
   ///@class SFCO 
   ///@brief is the connection board (rectangular with Hirose connectors) 
   class SFCO : public AgBlock 
   {  public: 
      SFCO() : AgBlock("SFCO","is the connection board (rectangular with Hirose connectors)"){ }; 
      ~SFCO(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SFCO,1); 
   }; 
   // ---------------------------------------------------------------------- SFCM -- 
   ///@defgroup SFCM_doc 
   ///@class SFCM 
   ///@brief is the mother volume of the second connection board 
   class SFCM : public AgBlock 
   {  public: 
      SFCM() : AgBlock("SFCM","is the mother volume of the second connection board"){ }; 
      ~SFCM(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SFCM,1); 
   }; 
   // ---------------------------------------------------------------------- SFCB -- 
   ///@defgroup SFCB_doc 
   ///@class SFCB 
   ///@brief is the big part of the second connection board 
   class SFCB : public AgBlock 
   {  public: 
      SFCB() : AgBlock("SFCB","is the big part of the second connection board"){ }; 
      ~SFCB(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SFCB,1); 
   }; 
   // ---------------------------------------------------------------------- SFCS -- 
   ///@defgroup SFCS_doc 
   ///@class SFCS 
   ///@brief is the big part of the second connection board 
   class SFCS : public AgBlock 
   {  public: 
      SFCS() : AgBlock("SFCS","is the big part of the second connection board"){ }; 
      ~SFCS(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SFCS,1); 
   }; 
   // ---------------------------------------------------------------------- SFKF -- 
   ///@defgroup SFKF_doc 
   ///@class SFKF 
   ///@brief is the first part of the kapton flex circuit 
   class SFKF : public AgBlock 
   {  public: 
      SFKF() : AgBlock("SFKF","is the first part of the kapton flex circuit"){ }; 
      ~SFKF(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SFKF,1); 
   }; 
   // ---------------------------------------------------------------------- SFKS -- 
   ///@defgroup SFKS_doc 
   ///@class SFKS 
   ///@brief is the second part of the kapton flex circuit 
   class SFKS : public AgBlock 
   {  public: 
      SFKS() : AgBlock("SFKS","is the second part of the kapton flex circuit"){ }; 
      ~SFKS(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SFKS,1); 
   }; 
   // ---------------------------------------------------------------------- SFPR -- 
   ///@defgroup SFPR_doc 
   ///@class SFPR 
   ///@brief is the ladder end inside mechanical part (prism with g10 with half density) 
   class SFPR : public AgBlock 
   {  public: 
      SFPR() : AgBlock("SFPR","is the ladder end inside mechanical part (prism with g10 with half density)"){ }; 
      ~SFPR(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SFPR,1); 
   }; 
   // ---------------------------------------------------------------------- SFPB -- 
   ///@defgroup SFPB_doc 
   ///@class SFPB 
   ///@brief is the ladder end outside mechanical part (rectangle with g10) 
   class SFPB : public AgBlock 
   {  public: 
      SFPB() : AgBlock("SFPB","is the ladder end outside mechanical part (rectangle with g10)"){ }; 
      ~SFPB(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SFPB,1); 
   }; 
   // ---------------------------------------------------------------------- SSBS -- 
   ///@defgroup SSBS_doc 
   ///@class SSBS 
   ///@brief is the small part of the aluminium plate linking the ladder to the sector 
   class SSBS : public AgBlock 
   {  public: 
      SSBS() : AgBlock("SSBS","is the small part of the aluminium plate linking the ladder to the sector"){ }; 
      ~SSBS(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SSBS,1); 
   }; 
   // ---------------------------------------------------------------------- SSBB -- 
   ///@defgroup SSBB_doc 
   ///@class SSBB 
   ///@brief is the Big part of the aluminium plate linking the ladder to the sector 
   class SSBB : public AgBlock 
   {  public: 
      SSBB() : AgBlock("SSBB","is the Big part of the aluminium plate linking the ladder to the sector"){ }; 
      ~SSBB(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SSBB,1); 
   }; 
   // ---------------------------------------------------------------------- SSST -- 
   ///@defgroup SSST_doc 
   ///@class SSST 
   ///@brief is the top of the small sector 
   class SSST : public AgBlock 
   {  public: 
      SSST() : AgBlock("SSST","is the top of the small sector"){ }; 
      ~SSST(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SSST,1); 
   }; 
   // ---------------------------------------------------------------------- SSSS -- 
   ///@defgroup SSSS_doc 
   ///@class SSSS 
   ///@brief is the side of the small sector 
   class SSSS : public AgBlock 
   {  public: 
      SSSS() : AgBlock("SSSS","is the side of the small sector"){ }; 
      ~SSSS(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SSSS,1); 
   }; 
   // ---------------------------------------------------------------------- SSRT -- 
   ///@defgroup SSRT_doc 
   ///@class SSRT 
   ///@brief is the top of the side rib 
   class SSRT : public AgBlock 
   {  public: 
      SSRT() : AgBlock("SSRT","is the top of the side rib"){ }; 
      ~SSRT(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SSRT,1); 
   }; 
   // ---------------------------------------------------------------------- SSRS -- 
   ///@defgroup SSRS_doc 
   ///@class SSRS 
   ///@brief is the side of the small rib 
   class SSRS : public AgBlock 
   {  public: 
      SSRS() : AgBlock("SSRS","is the side of the small rib"){ }; 
      ~SSRS(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SSRS,1); 
   }; 
   // ---------------------------------------------------------------------- SSLB -- 
   ///@defgroup SSLB_doc 
   ///@class SSLB 
   ///@brief is the linking (sector to the cone) box 
   class SSLB : public AgBlock 
   {  public: 
      SSLB() : AgBlock("SSLB","is the linking (sector to the cone) box"){ }; 
      ~SSLB(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SSLB,1); 
   }; 
   // ---------------------------------------------------------------------- SSLT -- 
   ///@defgroup SSLT_doc 
   ///@class SSLT 
   ///@brief is the linking (sector to the cone) tube 
   class SSLT : public AgBlock 
   {  public: 
      SSLT() : AgBlock("SSLT","is the linking (sector to the cone) tube"){ }; 
      ~SSLT(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SSLT,1); 
   }; 
   // ---------------------------------------------------------------------- SCMP -- 
   ///@defgroup SCMP_doc 
   ///@class SCMP 
   ///@brief is the mounting plate inserted in the cones. 
   class SCMP : public AgBlock 
   {  public: 
      SCMP() : AgBlock("SCMP","is the mounting plate inserted in the cones."){ }; 
      ~SCMP(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SCMP,1); 
   }; 
   // ---------------------------------------------------------------------- SCVM -- 
   ///@defgroup SCVM_doc 
   ///@class SCVM 
   ///@brief is the mother volume of the V-shape piece 
   class SCVM : public AgBlock 
   {  public: 
      SCVM() : AgBlock("SCVM","is the mother volume of the V-shape piece"){ }; 
      ~SCVM(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SCVM,1); 
   }; 
   // ---------------------------------------------------------------------- SCVB -- 
   ///@defgroup SCVB_doc 
   ///@class SCVB 
   ///@brief is the base plate of the V-shape piece 
   class SCVB : public AgBlock 
   {  public: 
      SCVB() : AgBlock("SCVB","is the base plate of the V-shape piece"){ }; 
      ~SCVB(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SCVB,1); 
   }; 
   // ---------------------------------------------------------------------- SCVS -- 
   ///@defgroup SCVS_doc 
   ///@class SCVS 
   ///@brief is the side plate of the V-shape piece 
   class SCVS : public AgBlock 
   {  public: 
      SCVS() : AgBlock("SCVS","is the side plate of the V-shape piece"){ }; 
      ~SCVS(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SCVS,1); 
   }; 
   /// \class SisdGeo1 
   /// \brief   is the Silicon Strip Detector  
   class SisdGeo1 : public AgModule 
   { 
      public: 
      SisdGeo1(); 
      virtual void ConstructGeometry( const Char_t *dummy="" ); 
      ~SisdGeo1(){ }; 
      ClassDef(SisdGeo1,1); 
      public: 
   }; 
   // endElement in class Module 
}; // namespace SisdGeo1 
#endif // __SisdGeo1__ 
