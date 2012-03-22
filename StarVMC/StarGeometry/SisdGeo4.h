#ifndef __SisdGeo4__ 
#define __SisdGeo4__ 
 
#include "StarVMC/StarAgmlLib/AgModule.h" 
 
namespace SISDGEO4 // $NMSPC 
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
   class Sfjp_t : public AgStructure 
   { 
      ClassDef(Sfjp_t,1); 
      public: 
      Float_t version; 
      Float_t alphaz; 
      Float_t alphazh; 
      Float_t ssst_rmin; 
      Float_t ssst_rmax; 
      Float_t ssst_width; 
      Float_t ssst_pz; 
      Float_t ssss_rmin; 
      Float_t ssss_width; 
      Float_t ssrs_rmin; 
      Float_t sslb_dx; 
      Float_t sslb_px; 
      Float_t sslb_dy; 
      Float_t sslb_dz; 
      Float_t ssbq_dx; 
      Float_t ssbq_dy; 
      Float_t ssbq_dz; 
      Float_t sscr_tha; 
      Float_t sscr_thd; 
      Float_t sscr_wd; 
      Float_t sscr_hl; 
      Float_t sscr_vl; 
      Float_t sslt_px; 
      Float_t sslt_rmax; 
      Float_t sslt_dz; 
      Float_t scmp_dx; 
      Float_t scmp_dy; 
      Float_t scmp_dz; 
      Float_t scmp_px; 
      Float_t scvm_dz; 
      Float_t scvm_dx; 
      Float_t scvm_px; 
      Float_t scvm_dy; 
      Float_t scvm_pz; 
      Float_t scvb_dy; 
      Float_t scvs_dx; 
      Float_t scvs_dy; 
      Float_t scvs_px; 
      Float_t scvs_py; 
      Float_t sfco_dx; 
      Float_t sfco_dy; 
      Float_t sfco_dz; 
      Float_t sfco_px; 
      Float_t sfco_py; 
      Float_t sfcm_dx; 
      Float_t sfcs_dz; 
      Float_t sfkf_dy; 
      Float_t sfkf_dz; 
      Float_t sfkf_dx; 
      Float_t sfkf_px; 
      Float_t sfkf_py; 
      Float_t sfks_dx; 
      Float_t sfks_px; 
      Float_t sfks_py; 
      Float_t sfpr_py; 
      Float_t sfpb_py; 
      Float_t sfpb_py2; 
      Float_t sfcb_dx; 
      Float_t ssbs_dy; 
      Float_t ssbs_dx; 
      Float_t ssbb_dx; 
      Float_t ssbb_dz; 
      Float_t flex_di; 
      Float_t sfpb_dx; 
      Float_t sfpb_dy; 
      Float_t sfpbdz; 
      Float_t sapp_dxe; 
      Float_t sapp_dxz; 
      Float_t sapp_dy; 
      Float_t sapp_dz; 
      Float_t sapp_py1; 
      Float_t sapp_py2; 
      Float_t sapp_py3; 
      Float_t sfam_dxe; 
      Float_t sfam_dxz; 
      Float_t sfam_dy; 
      Float_t sfam_dz; 
      Float_t sfam_dzs; 
      Float_t sfla_dx; 
      Float_t sfla_dy; 
      Float_t sfla_dz; 
      Float_t sflb_dz; 
      Float_t sflc_dz; 
      Float_t sfeb_dx; 
      Float_t sfeb_dz; 
      Float_t sfes_dx; 
      Float_t sffk_dxe; 
      Float_t sffk_dy; 
      Float_t sffx_dye; 
      Float_t sffk_dz; 
      Float_t sffl_dx; 
      Float_t sffk_dxz; 
      Float_t sffk_px; 
      Float_t sffk_py1; 
      Float_t sffk_py2; 
      Float_t sfkl_px; 
      Float_t sfkk_dy; 
      Float_t sflu_dz; 
      Float_t sflu_h1; 
      Float_t sflu_bl1; 
      Float_t sfra_dx; 
      Float_t sfra_dy; 
      Float_t sfra_py; 
      Float_t sfra_dz; 
      Float_t sfra_pz; 
      Float_t sfsw_dy; 
      Float_t sfsw_dz; 
      Float_t sfrs_dx; 
      Float_t sfrs_dy; 
      Float_t sfrs_px; 
      Float_t sfrs_py; 
      Float_t sfsm_ll; 
      Float_t sffx_dx; 
      Float_t sffx_dyz; 
      Float_t sfpi_rmin; 
      Float_t sfpi_rmax; 
      Float_t sfpi_px; 
      Float_t sfpi_py1; 
      Float_t sfpi_py2; 
      Float_t sfpi_pz; 
      Float_t sfpj_dx1; 
      Float_t sfpj_dx2; 
      Float_t sfpj_dy; 
      Float_t sfpj_dz; 
      Float_t sfaa_dx; 
      Float_t sfaa_dy; 
      Float_t sfaa_dz; 
      Float_t sfaa_px1; 
      Float_t sfaa_px2; 
      Float_t sfaa_px3; 
      Float_t sfaa_pz1; 
      Float_t sfaa_pz2; 
      Float_t sfaa_pz3; 
      Float_t sfsd_dx; 
      Float_t sfsd_dy; 
      Float_t sfsd_dz; 
      Float_t sfla_px; 
      Float_t sfla_py; 
      Float_t sflc_px; 
      Float_t sflc_py; 
      Float_t sfes_px; 
      Float_t sfes_py; 
      Float_t sfes_pz; 
      Float_t sfeb_px; 
      Float_t sfeb_py; 
      Sfjp_t() : AgStructure("Sfjp_t","User-defined AgML structure") 
      { 
         version=0; 
         alphaz=0; 
         alphazh=0; 
         ssst_rmin=0; 
         ssst_rmax=0; 
         ssst_width=0; 
         ssst_pz=0; 
         ssss_rmin=0; 
         ssss_width=0; 
         ssrs_rmin=0; 
         sslb_dx=0; 
         sslb_px=0; 
         sslb_dy=0; 
         sslb_dz=0; 
         ssbq_dx=0; 
         ssbq_dy=0; 
         ssbq_dz=0; 
         sscr_tha=0; 
         sscr_thd=0; 
         sscr_wd=0; 
         sscr_hl=0; 
         sscr_vl=0; 
         sslt_px=0; 
         sslt_rmax=0; 
         sslt_dz=0; 
         scmp_dx=0; 
         scmp_dy=0; 
         scmp_dz=0; 
         scmp_px=0; 
         scvm_dz=0; 
         scvm_dx=0; 
         scvm_px=0; 
         scvm_dy=0; 
         scvm_pz=0; 
         scvb_dy=0; 
         scvs_dx=0; 
         scvs_dy=0; 
         scvs_px=0; 
         scvs_py=0; 
         sfco_dx=0; 
         sfco_dy=0; 
         sfco_dz=0; 
         sfco_px=0; 
         sfco_py=0; 
         sfcm_dx=0; 
         sfcs_dz=0; 
         sfkf_dy=0; 
         sfkf_dz=0; 
         sfkf_dx=0; 
         sfkf_px=0; 
         sfkf_py=0; 
         sfks_dx=0; 
         sfks_px=0; 
         sfks_py=0; 
         sfpr_py=0; 
         sfpb_py=0; 
         sfpb_py2=0; 
         sfcb_dx=0; 
         ssbs_dy=0; 
         ssbs_dx=0; 
         ssbb_dx=0; 
         ssbb_dz=0; 
         flex_di=0; 
         sfpb_dx=0; 
         sfpb_dy=0; 
         sfpbdz=0; 
         sapp_dxe=0; 
         sapp_dxz=0; 
         sapp_dy=0; 
         sapp_dz=0; 
         sapp_py1=0; 
         sapp_py2=0; 
         sapp_py3=0; 
         sfam_dxe=0; 
         sfam_dxz=0; 
         sfam_dy=0; 
         sfam_dz=0; 
         sfam_dzs=0; 
         sfla_dx=0; 
         sfla_dy=0; 
         sfla_dz=0; 
         sflb_dz=0; 
         sflc_dz=0; 
         sfeb_dx=0; 
         sfeb_dz=0; 
         sfes_dx=0; 
         sffk_dxe=0; 
         sffk_dy=0; 
         sffx_dye=0; 
         sffk_dz=0; 
         sffl_dx=0; 
         sffk_dxz=0; 
         sffk_px=0; 
         sffk_py1=0; 
         sffk_py2=0; 
         sfkl_px=0; 
         sfkk_dy=0; 
         sflu_dz=0; 
         sflu_h1=0; 
         sflu_bl1=0; 
         sfra_dx=0; 
         sfra_dy=0; 
         sfra_py=0; 
         sfra_dz=0; 
         sfra_pz=0; 
         sfsw_dy=0; 
         sfsw_dz=0; 
         sfrs_dx=0; 
         sfrs_dy=0; 
         sfrs_px=0; 
         sfrs_py=0; 
         sfsm_ll=0; 
         sffx_dx=0; 
         sffx_dyz=0; 
         sfpi_rmin=0; 
         sfpi_rmax=0; 
         sfpi_px=0; 
         sfpi_py1=0; 
         sfpi_py2=0; 
         sfpi_pz=0; 
         sfpj_dx1=0; 
         sfpj_dx2=0; 
         sfpj_dy=0; 
         sfpj_dz=0; 
         sfaa_dx=0; 
         sfaa_dy=0; 
         sfaa_dz=0; 
         sfaa_px1=0; 
         sfaa_px2=0; 
         sfaa_px3=0; 
         sfaa_pz1=0; 
         sfaa_pz2=0; 
         sfaa_pz3=0; 
         sfsd_dx=0; 
         sfsd_dy=0; 
         sfsd_dz=0; 
         sfla_px=0; 
         sfla_py=0; 
         sflc_px=0; 
         sflc_py=0; 
         sfes_px=0; 
         sfes_py=0; 
         sfes_pz=0; 
         sfeb_px=0; 
         sfeb_py=0; 
         _index=0; 
      } 
      ~ Sfjp_t(){ /* nada */ }; 
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
   // ---------------------------------------------------------------------- SBCH -- 
   ///@defgroup SBCH_doc 
   ///@class SBCH 
   ///@brief is the horizontal branch of the DELRIN cross 
   class SBCH : public AgBlock 
   {  public: 
      SBCH() : AgBlock("SBCH","is the horizontal branch of the DELRIN cross"){ }; 
      ~SBCH(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SBCH,1); 
   }; 
   // ---------------------------------------------------------------------- SBFH -- 
   ///@defgroup SBFH_doc 
   ///@class SBFH 
   ///@brief is the horizontal branch of the Alumimium cross 
   class SBFH : public AgBlock 
   {  public: 
      SBFH() : AgBlock("SBFH","is the horizontal branch of the Alumimium cross"){ }; 
      ~SBFH(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SBFH,1); 
   }; 
   // ---------------------------------------------------------------------- SBCV -- 
   ///@defgroup SBCV_doc 
   ///@class SBCV 
   ///@brief is the vertical branch of the DELRIN cross 
   class SBCV : public AgBlock 
   {  public: 
      SBCV() : AgBlock("SBCV","is the vertical branch of the DELRIN cross"){ }; 
      ~SBCV(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SBCV,1); 
   }; 
   // ---------------------------------------------------------------------- SBFV -- 
   ///@defgroup SBFV_doc 
   ///@class SBFV 
   ///@brief is the vertical branch of the Alumimium cross 
   class SBFV : public AgBlock 
   {  public: 
      SBFV() : AgBlock("SBFV","is the vertical branch of the Alumimium cross"){ }; 
      ~SBFV(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SBFV,1); 
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
   /// \class SisdGeo4 
   /// \brief   is the Silicon Strip Detector  
   class SisdGeo4 : public AgModule 
   { 
      public: 
      SisdGeo4(); 
      virtual void ConstructGeometry( const Char_t *dummy="" ); 
      ~SisdGeo4(){ }; 
      ClassDef(SisdGeo4,1); 
      public: 
   }; 
   // endElement in class Module 
}; // namespace SisdGeo4 
#endif // __SisdGeo4__ 
