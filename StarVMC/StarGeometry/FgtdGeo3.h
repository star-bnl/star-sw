#ifndef __FgtdGeo3__ 
#define __FgtdGeo3__ 
 
#include "StarVMC/StarAgmlLib/AgModule.h" 
 
namespace FGTDGEO3 // $NMSPC 
{ 
   class Fggg_t : public AgStructure 
   { 
      ClassDef(Fggg_t,1); 
      public: 
      Float_t fgstconfig; 
      Fggg_t() : AgStructure("Fggg_t","User-defined AgML structure") 
      { 
         fgstconfig=0; 
         _index=0; 
      } 
      ~ Fggg_t(){ /* nada */ }; 
   }; 
   class Fgst_t : public AgStructure 
   { 
      ClassDef(Fgst_t,1); 
      public: 
      Float_t config; 
      Int_t ndisk; 
      Int_t nquad; 
      Array_t<Float_t> zdisca; 
      Fgst_t() : AgStructure("Fgst_t","User-defined AgML structure") 
      { 
         config=0; 
         ndisk=0; 
         nquad=0; 
         zdisca = Array_t<Float_t>(6); 
         _index=0; 
      } 
      ~ Fgst_t(){ /* nada */ }; 
   }; 
   // ---------------------------------------------------------------------- FGTM -- 
   ///@defgroup FGTM_doc 
   ///@class FGTM 
   ///@brief mother volume for FGT assembly 
   class FGTM : public AgBlock 
   {  public: 
      FGTM() : AgBlock("FGTM","mother volume for FGT assembly"){ }; 
      ~FGTM(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FGTM,1); 
   }; 
   // ---------------------------------------------------------------------- FGCT -- 
   ///@defgroup FGCT_doc 
   ///@class FGCT 
   ///@brief inner cooling tube 
   class FGCT : public AgBlock 
   {  public: 
      FGCT() : AgBlock("FGCT","inner cooling tube"){ }; 
      ~FGCT(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FGCT,1); 
   }; 
   // ---------------------------------------------------------------------- FGCN -- 
   ///@defgroup FGCN_doc 
   ///@class FGCN 
   ///@brief nylon 1st ring 
   class FGCN : public AgBlock 
   {  public: 
      FGCN() : AgBlock("FGCN","nylon 1st ring"){ }; 
      ~FGCN(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FGCN,1); 
   }; 
   // ---------------------------------------------------------------------- FGCM -- 
   ///@defgroup FGCM_doc 
   ///@class FGCM 
   ///@brief nylon and Al ring 
   class FGCM : public AgBlock 
   {  public: 
      FGCM() : AgBlock("FGCM","nylon and Al ring"){ }; 
      ~FGCM(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FGCM,1); 
   }; 
   // ---------------------------------------------------------------------- FGTD -- 
   ///@defgroup FGTD_doc 
   ///@class FGTD 
   ///@brief mother volume for FGT disk 
   class FGTD : public AgBlock 
   {  public: 
      FGTD() : AgBlock("FGTD","mother volume for FGT disk"){ }; 
      ~FGTD(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FGTD,1); 
   }; 
   // ---------------------------------------------------------------------- FGTH -- 
   ///@defgroup FGTH_doc 
   ///@class FGTH 
   ///@brief mother volume for FGT disk 
   class FGTH : public AgBlock 
   {  public: 
      FGTH() : AgBlock("FGTH","mother volume for FGT disk"){ }; 
      ~FGTH(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FGTH,1); 
   }; 
   // ---------------------------------------------------------------------- FGTQ -- 
   ///@defgroup FGTQ_doc 
   ///@class FGTQ 
   ///@brief quadrant  
   class FGTQ : public AgBlock 
   {  public: 
      FGTQ() : AgBlock("FGTQ","quadrant "){ }; 
      ~FGTQ(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FGTQ,1); 
   }; 
   // ---------------------------------------------------------------------- FGVN -- 
   ///@defgroup FGVN_doc 
   ///@class FGVN 
   ///@brief quad nomex mixture volume 
   class FGVN : public AgBlock 
   {  public: 
      FGVN() : AgBlock("FGVN","quad nomex mixture volume"){ }; 
      ~FGVN(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FGVN,1); 
   }; 
   // ---------------------------------------------------------------------- FGZC -- 
   ///@defgroup FGZC_doc 
   ///@class FGZC 
   ///@brief sensitive volume 
   class FGZC : public AgBlock 
   {  public: 
      FGZC() : AgBlock("FGZC","sensitive volume"){ }; 
      ~FGZC(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FGZC,1); 
   }; 
   // ---------------------------------------------------------------------- FGVG -- 
   ///@defgroup FGVG_doc 
   ///@class FGVG 
   ///@brief quad 3GEM  mixture volume 
   class FGVG : public AgBlock 
   {  public: 
      FGVG() : AgBlock("FGVG","quad 3GEM  mixture volume"){ }; 
      ~FGVG(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FGVG,1); 
   }; 
   // ---------------------------------------------------------------------- FGVR -- 
   ///@defgroup FGVR_doc 
   ///@class FGVR 
   ///@brief quad readout mixture volume 
   class FGVR : public AgBlock 
   {  public: 
      FGVR() : AgBlock("FGVR","quad readout mixture volume"){ }; 
      ~FGVR(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FGVR,1); 
   }; 
   // ---------------------------------------------------------------------- FGVE -- 
   ///@defgroup FGVE_doc 
   ///@class FGVE 
   ///@brief quad electronics volume  
   class FGVE : public AgBlock 
   {  public: 
      FGVE() : AgBlock("FGVE","quad electronics volume "){ }; 
      ~FGVE(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FGVE,1); 
   }; 
   // ---------------------------------------------------------------------- FGQA -- 
   ///@defgroup FGQA_doc 
   ///@class FGQA 
   ///@brief A-spacer frame 
   class FGQA : public AgBlock 
   {  public: 
      FGQA() : AgBlock("FGQA","A-spacer frame"){ }; 
      ~FGQA(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FGQA,1); 
   }; 
   // ---------------------------------------------------------------------- FGQB -- 
   ///@defgroup FGQB_doc 
   ///@class FGQB 
   ///@brief B-spacer frame, arc 
   class FGQB : public AgBlock 
   {  public: 
      FGQB() : AgBlock("FGQB","B-spacer frame, arc"){ }; 
      ~FGQB(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FGQB,1); 
   }; 
   // ---------------------------------------------------------------------- FGQC -- 
   ///@defgroup FGQC_doc 
   ///@class FGQC 
   ///@brief C-spacer frame, bar 
   class FGQC : public AgBlock 
   {  public: 
      FGQC() : AgBlock("FGQC","C-spacer frame, bar"){ }; 
      ~FGQC(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FGQC,1); 
   }; 
   // ---------------------------------------------------------------------- FGQD -- 
   ///@defgroup FGQD_doc 
   ///@class FGQD 
   ///@brief D-spacer frame, arc 
   class FGQD : public AgBlock 
   {  public: 
      FGQD() : AgBlock("FGQD","D-spacer frame, arc"){ }; 
      ~FGQD(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FGQD,1); 
   }; 
   // ---------------------------------------------------------------------- FGQE -- 
   ///@defgroup FGQE_doc 
   ///@class FGQE 
   ///@brief E-spacer frame, flat 
   class FGQE : public AgBlock 
   {  public: 
      FGQE() : AgBlock("FGQE","E-spacer frame, flat"){ }; 
      ~FGQE(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FGQE,1); 
   }; 
   // ---------------------------------------------------------------------- FGQF -- 
   ///@defgroup FGQF_doc 
   ///@class FGQF 
   ///@brief F-spacer frame,  arc 
   class FGQF : public AgBlock 
   {  public: 
      FGQF() : AgBlock("FGQF","F-spacer frame,  arc"){ }; 
      ~FGQF(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FGQF,1); 
   }; 
   // ---------------------------------------------------------------------- FGXA -- 
   ///@defgroup FGXA_doc 
   ///@class FGXA 
   ///@brief A-grid, arc 
   class FGXA : public AgBlock 
   {  public: 
      FGXA() : AgBlock("FGXA","A-grid, arc"){ }; 
      ~FGXA(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FGXA,1); 
   }; 
   // ---------------------------------------------------------------------- FGXB -- 
   ///@defgroup FGXB_doc 
   ///@class FGXB 
   ///@brief B-grid, arc 
   class FGXB : public AgBlock 
   {  public: 
      FGXB() : AgBlock("FGXB","B-grid, arc"){ }; 
      ~FGXB(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FGXB,1); 
   }; 
   // ---------------------------------------------------------------------- FGXC -- 
   ///@defgroup FGXC_doc 
   ///@class FGXC 
   ///@brief C-grid, ray 
   class FGXC : public AgBlock 
   {  public: 
      FGXC() : AgBlock("FGXC","C-grid, ray"){ }; 
      ~FGXC(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FGXC,1); 
   }; 
   // ---------------------------------------------------------------------- FGXD -- 
   ///@defgroup FGXD_doc 
   ///@class FGXD 
   ///@brief D-grid, ray 
   class FGXD : public AgBlock 
   {  public: 
      FGXD() : AgBlock("FGXD","D-grid, ray"){ }; 
      ~FGXD(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FGXD,1); 
   }; 
   // ---------------------------------------------------------------------- FGWA -- 
   ///@defgroup FGWA_doc 
   ///@class FGWA 
   ///@brief A-elec, bar  
   class FGWA : public AgBlock 
   {  public: 
      FGWA() : AgBlock("FGWA","A-elec, bar "){ }; 
      ~FGWA(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FGWA,1); 
   }; 
   // ---------------------------------------------------------------------- FGWB -- 
   ///@defgroup FGWB_doc 
   ///@class FGWB 
   ///@brief APV board  
   class FGWB : public AgBlock 
   {  public: 
      FGWB() : AgBlock("FGWB","APV board "){ }; 
      ~FGWB(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FGWB,1); 
   }; 
   // ---------------------------------------------------------------------- FGWC -- 
   ///@defgroup FGWC_doc 
   ///@class FGWC 
   ///@brief interconnect board wider  
   class FGWC : public AgBlock 
   {  public: 
      FGWC() : AgBlock("FGWC","interconnect board wider "){ }; 
      ~FGWC(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FGWC,1); 
   }; 
   // ---------------------------------------------------------------------- FGWD -- 
   ///@defgroup FGWD_doc 
   ///@class FGWD 
   ///@brief gas feed connection  
   class FGWD : public AgBlock 
   {  public: 
      FGWD() : AgBlock("FGWD","gas feed connection "){ }; 
      ~FGWD(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FGWD,1); 
   }; 
   // ---------------------------------------------------------------------- FGWE -- 
   ///@defgroup FGWE_doc 
   ///@class FGWE 
   ///@brief HV board   
   class FGWE : public AgBlock 
   {  public: 
      FGWE() : AgBlock("FGWE","HV board  "){ }; 
      ~FGWE(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FGWE,1); 
   }; 
   /// \class FgtdGeo3 
   /// \brief  forward GEM tracking detector for 2012 , 2013 
   class FgtdGeo3 : public AgModule 
   { 
      public: 
      FgtdGeo3(); 
      virtual void ConstructGeometry( const Char_t *dummy="" ); 
      ~FgtdGeo3(){ }; 
      ClassDef(FgtdGeo3,1); 
      public: 
   }; 
   // endElement in class Module 
}; // namespace FgtdGeo3 
#endif // __FgtdGeo3__ 
