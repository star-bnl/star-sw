#ifndef __VpddGeo2__ 
#define __VpddGeo2__ 
 
#include "StarVMC/StarAgmlLib/AgModule.h" 
 
namespace VPDDGEO2 // $NMSPC 
{ 
   class Vpdv_t : public AgStructure 
   { 
      ClassDef(Vpdv_t,1); 
      public: 
      Float_t version; 
      Float_t vpdconfig; 
      Vpdv_t() : AgStructure("Vpdv_t","User-defined AgML structure") 
      { 
         version=0; 
         vpdconfig=0; 
         _index=0; 
      } 
      ~ Vpdv_t(){ /* nada */ }; 
   }; 
   class Vpdg_t : public AgStructure 
   { 
      ClassDef(Vpdg_t,1); 
      public: 
      Float_t version; 
      Float_t zposeast; 
      Float_t zposwest; 
      Float_t rmin; 
      Float_t rmax; 
      Float_t bpwidth; 
      Float_t bplength; 
      Float_t bpthick; 
      Float_t bxheight; 
      Float_t bxlength; 
      Float_t bxwidth; 
      Float_t bxthick; 
      Float_t bxzposc; 
      Float_t fpwidth; 
      Float_t fpheight; 
      Float_t fpthick; 
      Float_t fpawidth; 
      Float_t fpahght; 
      Float_t fpchght; 
      Float_t fphhght; 
      Float_t fphwidth; 
      Float_t fphthick; 
      Float_t stthick; 
      Float_t stheight; 
      Float_t stangle; 
      Float_t stdiagsz; 
      Float_t scwidth; 
      Float_t scheight; 
      Float_t sclength; 
      Float_t scthick; 
      Float_t clheight; 
      Float_t clwidth; 
      Float_t cllength; 
      Float_t clthick; 
      Float_t detlen; 
      Float_t detrad; 
      Float_t detfront; 
      Float_t convthk; 
      Float_t radithk; 
      Float_t eleleng; 
      Float_t drlayer; 
      Float_t numpmt; 
      Float_t pmtwall; 
      Float_t pmtrad; 
      Float_t pmtlen; 
      Float_t ibchoice; 
      Float_t ibposyc; 
      Float_t ibposzc; 
      Float_t ibleng; 
      Float_t ibthickh; 
      Float_t ibthickv; 
      Float_t ibheight; 
      Float_t ibwidth; 
      Float_t ibwlen; 
      Float_t ibwhghtf; 
      Float_t ibwhghtb; 
      Float_t ewshift; 
      Float_t udshift; 
      Float_t boltshift; 
      Vpdg_t() : AgStructure("Vpdg_t","User-defined AgML structure") 
      { 
         version=0; 
         zposeast=0; 
         zposwest=0; 
         rmin=0; 
         rmax=0; 
         bpwidth=0; 
         bplength=0; 
         bpthick=0; 
         bxheight=0; 
         bxlength=0; 
         bxwidth=0; 
         bxthick=0; 
         bxzposc=0; 
         fpwidth=0; 
         fpheight=0; 
         fpthick=0; 
         fpawidth=0; 
         fpahght=0; 
         fpchght=0; 
         fphhght=0; 
         fphwidth=0; 
         fphthick=0; 
         stthick=0; 
         stheight=0; 
         stangle=0; 
         stdiagsz=0; 
         scwidth=0; 
         scheight=0; 
         sclength=0; 
         scthick=0; 
         clheight=0; 
         clwidth=0; 
         cllength=0; 
         clthick=0; 
         detlen=0; 
         detrad=0; 
         detfront=0; 
         convthk=0; 
         radithk=0; 
         eleleng=0; 
         drlayer=0; 
         numpmt=0; 
         pmtwall=0; 
         pmtrad=0; 
         pmtlen=0; 
         ibchoice=0; 
         ibposyc=0; 
         ibposzc=0; 
         ibleng=0; 
         ibthickh=0; 
         ibthickv=0; 
         ibheight=0; 
         ibwidth=0; 
         ibwlen=0; 
         ibwhghtf=0; 
         ibwhghtb=0; 
         ewshift=0; 
         udshift=0; 
         boltshift=0; 
         _index=0; 
      } 
      ~ Vpdg_t(){ /* nada */ }; 
   }; 
   class Vpdh_t : public AgStructure 
   { 
      ClassDef(Vpdh_t,1); 
      public: 
      Float_t version; 
      Float_t zposeast; 
      Float_t zposwest; 
      Float_t rmin; 
      Float_t rmax; 
      Float_t length; 
      Float_t detrad; 
      Float_t detlen; 
      Float_t pmtrad; 
      Float_t pmtlen; 
      Float_t detwall; 
      Float_t detfront; 
      Float_t leadthick; 
      Float_t scintthick; 
      Float_t ring1_ndet; 
      Float_t ring1_rad; 
      Float_t ring1_phi0; 
      Float_t ring1_dphi; 
      Float_t ring1_kproj; 
      Float_t ring2_ndet; 
      Float_t ring2_rad; 
      Float_t ring2_phi0; 
      Float_t ring2_dphi; 
      Float_t ring2_kproj; 
      Float_t ring3_ndet; 
      Float_t ring3_rad; 
      Float_t ring3_phi0; 
      Float_t ring3_dphi; 
      Float_t ring3_kproj; 
      Float_t ewshift; 
      Float_t udshift; 
      Float_t boltshift; 
      Vpdh_t() : AgStructure("Vpdh_t","User-defined AgML structure") 
      { 
         version=0; 
         zposeast=0; 
         zposwest=0; 
         rmin=0; 
         rmax=0; 
         length=0; 
         detrad=0; 
         detlen=0; 
         pmtrad=0; 
         pmtlen=0; 
         detwall=0; 
         detfront=0; 
         leadthick=0; 
         scintthick=0; 
         ring1_ndet=0; 
         ring1_rad=0; 
         ring1_phi0=0; 
         ring1_dphi=0; 
         ring1_kproj=0; 
         ring2_ndet=0; 
         ring2_rad=0; 
         ring2_phi0=0; 
         ring2_dphi=0; 
         ring2_kproj=0; 
         ring3_ndet=0; 
         ring3_rad=0; 
         ring3_phi0=0; 
         ring3_dphi=0; 
         ring3_kproj=0; 
         ewshift=0; 
         udshift=0; 
         boltshift=0; 
         _index=0; 
      } 
      ~ Vpdh_t(){ /* nada */ }; 
   }; 
   class Vpds_t : public AgStructure 
   { 
      ClassDef(Vpds_t,1); 
      public: 
      Float_t version; 
      Float_t ibsazc; 
      Float_t ibsayc; 
      Float_t ibsaxc; 
      Float_t ibsbzc; 
      Float_t ibsbyc; 
      Float_t ibsbxc; 
      Float_t ibsczc; 
      Float_t ibscyc; 
      Float_t ibscxc; 
      Float_t ibsdzc1; 
      Float_t ibsdzc2; 
      Float_t ibsdyc1; 
      Float_t ibsdyc2; 
      Float_t ibsdxc; 
      Float_t ibsezc1; 
      Float_t ibsezc2; 
      Float_t ibseyc; 
      Float_t ibsexc; 
      Float_t ibsfzc; 
      Float_t ibsfyc; 
      Float_t ibsfxc; 
      Float_t ibsgzc1; 
      Float_t ibsgzc2; 
      Float_t ibsgzc3; 
      Float_t ibsgyc; 
      Float_t ibsgxc; 
      Float_t ibshzc1; 
      Float_t ibshzc2; 
      Float_t ibshyc; 
      Float_t ibshxc1; 
      Float_t ibshxc2; 
      Float_t bsalenx; 
      Float_t bsaleny; 
      Float_t bsalenz; 
      Float_t baalenz; 
      Float_t bsbleny; 
      Float_t bsclenx; 
      Float_t bscleny; 
      Float_t bsclenz; 
      Float_t baclenz; 
      Float_t bsdlenx; 
      Float_t bseleny; 
      Float_t bselenz; 
      Float_t bsfrmax; 
      Float_t bsflenz; 
      Float_t bsslenz; 
      Float_t bsgrmax; 
      Float_t bsglenz1; 
      Float_t bsglenz2; 
      Float_t bsglenz3; 
      Float_t bshlenz; 
      Float_t elecwid; 
      Float_t electhck; 
      Float_t eleclen; 
      Float_t vfeeposx; 
      Float_t vfeeposy; 
      Float_t vfeeposz; 
      Array_t<Float_t> vlemposx; 
      Float_t vlemposy; 
      Array_t<Float_t> vlemposz; 
      Float_t vlemlenx; 
      Float_t vlemleny; 
      Float_t vlemlenz; 
      Float_t vpipposx; 
      Float_t vpipposy; 
      Float_t vpipposz; 
      Float_t vpiprmin; 
      Float_t vpiprmax; 
      Float_t vpiplenz; 
      Vpds_t() : AgStructure("Vpds_t","User-defined AgML structure") 
      { 
         version=0; 
         ibsazc=0; 
         ibsayc=0; 
         ibsaxc=0; 
         ibsbzc=0; 
         ibsbyc=0; 
         ibsbxc=0; 
         ibsczc=0; 
         ibscyc=0; 
         ibscxc=0; 
         ibsdzc1=0; 
         ibsdzc2=0; 
         ibsdyc1=0; 
         ibsdyc2=0; 
         ibsdxc=0; 
         ibsezc1=0; 
         ibsezc2=0; 
         ibseyc=0; 
         ibsexc=0; 
         ibsfzc=0; 
         ibsfyc=0; 
         ibsfxc=0; 
         ibsgzc1=0; 
         ibsgzc2=0; 
         ibsgzc3=0; 
         ibsgyc=0; 
         ibsgxc=0; 
         ibshzc1=0; 
         ibshzc2=0; 
         ibshyc=0; 
         ibshxc1=0; 
         ibshxc2=0; 
         bsalenx=0; 
         bsaleny=0; 
         bsalenz=0; 
         baalenz=0; 
         bsbleny=0; 
         bsclenx=0; 
         bscleny=0; 
         bsclenz=0; 
         baclenz=0; 
         bsdlenx=0; 
         bseleny=0; 
         bselenz=0; 
         bsfrmax=0; 
         bsflenz=0; 
         bsslenz=0; 
         bsgrmax=0; 
         bsglenz1=0; 
         bsglenz2=0; 
         bsglenz3=0; 
         bshlenz=0; 
         elecwid=0; 
         electhck=0; 
         eleclen=0; 
         vfeeposx=0; 
         vfeeposy=0; 
         vfeeposz=0; 
         vlemposx = Array_t<Float_t>(15); 
         vlemposy=0; 
         vlemposz = Array_t<Float_t>(15); 
         vlemlenx=0; 
         vlemleny=0; 
         vlemlenz=0; 
         vpipposx=0; 
         vpipposy=0; 
         vpipposz=0; 
         vpiprmin=0; 
         vpiprmax=0; 
         vpiplenz=0; 
         _index=0; 
      } 
      ~ Vpds_t(){ /* nada */ }; 
   }; 
   // ---------------------------------------------------------------------- VPDD -- 
   ///@defgroup VPDD_doc 
   ///@class VPDD 
   ///@brief is the whole VPPD assembly 
   class VPDD : public AgBlock 
   {  public: 
      VPDD() : AgBlock("VPDD","is the whole VPPD assembly"){ }; 
      ~VPDD(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(VPDD,1); 
   }; 
   // ---------------------------------------------------------------------- VPBP -- 
   ///@defgroup VPBP_doc 
   ///@class VPBP 
   ///@brief is the Base Plate 
   class VPBP : public AgBlock 
   {  public: 
      VPBP() : AgBlock("VPBP","is the Base Plate"){ }; 
      ~VPBP(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(VPBP,1); 
   }; 
   // ---------------------------------------------------------------------- VPBO -- 
   ///@defgroup VPBO_doc 
   ///@class VPBO 
   ///@brief is container for the hook 
   class VPBO : public AgBlock 
   {  public: 
      VPBO() : AgBlock("VPBO","is container for the hook"){ }; 
      ~VPBO(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(VPBO,1); 
   }; 
   // ---------------------------------------------------------------------- VPBA -- 
   ///@defgroup VPBA_doc 
   ///@class VPBA 
   ///@brief is the part of the hook that mounts to the front/back plate 
   class VPBA : public AgBlock 
   {  public: 
      VPBA() : AgBlock("VPBA","is the part of the hook that mounts to the front/back plate"){ }; 
      ~VPBA(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(VPBA,1); 
   }; 
   // ---------------------------------------------------------------------- VPBB -- 
   ///@defgroup VPBB_doc 
   ///@class VPBB 
   ///@brief is the part of the hook that mounts to the base plate 
   class VPBB : public AgBlock 
   {  public: 
      VPBB() : AgBlock("VPBB","is the part of the hook that mounts to the base plate"){ }; 
      ~VPBB(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(VPBB,1); 
   }; 
   // ---------------------------------------------------------------------- VPFP -- 
   ///@defgroup VPFP_doc 
   ///@class VPFP 
   ///@brief is a single rectangular piece of the frontpanel 
   class VPFP : public AgBlock 
   {  public: 
      VPFP() : AgBlock("VPFP","is a single rectangular piece of the frontpanel"){ }; 
      ~VPFP(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(VPFP,1); 
   }; 
   // ---------------------------------------------------------------------- VPFA -- 
   ///@defgroup VPFA_doc 
   ///@class VPFA 
   ///@brief is the central upper part of the frontplate 
   class VPFA : public AgBlock 
   {  public: 
      VPFA() : AgBlock("VPFA","is the central upper part of the frontplate"){ }; 
      ~VPFA(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(VPFA,1); 
   }; 
   // ---------------------------------------------------------------------- VPFB -- 
   ///@defgroup VPFB_doc 
   ///@class VPFB 
   ///@brief is the middle upper part of the frontplate 
   class VPFB : public AgBlock 
   {  public: 
      VPFB() : AgBlock("VPFB","is the middle upper part of the frontplate"){ }; 
      ~VPFB(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(VPFB,1); 
   }; 
   // ---------------------------------------------------------------------- VPFC -- 
   ///@defgroup VPFC_doc 
   ///@class VPFC 
   ///@brief is the outer upper part of the frontplate 
   class VPFC : public AgBlock 
   {  public: 
      VPFC() : AgBlock("VPFC","is the outer upper part of the frontplate"){ }; 
      ~VPFC(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(VPFC,1); 
   }; 
   // ---------------------------------------------------------------------- VPST -- 
   ///@defgroup VPST_doc 
   ///@class VPST 
   ///@brief is the strut volume 
   class VPST : public AgBlock 
   {  public: 
      VPST() : AgBlock("VPST","is the strut volume"){ }; 
      ~VPST(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(VPST,1); 
   }; 
   // ---------------------------------------------------------------------- VPSV -- 
   ///@defgroup VPSV_doc 
   ///@class VPSV 
   ///@brief is the actual strut between front and backplates 
   class VPSV : public AgBlock 
   {  public: 
      VPSV() : AgBlock("VPSV","is the actual strut between front and backplates"){ }; 
      ~VPSV(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(VPSV,1); 
   }; 
   // ---------------------------------------------------------------------- VPSW -- 
   ///@defgroup VPSW_doc 
   ///@class VPSW 
   ///@brief is a tiny piece of aluminium that belongs to the strut 
   class VPSW : public AgBlock 
   {  public: 
      VPSW() : AgBlock("VPSW","is a tiny piece of aluminium that belongs to the strut"){ }; 
      ~VPSW(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(VPSW,1); 
   }; 
   // ---------------------------------------------------------------------- VPSC -- 
   ///@defgroup VPSC_doc 
   ///@class VPSC 
   ///@brief is a clamp that holds the strut 
   class VPSC : public AgBlock 
   {  public: 
      VPSC() : AgBlock("VPSC","is a clamp that holds the strut"){ }; 
      ~VPSC(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(VPSC,1); 
   }; 
   // ---------------------------------------------------------------------- VPSA -- 
   ///@defgroup VPSA_doc 
   ///@class VPSA 
   ///@brief is part of a strut clamp that holds to the frontplate 
   class VPSA : public AgBlock 
   {  public: 
      VPSA() : AgBlock("VPSA","is part of a strut clamp that holds to the frontplate"){ }; 
      ~VPSA(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(VPSA,1); 
   }; 
   // ---------------------------------------------------------------------- VPSB -- 
   ///@defgroup VPSB_doc 
   ///@class VPSB 
   ///@brief is part of a strut clamp that holds to the strut 
   class VPSB : public AgBlock 
   {  public: 
      VPSB() : AgBlock("VPSB","is part of a strut clamp that holds to the strut"){ }; 
      ~VPSB(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(VPSB,1); 
   }; 
   // ---------------------------------------------------------------------- VPBX -- 
   ///@defgroup VPBX_doc 
   ///@class VPBX 
   ///@brief is the FEE box 
   class VPBX : public AgBlock 
   {  public: 
      VPBX() : AgBlock("VPBX","is the FEE box"){ }; 
      ~VPBX(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(VPBX,1); 
   }; 
   // ---------------------------------------------------------------------- VPBI -- 
   ///@defgroup VPBI_doc 
   ///@class VPBI 
   ///@brief is the empty space inside of the FEE box 
   class VPBI : public AgBlock 
   {  public: 
      VPBI() : AgBlock("VPBI","is the empty space inside of the FEE box"){ }; 
      ~VPBI(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(VPBI,1); 
   }; 
   // ---------------------------------------------------------------------- VFEE -- 
   ///@defgroup VFEE_doc 
   ///@class VFEE 
   ///@brief is the FEE inside the box 
   class VFEE : public AgBlock 
   {  public: 
      VFEE() : AgBlock("VFEE","is the FEE inside the box"){ }; 
      ~VFEE(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(VFEE,1); 
   }; 
   // ---------------------------------------------------------------------- VLEM -- 
   ///@defgroup VLEM_doc 
   ///@class VLEM 
   ///@brief is a Lemo connector on the FEE boards 
   class VLEM : public AgBlock 
   {  public: 
      VLEM() : AgBlock("VLEM","is a Lemo connector on the FEE boards"){ }; 
      ~VLEM(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(VLEM,1); 
   }; 
   // ---------------------------------------------------------------------- VPIP -- 
   ///@defgroup VPIP_doc 
   ///@class VPIP 
   ///@brief is the Long Pipe 
   class VPIP : public AgBlock 
   {  public: 
      VPIP() : AgBlock("VPIP","is the Long Pipe"){ }; 
      ~VPIP(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(VPIP,1); 
   }; 
   // ---------------------------------------------------------------------- VRNG -- 
   ///@defgroup VRNG_doc 
   ///@class VRNG 
   ///@brief is a single pVPD Ring or the entire upVPD 
   class VRNG : public AgBlock 
   {  public: 
      VRNG() : AgBlock("VRNG","is a single pVPD Ring or the entire upVPD"){ }; 
      ~VRNG(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(VRNG,1); 
   }; 
   // ---------------------------------------------------------------------- VSEC -- 
   ///@defgroup VSEC_doc 
   ///@class VSEC 
   ///@brief is one pVPD sector with all stuff inside 
   class VSEC : public AgBlock 
   {  public: 
      VSEC() : AgBlock("VSEC","is one pVPD sector with all stuff inside"){ }; 
      ~VSEC(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(VSEC,1); 
   }; 
   // ---------------------------------------------------------------------- VDET -- 
   ///@defgroup VDET_doc 
   ///@class VDET 
   ///@brief is a single detector (Radiator+converter and PMT+electroncs) 
   class VDET : public AgBlock 
   {  public: 
      VDET() : AgBlock("VDET","is a single detector (Radiator+converter and PMT+electroncs)"){ }; 
      ~VDET(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(VDET,1); 
   }; 
   // ---------------------------------------------------------------------- VDTI -- 
   ///@defgroup VDTI_doc 
   ///@class VDTI 
   ///@brief is inner part of the single detector 
   class VDTI : public AgBlock 
   {  public: 
      VDTI() : AgBlock("VDTI","is inner part of the single detector"){ }; 
      ~VDTI(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(VDTI,1); 
   }; 
   // ---------------------------------------------------------------------- VCNV -- 
   ///@defgroup VCNV_doc 
   ///@class VCNV 
   ///@brief is converter layer (radiator included) 
   class VCNV : public AgBlock 
   {  public: 
      VCNV() : AgBlock("VCNV","is converter layer (radiator included)"){ }; 
      ~VCNV(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(VCNV,1); 
   }; 
   // ---------------------------------------------------------------------- VRAD -- 
   ///@defgroup VRAD_doc 
   ///@class VRAD 
   ///@brief is light-producing layer (scintillator or quartz) 
   class VRAD : public AgBlock 
   {  public: 
      VRAD() : AgBlock("VRAD","is light-producing layer (scintillator or quartz)"){ }; 
      ~VRAD(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(VRAD,1); 
   }; 
   // ---------------------------------------------------------------------- VPMT -- 
   ///@defgroup VPMT_doc 
   ///@class VPMT 
   ///@brief is the PMT inner volume 
   class VPMT : public AgBlock 
   {  public: 
      VPMT() : AgBlock("VPMT","is the PMT inner volume"){ }; 
      ~VPMT(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(VPMT,1); 
   }; 
   // ---------------------------------------------------------------------- VXST -- 
   ///@defgroup VXST_doc 
   ///@class VXST 
   ///@brief are PMT output cables (just to look nicer) 
   class VXST : public AgBlock 
   {  public: 
      VXST() : AgBlock("VXST","are PMT output cables (just to look nicer)"){ }; 
      ~VXST(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(VXST,1); 
   }; 
   // ---------------------------------------------------------------------- VPCL -- 
   ///@defgroup VPCL_doc 
   ///@class VPCL 
   ///@brief is the boat clamp 
   class VPCL : public AgBlock 
   {  public: 
      VPCL() : AgBlock("VPCL","is the boat clamp"){ }; 
      ~VPCL(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(VPCL,1); 
   }; 
   // ---------------------------------------------------------------------- VPCF -- 
   ///@defgroup VPCF_doc 
   ///@class VPCF 
   ///@brief is the front plate of the boat clamp 
   class VPCF : public AgBlock 
   {  public: 
      VPCF() : AgBlock("VPCF","is the front plate of the boat clamp"){ }; 
      ~VPCF(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(VPCF,1); 
   }; 
   // ---------------------------------------------------------------------- VPCH -- 
   ///@defgroup VPCH_doc 
   ///@class VPCH 
   ///@brief is the horizontal plate of the boat clamp 
   class VPCH : public AgBlock 
   {  public: 
      VPCH() : AgBlock("VPCH","is the horizontal plate of the boat clamp"){ }; 
      ~VPCH(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(VPCH,1); 
   }; 
   // ---------------------------------------------------------------------- VPCV -- 
   ///@defgroup VPCV_doc 
   ///@class VPCV 
   ///@brief is the vertical plate of the boat clamp 
   class VPCV : public AgBlock 
   {  public: 
      VPCV() : AgBlock("VPCV","is the vertical plate of the boat clamp"){ }; 
      ~VPCV(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(VPCV,1); 
   }; 
   // ---------------------------------------------------------------------- IBEM -- 
   ///@defgroup IBEM_doc 
   ///@class IBEM 
   ///@brief is the IBeam structure beneath the Bell reducer cone 
   class IBEM : public AgBlock 
   {  public: 
      IBEM() : AgBlock("IBEM","is the IBeam structure beneath the Bell reducer cone"){ }; 
      ~IBEM(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(IBEM,1); 
   }; 
   // ---------------------------------------------------------------------- IBEH -- 
   ///@defgroup IBEH_doc 
   ///@class IBEH 
   ///@brief is a horizontal IBeam plate 
   class IBEH : public AgBlock 
   {  public: 
      IBEH() : AgBlock("IBEH","is a horizontal IBeam plate"){ }; 
      ~IBEH(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(IBEH,1); 
   }; 
   // ---------------------------------------------------------------------- IBEV -- 
   ///@defgroup IBEV_doc 
   ///@class IBEV 
   ///@brief is a vertical IBeam plate 
   class IBEV : public AgBlock 
   {  public: 
      IBEV() : AgBlock("IBEV","is a vertical IBeam plate"){ }; 
      ~IBEV(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(IBEV,1); 
   }; 
   // ---------------------------------------------------------------------- IBEW -- 
   ///@defgroup IBEW_doc 
   ///@class IBEW 
   ///@brief is the first part of the IBeam plate 
   class IBEW : public AgBlock 
   {  public: 
      IBEW() : AgBlock("IBEW","is the first part of the IBeam plate"){ }; 
      ~IBEW(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(IBEW,1); 
   }; 
   // ---------------------------------------------------------------------- IBSA -- 
   ///@defgroup IBSA_doc 
   ///@class IBSA 
   ///@brief is the vertical post on the balcony (Envelope) 
   class IBSA : public AgBlock 
   {  public: 
      IBSA() : AgBlock("IBSA","is the vertical post on the balcony (Envelope)"){ }; 
      ~IBSA(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(IBSA,1); 
   }; 
   // ---------------------------------------------------------------------- IBAA -- 
   ///@defgroup IBAA_doc 
   ///@class IBAA 
   ///@brief is the vertical post on the balcony (Aluminum) 
   class IBAA : public AgBlock 
   {  public: 
      IBAA() : AgBlock("IBAA","is the vertical post on the balcony (Aluminum)"){ }; 
      ~IBAA(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(IBAA,1); 
   }; 
   // ---------------------------------------------------------------------- IBSB -- 
   ///@defgroup IBSB_doc 
   ///@class IBSB 
   ///@brief is the diagonal post from the balcony (Envelope) 
   class IBSB : public AgBlock 
   {  public: 
      IBSB() : AgBlock("IBSB","is the diagonal post from the balcony (Envelope)"){ }; 
      ~IBSB(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(IBSB,1); 
   }; 
   // ---------------------------------------------------------------------- IBAB -- 
   ///@defgroup IBAB_doc 
   ///@class IBAB 
   ///@brief is the diagonal post from the balcony (Aluminum) 
   class IBAB : public AgBlock 
   {  public: 
      IBAB() : AgBlock("IBAB","is the diagonal post from the balcony (Aluminum)"){ }; 
      ~IBAB(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(IBAB,1); 
   }; 
   // ---------------------------------------------------------------------- IBSC -- 
   ///@defgroup IBSC_doc 
   ///@class IBSC 
   ///@brief is the cross post below the I-Beam (Envelope) 
   class IBSC : public AgBlock 
   {  public: 
      IBSC() : AgBlock("IBSC","is the cross post below the I-Beam (Envelope)"){ }; 
      ~IBSC(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(IBSC,1); 
   }; 
   // ---------------------------------------------------------------------- IBAC -- 
   ///@defgroup IBAC_doc 
   ///@class IBAC 
   ///@brief is vertical parts of the cross post below the I-Beam (Aluminum) 
   class IBAC : public AgBlock 
   {  public: 
      IBAC() : AgBlock("IBAC","is vertical parts of the cross post below the I-Beam (Aluminum)"){ }; 
      ~IBAC(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(IBAC,1); 
   }; 
   // ---------------------------------------------------------------------- IBBC -- 
   ///@defgroup IBBC_doc 
   ///@class IBBC 
   ///@brief is the horizontal part of the cross post below the I-Beam (Aluminum) 
   class IBBC : public AgBlock 
   {  public: 
      IBBC() : AgBlock("IBBC","is the horizontal part of the cross post below the I-Beam (Aluminum)"){ }; 
      ~IBBC(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(IBBC,1); 
   }; 
   // ---------------------------------------------------------------------- IBCC -- 
   ///@defgroup IBCC_doc 
   ///@class IBCC 
   ///@brief is the end caps on the cross post below the I-Beam (Aluminum) 
   class IBCC : public AgBlock 
   {  public: 
      IBCC() : AgBlock("IBCC","is the end caps on the cross post below the I-Beam (Aluminum)"){ }; 
      ~IBCC(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(IBCC,1); 
   }; 
   // ---------------------------------------------------------------------- IBSD -- 
   ///@defgroup IBSD_doc 
   ///@class IBSD 
   ///@brief are the horizontal plates that hold the pipe-support brackets (Aluminum) 
   class IBSD : public AgBlock 
   {  public: 
      IBSD() : AgBlock("IBSD","are the horizontal plates that hold the pipe-support brackets (Aluminum)"){ }; 
      ~IBSD(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(IBSD,1); 
   }; 
   // ---------------------------------------------------------------------- IBSE -- 
   ///@defgroup IBSE_doc 
   ///@class IBSE 
   ///@brief are the vertical parts of the pipe-support brackets (Aluminum) 
   class IBSE : public AgBlock 
   {  public: 
      IBSE() : AgBlock("IBSE","are the vertical parts of the pipe-support brackets (Aluminum)"){ }; 
      ~IBSE(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(IBSE,1); 
   }; 
   // ---------------------------------------------------------------------- IBSF -- 
   ///@defgroup IBSF_doc 
   ///@class IBSF 
   ///@brief are the long threaded rods for X-support of the I-beam 
   class IBSF : public AgBlock 
   {  public: 
      IBSF() : AgBlock("IBSF","are the long threaded rods for X-support of the I-beam"){ }; 
      ~IBSF(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(IBSF,1); 
   }; 
   // ---------------------------------------------------------------------- IBSS -- 
   ///@defgroup IBSS_doc 
   ///@class IBSS 
   ///@brief are the long threaded rods for X-support of the I-beam, short stubs inside IBEM;// 
   class IBSS : public AgBlock 
   {  public: 
      IBSS() : AgBlock("IBSS","are the long threaded rods for X-support of the I-beam, short stubs inside IBEM;//"){ }; 
      ~IBSS(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(IBSS,1); 
   }; 
   // ---------------------------------------------------------------------- IBSG -- 
   ///@defgroup IBSG_doc 
   ///@class IBSG 
   ///@brief are the vertical bolts to the pipe-support brackets 
   class IBSG : public AgBlock 
   {  public: 
      IBSG() : AgBlock("IBSG","are the vertical bolts to the pipe-support brackets"){ }; 
      ~IBSG(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(IBSG,1); 
   }; 
   // ---------------------------------------------------------------------- IBSH -- 
   ///@defgroup IBSH_doc 
   ///@class IBSH 
   ///@brief are the cross-bolts from the pipe-support brackets to the pipe 
   class IBSH : public AgBlock 
   {  public: 
      IBSH() : AgBlock("IBSH","are the cross-bolts from the pipe-support brackets to the pipe"){ }; 
      ~IBSH(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(IBSH,1); 
   }; 
   /// \class VpddGeo2 
   /// \brief   is the StartDet and pipe support hardware  
   class VpddGeo2 : public AgModule 
   { 
      public: 
      VpddGeo2(); 
      virtual void ConstructGeometry( const Char_t *dummy="" ); 
      ~VpddGeo2(){ }; 
      ClassDef(VpddGeo2,1); 
      public: 
   }; 
   // endElement in class Module 
}; // namespace VpddGeo2 
#endif // __VpddGeo2__ 
