#ifndef __BtofGeo1__ 
#define __BtofGeo1__ 
 
#include "StarVMC/StarAgmlLib/AgModule.h" 
 
namespace BTOFGEO1 // $NMSPC 
{ 
   class Btog_t : public AgStructure 
   { 
      ClassDef(Btog_t,1); 
      public: 
      Float_t version; 
      Float_t rmin; 
      Float_t rmax; 
      Float_t dz; 
      Float_t choice; 
      Float_t posit1; 
      Float_t posit2; 
      Btog_t() : AgStructure("Btog_t","User-defined AgML structure") 
      { 
         version=0; 
         rmin=0; 
         rmax=0; 
         dz=0; 
         choice=0; 
         posit1=0; 
         posit2=0; 
         _index=0; 
      } 
      ~ Btog_t(){ /* nada */ }; 
   }; 
   class Tray_t : public AgStructure 
   { 
      ClassDef(Tray_t,1); 
      public: 
      Float_t height; 
      Float_t width; 
      Float_t length; 
      Float_t wallthk; 
      Float_t supfullh; 
      Float_t supfullw; 
      Float_t suplen; 
      Float_t supbaset; 
      Float_t supbasew; 
      Float_t suparmt; 
      Float_t cooloutr; 
      Float_t coolinnr; 
      Float_t stript; 
      Float_t footinse; 
      Float_t footthk; 
      Float_t foot1len; 
      Float_t foot2thk; 
      Float_t foot3len; 
      Tray_t() : AgStructure("Tray_t","User-defined AgML structure") 
      { 
         height=0; 
         width=0; 
         length=0; 
         wallthk=0; 
         supfullh=0; 
         supfullw=0; 
         suplen=0; 
         supbaset=0; 
         supbasew=0; 
         suparmt=0; 
         cooloutr=0; 
         coolinnr=0; 
         stript=0; 
         footinse=0; 
         footthk=0; 
         foot1len=0; 
         foot2thk=0; 
         foot3len=0; 
         _index=0; 
      } 
      ~ Tray_t(){ /* nada */ }; 
   }; 
   class Ctbb_t : public AgStructure 
   { 
      ClassDef(Ctbb_t,1); 
      public: 
      Float_t slab1len; 
      Float_t slab2len; 
      Float_t slab1x; 
      Float_t slab2x; 
      Float_t slabthck; 
      Float_t slabwid; 
      Float_t convlen; 
      Float_t convwidm; 
      Float_t convthck; 
      Float_t pmtlen; 
      Float_t pmtmaxr; 
      Float_t pmtminr; 
      Float_t baselen; 
      Float_t basemaxr; 
      Float_t baseminr; 
      Float_t electhck; 
      Float_t wrap; 
      Float_t shim; 
      Ctbb_t() : AgStructure("Ctbb_t","User-defined AgML structure") 
      { 
         slab1len=0; 
         slab2len=0; 
         slab1x=0; 
         slab2x=0; 
         slabthck=0; 
         slabwid=0; 
         convlen=0; 
         convwidm=0; 
         convthck=0; 
         pmtlen=0; 
         pmtmaxr=0; 
         pmtminr=0; 
         baselen=0; 
         basemaxr=0; 
         baseminr=0; 
         electhck=0; 
         wrap=0; 
         shim=0; 
         _index=0; 
      } 
      ~ Ctbb_t(){ /* nada */ }; 
   }; 
   class Toff_t : public AgStructure 
   { 
      ClassDef(Toff_t,1); 
      public: 
      Float_t slat1len; 
      Float_t slat1z; 
      Float_t slatdz; 
      Float_t slatthck; 
      Float_t slatwid; 
      Float_t slatang; 
      Float_t pmtlen; 
      Float_t pmtmaxr; 
      Float_t pmtminr; 
      Float_t baselen; 
      Float_t basemaxr; 
      Float_t baseminr; 
      Float_t elecx; 
      Float_t elec1z; 
      Float_t elecdz; 
      Float_t electhck; 
      Float_t elecwid; 
      Float_t eleclen; 
      Float_t railthck; 
      Float_t railwid; 
      Float_t coolinnr; 
      Float_t cooloutr; 
      Toff_t() : AgStructure("Toff_t","User-defined AgML structure") 
      { 
         slat1len=0; 
         slat1z=0; 
         slatdz=0; 
         slatthck=0; 
         slatwid=0; 
         slatang=0; 
         pmtlen=0; 
         pmtmaxr=0; 
         pmtminr=0; 
         baselen=0; 
         basemaxr=0; 
         baseminr=0; 
         elecx=0; 
         elec1z=0; 
         elecdz=0; 
         electhck=0; 
         elecwid=0; 
         eleclen=0; 
         railthck=0; 
         railwid=0; 
         coolinnr=0; 
         cooloutr=0; 
         _index=0; 
      } 
      ~ Toff_t(){ /* nada */ }; 
   }; 
   // ---------------------------------------------------------------------- BTOF -- 
   ///@defgroup BTOF_doc 
   ///@class BTOF 
   ///@brief is the whole CTF system envelope 
   class BTOF : public AgBlock 
   {  public: 
      BTOF() : AgBlock("BTOF","is the whole CTF system envelope"){ }; 
      ~BTOF(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(BTOF,1); 
   }; 
   // ---------------------------------------------------------------------- BTOH -- 
   ///@defgroup BTOH_doc 
   ///@class BTOH 
   ///@brief is a half of trigger system (west-east) 
   class BTOH : public AgBlock 
   {  public: 
      BTOH() : AgBlock("BTOH","is a half of trigger system (west-east)"){ }; 
      ~BTOH(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(BTOH,1); 
   }; 
   // ---------------------------------------------------------------------- BSEC -- 
   ///@defgroup BSEC_doc 
   ///@class BSEC 
   ///@brief is a sector of CTB/TOF Trigger Barrel Scintillators 
   class BSEC : public AgBlock 
   {  public: 
      BSEC() : AgBlock("BSEC","is a sector of CTB/TOF Trigger Barrel Scintillators"){ }; 
      ~BSEC(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(BSEC,1); 
   }; 
   // ---------------------------------------------------------------------- BTRA -- 
   ///@defgroup BTRA_doc 
   ///@class BTRA 
   ///@brief is one full tray plus supporting structure for CTB/TOF 
   class BTRA : public AgBlock 
   {  public: 
      BTRA() : AgBlock("BTRA","is one full tray plus supporting structure for CTB/TOF"){ }; 
      ~BTRA(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(BTRA,1); 
   }; 
   // ---------------------------------------------------------------------- BXTR -- 
   ///@defgroup BXTR_doc 
   ///@class BXTR 
   ///@brief is a Main TRay covering box for CTB 
   class BXTR : public AgBlock 
   {  public: 
      BXTR() : AgBlock("BXTR","is a Main TRay covering box for CTB"){ }; 
      ~BXTR(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(BXTR,1); 
   }; 
   // ---------------------------------------------------------------------- BMTC -- 
   ///@defgroup BMTC_doc 
   ///@class BMTC 
   ///@brief is the Main Tray Cavity filled with MANY details for CTB 
   class BMTC : public AgBlock 
   {  public: 
      BMTC() : AgBlock("BMTC","is the Main Tray Cavity filled with MANY details for CTB"){ }; 
      ~BMTC(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(BMTC,1); 
   }; 
   // ---------------------------------------------------------------------- BTTC -- 
   ///@defgroup BTTC_doc 
   ///@class BTTC 
   ///@brief is the Main Tray Cavity filled with MANY details for TOF 
   class BTTC : public AgBlock 
   {  public: 
      BTTC() : AgBlock("BTTC","is the Main Tray Cavity filled with MANY details for TOF"){ }; 
      ~BTTC(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(BTTC,1); 
   }; 
   // ---------------------------------------------------------------------- BMTM -- 
   ///@defgroup BMTM_doc 
   ///@class BMTM 
   ///@brief is the Main Tray cavity divisions Mother volume for TOF 
   class BMTM : public AgBlock 
   {  public: 
      BMTM() : AgBlock("BMTM","is the Main Tray cavity divisions Mother volume for TOF"){ }; 
      ~BMTM(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(BMTM,1); 
   }; 
   // ---------------------------------------------------------------------- BMTD -- 
   ///@defgroup BMTD_doc 
   ///@class BMTD 
   ///@brief is a phi column of TOF Scintillators 
   class BMTD : public AgBlock 
   {  public: 
      BMTD() : AgBlock("BMTD","is a phi column of TOF Scintillators"){ }; 
      ~BMTD(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(BMTD,1); 
   }; 
   // ---------------------------------------------------------------------- BASS -- 
   ///@defgroup BASS_doc 
   ///@class BASS 
   ///@brief is a single TOF Slat Assembly (slat+PMT+base) 
   class BASS : public AgBlock 
   {  public: 
      BASS() : AgBlock("BASS","is a single TOF Slat Assembly (slat+PMT+base)"){ }; 
      ~BASS(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(BASS,1); 
   }; 
   // ---------------------------------------------------------------------- BXSA -- 
   ///@defgroup BXSA_doc 
   ///@class BXSA 
   ///@brief is the active trigger scintillator SLAB for ctb 
   class BXSA : public AgBlock 
   {  public: 
      BXSA() : AgBlock("BXSA","is the active trigger scintillator SLAB for ctb"){ }; 
      ~BXSA(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(BXSA,1); 
   }; 
   // ---------------------------------------------------------------------- BCSB -- 
   ///@defgroup BCSB_doc 
   ///@class BCSB 
   ///@brief is the active trigger scintillator SLAB for tof 
   class BCSB : public AgBlock 
   {  public: 
      BCSB() : AgBlock("BCSB","is the active trigger scintillator SLAB for tof"){ }; 
      ~BCSB(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(BCSB,1); 
   }; 
   // ---------------------------------------------------------------------- BCCV -- 
   ///@defgroup BCCV_doc 
   ///@class BCCV 
   ///@brief is a Ctb optical ConVerter 
   class BCCV : public AgBlock 
   {  public: 
      BCCV() : AgBlock("BCCV","is a Ctb optical ConVerter"){ }; 
      ~BCCV(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(BCCV,1); 
   }; 
   // ---------------------------------------------------------------------- BCPM -- 
   ///@defgroup BCPM_doc 
   ///@class BCPM 
   ///@brief is a PhotoMultiplier Tube (same for CTB and TOF) 
   class BCPM : public AgBlock 
   {  public: 
      BCPM() : AgBlock("BCPM","is a PhotoMultiplier Tube (same for CTB and TOF)"){ }; 
      ~BCPM(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(BCPM,1); 
   }; 
   // ---------------------------------------------------------------------- BCSK -- 
   ///@defgroup BCSK_doc 
   ///@class BCSK 
   ///@brief is a CTB Linear Base tube 
   class BCSK : public AgBlock 
   {  public: 
      BCSK() : AgBlock("BCSK","is a CTB Linear Base tube"){ }; 
      ~BCSK(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(BCSK,1); 
   }; 
   // ---------------------------------------------------------------------- BTSK -- 
   ///@defgroup BTSK_doc 
   ///@class BTSK 
   ///@brief is the outer shell of a TOF CW Base 
   class BTSK : public AgBlock 
   {  public: 
      BTSK() : AgBlock("BTSK","is the outer shell of a TOF CW Base"){ }; 
      ~BTSK(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(BTSK,1); 
   }; 
   // ---------------------------------------------------------------------- BZEL -- 
   ///@defgroup BZEL_doc 
   ///@class BZEL 
   ///@brief is a Ctb PM electronics 
   class BZEL : public AgBlock 
   {  public: 
      BZEL() : AgBlock("BZEL","is a Ctb PM electronics"){ }; 
      ~BZEL(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(BZEL,1); 
   }; 
   // ---------------------------------------------------------------------- BCEL -- 
   ///@defgroup BCEL_doc 
   ///@class BCEL 
   ///@brief is a G10 board in the CW Base for TOF 
   class BCEL : public AgBlock 
   {  public: 
      BCEL() : AgBlock("BCEL","is a G10 board in the CW Base for TOF"){ }; 
      ~BCEL(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(BCEL,1); 
   }; 
   // ---------------------------------------------------------------------- BFEE -- 
   ///@defgroup BFEE_doc 
   ///@class BFEE 
   ///@brief is a G10 discriminator/CW control board for TOF 
   class BFEE : public AgBlock 
   {  public: 
      BFEE() : AgBlock("BFEE","is a G10 discriminator/CW control board for TOF"){ }; 
      ~BFEE(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(BFEE,1); 
   }; 
   // ---------------------------------------------------------------------- BCOO -- 
   ///@defgroup BCOO_doc 
   ///@class BCOO 
   ///@brief are the cooling rails/loops 
   class BCOO : public AgBlock 
   {  public: 
      BCOO() : AgBlock("BCOO","are the cooling rails/loops"){ }; 
      ~BCOO(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(BCOO,1); 
   }; 
   // ---------------------------------------------------------------------- BRAI -- 
   ///@defgroup BRAI_doc 
   ///@class BRAI 
   ///@brief is the Rail for the cooling loop 
   class BRAI : public AgBlock 
   {  public: 
      BRAI() : AgBlock("BRAI","is the Rail for the cooling loop"){ }; 
      ~BRAI(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(BRAI,1); 
   }; 
   // ---------------------------------------------------------------------- BPIP -- 
   ///@defgroup BPIP_doc 
   ///@class BPIP 
   ///@brief is the Pipe for the cooling loop 
   class BPIP : public AgBlock 
   {  public: 
      BPIP() : AgBlock("BPIP","is the Pipe for the cooling loop"){ }; 
      ~BPIP(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(BPIP,1); 
   }; 
   // ---------------------------------------------------------------------- BUND -- 
   ///@defgroup BUND_doc 
   ///@class BUND 
   ///@brief is Undercarriage support tray - same both for CTB and TOF 
   class BUND : public AgBlock 
   {  public: 
      BUND() : AgBlock("BUND","is Undercarriage support tray - same both for CTB and TOF"){ }; 
      ~BUND(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(BUND,1); 
   }; 
   // ---------------------------------------------------------------------- BTFT -- 
   ///@defgroup BTFT_doc 
   ///@class BTFT 
   ///@brief is the Foot structure ( Material Aluminium ) 
   class BTFT : public AgBlock 
   {  public: 
      BTFT() : AgBlock("BTFT","is the Foot structure ( Material Aluminium )"){ }; 
      ~BTFT(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(BTFT,1); 
   }; 
   // ---------------------------------------------------------------------- BARM -- 
   ///@defgroup BARM_doc 
   ///@class BARM 
   ///@brief is a TPC cooling structure arm ( Material Aluminium ) 
   class BARM : public AgBlock 
   {  public: 
      BARM() : AgBlock("BARM","is a TPC cooling structure arm ( Material Aluminium )"){ }; 
      ~BARM(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(BARM,1); 
   }; 
   // ---------------------------------------------------------------------- BANG -- 
   ///@defgroup BANG_doc 
   ///@class BANG 
   ///@brief is an angled part of TPC cooling structure ( Aile ) 
   class BANG : public AgBlock 
   {  public: 
      BANG() : AgBlock("BANG","is an angled part of TPC cooling structure ( Aile )"){ }; 
      ~BANG(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(BANG,1); 
   }; 
   // ---------------------------------------------------------------------- BASE -- 
   ///@defgroup BASE_doc 
   ///@class BASE 
   ///@brief is a bottom of TPC coolant structure 
   class BASE : public AgBlock 
   {  public: 
      BASE() : AgBlock("BASE","is a bottom of TPC coolant structure"){ }; 
      ~BASE(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(BASE,1); 
   }; 
   // ---------------------------------------------------------------------- BCOV -- 
   ///@defgroup BCOV_doc 
   ///@class BCOV 
   ///@brief is a whole TPC cooling channel 
   class BCOV : public AgBlock 
   {  public: 
      BCOV() : AgBlock("BCOV","is a whole TPC cooling channel"){ }; 
      ~BCOV(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(BCOV,1); 
   }; 
   // ---------------------------------------------------------------------- BWAT -- 
   ///@defgroup BWAT_doc 
   ///@class BWAT 
   ///@brief is TPC cooling water 
   class BWAT : public AgBlock 
   {  public: 
      BWAT() : AgBlock("BWAT","is TPC cooling water"){ }; 
      ~BWAT(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(BWAT,1); 
   }; 
   /// \class BtofGeo1 
   /// \brief  is the Geometry of Barrel Trigger / Time Of Flight system   
   class BtofGeo1 : public AgModule 
   { 
      public: 
      BtofGeo1(); 
      virtual void ConstructGeometry( const Char_t *dummy="" ); 
      ~BtofGeo1(){ }; 
      ClassDef(BtofGeo1,1); 
      public: 
   }; 
   // endElement in class Module 
}; // namespace BtofGeo1 
#endif // __BtofGeo1__ 
