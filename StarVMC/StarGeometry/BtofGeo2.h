#ifndef __BtofGeo2__ 
#define __BtofGeo2__ 
 
#include "StarVMC/StarAgmlLib/AgModule.h" 
 
namespace BTOFGEO2 // $NMSPC 
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
      Array_t<Float_t> posit1; 
      Float_t posit2; 
      Btog_t() : AgStructure("Btog_t","User-defined AgML structure") 
      { 
         version=0; 
         rmin=0; 
         rmax=0; 
         dz=0; 
         choice=0; 
         posit1 = Array_t<Float_t>(2); 
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
      Float_t boxwidth; 
      Float_t slatlen; 
      Float_t slat01z; 
      Float_t slat02z; 
      Float_t slat03z; 
      Float_t slat04z; 
      Float_t slat05z; 
      Float_t slat06z; 
      Float_t slat07z; 
      Float_t slat08z; 
      Float_t slat09z; 
      Float_t slat10z; 
      Float_t slatthck; 
      Float_t slatwid; 
      Float_t slatang; 
      Float_t pmtlen; 
      Float_t pmtmaxr; 
      Float_t pmtminr; 
      Float_t baselen; 
      Float_t basemaxr; 
      Float_t baseminr; 
      Float_t socklen; 
      Float_t cellwid; 
      Float_t cellhgt; 
      Float_t elechgt; 
      Float_t electhck; 
      Float_t elecwid; 
      Float_t eleclen; 
      Float_t elec01z; 
      Float_t elec02z; 
      Float_t elec03z; 
      Float_t elec04z; 
      Float_t elec05z; 
      Float_t elec06z; 
      Float_t elec07z; 
      Float_t elec08z; 
      Float_t elec09z; 
      Float_t elec10z; 
      Float_t railthck; 
      Float_t railwid; 
      Float_t coolinnr; 
      Float_t cooloutr; 
      Toff_t() : AgStructure("Toff_t","User-defined AgML structure") 
      { 
         boxwidth=0; 
         slatlen=0; 
         slat01z=0; 
         slat02z=0; 
         slat03z=0; 
         slat04z=0; 
         slat05z=0; 
         slat06z=0; 
         slat07z=0; 
         slat08z=0; 
         slat09z=0; 
         slat10z=0; 
         slatthck=0; 
         slatwid=0; 
         slatang=0; 
         pmtlen=0; 
         pmtmaxr=0; 
         pmtminr=0; 
         baselen=0; 
         basemaxr=0; 
         baseminr=0; 
         socklen=0; 
         cellwid=0; 
         cellhgt=0; 
         elechgt=0; 
         electhck=0; 
         elecwid=0; 
         eleclen=0; 
         elec01z=0; 
         elec02z=0; 
         elec03z=0; 
         elec04z=0; 
         elec05z=0; 
         elec06z=0; 
         elec07z=0; 
         elec08z=0; 
         elec09z=0; 
         elec10z=0; 
         railthck=0; 
         railwid=0; 
         coolinnr=0; 
         cooloutr=0; 
         _index=0; 
      } 
      ~ Toff_t(){ /* nada */ }; 
   }; 
   class Modr_t : public AgStructure 
   { 
      ClassDef(Modr_t,1); 
      public: 
      Float_t height; 
      Float_t width; 
      Float_t length; 
      Float_t center; 
      Array_t<Float_t> mrpcx; 
      Array_t<Float_t> mrpcz; 
      Array_t<Float_t> mrpca; 
      Float_t hchgt; 
      Float_t hcwid; 
      Float_t hclen; 
      Float_t pcbhgt; 
      Float_t pcbwid; 
      Float_t pcblen; 
      Float_t myhgt; 
      Float_t mywid; 
      Float_t mylen; 
      Float_t grhgt; 
      Float_t grwid; 
      Float_t grlen; 
      Float_t oghgt; 
      Float_t ogwid; 
      Float_t oglen; 
      Float_t ighgt; 
      Float_t igwid; 
      Float_t iglen; 
      Float_t sprmin; 
      Float_t sprmax; 
      Float_t splen; 
      Float_t wgrmin; 
      Float_t wgrmax; 
      Float_t wglen; 
      Float_t feeh; 
      Float_t hbwid; 
      Float_t ngap; 
      Modr_t() : AgStructure("Modr_t","User-defined AgML structure") 
      { 
         height=0; 
         width=0; 
         length=0; 
         center=0; 
         mrpcx = Array_t<Float_t>(33); 
         mrpcz = Array_t<Float_t>(33); 
         mrpca = Array_t<Float_t>(33); 
         hchgt=0; 
         hcwid=0; 
         hclen=0; 
         pcbhgt=0; 
         pcbwid=0; 
         pcblen=0; 
         myhgt=0; 
         mywid=0; 
         mylen=0; 
         grhgt=0; 
         grwid=0; 
         grlen=0; 
         oghgt=0; 
         ogwid=0; 
         oglen=0; 
         ighgt=0; 
         igwid=0; 
         iglen=0; 
         sprmin=0; 
         sprmax=0; 
         splen=0; 
         wgrmin=0; 
         wgrmax=0; 
         wglen=0; 
         feeh=0; 
         hbwid=0; 
         ngap=0; 
         _index=0; 
      } 
      ~ Modr_t(){ /* nada */ }; 
   }; 
   class Mod4_t : public AgStructure 
   { 
      ClassDef(Mod4_t,1); 
      public: 
      Float_t height; 
      Float_t width; 
      Float_t length; 
      Float_t center; 
      Array_t<Float_t> mrpcx; 
      Array_t<Float_t> mrpcz; 
      Array_t<Float_t> mrpca; 
      Float_t hchgt; 
      Float_t hcwid; 
      Float_t hclen; 
      Float_t pcbhgt; 
      Float_t pcbwid; 
      Float_t pcblen; 
      Float_t myhgt; 
      Float_t mywid; 
      Float_t mylen; 
      Float_t grhgt; 
      Float_t grwid; 
      Float_t grlen; 
      Float_t oghgt; 
      Float_t ogwid; 
      Float_t oglen; 
      Float_t ighgt; 
      Float_t igwid; 
      Float_t iglen; 
      Float_t sprmin; 
      Float_t sprmax; 
      Float_t splen; 
      Float_t wgrmin; 
      Float_t wgrmax; 
      Float_t wglen; 
      Float_t feeh; 
      Float_t hbwid; 
      Float_t ngap; 
      Float_t trayedgez; 
      Mod4_t() : AgStructure("Mod4_t","User-defined AgML structure") 
      { 
         height=0; 
         width=0; 
         length=0; 
         center=0; 
         mrpcx = Array_t<Float_t>(32); 
         mrpcz = Array_t<Float_t>(32); 
         mrpca = Array_t<Float_t>(32); 
         hchgt=0; 
         hcwid=0; 
         hclen=0; 
         pcbhgt=0; 
         pcbwid=0; 
         pcblen=0; 
         myhgt=0; 
         mywid=0; 
         mylen=0; 
         grhgt=0; 
         grwid=0; 
         grlen=0; 
         oghgt=0; 
         ogwid=0; 
         oglen=0; 
         ighgt=0; 
         igwid=0; 
         iglen=0; 
         sprmin=0; 
         sprmax=0; 
         splen=0; 
         wgrmin=0; 
         wgrmax=0; 
         wglen=0; 
         feeh=0; 
         hbwid=0; 
         ngap=0; 
         trayedgez=0; 
         _index=0; 
      } 
      ~ Mod4_t(){ /* nada */ }; 
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
   // ---------------------------------------------------------------------- BRFE -- 
   ///@defgroup BRFE_doc 
   ///@class BRFE 
   ///@brief is the FEE of tofr (run-3) 
   class BRFE : public AgBlock 
   {  public: 
      BRFE() : AgBlock("BRFE","is the FEE of tofr (run-3)"){ }; 
      ~BRFE(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(BRFE,1); 
   }; 
   // ---------------------------------------------------------------------- BXTR -- 
   ///@defgroup BXTR_doc 
   ///@class BXTR 
   ///@brief is a Main TRay covering box for CTB or TOF 
   class BXTR : public AgBlock 
   {  public: 
      BXTR() : AgBlock("BXTR","is a Main TRay covering box for CTB or TOF"){ }; 
      ~BXTR(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(BXTR,1); 
   }; 
   // ---------------------------------------------------------------------- BMTC -- 
   ///@defgroup BMTC_doc 
   ///@class BMTC 
   ///@brief is the Main Tray Cavity filled with the details for CTB 
   class BMTC : public AgBlock 
   {  public: 
      BMTC() : AgBlock("BMTC","is the Main Tray Cavity filled with the details for CTB"){ }; 
      ~BMTC(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(BMTC,1); 
   }; 
   // ---------------------------------------------------------------------- BTTC -- 
   ///@defgroup BTTC_doc 
   ///@class BTTC 
   ///@brief is the Main Tray Cavity filled with the details for TOFp 
   class BTTC : public AgBlock 
   {  public: 
      BTTC() : AgBlock("BTTC","is the Main Tray Cavity filled with the details for TOFp"){ }; 
      ~BTTC(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(BTTC,1); 
   }; 
   // ---------------------------------------------------------------------- BUPC -- 
   ///@defgroup BUPC_doc 
   ///@class BUPC 
   ///@brief is the up pcb cover of tofr 
   class BUPC : public AgBlock 
   {  public: 
      BUPC() : AgBlock("BUPC","is the up pcb cover of tofr"){ }; 
      ~BUPC(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(BUPC,1); 
   }; 
   // ---------------------------------------------------------------------- BRTC -- 
   ///@defgroup BRTC_doc 
   ///@class BRTC 
   ///@brief is the Main Tray Cavity filled with the details for TOFr (run3 or run4) 
   class BRTC : public AgBlock 
   {  public: 
      BRTC() : AgBlock("BRTC","is the Main Tray Cavity filled with the details for TOFr (run3 or run4)"){ }; 
      ~BRTC(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(BRTC,1); 
   }; 
   // ---------------------------------------------------------------------- BGMT -- 
   ///@defgroup BGMT_doc 
   ///@class BGMT 
   ///@brief is the mixture gas box in tray that change the hc box into slim 
   class BGMT : public AgBlock 
   {  public: 
      BGMT() : AgBlock("BGMT","is the mixture gas box in tray that change the hc box into slim"){ }; 
      ~BGMT(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(BGMT,1); 
   }; 
   // ---------------------------------------------------------------------- BMAA -- 
   ///@defgroup BMAA_doc 
   ///@class BMAA 
   ///@brief is a b1ox for a 4wide AND 5wide phi column of TOF Scintillators 
   class BMAA : public AgBlock 
   {  public: 
      BMAA() : AgBlock("BMAA","is a b1ox for a 4wide AND 5wide phi column of TOF Scintillators"){ }; 
      ~BMAA(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(BMAA,1); 
   }; 
   // ---------------------------------------------------------------------- BMTD -- 
   ///@defgroup BMTD_doc 
   ///@class BMTD 
   ///@brief is a 5wide phi column of TOF Scintillators 
   class BMTD : public AgBlock 
   {  public: 
      BMTD() : AgBlock("BMTD","is a 5wide phi column of TOF Scintillators"){ }; 
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
   // ---------------------------------------------------------------------- BCEL -- 
   ///@defgroup BCEL_doc 
   ///@class BCEL 
   ///@brief is a circular G10 board in the CW Base for TOF 
   class BCEL : public AgBlock 
   {  public: 
      BCEL() : AgBlock("BCEL","is a circular G10 board in the CW Base for TOF"){ }; 
      ~BCEL(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(BCEL,1); 
   }; 
   // ---------------------------------------------------------------------- BCEB -- 
   ///@defgroup BCEB_doc 
   ///@class BCEB 
   ///@brief is a square G10 board in the CW Base for TOF 
   class BCEB : public AgBlock 
   {  public: 
      BCEB() : AgBlock("BCEB","is a square G10 board in the CW Base for TOF"){ }; 
      ~BCEB(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(BCEB,1); 
   }; 
   // ---------------------------------------------------------------------- BPLA -- 
   ///@defgroup BPLA_doc 
   ///@class BPLA 
   ///@brief is the plastic angle pieces that hold the upper foam supports... 
   class BPLA : public AgBlock 
   {  public: 
      BPLA() : AgBlock("BPLA","is the plastic angle pieces that hold the upper foam supports..."){ }; 
      ~BPLA(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(BPLA,1); 
   }; 
   // ---------------------------------------------------------------------- BCON -- 
   ///@defgroup BCON_doc 
   ///@class BCON 
   ///@brief is a generic plastic block for various connectors, foam-support-angles, etc...... 
   class BCON : public AgBlock 
   {  public: 
      BCON() : AgBlock("BCON","is a generic plastic block for various connectors, foam-support-angles, etc......"){ }; 
      ~BCON(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(BCON,1); 
   }; 
   // ---------------------------------------------------------------------- BFEE -- 
   ///@defgroup BFEE_doc 
   ///@class BFEE 
   ///@brief is a G10 FrontEndElectronics board for TOF 
   class BFEE : public AgBlock 
   {  public: 
      BFEE() : AgBlock("BFEE","is a G10 FrontEndElectronics board for TOF"){ }; 
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
   ///@brief is the Long Pipe for the cooling loop 
   class BPIP : public AgBlock 
   {  public: 
      BPIP() : AgBlock("BPIP","is the Long Pipe for the cooling loop"){ }; 
      ~BPIP(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(BPIP,1); 
   }; 
   // ---------------------------------------------------------------------- BPIQ -- 
   ///@defgroup BPIQ_doc 
   ///@class BPIQ 
   ///@brief is the Short Pipe for the cooling loop 
   class BPIQ : public AgBlock 
   {  public: 
      BPIQ() : AgBlock("BPIQ","is the Short Pipe for the cooling loop"){ }; 
      ~BPIQ(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(BPIQ,1); 
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
   // ---------------------------------------------------------------------- BRMD -- 
   ///@defgroup BRMD_doc 
   ///@class BRMD 
   ///@brief is a six channel module for TOFr 
   class BRMD : public AgBlock 
   {  public: 
      BRMD() : AgBlock("BRMD","is a six channel module for TOFr"){ }; 
      ~BRMD(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(BRMD,1); 
   }; 
   // ---------------------------------------------------------------------- BRHC -- 
   ///@defgroup BRHC_doc 
   ///@class BRHC 
   ///@brief is the HoneyComb in the TOFr module 
   class BRHC : public AgBlock 
   {  public: 
      BRHC() : AgBlock("BRHC","is the HoneyComb in the TOFr module"){ }; 
      ~BRHC(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(BRHC,1); 
   }; 
   // ---------------------------------------------------------------------- BRCB -- 
   ///@defgroup BRCB_doc 
   ///@class BRCB 
   ///@brief is the PCB in the TOFr module 
   class BRCB : public AgBlock 
   {  public: 
      BRCB() : AgBlock("BRCB","is the PCB in the TOFr module"){ }; 
      ~BRCB(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(BRCB,1); 
   }; 
   // ---------------------------------------------------------------------- BRMY -- 
   ///@defgroup BRMY_doc 
   ///@class BRMY 
   ///@brief is the MYlar in the TOFr module 
   class BRMY : public AgBlock 
   {  public: 
      BRMY() : AgBlock("BRMY","is the MYlar in the TOFr module"){ }; 
      ~BRMY(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(BRMY,1); 
   }; 
   // ---------------------------------------------------------------------- BRGR -- 
   ///@defgroup BRGR_doc 
   ///@class BRGR 
   ///@brief is the GRaphite in the TOFr module 
   class BRGR : public AgBlock 
   {  public: 
      BRGR() : AgBlock("BRGR","is the GRaphite in the TOFr module"){ }; 
      ~BRGR(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(BRGR,1); 
   }; 
   // ---------------------------------------------------------------------- BROG -- 
   ///@defgroup BROG_doc 
   ///@class BROG 
   ///@brief is the Outer Glass in the TOFr module 
   class BROG : public AgBlock 
   {  public: 
      BROG() : AgBlock("BROG","is the Outer Glass in the TOFr module"){ }; 
      ~BROG(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(BROG,1); 
   }; 
   // ---------------------------------------------------------------------- BRDT -- 
   ///@defgroup BRDT_doc 
   ///@class BRDT 
   ///@brief is the middle part (including innner glass and gas)in the MRPC 
   class BRDT : public AgBlock 
   {  public: 
      BRDT() : AgBlock("BRDT","is the middle part (including innner glass and gas)in the MRPC"){ }; 
      ~BRDT(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(BRDT,1); 
   }; 
   // ---------------------------------------------------------------------- BRIG -- 
   ///@defgroup BRIG_doc 
   ///@class BRIG 
   ///@brief is the Inner Glass in the TOFr module 
   class BRIG : public AgBlock 
   {  public: 
      BRIG() : AgBlock("BRIG","is the Inner Glass in the TOFr module"){ }; 
      ~BRIG(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(BRIG,1); 
   }; 
   // ---------------------------------------------------------------------- BRSG -- 
   ///@defgroup BRSG_doc 
   ///@class BRSG 
   ///@brief is the sensitive gas layer in the TOFr module 
   class BRSG : public AgBlock 
   {  public: 
      BRSG() : AgBlock("BRSG","is the sensitive gas layer in the TOFr module"){ }; 
      ~BRSG(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(BRSG,1); 
   }; 
   // ---------------------------------------------------------------------- BRWG -- 
   ///@defgroup BRWG_doc 
   ///@class BRWG 
   ///@brief is the WedGe(support) in the TOFr module 
   class BRWG : public AgBlock 
   {  public: 
      BRWG() : AgBlock("BRWG","is the WedGe(support) in the TOFr module"){ }; 
      ~BRWG(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(BRWG,1); 
   }; 
   /// \class BtofGeo2 
   /// \brief  is the Geometry of Barrel Trigger / Time Of Flight system   
   class BtofGeo2 : public AgModule 
   { 
      public: 
      BtofGeo2(); 
      virtual void ConstructGeometry( const Char_t *dummy="" ); 
      ~BtofGeo2(){ }; 
      ClassDef(BtofGeo2,1); 
      public: 
   }; 
   // endElement in class Module 
}; // namespace BtofGeo2 
#endif // __BtofGeo2__ 
