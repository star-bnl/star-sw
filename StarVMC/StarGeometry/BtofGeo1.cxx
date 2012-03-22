#include "BtofGeo1.h"  
 // ---------------------------------------------------------------------------------------------------  
 //  
 #include "StarVMC/StarAgmlLib/StarAgmlStacker.h"  
 //  
 #include "StarVMC/StarAgmlLib/AgMaterial.h"  
 #include "StarVMC/StarAgmlLib/AgMedium.h"  
 #include "StarVMC/StarAgmlLib/AgShape.h"  
 #include "StarVMC/StarAgmlLib/AgBlock.h"  
 #include "StarVMC/StarAgmlLib/AgMath.h"  
 #include "StarVMC/StarAgmlLib/AgSTAR.h"  
 //  
 #include "StarVMC/StarAgmlLib/Mortran.h"  
 #include "StarVMC/StarAgmlLib/AgMath.h"  
 #include <iostream>  
 #include <vector>  
 #include <map>  
 const Int_t _printlevel = 0;  
 #define LOG_PRINT if(_printlevel>0) std::cout << GetName() << " -Print- "  
 #define LOG_INFO  if(_printlevel>1) std::cout << GetName() << " -Info-  "  
 #define LOG_DEBUG if(_printlevel>2) std::cout << GetName() << " -Debug- "  
 #define LOG_WARN  if(_printlevel>3) std::cout << GetName() << " -Warn-  "  
 #define printf(fmt,...) LOG_PRINT << Form(fmt,##__VA_ARGS__) << std::endl;  
 #include "StarVMC/Geometry/Helpers.h"  
 //  
 namespace BTOFGEO1 // $NMSPC  
 {     
       //     
       // ---------------------------------------------------------------------------------------------------     
          //  -----------------------------------------------------     
          /// @defgroup btog_doc     
          /// \class Btog_t     
          /// \brief User-defined structure     
          ///                             
          /// AgML structure members:     
          ///                             
          ///Float_t version;     
          ///Float_t rmin;     
          ///Float_t rmax;     
          ///Float_t dz;     
          ///Float_t choice;     
          ///Float_t posit1;     
          ///Float_t posit2;     
          ///Int_t _index;     
          //     
          Btog_t btog;     
          //     
          //  -----------------------------------------------------     
          /// @defgroup tray_doc     
          /// \class Tray_t     
          /// \brief User-defined structure     
          ///                             
          /// AgML structure members:     
          ///                             
          ///Float_t height;     
          ///Float_t width;     
          ///Float_t length;     
          ///Float_t wallthk;     
          ///Float_t supfullh;     
          ///Float_t supfullw;     
          ///Float_t suplen;     
          ///Float_t supbaset;     
          ///Float_t supbasew;     
          ///Float_t suparmt;     
          ///Float_t cooloutr;     
          ///Float_t coolinnr;     
          ///Float_t stript;     
          ///Float_t footinse;     
          ///Float_t footthk;     
          ///Float_t foot1len;     
          ///Float_t foot2thk;     
          ///Float_t foot3len;     
          ///Int_t _index;     
          //     
          Tray_t tray;     
          //     
          //  -----------------------------------------------------     
          /// @defgroup ctbb_doc     
          /// \class Ctbb_t     
          /// \brief User-defined structure     
          ///                             
          /// AgML structure members:     
          ///                             
          ///Float_t slab1len;     
          ///Float_t slab2len;     
          ///Float_t slab1x;     
          ///Float_t slab2x;     
          ///Float_t slabthck;     
          ///Float_t slabwid;     
          ///Float_t convlen;     
          ///Float_t convwidm;     
          ///Float_t convthck;     
          ///Float_t pmtlen;     
          ///Float_t pmtmaxr;     
          ///Float_t pmtminr;     
          ///Float_t baselen;     
          ///Float_t basemaxr;     
          ///Float_t baseminr;     
          ///Float_t electhck;     
          ///Float_t wrap;     
          ///Float_t shim;     
          ///Int_t _index;     
          //     
          Ctbb_t ctbb;     
          //     
          //  -----------------------------------------------------     
          /// @defgroup toff_doc     
          /// \class Toff_t     
          /// \brief User-defined structure     
          ///                             
          /// AgML structure members:     
          ///                             
          ///Float_t slat1len;     
          ///Float_t slat1z;     
          ///Float_t slatdz;     
          ///Float_t slatthck;     
          ///Float_t slatwid;     
          ///Float_t slatang;     
          ///Float_t pmtlen;     
          ///Float_t pmtmaxr;     
          ///Float_t pmtminr;     
          ///Float_t baselen;     
          ///Float_t basemaxr;     
          ///Float_t baseminr;     
          ///Float_t elecx;     
          ///Float_t elec1z;     
          ///Float_t elecdz;     
          ///Float_t electhck;     
          ///Float_t elecwid;     
          ///Float_t eleclen;     
          ///Float_t railthck;     
          ///Float_t railwid;     
          ///Float_t coolinnr;     
          ///Float_t cooloutr;     
          ///Int_t _index;     
          //     
          Toff_t toff;     
          //     
          ///@addtogroup BtofGeo1_vars     
          ///@{        
                Float_t support_arm_width,support_arm_xpos,support_arm_ypos,support_aile_width,support_aile_ypos,xpos,ypos,zpos,totlen,zpbass,zpfee;        
                //        
                /// Float_t support_arm_width,support_arm_xpos,support_arm_ypos,support_aile_width,support_aile_ypos,xpos,ypos,zpos,totlen,zpbass,zpfee        
          ///@}     
          ///@addtogroup BtofGeo1_vars     
          ///@{        
                Int_t i,is,choice,tof,year;        
                //        
                /// Int_t i,is,choice,tof,year        
          ///@}     
       BtofGeo1::BtofGeo1()     
         : AgModule("BtofGeo1"," is the Geometry of Barrel Trigger / Time Of Flight system  ")     
       {        
       }     
          // ---------------------------------------------------------------------------------------------------     
          void BTOF::Block( AgCreate create )     
          {         
                ///@addtogroup BTOF_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("BTOF");              
                            attr.par("seen")=0;              
                            attr.par("colo")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material Air            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Air");              
                            _material = mat;              
                      }           
                      /// Medium Standard           
                      {  AgMedium med = AgMedium::CopyMedium("Standard");              
                            _medium = med;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=btog.rmin;              
                            shape.par("rmax")=btog.rmax;              
                            shape.par("dz")=btog.dz;              
                            /// Shape Tube rmin=btog.rmin rmax=btog.rmax dz=btog.dz               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_BTOF;              
                            _stacker -> Build(this);              
                      }           
                      choice = btog.choice;           
                      _create = AgCreate("BTOH");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create BTOH              
                            Create("BTOH");               
                      }           
                      { AgPlacement place = AgPlacement("BTOH","BTOF");              
                            /// Add daughter volume BTOH to mother BTOF              
                            place.TranslateZ(+btog.dz/2);              
                            /// Translate z = +btog.dz/2              
                            place.AlphaY(180);              
                            /// Rotate: AlphaY = 180              
                            /// G3 Reference: thetax = 90              
                            /// G3 Reference: phix = 0              
                            /// G3 Reference: thetay = 90              
                            /// G3 Reference: phiy = 90              
                            /// G3 Reference: thetaz = 0              
                            /// G3 Reference: phiz = 0              
                            _stacker -> Position( AgBlock::Find("BTOH"), place );              
                      } // end placement of BTOH           
                      if ( (choice!=2) )           
                      {              
                            choice = 1;              
                      }           
                      _create = AgCreate("BTOH");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create BTOH              
                            Create("BTOH");               
                      }           
                      { AgPlacement place = AgPlacement("BTOH","BTOF");              
                            /// Add daughter volume BTOH to mother BTOF              
                            place.TranslateZ(-btog.dz/2);              
                            /// Translate z = -btog.dz/2              
                            _stacker -> Position( AgBlock::Find("BTOH"), place );              
                      } // end placement of BTOH           
                      END_OF_BTOF:           
                      mCurrent = _save;           
                ///@}        
          } // End Block BTOF     
          // ---------------------------------------------------------------------------------------------------     
          void BTOH::Block( AgCreate create )     
          {         
                ///@addtogroup BTOH_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("BTOH");              
                            attr.par("seen")=0;              
                            attr.par("colo")=1;              
                            attr.par("serial")=choice;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dz")=btog.dz/2;              
                            /// Shape Tube dz=btog.dz/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_BTOH;              
                            _stacker -> Build(this);              
                      }           
                      /// Loop on is from 1 to 60 step=1           
                      for ( is=1; (1>0)? (is<=60):(is>=60); is+=1 )           
                      {              
                            tof=0;              
                            if ( (choice==2) )              
                            {                 
                                  tof=1;                 
                            }              
                            if ( (choice==3&&51<=is&&is<=65) )              
                            {                 
                                  tof=1;                 
                            }              
                            if ( (choice==4&&is==btog.posit1) )              
                            {                 
                                  tof=1;                 
                            }              
                            _create = AgCreate("BSEC");              
                            {                 
                                  AgShape myshape; // undefined shape                 
                                  ///Create BSEC                 
                                  Create("BSEC");                  
                            }              
                            { AgPlacement place = AgPlacement("BSEC","BTOH");                 
                                  /// Add daughter volume BSEC to mother BTOH                 
                                  place.AlphaZ(102+6*is);                 
                                  /// Rotate: AlphaZ = 102+6*is                 
                                  /// G3 Reference: thetax = 90                 
                                  /// G3 Reference: phix = 0                 
                                  /// G3 Reference: thetay = 90                 
                                  /// G3 Reference: phiy = 90                 
                                  /// G3 Reference: thetaz = 0                 
                                  /// G3 Reference: phiz = 0                 
                                  _stacker -> Position( AgBlock::Find("BSEC"), place );                 
                            } // end placement of BSEC              
                      }           
                      END_OF_BTOH:           
                      mCurrent = _save;           
                ///@}        
          } // End Block BTOH     
          // ---------------------------------------------------------------------------------------------------     
          void BSEC::Block( AgCreate create )     
          {         
                ///@addtogroup BSEC_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("BSEC");              
                            attr.par("seen")=0;              
                            attr.par("colo")=1;              
                            attr.par("serial")=tof;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tubs");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("phi1")=-3.0;              
                            shape.par("phi2")=3.0;              
                            /// Shape Tubs phi1=-3.0 phi2=3.0               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_BSEC;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("BTRA");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create BTRA              
                            Create("BTRA");               
                      }           
                      { AgPlacement place = AgPlacement("BTRA","BSEC");              
                            /// Add daughter volume BTRA to mother BSEC              
                            place.TranslateX(btog.rmin+(tray.supfullh+tray.height+tray.stript)/2);              
                            /// Translate x = btog.rmin+(tray.supfullh+tray.height+tray.stript)/2              
                            _stacker -> Position( AgBlock::Find("BTRA"), place );              
                      } // end placement of BTRA           
                      END_OF_BSEC:           
                      mCurrent = _save;           
                ///@}        
          } // End Block BSEC     
          // ---------------------------------------------------------------------------------------------------     
          void BTRA::Block( AgCreate create )     
          {         
                ///@addtogroup BTRA_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("BTRA");              
                            attr.par("seen")=0;              
                            attr.par("colo")=2;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=(tray.supfullh+tray.height+tray.stript)/2;              
                            shape.par("dy")=tray.width/2;              
                            /// Shape Bbox dx=(tray.supfullh+tray.height+tray.stript)/2 dy=tray.width/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_BTRA;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("BXTR");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create BXTR              
                            Create("BXTR");               
                      }           
                      { AgPlacement place = AgPlacement("BXTR","BTRA");              
                            /// Add daughter volume BXTR to mother BTRA              
                            place.TranslateX((tray.supfullh+tray.stript)/2);              
                            /// Translate x = (tray.supfullh+tray.stript)/2              
                            place.TranslateZ((btog.dz-tray.length)/2);              
                            /// Translate z = (btog.dz-tray.length)/2              
                            _stacker -> Position( AgBlock::Find("BXTR"), place );              
                      } // end placement of BXTR           
                      _create = AgCreate("BUND");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create BUND              
                            Create("BUND");               
                      }           
                      { AgPlacement place = AgPlacement("BUND","BTRA");              
                            /// Add daughter volume BUND to mother BTRA              
                            place.TranslateX(-(tray.height+tray.stript)/2);              
                            /// Translate x = -(tray.height+tray.stript)/2              
                            place.TranslateZ((btog.dz-tray.suplen)/2);              
                            /// Translate z = (btog.dz-tray.suplen)/2              
                            _stacker -> Position( AgBlock::Find("BUND"), place );              
                      } // end placement of BUND           
                      END_OF_BTRA:           
                      mCurrent = _save;           
                ///@}        
          } // End Block BTRA     
          // ---------------------------------------------------------------------------------------------------     
          void BXTR::Block( AgCreate create )     
          {         
                ///@addtogroup BXTR_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("BXTR");              
                            attr.par("seen")=1;              
                            attr.par("colo")=2;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material Aluminium            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Aluminium");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=tray.height/2;              
                            shape.par("dz")=tray.length/2;              
                            /// Shape Bbox dx=tray.height/2 dz=tray.length/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_BXTR;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("BTTC");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create BTTC              
                            Create("BTTC");               
                      }           
                      { AgPlacement place = AgPlacement("BTTC","BXTR");              
                            /// Add daughter volume BTTC to mother BXTR              
                            _stacker -> Position( AgBlock::Find("BTTC"), place );              
                      } // end placement of BTTC           
                      _create = AgCreate("BMTC");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create BMTC              
                            Create("BMTC");               
                      }           
                      { AgPlacement place = AgPlacement("BMTC","BXTR");              
                            /// Add daughter volume BMTC to mother BXTR              
                            _stacker -> Position( AgBlock::Find("BMTC"), place );              
                      } // end placement of BMTC           
                      END_OF_BXTR:           
                      mCurrent = _save;           
                ///@}        
          } // End Block BXTR     
          // ---------------------------------------------------------------------------------------------------     
          void BMTC::Block( AgCreate create )     
          {         
                ///@addtogroup BMTC_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("BMTC");              
                            attr.par("seen")=1;              
                            attr.par("colo")=5;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material Air            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Air");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=tray.height/2-tray.wallthk;              
                            shape.par("dy")=tray.width/2-tray.wallthk;              
                            shape.par("dz")=tray.length/2-tray.wallthk;              
                            /// Shape Bbox dx=tray.height/2-tray.wallthk dy=tray.width/2-tray.wallthk dz=tray.length/2-tray.wallthk               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_BMTC;              
                            _stacker -> Build(this);              
                      }           
                      zpos  =  (tray.length-ctbb.slab1len)/2-tray.wallthk-ctbb.wrap;           
                      xpos  =  -tray.height/2+ctbb.slab1x;           
                      _create = AgCreate("BXSA");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create BXSA              
                            Create("BXSA");               
                      }           
                      { AgPlacement place = AgPlacement("BXSA","BMTC");              
                            /// Add daughter volume BXSA to mother BMTC              
                            place.TranslateX(xpos);              
                            /// Translate x = xpos              
                            place.TranslateZ(zpos);              
                            /// Translate z = zpos              
                            place.par("dx")=ctbb.slabthck/2;              
                            place.par("dy")=ctbb.slabwid/2;              
                            place.par("dz")=ctbb.slab1len/2;              
                            _stacker -> Position( AgBlock::Find("BXSA"), place );              
                      } // end placement of BXSA           
                      zpos = zpos - (ctbb.slab1len + ctbb.convlen)/2;           
                      _create = AgCreate("BCCV");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create BCCV              
                            Create("BCCV");               
                      }           
                      { AgPlacement place = AgPlacement("BCCV","BMTC");              
                            /// Add daughter volume BCCV to mother BMTC              
                            place.TranslateX(xpos);              
                            /// Translate x = xpos              
                            place.TranslateZ(zpos);              
                            /// Translate z = zpos              
                            place.par("dz")=ctbb.convlen/2;              
                            place.par("dx1")=ctbb.slabthck/2;              
                            place.par("dx2")=ctbb.slabthck/2;              
                            place.par("dy1")=ctbb.convwidm/2;              
                            place.par("dy2")=ctbb.slabwid/2;              
                            _stacker -> Position( AgBlock::Find("BCCV"), place );              
                      } // end placement of BCCV           
                      zpos = zpos - (ctbb.convlen + ctbb.pmtlen)/2;           
                      _create = AgCreate("BCPM");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create BCPM              
                            Create("BCPM");               
                      }           
                      { AgPlacement place = AgPlacement("BCPM","BMTC");              
                            /// Add daughter volume BCPM to mother BMTC              
                            place.TranslateX(xpos);              
                            /// Translate x = xpos              
                            place.TranslateZ(zpos);              
                            /// Translate z = zpos              
                            place.par("dz")=ctbb.pmtlen/2;              
                            place.par("rmin")=ctbb.pmtminr;              
                            place.par("rmax")=ctbb.pmtmaxr;              
                            _stacker -> Position( AgBlock::Find("BCPM"), place );              
                      } // end placement of BCPM           
                      zpos = zpos - (ctbb.pmtlen + ctbb.baselen)/2;           
                      _create = AgCreate("BCSK");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create BCSK              
                            Create("BCSK");               
                      }           
                      { AgPlacement place = AgPlacement("BCSK","BMTC");              
                            /// Add daughter volume BCSK to mother BMTC              
                            place.TranslateX(xpos);              
                            /// Translate x = xpos              
                            place.TranslateZ(zpos);              
                            /// Translate z = zpos              
                            place.par("dz")=ctbb.baselen/2;              
                            place.par("rmin")=ctbb.baseminr;              
                            place.par("rmax")=ctbb.basemaxr;              
                            _stacker -> Position( AgBlock::Find("BCSK"), place );              
                      } // end placement of BCSK           
                      _create = AgCreate("BZEL");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create BZEL              
                            Create("BZEL");               
                      }           
                      { AgPlacement place = AgPlacement("BZEL","BMTC");              
                            /// Add daughter volume BZEL to mother BMTC              
                            place.TranslateX(xpos);              
                            /// Translate x = xpos              
                            place.TranslateZ(zpos);              
                            /// Translate z = zpos              
                            place.par("dx")=ctbb.electhck/2;              
                            place.par("dy")=ctbb.baseminr-0.1;              
                            place.par("dz")=ctbb.baselen/2;              
                            _stacker -> Position( AgBlock::Find("BZEL"), place );              
                      } // end placement of BZEL           
                      zpos  =  (tray.length-ctbb.slab2len)/2-tray.wallthk-ctbb.wrap-ctbb.shim;           
                      xpos  =  -tray.height/2+ctbb.slab2x;           
                      _create = AgCreate("BXSA");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create BXSA              
                            Create("BXSA");               
                      }           
                      { AgPlacement place = AgPlacement("BXSA","BMTC");              
                            /// Add daughter volume BXSA to mother BMTC              
                            place.TranslateX(xpos);              
                            /// Translate x = xpos              
                            place.TranslateZ(-zpos);              
                            /// Translate z = -zpos              
                            place.par("dx")=ctbb.slabthck/2;              
                            place.par("dy")=ctbb.slabwid/2;              
                            place.par("dz")=ctbb.slab2len/2;              
                            _stacker -> Position( AgBlock::Find("BXSA"), place );              
                      } // end placement of BXSA           
                      zpos = zpos - (ctbb.slab2len + ctbb.convlen)/2;           
                      { AgPlacement place = AgPlacement("BCCV","BMTC");              
                            /// Add daughter volume BCCV to mother BMTC              
                            place.TranslateX(xpos);              
                            /// Translate x = xpos              
                            place.TranslateZ(-zpos);              
                            /// Translate z = -zpos              
                            place.par("dz")=ctbb.convlen/2;              
                            place.par("dx1")=ctbb.slabthck/2;              
                            place.par("dx2")=ctbb.slabthck/2;              
                            place.par("dy1")=ctbb.convwidm/2;              
                            place.par("dy2")=ctbb.slabwid/2;              
                            place.AlphaX(180);              
                            /// Rotate: AlphaX = 180              
                            /// G3 Reference: thetax = 90              
                            /// G3 Reference: phix = 0              
                            /// G3 Reference: thetay = 90              
                            /// G3 Reference: phiy = 90              
                            /// G3 Reference: thetaz = 0              
                            /// G3 Reference: phiz = 0              
                            _stacker -> Position( AgBlock::Find("BCCV"), place );              
                      } // end placement of BCCV           
                      zpos = zpos - (ctbb.convlen + ctbb.pmtlen)/2;           
                      { AgPlacement place = AgPlacement("BCPM","BMTC");              
                            /// Add daughter volume BCPM to mother BMTC              
                            place.TranslateX(xpos);              
                            /// Translate x = xpos              
                            place.TranslateZ(-zpos);              
                            /// Translate z = -zpos              
                            place.par("dz")=ctbb.pmtlen/2;              
                            place.par("rmin")=ctbb.pmtminr;              
                            place.par("rmax")=ctbb.pmtmaxr;              
                            _stacker -> Position( AgBlock::Find("BCPM"), place );              
                      } // end placement of BCPM           
                      zpos = zpos - (ctbb.pmtlen + ctbb.baselen)/2;           
                      { AgPlacement place = AgPlacement("BCSK","BMTC");              
                            /// Add daughter volume BCSK to mother BMTC              
                            place.TranslateX(xpos);              
                            /// Translate x = xpos              
                            place.TranslateZ(-zpos);              
                            /// Translate z = -zpos              
                            place.par("dz")=ctbb.baselen/2;              
                            place.par("rmin")=ctbb.baseminr;              
                            place.par("rmax")=ctbb.basemaxr;              
                            _stacker -> Position( AgBlock::Find("BCSK"), place );              
                      } // end placement of BCSK           
                      { AgPlacement place = AgPlacement("BZEL","BMTC");              
                            /// Add daughter volume BZEL to mother BMTC              
                            place.TranslateX(xpos);              
                            /// Translate x = xpos              
                            place.TranslateZ(-zpos);              
                            /// Translate z = -zpos              
                            place.par("dx")=ctbb.electhck/2;              
                            place.par("dy")=ctbb.baseminr-0.1;              
                            place.par("dz")=ctbb.baselen/2;              
                            _stacker -> Position( AgBlock::Find("BZEL"), place );              
                      } // end placement of BZEL           
                      END_OF_BMTC:           
                      mCurrent = _save;           
                ///@}        
          } // End Block BMTC     
          // ---------------------------------------------------------------------------------------------------     
          void BTTC::Block( AgCreate create )     
          {         
                ///@addtogroup BTTC_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("BTTC");              
                            attr.par("seen")=1;              
                            attr.par("colo")=6;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Component C	a=12	z=6	w=1           
                      /// Component H2	a=1	z=1	w=2           
                      /// Mixture LastAFoam dens=0.048           
                      {  AgMaterial &mix = AgMaterial::Get("Lastafoam");              
                            mix.Component("C",12,6,1);              
                            mix.Component("H2",1,1,2);              
                            mix.par("dens")=0.048;              
                            mix.lock();              
                            _material = mix;              
                            _material.lock();              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=tray.height/2-tray.wallthk;              
                            shape.par("dy")=tray.width/2-tray.wallthk;              
                            shape.par("dz")=tray.length/2-tray.wallthk;              
                            /// Shape Bbox dx=tray.height/2-tray.wallthk dy=tray.width/2-tray.wallthk dz=tray.length/2-tray.wallthk               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_BTTC;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("BFEE");           
                      { // Paramters passed in via the Create operatir              
                            AgCreate create("BFEE");              
                            create.par("dz")=toff.eleclen/2;              
                            create.par("dx")=toff.electhck/2;              
                            create.par("dy")=toff.elecwid/2;              
                            _create = create;              
                      }           
                      {              
                            AgShape myshape; // undefined shape              
                            /// Set shape par: dz = toff.eleclen/2              
                            myshape.par("dz")=toff.eleclen/2;              
                            /// Set shape par: dx = toff.electhck/2              
                            myshape.par("dx")=toff.electhck/2;              
                            /// Set shape par: dy = toff.elecwid/2              
                            myshape.par("dy")=toff.elecwid/2;              
                            ///Create BFEE              
                            Create("BFEE");               
                      }           
                      zpfee     = toff.elec1z;           
                      /// Loop on i from 1 to 9 step=1           
                      for ( i=1; (1>0)? (i<=9):(i>=9); i+=1 )           
                      {              
                            { AgPlacement place = AgPlacement("BFEE","BTTC");                 
                                  /// Add daughter volume BFEE to mother BTTC                 
                                  place.TranslateX(toff.elecx);                 
                                  /// Translate x = toff.elecx                 
                                  place.TranslateZ(zpfee);                 
                                  /// Translate z = zpfee                 
                                  _stacker -> Position( AgBlock::Find("BFEE"), place );                 
                            } // end placement of BFEE              
                            zpfee    = zpfee - toff.elecdz;              
                      }           
                      _create = AgCreate("BCOO");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create BCOO              
                            Create("BCOO");               
                      }           
                      { AgPlacement place = AgPlacement("BCOO","BTTC");              
                            /// Add daughter volume BCOO to mother BTTC              
                            place.TranslateX(0);              
                            /// Translate x = 0              
                            place.TranslateY(0);              
                            /// Translate y = 0              
                            place.par("dx")=0;              
                            place.par("dy")=0;              
                            place.par("dz")=0;              
                            _stacker -> Position( AgBlock::Find("BCOO"), place );              
                      } // end placement of BCOO           
                      _create = AgCreate("BMTM");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create BMTM              
                            Create("BMTM");               
                      }           
                      { AgPlacement place = AgPlacement("BMTM","BTTC");              
                            /// Add daughter volume BMTM to mother BTTC              
                            _stacker -> Position( AgBlock::Find("BMTM"), place );              
                      } // end placement of BMTM           
                      END_OF_BTTC:           
                      mCurrent = _save;           
                ///@}        
          } // End Block BTTC     
          // ---------------------------------------------------------------------------------------------------     
          void BMTM::Block( AgCreate create )     
          {         
                ///@addtogroup BMTM_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("BMTM");              
                            attr.par("seen")=0;              
                            attr.par("colo")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material Air            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Air");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=tray.height/2-tray.wallthk;              
                            shape.par("dy")=tray.width/2-tray.wallthk;              
                            shape.par("dz")=tray.length/2-tray.wallthk;              
                            /// Shape Bbox dx=tray.height/2-tray.wallthk dy=tray.width/2-tray.wallthk dz=tray.length/2-tray.wallthk               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_BMTM;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("BMTD");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create BMTD              
                            Create("BMTD");               
                      }           
                      END_OF_BMTM:           
                      mCurrent = _save;           
                ///@}        
          } // End Block BMTM     
          // ---------------------------------------------------------------------------------------------------     
          void BMTD::Block( AgCreate create )     
          {         
                ///@addtogroup BMTD_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("BMTD");              
                            attr.par("seen")=1;              
                            attr.par("colo")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Division");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("ndiv")=5;              
                            shape.par("iaxis")=2;              
                            /// Shape Division ndiv=5 iaxis=2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_BMTD;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("BASS");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create BASS              
                            Create("BASS");               
                      }           
                      zpbass    = toff.slat1z;           
                      /// Loop on i from 1 to 9 step=1           
                      for ( i=1; (1>0)? (i<=9):(i>=9); i+=1 )           
                      {              
                            { AgPlacement place = AgPlacement("BASS","BMTD");                 
                                  /// Add daughter volume BASS to mother BMTD                 
                                  place.TranslateX(-0.8);                 
                                  /// Translate x = -0.8                 
                                  place.TranslateZ(zpbass);                 
                                  /// Translate z = zpbass                 
                                  place.par("only")=AgPlacement::kMany;                 
                                  /// Overlap: agplacement::kmany                 
                                  place.AlphaY(toff.slatang);                 
                                  /// Rotate: AlphaY = toff.slatang                 
                                  /// G3 Reference: thetax = 90                 
                                  /// G3 Reference: phix = 0                 
                                  /// G3 Reference: thetay = 90                 
                                  /// G3 Reference: phiy = 90                 
                                  /// G3 Reference: thetaz = 0                 
                                  /// G3 Reference: phiz = 0                 
                                  _stacker -> Position( AgBlock::Find("BASS"), place );                 
                            } // end placement of BASS              
                            zpbass   = zpbass - toff.slatdz;              
                      }           
                      END_OF_BMTD:           
                      mCurrent = _save;           
                ///@}        
          } // End Block BMTD     
          // ---------------------------------------------------------------------------------------------------     
          void BASS::Block( AgCreate create )     
          {         
                ///@addtogroup BASS_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("BASS");              
                            attr.par("seen")=1;              
                            attr.par("colo")=6;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      totlen = toff.slat1len+toff.pmtlen+toff.baselen;           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=toff.pmtmaxr;              
                            shape.par("dy")=(tray.width/2-tray.wallthk)/5.;              
                            shape.par("dz")=totlen/2.;              
                            /// Shape Bbox dx=toff.pmtmaxr dy=(tray.width/2-tray.wallthk)/5. dz=totlen/2.               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_BASS;              
                            _stacker -> Build(this);              
                      }           
                      zpos = -(totlen-toff.slat1len)/2;           
                      _create = AgCreate("BCSB");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create BCSB              
                            Create("BCSB");               
                      }           
                      { AgPlacement place = AgPlacement("BCSB","BASS");              
                            /// Add daughter volume BCSB to mother BASS              
                            place.TranslateZ(zpos);              
                            /// Translate z = zpos              
                            _stacker -> Position( AgBlock::Find("BCSB"), place );              
                      } // end placement of BCSB           
                      zpos = zpos + (toff.slat1len+toff.pmtlen)/2;           
                      _create = AgCreate("BCPM");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create BCPM              
                            Create("BCPM");               
                      }           
                      { AgPlacement place = AgPlacement("BCPM","BASS");              
                            /// Add daughter volume BCPM to mother BASS              
                            place.TranslateX(0);              
                            /// Translate x = 0              
                            place.TranslateZ(zpos);              
                            /// Translate z = zpos              
                            place.par("dz")=toff.pmtlen/2;              
                            place.par("rmin")=toff.pmtminr;              
                            place.par("rmax")=toff.pmtmaxr;              
                            _stacker -> Position( AgBlock::Find("BCPM"), place );              
                      } // end placement of BCPM           
                      zpos = zpos + (toff.pmtlen + toff.baselen)/2;           
                      _create = AgCreate("BTSK");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create BTSK              
                            Create("BTSK");               
                      }           
                      { AgPlacement place = AgPlacement("BTSK","BASS");              
                            /// Add daughter volume BTSK to mother BASS              
                            place.TranslateX(0);              
                            /// Translate x = 0              
                            place.TranslateZ(zpos);              
                            /// Translate z = zpos              
                            place.par("dz")=toff.baselen/2;              
                            place.par("rmin")=toff.pmtminr;              
                            place.par("rmax")=toff.pmtmaxr;              
                            _stacker -> Position( AgBlock::Find("BTSK"), place );              
                      } // end placement of BTSK           
                      _create = AgCreate("BCEL");           
                      { // Paramters passed in via the Create operatir              
                            AgCreate create("BCEL");              
                            create.par("rmax")=toff.pmtminr;              
                            create.par("rmin")=0;              
                            create.par("dz")=toff.electhck/2;              
                            _create = create;              
                      }           
                      {              
                            AgShape myshape; // undefined shape              
                            /// Set shape par: rmax = toff.pmtminr              
                            myshape.par("rmax")=toff.pmtminr;              
                            /// Set shape par: rmin = 0              
                            myshape.par("rmin")=0;              
                            /// Set shape par: dz = toff.electhck/2              
                            myshape.par("dz")=toff.electhck/2;              
                            ///Create BCEL              
                            Create("BCEL");               
                      }           
                      zpos = zpos + 0.4 - toff.baselen/2;           
                      /// Loop on i from 1 to 4 step=1           
                      for ( i=1; (1>0)? (i<=4):(i>=4); i+=1 )           
                      {              
                            { AgPlacement place = AgPlacement("BCEL","BASS");                 
                                  /// Add daughter volume BCEL to mother BASS                 
                                  place.TranslateZ(zpos+i);                 
                                  /// Translate z = zpos+i                 
                                  _stacker -> Position( AgBlock::Find("BCEL"), place );                 
                            } // end placement of BCEL              
                      }           
                      END_OF_BASS:           
                      mCurrent = _save;           
                ///@}        
          } // End Block BASS     
          // ---------------------------------------------------------------------------------------------------     
          void BXSA::Block( AgCreate create )     
          {         
                ///@addtogroup BXSA_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("BXSA");              
                            attr.par("seen")=1;              
                            attr.par("colo")=3;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material polystyren            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Polystyren");              
                            _material = mat;              
                      }           
                      /// Medium sensitive           
                      ///  isvol = 1            
                      {  AgMedium &med = AgMedium::Get("Sensitive");              
                               med.Inherit(this);              
                            med.par("isvol")=1 ;              
                            _medium = med;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=0;              
                            shape.par("dy")=0;              
                            shape.par("dz")=0;              
                            /// Shape Bbox dx=0 dy=0 dz=0               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_BXSA;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_BXSA:           
                      mCurrent = _save;           
                ///@}        
          } // End Block BXSA     
          // ---------------------------------------------------------------------------------------------------     
          void BCSB::Block( AgCreate create )     
          {         
                ///@addtogroup BCSB_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("BCSB");              
                            attr.par("seen")=1;              
                            attr.par("colo")=4;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material polystyren            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Polystyren");              
                            _material = mat;              
                      }           
                      /// Medium sensitive           
                      ///  isvol = 1            
                      {  AgMedium &med = AgMedium::Get("Sensitive");              
                               med.Inherit(this);              
                            med.par("isvol")=1 ;              
                            _medium = med;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=toff.slatthck/2;              
                            shape.par("dy")=toff.slatwid/2;              
                            shape.par("dz")=toff.slat1len/2;              
                            /// Shape Bbox dx=toff.slatthck/2 dy=toff.slatwid/2 dz=toff.slat1len/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_BCSB;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_BCSB:           
                      mCurrent = _save;           
                ///@}        
          } // End Block BCSB     
          // ---------------------------------------------------------------------------------------------------     
          void BCCV::Block( AgCreate create )     
          {         
                ///@addtogroup BCCV_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("BCCV");              
                            attr.par("seen")=1;              
                            attr.par("colo")=3;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material polystyren            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Polystyren");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Trd2");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx1")=0;              
                            shape.par("dx2")=0;              
                            shape.par("dy1")=0;              
                            shape.par("dy2")=0;              
                            shape.par("dz")=0;              
                            /// Shape Trd2 dx1=0 dx2=0 dy1=0 dy2=0 dz=0               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_BCCV;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_BCCV:           
                      mCurrent = _save;           
                ///@}        
          } // End Block BCCV     
          // ---------------------------------------------------------------------------------------------------     
          void BCPM::Block( AgCreate create )     
          {         
                ///@addtogroup BCPM_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("BCPM");              
                            attr.par("seen")=1;              
                            attr.par("colo")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material polystyren            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Polystyren");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=0;              
                            shape.par("rmax")=0;              
                            shape.par("dz")=0;              
                            /// Shape Tube rmin=0 rmax=0 dz=0               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_BCPM;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_BCPM:           
                      mCurrent = _save;           
                ///@}        
          } // End Block BCPM     
          // ---------------------------------------------------------------------------------------------------     
          void BCSK::Block( AgCreate create )     
          {         
                ///@addtogroup BCSK_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("BCSK");              
                            attr.par("seen")=1;              
                            attr.par("colo")=2;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material polystyren            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Polystyren");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=0;              
                            shape.par("rmax")=0;              
                            shape.par("dz")=0;              
                            /// Shape Tube rmin=0 rmax=0 dz=0               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_BCSK;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_BCSK:           
                      mCurrent = _save;           
                ///@}        
          } // End Block BCSK     
          // ---------------------------------------------------------------------------------------------------     
          void BTSK::Block( AgCreate create )     
          {         
                ///@addtogroup BTSK_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("BTSK");              
                            attr.par("seen")=1;              
                            attr.par("colo")=2;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material Aluminium            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Aluminium");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=0;              
                            shape.par("rmax")=0;              
                            shape.par("dz")=0;              
                            /// Shape Tube rmin=0 rmax=0 dz=0               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_BTSK;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_BTSK:           
                      mCurrent = _save;           
                ///@}        
          } // End Block BTSK     
          // ---------------------------------------------------------------------------------------------------     
          void BZEL::Block( AgCreate create )     
          {         
                ///@addtogroup BZEL_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("BZEL");              
                            attr.par("seen")=1;              
                            attr.par("colo")=6;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material silicon            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Silicon");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=0;              
                            shape.par("dy")=0;              
                            shape.par("dz")=0;              
                            /// Shape Bbox dx=0 dy=0 dz=0               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_BZEL;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_BZEL:           
                      mCurrent = _save;           
                ///@}        
          } // End Block BZEL     
          // ---------------------------------------------------------------------------------------------------     
          void BCEL::Block( AgCreate create )     
          {         
                ///@addtogroup BCEL_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("BCEL");              
                            attr.par("seen")=1;              
                            attr.par("colo")=3;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Component Si	a=28.08	z=14	w=0.6*1*28./60.           
                      /// Component O	a=16	z=8	w=0.6*2*16./60. + 0.4*4*16./174.           
                      /// Component C	a=12	z=6	w=0.4*8*12./174.           
                      /// Component H	a=1	z=1	w=0.4*14*1./174.           
                      /// Mixture G10 dens=1.7           
                      {  AgMaterial &mix = AgMaterial::Get("G10");              
                            mix.Component("Si",28.08,14,0.6*1*28./60.);              
                            mix.Component("O",16,8,0.6*2*16./60. + 0.4*4*16./174.);              
                            mix.Component("C",12,6,0.4*8*12./174.);              
                            mix.Component("H",1,1,0.4*14*1./174.);              
                            mix.par("dens")=1.7;              
                            mix.lock();              
                            _material = mix;              
                            _material.lock();              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=0;              
                            shape.par("rmax")=0;              
                            shape.par("dz")=0;              
                            /// Shape Tube rmin=0 rmax=0 dz=0               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_BCEL;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_BCEL:           
                      mCurrent = _save;           
                ///@}        
          } // End Block BCEL     
          // ---------------------------------------------------------------------------------------------------     
          void BFEE::Block( AgCreate create )     
          {         
                ///@addtogroup BFEE_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("BFEE");              
                            attr.par("seen")=1;              
                            attr.par("colo")=3;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Component Si	a=28.08	z=14	w=0.6*1*28./60.           
                      /// Component O	a=16	z=8	w=0.6*2*16./60.           
                      /// Component C	a=12	z=6	w=0.4*8*12./174.           
                      /// Component H	a=1	z=1	w=0.4*14*1./174.           
                      /// Component O	a=16	z=8	w=0.4*4*16./174.           
                      /// Mixture G10 dens=1.7           
                      {  AgMaterial &mix = AgMaterial::Get("G10");              
                            mix.Component("Si",28.08,14,0.6*1*28./60.);              
                            mix.Component("O",16,8,0.6*2*16./60.);              
                            mix.Component("C",12,6,0.4*8*12./174.);              
                            mix.Component("H",1,1,0.4*14*1./174.);              
                            mix.Component("O",16,8,0.4*4*16./174.);              
                            mix.par("dens")=1.7;              
                            mix.lock();              
                            _material = mix;              
                            _material.lock();              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=0;              
                            shape.par("dy")=0;              
                            shape.par("dz")=0;              
                            /// Shape Bbox dx=0 dy=0 dz=0               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_BFEE;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_BFEE:           
                      mCurrent = _save;           
                ///@}        
          } // End Block BFEE     
          // ---------------------------------------------------------------------------------------------------     
          void BCOO::Block( AgCreate create )     
          {         
                ///@addtogroup BCOO_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("BCOO");              
                            attr.par("seen")=1;              
                            attr.par("colo")=2;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=0;              
                            shape.par("dy")=0;              
                            shape.par("dz")=0;              
                            /// Shape Bbox dx=0 dy=0 dz=0               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_BCOO;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("BRAI");           
                      { // Paramters passed in via the Create operatir              
                            AgCreate create("BRAI");              
                            create.par("dz")=tray.length/2-tray.wallthk;              
                            create.par("dx")=toff.railthck/2;              
                            create.par("dy")=toff.railwid/2;              
                            _create = create;              
                      }           
                      {              
                            AgShape myshape; // undefined shape              
                            /// Set shape par: dz = tray.length/2-tray.wallthk              
                            myshape.par("dz")=tray.length/2-tray.wallthk;              
                            /// Set shape par: dx = toff.railthck/2              
                            myshape.par("dx")=toff.railthck/2;              
                            /// Set shape par: dy = toff.railwid/2              
                            myshape.par("dy")=toff.railwid/2;              
                            ///Create BRAI              
                            Create("BRAI");               
                      }           
                      { AgPlacement place = AgPlacement("BRAI","BCOO");              
                            /// Add daughter volume BRAI to mother BCOO              
                            place.TranslateX(toff.elecx-toff.railthck);              
                            /// Translate x = toff.elecx-toff.railthck              
                            place.TranslateY((tray.width/2-toff.railwid/2-tray.wallthk));              
                            /// Translate y = (tray.width/2-toff.railwid/2-tray.wallthk)              
                            place.par("only")=AgPlacement::kMany;              
                            /// Overlap: agplacement::kmany              
                            _stacker -> Position( AgBlock::Find("BRAI"), place );              
                      } // end placement of BRAI           
                      { AgPlacement place = AgPlacement("BRAI","BCOO");              
                            /// Add daughter volume BRAI to mother BCOO              
                            place.TranslateX(toff.elecx-toff.railwid/2-toff.railthck/2);              
                            /// Translate x = toff.elecx-toff.railwid/2-toff.railthck/2              
                            place.TranslateY((tray.width/2-toff.railthck/2-tray.wallthk));              
                            /// Translate y = (tray.width/2-toff.railthck/2-tray.wallthk)              
                            place.par("only")=AgPlacement::kMany;              
                            /// Overlap: agplacement::kmany              
                            place.AlphaZ(90);              
                            /// Rotate: AlphaZ = 90              
                            /// G3 Reference: thetax = 90              
                            /// G3 Reference: phix = 0              
                            /// G3 Reference: thetay = 90              
                            /// G3 Reference: phiy = 90              
                            /// G3 Reference: thetaz = 0              
                            /// G3 Reference: phiz = 0              
                            _stacker -> Position( AgBlock::Find("BRAI"), place );              
                      } // end placement of BRAI           
                      { AgPlacement place = AgPlacement("BRAI","BCOO");              
                            /// Add daughter volume BRAI to mother BCOO              
                            place.TranslateX(toff.elecx-toff.railthck);              
                            /// Translate x = toff.elecx-toff.railthck              
                            place.TranslateY(-(tray.width/2-toff.railwid/2-tray.wallthk));              
                            /// Translate y = -(tray.width/2-toff.railwid/2-tray.wallthk)              
                            place.par("only")=AgPlacement::kMany;              
                            /// Overlap: agplacement::kmany              
                            _stacker -> Position( AgBlock::Find("BRAI"), place );              
                      } // end placement of BRAI           
                      { AgPlacement place = AgPlacement("BRAI","BCOO");              
                            /// Add daughter volume BRAI to mother BCOO              
                            place.TranslateX(toff.elecx-toff.railwid/2-toff.railthck/2);              
                            /// Translate x = toff.elecx-toff.railwid/2-toff.railthck/2              
                            place.TranslateY(-(tray.width/2-toff.railthck/2-tray.wallthk));              
                            /// Translate y = -(tray.width/2-toff.railthck/2-tray.wallthk)              
                            place.par("only")=AgPlacement::kMany;              
                            /// Overlap: agplacement::kmany              
                            place.AlphaZ(90);              
                            /// Rotate: AlphaZ = 90              
                            /// G3 Reference: thetax = 90              
                            /// G3 Reference: phix = 0              
                            /// G3 Reference: thetay = 90              
                            /// G3 Reference: phiy = 90              
                            /// G3 Reference: thetaz = 0              
                            /// G3 Reference: phiz = 0              
                            _stacker -> Position( AgBlock::Find("BRAI"), place );              
                      } // end placement of BRAI           
                      _create = AgCreate("BPIP");           
                      { // Paramters passed in via the Create operatir              
                            AgCreate create("BPIP");              
                            create.par("rmax")=toff.cooloutr;              
                            create.par("rmin")=toff.coolinnr;              
                            create.par("dz")=tray.length/2-tray.wallthk;              
                            _create = create;              
                      }           
                      {              
                            AgShape myshape; // undefined shape              
                            /// Set shape par: rmax = toff.cooloutr              
                            myshape.par("rmax")=toff.cooloutr;              
                            /// Set shape par: rmin = toff.coolinnr              
                            myshape.par("rmin")=toff.coolinnr;              
                            /// Set shape par: dz = tray.length/2-tray.wallthk              
                            myshape.par("dz")=tray.length/2-tray.wallthk;              
                            ///Create BPIP              
                            Create("BPIP");               
                      }           
                      { AgPlacement place = AgPlacement("BPIP","BCOO");              
                            /// Add daughter volume BPIP to mother BCOO              
                            place.TranslateX(toff.elecx-3.*toff.railthck/2.-toff.cooloutr);              
                            /// Translate x = toff.elecx-3.*toff.railthck/2.-toff.cooloutr              
                            place.TranslateY((tray.width/2-toff.railthck-tray.wallthk-toff.cooloutr));              
                            /// Translate y = (tray.width/2-toff.railthck-tray.wallthk-toff.cooloutr)              
                            place.par("only")=AgPlacement::kMany;              
                            /// Overlap: agplacement::kmany              
                            _stacker -> Position( AgBlock::Find("BPIP"), place );              
                      } // end placement of BPIP           
                      { AgPlacement place = AgPlacement("BPIP","BCOO");              
                            /// Add daughter volume BPIP to mother BCOO              
                            place.TranslateX(toff.elecx-3.*toff.railthck/2.-toff.cooloutr);              
                            /// Translate x = toff.elecx-3.*toff.railthck/2.-toff.cooloutr              
                            place.TranslateY(-(tray.width/2-toff.railthck-tray.wallthk-toff.cooloutr));              
                            /// Translate y = -(tray.width/2-toff.railthck-tray.wallthk-toff.cooloutr)              
                            place.par("only")=AgPlacement::kMany;              
                            /// Overlap: agplacement::kmany              
                            _stacker -> Position( AgBlock::Find("BPIP"), place );              
                      } // end placement of BPIP           
                      END_OF_BCOO:           
                      mCurrent = _save;           
                ///@}        
          } // End Block BCOO     
          // ---------------------------------------------------------------------------------------------------     
          void BRAI::Block( AgCreate create )     
          {         
                ///@addtogroup BRAI_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("BRAI");              
                            attr.par("seen")=1;              
                            attr.par("colo")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material Aluminium            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Aluminium");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=0.0;              
                            shape.par("dy")=0.0;              
                            shape.par("dz")=0.0;              
                            /// Shape Bbox dx=0.0 dy=0.0 dz=0.0               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_BRAI;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_BRAI:           
                      mCurrent = _save;           
                ///@}        
          } // End Block BRAI     
          // ---------------------------------------------------------------------------------------------------     
          void BPIP::Block( AgCreate create )     
          {         
                ///@addtogroup BPIP_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("BPIP");              
                            attr.par("seen")=1;              
                            attr.par("colo")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material Aluminium            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Aluminium");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=0;              
                            shape.par("rmax")=0;              
                            shape.par("dz")=0;              
                            /// Shape Tube rmin=0 rmax=0 dz=0               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_BPIP;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_BPIP:           
                      mCurrent = _save;           
                ///@}        
          } // End Block BPIP     
          // ---------------------------------------------------------------------------------------------------     
          void BUND::Block( AgCreate create )     
          {         
                ///@addtogroup BUND_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("BUND");              
                            attr.par("seen")=0;              
                            attr.par("colo")=1;              
                            attr.par("serial")=0;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=tray.supfullh/2;              
                            shape.par("dy")=tray.width/2;              
                            shape.par("dz")=tray.suplen/2;              
                            /// Shape Bbox dx=tray.supfullh/2 dy=tray.width/2 dz=tray.suplen/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_BUND;              
                            _stacker -> Build(this);              
                      }           
                      /// Material Aluminium            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Aluminium");              
                            _material = mat;              
                      }           
                      xpos = (tray.supfullh - tray.footthk)/2;           
                      ypos = (tray.width - tray.foot1len)/2 - tray.footinse;           
                      _create = AgCreate("BTFT");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create BTFT              
                            Create("BTFT");               
                      }           
                      { AgPlacement place = AgPlacement("BTFT","BUND");              
                            /// Add daughter volume BTFT to mother BUND              
                            place.TranslateX(xpos);              
                            /// Translate x = xpos              
                            place.TranslateY(-ypos);              
                            /// Translate y = -ypos              
                            place.par("dx")=tray.footthk/2;              
                            place.par("dy")=tray.foot1len/2;              
                            _stacker -> Position( AgBlock::Find("BTFT"), place );              
                      } // end placement of BTFT           
                      { AgPlacement place = AgPlacement("BTFT","BUND");              
                            /// Add daughter volume BTFT to mother BUND              
                            place.TranslateX(xpos);              
                            /// Translate x = xpos              
                            place.TranslateY(+ypos);              
                            /// Translate y = +ypos              
                            place.par("dx")=tray.footthk/2;              
                            place.par("dy")=tray.foot1len/2;              
                            _stacker -> Position( AgBlock::Find("BTFT"), place );              
                      } // end placement of BTFT           
                      xpos = (tray.supfullh - tray.foot2thk)/2;           
                      ypos = ypos - (tray.foot1len + tray.footthk)/2;           
                      { AgPlacement place = AgPlacement("BTFT","BUND");              
                            /// Add daughter volume BTFT to mother BUND              
                            place.TranslateX(xpos);              
                            /// Translate x = xpos              
                            place.TranslateY(-ypos);              
                            /// Translate y = -ypos              
                            place.par("dx")=tray.foot2thk/2;              
                            place.par("dy")=tray.footthk/2;              
                            _stacker -> Position( AgBlock::Find("BTFT"), place );              
                      } // end placement of BTFT           
                      { AgPlacement place = AgPlacement("BTFT","BUND");              
                            /// Add daughter volume BTFT to mother BUND              
                            place.TranslateX(xpos);              
                            /// Translate x = xpos              
                            place.TranslateY(+ypos);              
                            /// Translate y = +ypos              
                            place.par("dx")=tray.foot2thk/2;              
                            place.par("dy")=tray.footthk/2;              
                            _stacker -> Position( AgBlock::Find("BTFT"), place );              
                      } // end placement of BTFT           
                      xpos = (tray.supfullh + tray.footthk)/2 - tray.foot2thk;           
                      ypos = ypos - (tray.footthk + tray.foot3len)/2;           
                      { AgPlacement place = AgPlacement("BTFT","BUND");              
                            /// Add daughter volume BTFT to mother BUND              
                            place.TranslateX(xpos);              
                            /// Translate x = xpos              
                            place.TranslateY(-ypos);              
                            /// Translate y = -ypos              
                            place.par("dx")=tray.footthk/2;              
                            place.par("dy")=tray.foot3len/2;              
                            _stacker -> Position( AgBlock::Find("BTFT"), place );              
                      } // end placement of BTFT           
                      { AgPlacement place = AgPlacement("BTFT","BUND");              
                            /// Add daughter volume BTFT to mother BUND              
                            place.TranslateX(xpos);              
                            /// Translate x = xpos              
                            place.TranslateY(+ypos);              
                            /// Translate y = +ypos              
                            place.par("dx")=tray.footthk/2;              
                            place.par("dy")=tray.foot3len/2;              
                            _stacker -> Position( AgBlock::Find("BTFT"), place );              
                      } // end placement of BTFT           
                      support_aile_width = ( tray.supfullh-tray.suparmt )/tan(60*degrad);           
                      support_arm_width  = ( tray.supfullw-tray.supbasew)/2-support_aile_width;           
                      support_aile_ypos  = ( tray.supbasew+support_aile_width)/2;           
                      support_arm_xpos   = ( tray.supfullh-tray.suparmt )/2;           
                      support_arm_ypos   = ( tray.supfullw-support_arm_width)/2;           
                      /// Material Aluminium            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Aluminium");              
                            _material = mat;              
                      }           
                      _create = AgCreate("BASE");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create BASE              
                            Create("BASE");               
                      }           
                      { AgPlacement place = AgPlacement("BASE","BUND");              
                            /// Add daughter volume BASE to mother BUND              
                            place.TranslateX((-tray.supfullh+tray.supbaset)/2);              
                            /// Translate x = (-tray.supfullh+tray.supbaset)/2              
                            _stacker -> Position( AgBlock::Find("BASE"), place );              
                      } // end placement of BASE           
                      _create = AgCreate("BARM");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create BARM              
                            Create("BARM");               
                      }           
                      { AgPlacement place = AgPlacement("BARM","BUND");              
                            /// Add daughter volume BARM to mother BUND              
                            place.TranslateX(support_arm_xpos);              
                            /// Translate x = support_arm_xpos              
                            place.TranslateY(-support_arm_ypos);              
                            /// Translate y = -support_arm_ypos              
                            _stacker -> Position( AgBlock::Find("BARM"), place );              
                      } // end placement of BARM           
                      { AgPlacement place = AgPlacement("BARM","BUND");              
                            /// Add daughter volume BARM to mother BUND              
                            place.TranslateX(support_arm_xpos);              
                            /// Translate x = support_arm_xpos              
                            place.TranslateY(+support_arm_ypos);              
                            /// Translate y = +support_arm_ypos              
                            _stacker -> Position( AgBlock::Find("BARM"), place );              
                      } // end placement of BARM           
                      _create = AgCreate("BANG");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create BANG              
                            Create("BANG");               
                      }           
                      { AgPlacement place = AgPlacement("BANG","BUND");              
                            /// Add daughter volume BANG to mother BUND              
                            place.TranslateY(-support_aile_ypos);              
                            /// Translate y = -support_aile_ypos              
                            _stacker -> Position( AgBlock::Find("BANG"), place );              
                      } // end placement of BANG           
                      { AgPlacement place = AgPlacement("BANG","BUND");              
                            /// Add daughter volume BANG to mother BUND              
                            place.TranslateY(+support_aile_ypos);              
                            /// Translate y = +support_aile_ypos              
                            /// G3 Reference: thetax = 270              
                            /// G3 Reference: phix = 0              
                            /// G3 Reference: thetay = 90              
                            /// G3 Reference: phiy = 90              
                            /// G3 Reference: thetaz = 0              
                            /// G3 Reference: phiz = 0              
                            Double_t _thetax=270,_phix=0,_thetay=90,_phiy=90,_thetaz=0,_phiz=0;              
                            place.Reference( _thetax, _phix, _thetay, _phiy, _thetaz, _phiz );              
                            _stacker -> Position( AgBlock::Find("BANG"), place );              
                      } // end placement of BANG           
                      _create = AgCreate("BCOV");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create BCOV              
                            Create("BCOV");               
                      }           
                      { AgPlacement place = AgPlacement("BCOV","BUND");              
                            /// Add daughter volume BCOV to mother BUND              
                            place.TranslateX(-tray.supfullh/2+tray.cooloutr);              
                            /// Translate x = -tray.supfullh/2+tray.cooloutr              
                            _stacker -> Position( AgBlock::Find("BCOV"), place );              
                      } // end placement of BCOV           
                      END_OF_BUND:           
                      mCurrent = _save;           
                ///@}        
          } // End Block BUND     
          // ---------------------------------------------------------------------------------------------------     
          void BTFT::Block( AgCreate create )     
          {         
                ///@addtogroup BTFT_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("BTFT");              
                            attr.par("seen")=1;              
                            attr.par("colo")=2;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=0.0;              
                            shape.par("dy")=0.0;              
                            shape.par("dz")=0.0;              
                            /// Shape Bbox dx=0.0 dy=0.0 dz=0.0               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_BTFT;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_BTFT:           
                      mCurrent = _save;           
                ///@}        
          } // End Block BTFT     
          // ---------------------------------------------------------------------------------------------------     
          void BARM::Block( AgCreate create )     
          {         
                ///@addtogroup BARM_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("BARM");              
                            attr.par("seen")=1;              
                            attr.par("colo")=2;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=tray.suparmt/2;              
                            shape.par("dy")=support_arm_width/2;              
                            /// Shape Bbox dx=tray.suparmt/2 dy=support_arm_width/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_BARM;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_BARM:           
                      mCurrent = _save;           
                ///@}        
          } // End Block BARM     
          // ---------------------------------------------------------------------------------------------------     
          void BANG::Block( AgCreate create )     
          {         
                ///@addtogroup BANG_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("BANG");              
                            attr.par("seen")=1;              
                            attr.par("colo")=2;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Para");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=tray.suparmt/2;              
                            shape.par("dy")=support_aile_width/2;              
                            shape.par("alph")=-60;              
                            shape.par("thet")=0;              
                            shape.par("phi")=0;              
                            /// Shape Para dx=tray.suparmt/2 dy=support_aile_width/2 alph=-60 thet=0 phi=0               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_BANG;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_BANG:           
                      mCurrent = _save;           
                ///@}        
          } // End Block BANG     
          // ---------------------------------------------------------------------------------------------------     
          void BASE::Block( AgCreate create )     
          {         
                ///@addtogroup BASE_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("BASE");              
                            attr.par("seen")=1;              
                            attr.par("colo")=2;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=tray.supbaset/2;              
                            shape.par("dy")=tray.supbasew/2;              
                            /// Shape Bbox dx=tray.supbaset/2 dy=tray.supbasew/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_BASE;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_BASE:           
                      mCurrent = _save;           
                ///@}        
          } // End Block BASE     
          // ---------------------------------------------------------------------------------------------------     
          void BCOV::Block( AgCreate create )     
          {         
                ///@addtogroup BCOV_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("BCOV");              
                            attr.par("seen")=1;              
                            attr.par("colo")=2;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=0;              
                            shape.par("rmax")=tray.cooloutr;              
                            /// Shape Tube rmin=0 rmax=tray.cooloutr               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_BCOV;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("BWAT");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create BWAT              
                            Create("BWAT");               
                      }           
                      { AgPlacement place = AgPlacement("BWAT","BCOV");              
                            /// Add daughter volume BWAT to mother BCOV              
                            _stacker -> Position( AgBlock::Find("BWAT"), place );              
                      } // end placement of BWAT           
                      END_OF_BCOV:           
                      mCurrent = _save;           
                ///@}        
          } // End Block BCOV     
          // ---------------------------------------------------------------------------------------------------     
          void BWAT::Block( AgCreate create )     
          {         
                ///@addtogroup BWAT_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("BWAT");              
                            attr.par("seen")=1;              
                            attr.par("colo")=3;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Component H2	a=1	z=1	w=2           
                      /// Component O	a=16	z=8	w=1           
                      /// Mixture Water dens=1.0           
                      {  AgMaterial &mix = AgMaterial::Get("Water");              
                            mix.Component("H2",1,1,2);              
                            mix.Component("O",16,8,1);              
                            mix.par("dens")=1.0;              
                            mix.lock();              
                            _material = mix;              
                            _material.lock();              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=0;              
                            shape.par("rmax")=tray.coolinnr;              
                            /// Shape Tube rmin=0 rmax=tray.coolinnr               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_BWAT;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_BWAT:           
                      mCurrent = _save;           
                ///@}        
          } // End Block BWAT     
    // ----------------------------------------------------------------------- geoctr
       void BtofGeo1::ConstructGeometry( const Char_t *dummy )     
       {        
             ///@addtogroup BtofGeo1_revision        
             ///@{           
                   /// Author: W.J. Llope, Geary Eppley, Harlan Howe, Pablo Yepes           
             ///@}        
             ///@addtogroup BtofGeo1_revision        
             ///@{           
                   /// Created:     23 March 1996            
             ///@}        
             AddBlock("BTOF");        
             AddBlock("BTOH");        
             AddBlock("BSEC");        
             AddBlock("BTRA");        
             AddBlock("BUND");        
             AddBlock("BTFT");        
             AddBlock("BASE");        
             AddBlock("BARM");        
             AddBlock("BANG");        
             AddBlock("BWAT");        
             AddBlock("BCOV");        
             AddBlock("BXTR");        
             AddBlock("BMTC");        
             AddBlock("BTTC");        
             AddBlock("BMTM");        
             AddBlock("BMTD");        
             AddBlock("BASS");        
             AddBlock("BXSA");        
             AddBlock("BCSB");        
             AddBlock("BCCV");        
             AddBlock("BCPM");        
             AddBlock("BCSK");        
             AddBlock("BTSK");        
             AddBlock("BZEL");        
             AddBlock("BCEL");        
             AddBlock("BFEE");        
             AddBlock("BCOO");        
             AddBlock("BRAI");        
             AddBlock("BPIP");        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup btog_doc        
             ///@{           
                   ++btog._index;           
                   btog . version = 2; //  geometry version            
                   /// btog . version = 2; //  geometry version            
                   btog . rmin = 207.80; //  minimum CTB/TOF system radius (as built)            
                   /// btog . rmin = 207.80; //  minimum CTB/TOF system radius (as built)            
                   btog . rmax = 219.5; //  maximum CTB/TOF system radius            
                   /// btog . rmax = 219.5; //  maximum CTB/TOF system radius            
                   btog . dz = 246.0; //  CTB/TOF tube half length            
                   /// btog . dz = 246.0; //  CTB/TOF tube half length            
                   btog . choice = 4; //  1=CTB, 2=TOF, 3=25% TOF+CTB, 4=1 tray TOF+CTB            
                   /// btog . choice = 4; //  1=CTB, 2=TOF, 3=25% TOF+CTB, 4=1 tray TOF+CTB            
                   btog . posit1 = 24; //  TOF tray position for choice 4            
                   /// btog . posit1 = 24; //  TOF tray position for choice 4            
                   //           
                   btog.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup tray_doc        
             ///@{           
                   ++tray._index;           
                   tray . height = 8.89; //  tray height            
                   /// tray . height = 8.89; //  tray height            
                   tray . width = 21.59; //  full tray width            
                   /// tray . width = 21.59; //  full tray width            
                   tray . length = 241.62; //  full tray length            
                   /// tray . length = 241.62; //  full tray length            
                   tray . wallthk = 0.13; //  tray wall thickness            
                   /// tray . wallthk = 0.13; //  tray wall thickness            
                   tray . supfullh = 2.03; //  support height (radial)            
                   /// tray . supfullh = 2.03; //  support height (radial)            
                   tray . supfullw = 15.24; //  support full width with arms            
                   /// tray . supfullw = 15.24; //  support full width with arms            
                   tray . suplen = 215.9; //  support length            
                   /// tray . suplen = 215.9; //  support length            
                   tray . supbasew = 9.22; //  support base width            
                   /// tray . supbasew = 9.22; //  support base width            
                   tray . supbaset = 0.32; //  support base thickness              
                   /// tray . supbaset = 0.32; //  support base thickness              
                   tray . suparmt = 0.64; //  support arm  thickness            
                   /// tray . suparmt = 0.64; //  support arm  thickness            
                   tray . cooloutr = 0.80; //  Cooling channel outer radius            
                   /// tray . cooloutr = 0.80; //  Cooling channel outer radius            
                   tray . coolinnr = 0.48; //  Cooling channel inner radius            
                   /// tray . coolinnr = 0.48; //  Cooling channel inner radius            
                   tray . stript = 0.08; //  Thickness of polyethylene strip on bottom            
                   /// tray . stript = 0.08; //  Thickness of polyethylene strip on bottom            
                   tray . footinse = 1.06; //  foot inset from tray edge            
                   /// tray . footinse = 1.06; //  foot inset from tray edge            
                   tray . footthk = 0.23; //  thickness of foot material            
                   /// tray . footthk = 0.23; //  thickness of foot material            
                   tray . foot1len = 1.68; //  length (in section) of first part of foot            
                   /// tray . foot1len = 1.68; //  length (in section) of first part of foot            
                   tray . foot2thk = 1.16; //  thickness of second foot section            
                   /// tray . foot2thk = 1.16; //  thickness of second foot section            
                   tray . foot3len = 2.16; //  length of third part of foot            
                   /// tray . foot3len = 2.16; //  length of third part of foot            
                   //           
                   tray.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup ctbb_doc        
             ///@{           
                   ++ctbb._index;           
                   ctbb . slab1len = 112.5; //  first slab (B) length            
                   /// ctbb . slab1len = 112.5; //  first slab (B) length            
                   ctbb . slab2len = 130.0; //  second slab (A)length             
                   /// ctbb . slab2len = 130.0; //  second slab (A)length             
                   ctbb . slab1x = 5.84; //  first slab (B) x position            
                   /// ctbb . slab1x = 5.84; //  first slab (B) x position            
                   ctbb . slab2x = 2.67; //  second slab (A) x position            
                   /// ctbb . slab2x = 2.67; //  second slab (A) x position            
                   ctbb . slabthck = 1.0; //  scintillator slab thicknesses            
                   /// ctbb . slabthck = 1.0; //  scintillator slab thicknesses            
                   ctbb . slabwid = 21.0; //  scintillator slab width            
                   /// ctbb . slabwid = 21.0; //  scintillator slab width            
                   ctbb . convlen = 8.5; //  optical converter length            
                   /// ctbb . convlen = 8.5; //  optical converter length            
                   ctbb . convwidm = 4.0; //  optical convertor min width            
                   /// ctbb . convwidm = 4.0; //  optical convertor min width            
                   ctbb . convthck = 0.92; //  optical convertor thickness            
                   /// ctbb . convthck = 0.92; //  optical convertor thickness            
                   ctbb . pmtlen = 5.0; //  PMT length            
                   /// ctbb . pmtlen = 5.0; //  PMT length            
                   ctbb . pmtmaxr = 2.0; //  PMT max radius            
                   /// ctbb . pmtmaxr = 2.0; //  PMT max radius            
                   ctbb . pmtminr = 1.84; //  PMT min radius            
                   /// ctbb . pmtminr = 1.84; //  PMT min radius            
                   ctbb . baselen = 4.0; //  Base length            
                   /// ctbb . baselen = 4.0; //  Base length            
                   ctbb . basemaxr = 2.13; //  Base max radius            
                   /// ctbb . basemaxr = 2.13; //  Base max radius            
                   ctbb . baseminr = 1.0; //  Base min radius            
                   /// ctbb . baseminr = 1.0; //  Base min radius            
                   ctbb . electhck = 0.25; //  readout electronics thickness            
                   /// ctbb . electhck = 0.25; //  readout electronics thickness            
                   ctbb . wrap = 0.13; //  thickness of Tyvek + black plastic            
                   /// ctbb . wrap = 0.13; //  thickness of Tyvek + black plastic            
                   ctbb . shim = 0.26; //  thickness of shim to position slat 2            
                   /// ctbb . shim = 0.26; //  thickness of shim to position slat 2            
                   //           
                   ctbb.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup toff_doc        
             ///@{           
                   ++toff._index;           
                   toff . slat1len = 22.0; //  slat length            
                   /// toff . slat1len = 22.0; //  slat length            
                   toff . slat1z = 101.5; //  slat 1 Z position            
                   /// toff . slat1z = 101.5; //  slat 1 Z position            
                   toff . slatdz = 24.; //  slat Z separation (Same as toff_ElecDz)            
                   /// toff . slatdz = 24.; //  slat Z separation (Same as toff_ElecDz)            
                   toff . slatthck = 2.0; //  scintillator slab thicknesses            
                   /// toff . slatthck = 2.0; //  scintillator slab thicknesses            
                   toff . slatwid = 4.0; //  scintillator slab width            
                   /// toff . slatwid = 4.0; //  scintillator slab width            
                   toff . slatang = 8.; //  slat assy. angle w.r.t. tray            
                   /// toff . slatang = 8.; //  slat assy. angle w.r.t. tray            
                   toff . pmtlen = 5.0; //  PMT length            
                   /// toff . pmtlen = 5.0; //  PMT length            
                   toff . pmtmaxr = 1.91; //  PMT max radius            
                   /// toff . pmtmaxr = 1.91; //  PMT max radius            
                   toff . pmtminr = 1.8; //  PMT min radius            
                   /// toff . pmtminr = 1.8; //  PMT min radius            
                   toff . baselen = 5.1; //  Base length            
                   /// toff . baselen = 5.1; //  Base length            
                   toff . basemaxr = 1.91; //  Base max radius            
                   /// toff . basemaxr = 1.91; //  Base max radius            
                   toff . baseminr = 1.8; //  Base min radius              
                   /// toff . baseminr = 1.8; //  Base min radius              
                   toff . elecx = 4.1; //  FEE Board x position            
                   /// toff . elecx = 4.1; //  FEE Board x position            
                   toff . elec1z = 104.0; //  FEE Board 1 z position            
                   /// toff . elec1z = 104.0; //  FEE Board 1 z position            
                   toff . elecdz = toff.slatdz; //  FEE Board Dz (Same as toff_SlatDz)            
                   /// toff . elecdz = toff.slatdz; //  FEE Board Dz (Same as toff_SlatDz)            
                   toff . electhck = 0.17; //  FEE Board thickness (67 mils)            
                   /// toff . electhck = 0.17; //  FEE Board thickness (67 mils)            
                   toff . elecwid = 21.0; //  FEE Board width            
                   /// toff . elecwid = 21.0; //  FEE Board width            
                   toff . eleclen = 16.0; //  FEE Board length            
                   /// toff . eleclen = 16.0; //  FEE Board length            
                   toff . railthck = 0.2; //  Cooling loop rail thickness            
                   /// toff . railthck = 0.2; //  Cooling loop rail thickness            
                   toff . railwid = 1.0; //  Cooling loop rail width            
                   /// toff . railwid = 1.0; //  Cooling loop rail width            
                   toff . cooloutr = 0.375; //  Cooling loop pipe outer radius            
                   /// toff . cooloutr = 0.375; //  Cooling loop pipe outer radius            
                   toff . coolinnr = 0.350; //  Cooling loop pipe inner radius            
                   /// toff . coolinnr = 0.350; //  Cooling loop pipe inner radius            
                   //           
                   toff.fill();           
             ///@}        
             //        
             /// USE btog _index=1;        
             btog.Use();        
             /// USE tray _index=1;        
             tray.Use();        
             /// USE ctbb _index=1;        
             ctbb.Use();        
             /// USE toff _index=1;        
             toff.Use();        
             _create = AgCreate("BTOF");        
             {           
                   AgShape myshape; // undefined shape           
                   ///Create BTOF           
                   Create("BTOF");            
             }        
             { AgPlacement place = AgPlacement("BTOF","CAVE");           
                   /// Add daughter volume BTOF to mother CAVE           
                   _stacker -> Position( AgBlock::Find("BTOF"), place );           
             } // end placement of BTOF        
       }; // BtofGeo1     
 }; // namespace BtofGeo1  
 