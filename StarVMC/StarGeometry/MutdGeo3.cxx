#include "MutdGeo3.h"  
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
 namespace MUTDGEO3 // $NMSPC  
 {     
       //     
       // ---------------------------------------------------------------------------------------------------     
          //  -----------------------------------------------------     
          /// @defgroup mtdg_doc     
          /// \class Mtdg_t     
          /// \brief User-defined structure     
          ///                             
          /// AgML structure members:     
          ///                             
          ///Float_t version;     
          ///Float_t rpmtin;     
          ///Float_t rpmtout;     
          ///Float_t rmrpcin;     
          ///Float_t rmrpcout;     
          ///Float_t rmin;     
          ///Float_t rmax;     
          ///Float_t dz;     
          ///Float_t length;     
          ///Array_t<Float_t> radii;     
          ///Int_t _index;     
          //     
          Mtdg_t mtdg;     
          //     
          //  -----------------------------------------------------     
          /// @defgroup mtry_doc     
          /// \class Mtry_t     
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
          Mtry_t mtry;     
          //     
          //  -----------------------------------------------------     
          /// @defgroup mtbb_doc     
          /// \class Mtbb_t     
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
          Mtbb_t mtbb;     
          //     
          //  -----------------------------------------------------     
          /// @defgroup moff_doc     
          /// \class Moff_t     
          /// \brief User-defined structure     
          ///                             
          /// AgML structure members:     
          ///                             
          ///Float_t boxwidth;     
          ///Float_t slatlen;     
          ///Float_t slat01z;     
          ///Float_t slat02z;     
          ///Float_t slat03z;     
          ///Float_t slat04z;     
          ///Float_t slat05z;     
          ///Float_t slat06z;     
          ///Float_t slat07z;     
          ///Float_t slat08z;     
          ///Float_t slat09z;     
          ///Float_t slat10z;     
          ///Float_t slatthck;     
          ///Float_t slatwid;     
          ///Float_t slatang;     
          ///Float_t pmtlen;     
          ///Float_t pmtmaxr;     
          ///Float_t pmtminr;     
          ///Float_t baselen;     
          ///Float_t basemaxr;     
          ///Float_t baseminr;     
          ///Float_t socklen;     
          ///Float_t cellwid;     
          ///Float_t cellhgt;     
          ///Float_t elechgt;     
          ///Float_t electhck;     
          ///Float_t elecwid;     
          ///Float_t eleclen;     
          ///Float_t elec01z;     
          ///Float_t elec02z;     
          ///Float_t elec03z;     
          ///Float_t elec04z;     
          ///Float_t elec05z;     
          ///Float_t elec06z;     
          ///Float_t elec07z;     
          ///Float_t elec08z;     
          ///Float_t elec09z;     
          ///Float_t elec10z;     
          ///Float_t railthck;     
          ///Float_t railwid;     
          ///Float_t coolinnr;     
          ///Float_t cooloutr;     
          ///Int_t _index;     
          //     
          Moff_t moff;     
          //     
          ///@addtogroup MutdGeo3_vars     
          ///@{        
                Int_t ntray,iwid;        
                //        
                /// Int_t ntray,iwid        
          ///@}     
          ///@addtogroup MutdGeo3_vars     
          ///@{        
                Float_t barphi,xpos,ypos,zpos,sublen,subcen,totlen;        
                //        
                /// Float_t barphi,xpos,ypos,zpos,sublen,subcen,totlen        
          ///@}     
       MutdGeo3::MutdGeo3()     
         : AgModule("MutdGeo3"," is the geometry of the STAR muon trigger system ")     
       {        
       }     
          // ---------------------------------------------------------------------------------------------------     
          void MUTD::Block( AgCreate create )     
          {         
                ///@addtogroup MUTD_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Air            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Air");              
                            _material = mat;              
                      }           
                      /// Medium Standard           
                      {  AgMedium med = AgMedium::CopyMedium("Standard");              
                            _medium = med;              
                      }           
                      { AgAttribute attr = AgAttribute("MAGP");              
                            attr.par("seen")=0;              
                            attr.par("colo")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=mtdg.rpmtin+1;              
                            shape.par("rmax")=mtdg.rmax;              
                            shape.par("dz")=mtdg.length/2;              
                            /// Shape Tube rmin=mtdg.rpmtin+1 rmax=mtdg.rmax dz=mtdg.length/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_MUTD;              
                            _stacker -> Build(this);              
                      }           
                      barphi = 360.0/30.0;           
                      _create = AgCreate("MUSC");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create MUSC              
                            Create("MUSC");               
                      }           
                      { AgPlacement place = AgPlacement("MUSC","MUTD");              
                            /// Add daughter volume MUSC to mother MUTD              
                            place.TranslateZ(0);              
                            /// Translate z = 0              
                            place.AlphaZ(210);              
                            /// Rotate: AlphaZ = 210              
                            /// G3 Reference: thetax = 90              
                            /// G3 Reference: phix = 0              
                            /// G3 Reference: thetay = 90              
                            /// G3 Reference: phiy = 90              
                            /// G3 Reference: thetaz = 0              
                            /// G3 Reference: phiz = 0              
                            _stacker -> Position( AgBlock::Find("MUSC"), place );              
                      } // end placement of MUSC           
                      END_OF_MUTD:           
                      mCurrent = _save;           
                ///@}        
          } // End Block MUTD     
          // ---------------------------------------------------------------------------------------------------     
          void MUSC::Block( AgCreate create )     
          {         
                ///@addtogroup MUSC_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("MUSC");              
                            attr.par("seen")=0;              
                            attr.par("colo")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tubs");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("phi1")=-5.0;              
                            shape.par("phi2")=5.0;              
                            /// Shape Tubs phi1=-5.0 phi2=5.0               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_MUSC;              
                            _stacker -> Build(this);              
                      }           
                      /// Loop on ntray from 1 to 2 step=1           
                      for ( ntray=1; (1>0)? (ntray<=2):(ntray>=2); ntray+=1 )           
                      {              
                            _create = AgCreate("MTRA");              
                            {                 
                                  AgShape myshape; // undefined shape                 
                                  ///Create MTRA                 
                                  Create("MTRA");                  
                            }              
                            { AgPlacement place = AgPlacement("MTRA","MUSC");                 
                                  /// Add daughter volume MTRA to mother MUSC                 
                                  place.TranslateX(mtdg.radii(ntray)+(mtry.supfullh+mtry.height+mtry.stript)/2.0);                 
                                  /// Translate x = mtdg.radii(ntray)+(mtry.supfullh+mtry.height+mtry.stript)/2.0                 
                                  place.TranslateY(0);                 
                                  /// Translate y = 0                 
                                  place.TranslateZ(-128.81);                 
                                  /// Translate z = -128.81                 
                                  _stacker -> Position( AgBlock::Find("MTRA"), place );                 
                            } // end placement of MTRA              
                            _create = AgCreate("MPMT");              
                            {                 
                                  AgShape myshape; // undefined shape                 
                                  ///Create MPMT                 
                                  Create("MPMT");                  
                            }              
                            { AgPlacement place = AgPlacement("MPMT","MUSC");                 
                                  /// Add daughter volume MPMT to mother MUSC                 
                                  place.TranslateX(3.12/2+1.5+mtdg.rpmtin);                 
                                  /// Translate x = 3.12/2+1.5+mtdg.rpmtin                 
                                  place.TranslateY(0);                 
                                  /// Translate y = 0                 
                                  place.TranslateZ(0);                 
                                  /// Translate z = 0                 
                                  _stacker -> Position( AgBlock::Find("MPMT"), place );                 
                            } // end placement of MPMT              
                            _create = AgCreate("MMRP");              
                            {                 
                                  AgShape myshape; // undefined shape                 
                                  ///Create MMRP                 
                                  Create("MMRP");                  
                            }              
                            { AgPlacement place = AgPlacement("MMRP","MUSC");                 
                                  /// Add daughter volume MMRP to mother MUSC                 
                                  place.TranslateX(2.50/2.+mtdg.rmrpcin);                 
                                  /// Translate x = 2.50/2.+mtdg.rmrpcin                 
                                  place.TranslateY(0);                 
                                  /// Translate y = 0                 
                                  place.TranslateZ(-128.81);                 
                                  /// Translate z = -128.81                 
                                  _stacker -> Position( AgBlock::Find("MMRP"), place );                 
                            } // end placement of MMRP              
                      }           
                      END_OF_MUSC:           
                      mCurrent = _save;           
                ///@}        
          } // End Block MUSC     
          // ---------------------------------------------------------------------------------------------------     
          void MTRA::Block( AgCreate create )     
          {         
                ///@addtogroup MTRA_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("MTRA");              
                            attr.par("seen")=1;              
                            attr.par("colo")=2;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=(mtry.supfullh+mtry.height+mtry.stript)/2;              
                            shape.par("dy")=mtry.width/2;              
                            /// Shape Bbox dx=(mtry.supfullh+mtry.height+mtry.stript)/2 dy=mtry.width/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_MTRA;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("MXTR");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create MXTR              
                            Create("MXTR");               
                      }           
                      { AgPlacement place = AgPlacement("MXTR","MTRA");              
                            /// Add daughter volume MXTR to mother MTRA              
                            place.TranslateX((mtry.supfullh+mtry.stript)/2);              
                            /// Translate x = (mtry.supfullh+mtry.stript)/2              
                            place.TranslateZ((mtdg.dz-mtry.length)/2);              
                            /// Translate z = (mtdg.dz-mtry.length)/2              
                            _stacker -> Position( AgBlock::Find("MXTR"), place );              
                      } // end placement of MXTR           
                      END_OF_MTRA:           
                      mCurrent = _save;           
                ///@}        
          } // End Block MTRA     
          // ---------------------------------------------------------------------------------------------------     
          void MXTR::Block( AgCreate create )     
          {         
                ///@addtogroup MXTR_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("MXTR");              
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
                            shape.par("dx")=mtry.height/2;              
                            shape.par("dz")=mtry.length/2;              
                            /// Shape Bbox dx=mtry.height/2 dz=mtry.length/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_MXTR;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("MMTC");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create MMTC              
                            Create("MMTC");               
                      }           
                      { AgPlacement place = AgPlacement("MMTC","MXTR");              
                            /// Add daughter volume MMTC to mother MXTR              
                            _stacker -> Position( AgBlock::Find("MMTC"), place );              
                      } // end placement of MMTC           
                      END_OF_MXTR:           
                      mCurrent = _save;           
                ///@}        
          } // End Block MXTR     
          // ---------------------------------------------------------------------------------------------------     
          void MMTC::Block( AgCreate create )     
          {         
                ///@addtogroup MMTC_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("MMTC");              
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
                            shape.par("dx")=mtry.height/2-mtry.wallthk;              
                            shape.par("dy")=mtry.width/2-mtry.wallthk;              
                            shape.par("dz")=mtry.length/2-mtry.wallthk;              
                            /// Shape Bbox dx=mtry.height/2-mtry.wallthk dy=mtry.width/2-mtry.wallthk dz=mtry.length/2-mtry.wallthk               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_MMTC;              
                            _stacker -> Build(this);              
                      }           
                      zpos  =  (mtry.length-mtbb.slab1len)/2-mtry.wallthk-mtbb.wrap;           
                      xpos  =  -mtry.height/2+mtbb.slab1x;           
                      _create = AgCreate("MXSA");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create MXSA              
                            Create("MXSA");               
                      }           
                      { AgPlacement place = AgPlacement("MXSA","MMTC");              
                            /// Add daughter volume MXSA to mother MMTC              
                            place.TranslateX(xpos);              
                            /// Translate x = xpos              
                            place.TranslateZ(zpos);              
                            /// Translate z = zpos              
                            place.par("dx")=mtbb.slabthck/2;              
                            place.par("dy")=mtbb.slabwid/2;              
                            place.par("dz")=mtbb.slab1len/2;              
                            _stacker -> Position( AgBlock::Find("MXSA"), place );              
                      } // end placement of MXSA           
                      zpos  =  (mtry.length-mtbb.slab2len)/2-mtry.wallthk-mtbb.wrap-mtbb.shim;           
                      xpos  =  -mtry.height/2+mtbb.slab2x;           
                      _create = AgCreate("MXSA");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create MXSA              
                            Create("MXSA");               
                      }           
                      { AgPlacement place = AgPlacement("MXSA","MMTC");              
                            /// Add daughter volume MXSA to mother MMTC              
                            place.TranslateX(xpos);              
                            /// Translate x = xpos              
                            place.TranslateZ(-zpos);              
                            /// Translate z = -zpos              
                            place.par("dx")=mtbb.slabthck/2;              
                            place.par("dy")=mtbb.slabwid/2;              
                            place.par("dz")=mtbb.slab2len/2;              
                            _stacker -> Position( AgBlock::Find("MXSA"), place );              
                      } // end placement of MXSA           
                      END_OF_MMTC:           
                      mCurrent = _save;           
                ///@}        
          } // End Block MMTC     
          // ---------------------------------------------------------------------------------------------------     
          void MXSA::Block( AgCreate create )     
          {         
                ///@addtogroup MXSA_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("MXSA");              
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
                            if (_same_shape) goto END_OF_MXSA;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_MXSA:           
                      mCurrent = _save;           
                ///@}        
          } // End Block MXSA     
          // ---------------------------------------------------------------------------------------------------     
          void MPMT::Block( AgCreate create )     
          {         
                ///@addtogroup MPMT_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("MPMT");              
                            attr.par("seen")=1;              
                            attr.par("colo")=3;              
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
                            shape.par("dx")=3.12/2.;              
                            shape.par("dy")=57.20/2.;              
                            shape.par("dz")=mtdg.length/2;              
                            /// Shape Bbox dx=3.12/2. dy=57.20/2. dz=mtdg.length/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_MPMT;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_MPMT:           
                      mCurrent = _save;           
                ///@}        
          } // End Block MPMT     
          // ---------------------------------------------------------------------------------------------------     
          void MMRP::Block( AgCreate create )     
          {         
                ///@addtogroup MMRP_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("MMRP");              
                            attr.par("seen")=1;              
                            attr.par("colo")=3;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material RPCgas            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Rpcgas");              
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
                            shape.par("dx")=2.50/2.;              
                            shape.par("dy")=57.20/2.;              
                            shape.par("dz")=210.82/2;              
                            /// Shape Bbox dx=2.50/2. dy=57.20/2. dz=210.82/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_MMRP;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_MMRP:           
                      mCurrent = _save;           
                ///@}        
          } // End Block MMRP     
    // ----------------------------------------------------------------------- geoctr
       void MutdGeo3::ConstructGeometry( const Char_t *dummy )     
       {        
             ///@addtogroup MutdGeo3_revision        
             ///@{           
                   /// Author: Maxim Potekhin           
             ///@}        
             ///@addtogroup MutdGeo3_revision        
             ///@{           
                   /// Created:    21 March 2006            
             ///@}        
             AddBlock("MUTD");        
             AddBlock("MUSC");        
             AddBlock("MTRA");        
             AddBlock("MXTR");        
             AddBlock("MMTC");        
             AddBlock("MXSA");        
             AddBlock("MPMT");        
             AddBlock("MMRP");        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup mtdg_doc        
             ///@{           
                   ++mtdg._index;           
                   mtdg . version = 1; //  version number            
                   /// mtdg . version = 1; //  version number            
                   mtdg . rpmtin = 364.25; //  pmt box inner radius            
                   /// mtdg . rpmtin = 364.25; //  pmt box inner radius            
                   mtdg . rpmtout = 386.15; //  pmt box outer radius                  
                   /// mtdg . rpmtout = 386.15; //  pmt box outer radius                  
                   mtdg . rmrpcin = 403.60; //  mrpc box inner radius            
                   /// mtdg . rmrpcin = 403.60; //  mrpc box inner radius            
                   mtdg . rmrpcout = 411.22; //  mrpc box outer radius                  
                   /// mtdg . rmrpcout = 411.22; //  mrpc box outer radius                  
                   mtdg . rmin = 390.00; //  inner radius of the magnet system            
                   /// mtdg . rmin = 390.00; //  inner radius of the magnet system            
                   mtdg . rmax = 435.00; //  outer radius of the magnet system            
                   /// mtdg . rmax = 435.00; //  outer radius of the magnet system            
                   mtdg . dz = 246.0; //  CTB/TOF tube half length            
                   /// mtdg . dz = 246.0; //  CTB/TOF tube half length            
                   mtdg . length = 500.00; //  slightly longer than full length of the trays            
                   /// mtdg . length = 500.00; //  slightly longer than full length of the trays            
                   mtdg . radii.at(0) = 390.093; //  radii of trays            
                   ///mtdg . radii.at(0) = 390.093; //  radii of trays            
                   mtdg . radii.at(1) = 420.093; //  radii of trays            
                   ///mtdg . radii.at(1) = 420.093; //  radii of trays            
                   //           
                   mtdg.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup mtry_doc        
             ///@{           
                   ++mtry._index;           
                   mtry . height = 8.89; //  tray height(8.89)            
                   /// mtry . height = 8.89; //  tray height(8.89)            
                   mtry . width = 21.59; //  full tray width            
                   /// mtry . width = 21.59; //  full tray width            
                   mtry . length = 241.62; //  full tray length(241.62)            
                   /// mtry . length = 241.62; //  full tray length(241.62)            
                   mtry . wallthk = 0.13; //  tray wall thickness            
                   /// mtry . wallthk = 0.13; //  tray wall thickness            
                   mtry . supfullh = 2.03; //  support height (radial)            
                   /// mtry . supfullh = 2.03; //  support height (radial)            
                   mtry . supfullw = 15.24; //  support full width with arms            
                   /// mtry . supfullw = 15.24; //  support full width with arms            
                   mtry . suplen = 215.9; //  support length            
                   /// mtry . suplen = 215.9; //  support length            
                   mtry . supbasew = 9.22; //  support base width            
                   /// mtry . supbasew = 9.22; //  support base width            
                   mtry . supbaset = 0.32; //  support base thickness              
                   /// mtry . supbaset = 0.32; //  support base thickness              
                   mtry . suparmt = 0.64; //  support arm  thickness            
                   /// mtry . suparmt = 0.64; //  support arm  thickness            
                   mtry . cooloutr = 0.80; //  Cooling channel outer radius            
                   /// mtry . cooloutr = 0.80; //  Cooling channel outer radius            
                   mtry . coolinnr = 0.48; //  Cooling channel inner radius            
                   /// mtry . coolinnr = 0.48; //  Cooling channel inner radius            
                   mtry . stript = 0.08; //  Thickness of polyethylene strip on bottom            
                   /// mtry . stript = 0.08; //  Thickness of polyethylene strip on bottom            
                   mtry . footinse = 1.06; //  foot inset from tray edge            
                   /// mtry . footinse = 1.06; //  foot inset from tray edge            
                   mtry . footthk = 0.23; //  thickness of foot material            
                   /// mtry . footthk = 0.23; //  thickness of foot material            
                   mtry . foot1len = 1.68; //  length (in section) of first part of foot            
                   /// mtry . foot1len = 1.68; //  length (in section) of first part of foot            
                   mtry . foot2thk = 1.16; //  thickness of second foot section            
                   /// mtry . foot2thk = 1.16; //  thickness of second foot section            
                   mtry . foot3len = 2.16; //  length of third part of foot            
                   /// mtry . foot3len = 2.16; //  length of third part of foot            
                   //           
                   mtry.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup mtbb_doc        
             ///@{           
                   ++mtbb._index;           
                   mtbb . slab1len = 112.5; //  first slab (B) length            
                   /// mtbb . slab1len = 112.5; //  first slab (B) length            
                   mtbb . slab2len = 130.0; //  second slab (A)length             
                   /// mtbb . slab2len = 130.0; //  second slab (A)length             
                   mtbb . slab1x = 5.84; //  first slab (B) x position            
                   /// mtbb . slab1x = 5.84; //  first slab (B) x position            
                   mtbb . slab2x = 2.67; //  second slab (A) x position            
                   /// mtbb . slab2x = 2.67; //  second slab (A) x position            
                   mtbb . slabthck = 1.0; //  scintillator slab thicknesses            
                   /// mtbb . slabthck = 1.0; //  scintillator slab thicknesses            
                   mtbb . slabwid = 21.0; //  scintillator slab width            
                   /// mtbb . slabwid = 21.0; //  scintillator slab width            
                   mtbb . convlen = 8.5; //  optical converter length            
                   /// mtbb . convlen = 8.5; //  optical converter length            
                   mtbb . convwidm = 4.0; //  optical convertor min width            
                   /// mtbb . convwidm = 4.0; //  optical convertor min width            
                   mtbb . convthck = 0.92; //  optical convertor thickness            
                   /// mtbb . convthck = 0.92; //  optical convertor thickness            
                   mtbb . pmtlen = 5.0; //  PMT length            
                   /// mtbb . pmtlen = 5.0; //  PMT length            
                   mtbb . pmtmaxr = 2.0; //  PMT max radius            
                   /// mtbb . pmtmaxr = 2.0; //  PMT max radius            
                   mtbb . pmtminr = 1.84; //  PMT min radius            
                   /// mtbb . pmtminr = 1.84; //  PMT min radius            
                   mtbb . baselen = 4.0; //  Base length            
                   /// mtbb . baselen = 4.0; //  Base length            
                   mtbb . basemaxr = 2.13; //  Base max radius            
                   /// mtbb . basemaxr = 2.13; //  Base max radius            
                   mtbb . baseminr = 1.0; //  Base min radius            
                   /// mtbb . baseminr = 1.0; //  Base min radius            
                   mtbb . electhck = 0.25; //  readout electronics thickness            
                   /// mtbb . electhck = 0.25; //  readout electronics thickness            
                   mtbb . wrap = 0.13; //  thickness of Tyvek + black plastic            
                   /// mtbb . wrap = 0.13; //  thickness of Tyvek + black plastic            
                   mtbb . shim = 0.26; //  thickness of shim to position slat 2            
                   /// mtbb . shim = 0.26; //  thickness of shim to position slat 2            
                   //           
                   mtbb.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup moff_doc        
             ///@{           
                   ++moff._index;           
                   moff . boxwidth = 21.0875; //  width of the 5w box (BMTD)            
                   /// moff . boxwidth = 21.0875; //  width of the 5w box (BMTD)            
                   moff . slatlen = 20.0; //  slat length            
                   /// moff . slatlen = 20.0; //  slat length            
                   moff . slat01z = 104.938; //  5_wide_slat Z position for row 1 from AutoCAD            
                   /// moff . slat01z = 104.938; //  5_wide_slat Z position for row 1 from AutoCAD            
                   moff . slat02z = 84.060; //  4_wide_slat Z position for row 2 from AutoCAD            
                   /// moff . slat02z = 84.060; //  4_wide_slat Z position for row 2 from AutoCAD            
                   moff . slat03z = 62.860; //  4_wide_slat Z position for row 3 from AutoCAD            
                   /// moff . slat03z = 62.860; //  4_wide_slat Z position for row 3 from AutoCAD            
                   moff . slat04z = 41.254; //  4_wide_slat Z position for row 4 from AutoCAD            
                   /// moff . slat04z = 41.254; //  4_wide_slat Z position for row 4 from AutoCAD            
                   moff . slat05z = 18.966; //  4_wide_slat Z position for row 5 from AutoCAD            
                   /// moff . slat05z = 18.966; //  4_wide_slat Z position for row 5 from AutoCAD            
                   moff . slat06z = -3.954; //  4_wide_slat Z position for row 6 from AutoCAD            
                   /// moff . slat06z = -3.954; //  4_wide_slat Z position for row 6 from AutoCAD            
                   moff . slat07z = -27.528; //  4_wide_slat Z position for row 7 from AutoCAD            
                   /// moff . slat07z = -27.528; //  4_wide_slat Z position for row 7 from AutoCAD            
                   moff . slat08z = -51.254; //  4_wide_slat Z position for row 8 from AutoCAD            
                   /// moff . slat08z = -51.254; //  4_wide_slat Z position for row 8 from AutoCAD            
                   moff . slat09z = -75.634; //  4_wide_slat Z position for row 9 from AutoCAD            
                   /// moff . slat09z = -75.634; //  4_wide_slat Z position for row 9 from AutoCAD            
                   moff . slat10z = -100.683; //  4_wide_slat Z position for row 10 from AutoCAD            
                   /// moff . slat10z = -100.683; //  4_wide_slat Z position for row 10 from AutoCAD            
                   moff . slatthck = 2.0; //  scintillator slab thicknesses            
                   /// moff . slatthck = 2.0; //  scintillator slab thicknesses            
                   moff . slatwid = 3.81; //  scintillator slab width (4.0)            
                   /// moff . slatwid = 3.81; //  scintillator slab width (4.0)            
                   moff . slatang = 11.5; //  slat assy. angle w.r.t. tray            
                   /// moff . slatang = 11.5; //  slat assy. angle w.r.t. tray            
                   moff . pmtlen = 5.0; //  PMT length            
                   /// moff . pmtlen = 5.0; //  PMT length            
                   moff . pmtmaxr = 1.91; //  PMT max radius            
                   /// moff . pmtmaxr = 1.91; //  PMT max radius            
                   moff . pmtminr = 1.8; //  PMT min radius            
                   /// moff . pmtminr = 1.8; //  PMT min radius            
                   moff . socklen = 1.0; //  thickness of socket            
                   /// moff . socklen = 1.0; //  thickness of socket            
                   moff . baselen = 5.0; //  Base length            
                   /// moff . baselen = 5.0; //  Base length            
                   moff . basemaxr = 1.91; //  Base max radius            
                   /// moff . basemaxr = 1.91; //  Base max radius            
                   moff . baseminr = 1.8; //  Base min radius              
                   /// moff . baseminr = 1.8; //  Base min radius              
                   moff . cellwid = 3.1; //  Cell width             
                   /// moff . cellwid = 3.1; //  Cell width             
                   moff . cellhgt = 1.6; //  Cell height            
                   /// moff . cellhgt = 1.6; //  Cell height            
                   moff . elechgt = 3.0; //  FEE Board height in tray... (rails/loop too).            
                   /// moff . elechgt = 3.0; //  FEE Board height in tray... (rails/loop too).            
                   moff . electhck = 0.17; //  FEE Board thickness (67 mils)            
                   /// moff . electhck = 0.17; //  FEE Board thickness (67 mils)            
                   moff . elecwid = 20.3; //  FEE Board width (was 21)            
                   /// moff . elecwid = 20.3; //  FEE Board width (was 21)            
                   moff . eleclen = 5.715; //  FEE Board length (was 16)            
                   /// moff . eleclen = 5.715; //  FEE Board length (was 16)            
                   moff . elec01z = 105.610; //  FEE Z position for row 1 from AutoCAD            
                   /// moff . elec01z = 105.610; //  FEE Z position for row 1 from AutoCAD            
                   moff . elec02z = 84.573; //  FEE Z position for row 2 from AutoCAD            
                   /// moff . elec02z = 84.573; //  FEE Z position for row 2 from AutoCAD            
                   moff . elec03z = 63.224; //  FEE Z position for row 3 from AutoCAD            
                   /// moff . elec03z = 63.224; //  FEE Z position for row 3 from AutoCAD            
                   moff . elec04z = 41.667; //  FEE Z position for row 4 from AutoCAD            
                   /// moff . elec04z = 41.667; //  FEE Z position for row 4 from AutoCAD            
                   moff . elec05z = 19.379; //  FEE Z position for row 5 from AutoCAD            
                   /// moff . elec05z = 19.379; //  FEE Z position for row 5 from AutoCAD            
                   moff . elec06z = -3.542; //  FEE Z position for row 6 from AutoCAD            
                   /// moff . elec06z = -3.542; //  FEE Z position for row 6 from AutoCAD            
                   moff . elec07z = -27.165; //  FEE Z position for row 7 from AutoCAD            
                   /// moff . elec07z = -27.165; //  FEE Z position for row 7 from AutoCAD            
                   moff . elec08z = -50.841; //  FEE Z position for row 8 from AutoCAD            
                   /// moff . elec08z = -50.841; //  FEE Z position for row 8 from AutoCAD            
                   moff . elec09z = -75.170; //  FEE Z position for row 9 from AutoCAD            
                   /// moff . elec09z = -75.170; //  FEE Z position for row 9 from AutoCAD            
                   moff . elec10z = -100.270; //  FEE Z position for row 10 from AutoCAD            
                   /// moff . elec10z = -100.270; //  FEE Z position for row 10 from AutoCAD            
                   moff . railthck = 0.2; //  Cooling loop rail thickness            
                   /// moff . railthck = 0.2; //  Cooling loop rail thickness            
                   moff . railwid = 1.5; //  Cooling loop rail width            
                   /// moff . railwid = 1.5; //  Cooling loop rail width            
                   moff . cooloutr = 0.635; //  Cooling loop pipe outer radius, 0.5in/2            
                   /// moff . cooloutr = 0.635; //  Cooling loop pipe outer radius, 0.5in/2            
                   moff . coolinnr = 0.561; //  Cooling loop pipe inner radius, (0.5in-0.058in)/2            
                   /// moff . coolinnr = 0.561; //  Cooling loop pipe inner radius, (0.5in-0.058in)/2            
                   //           
                   moff.fill();           
             ///@}        
             //        
             /// USE mtdg _index=1;        
             mtdg.Use();        
             /// USE mtry _index=1;        
             mtry.Use();        
             /// USE mtbb _index=1;        
             mtbb.Use();        
             /// USE moff _index=1;        
             moff.Use();        
             /// Component H	a=1	z=1	w=0.90*2*1./102.+0.+0.05*10*1./58.        
             /// Component C	a=12	z=6	w=0.90*2*12./102.+0.+0.05*4*12./58.        
             /// Component F	a=19	z=9	w=0.90*4*19./102.+0.05*6*19./146.+0.        
             /// Component S	a=32	z=16	w=0.+0.05*1*32./146.+0.        
             /// Mixture RPCgas dens=4.55e-3        
             {  AgMaterial &mix = AgMaterial::Get("Rpcgas");           
                   mix.Component("H",1,1,0.90*2*1./102.+0.+0.05*10*1./58.);           
                   mix.Component("C",12,6,0.90*2*12./102.+0.+0.05*4*12./58.);           
                   mix.Component("F",19,9,0.90*4*19./102.+0.05*6*19./146.+0.);           
                   mix.Component("S",32,16,0.+0.05*1*32./146.+0.);           
                   mix.par("dens")=4.55e-3;           
                   mix.lock();           
                   _material = mix;           
                   _material.lock();           
             }        
             _create = AgCreate("MUTD");        
             {           
                   AgShape myshape; // undefined shape           
                   ///Create MUTD           
                   Create("MUTD");            
             }        
             { AgPlacement place = AgPlacement("MUTD","CAVE");           
                   /// Add daughter volume MUTD to mother CAVE           
                   _stacker -> Position( AgBlock::Find("MUTD"), place );           
             } // end placement of MUTD        
       }; // MutdGeo3     
 }; // namespace MutdGeo3  
 