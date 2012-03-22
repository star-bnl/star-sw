#include "SisdGeo.h"  
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
 namespace SISDGEO // $NMSPC  
 {     
       //     
       // ---------------------------------------------------------------------------------------------------     
          //  -----------------------------------------------------     
          /// @defgroup ssdp_doc     
          /// \class Ssdp_t     
          /// \brief User-defined structure     
          ///                             
          /// AgML structure members:     
          ///                             
          ///Float_t version;     
          ///Float_t config;     
          ///Int_t placement;     
          ///Int_t _index;     
          //     
          Ssdp_t ssdp;     
          //     
          //  -----------------------------------------------------     
          /// @defgroup sfpa_doc     
          /// \class Sfpa_t     
          /// \brief User-defined structure     
          ///                             
          /// AgML structure members:     
          ///                             
          ///Float_t version;     
          ///Float_t rmin;     
          ///Float_t rmax;     
          ///Float_t len;     
          ///Float_t rad;     
          ///Float_t nssd;     
          ///Float_t dmwid;     
          ///Float_t dmthk;     
          ///Float_t dmlen;     
          ///Float_t smwid;     
          ///Float_t smthk;     
          ///Float_t smlen;     
          ///Float_t sslen;     
          ///Float_t wplen;     
          ///Float_t sdlen;     
          ///Float_t tilt;     
          ///Float_t cprad;     
          ///Float_t cpral;     
          ///Float_t cfrad;     
          ///Float_t gpthk;     
          ///Array_t<Int_t> laddermap;     
          ///Array_t<Float_t> ladderangle;     
          ///Array_t<Float_t> laddertilt;     
          ///Array_t<Float_t> ladderradius;     
          ///Int_t _index;     
          //     
          Sfpa_t sfpa;     
          //     
          ///@addtogroup SisdGeo_vars     
          ///@{        
                Int_t ilad,iwaf,nc;        
                //        
                /// Int_t ilad,iwaf,nc        
          ///@}     
          ///@addtogroup SisdGeo_vars     
          ///@{        
                Float_t wafpcklen,dthk,radtilt,ang;        
                //        
                /// Float_t wafpcklen,dthk,radtilt,ang        
          ///@}     
       SisdGeo::SisdGeo()     
         : AgModule("SisdGeo","  is the Silicon Strip Detector ")     
       {        
       }     
          // ---------------------------------------------------------------------------------------------------     
          void SFMO::Block( AgCreate create )     
          {         
                ///@addtogroup SFMO_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Air            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Air");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("SISD");              
                            attr.par("seen")=1;              
                            attr.par("colo")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=sfpa.rmin;              
                            shape.par("rmax")=sfpa.rmax;              
                            shape.par("dz")=sfpa.len/2;              
                            /// Shape Tube rmin=sfpa.rmin rmax=sfpa.rmax dz=sfpa.len/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_SFMO;              
                            _stacker -> Build(this);              
                      }           
                      dthk=sfpa.smthk+sfpa.gpthk;           
                      _create = AgCreate("SFLM");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create SFLM              
                            Create("SFLM");               
                      }           
                      /// Loop on ilad from 1 to 20 step=1           
                      for ( ilad=1; (1>0)? (ilad<=20):(ilad>=20); ilad+=1 )           
                      {              
                            if ( sfpa.laddermap(ilad)>0 )              
                            {                 
                                  ang     = (sfpa.ladderangle(ilad)*pi)/180.0;                 
                                  radtilt = (sfpa.laddertilt(ilad) *pi)/180.0;                 
                                  if ( ilad==1 )                 
                                  {                    
                                        nc=1;                    
                                  }                 
                                  else                 
                                  {                    
                                        nc=20-ilad+2;                    
                                  }                 
                                  { AgPlacement place = AgPlacement("SFLM","SFMO");                    
                                        /// Add daughter volume SFLM to mother SFMO                    
                                        place.TranslateX((sfpa.ladderradius(ilad)*cos(ang)+(dthk*cos(ang+radtilt))/2.0));                    
                                        /// Translate x = (sfpa.ladderradius(ilad)*cos(ang)+(dthk*cos(ang+radtilt))/2.0)                    
                                        place.TranslateY((sfpa.ladderradius(ilad)*sin(ang)+(dthk*sin(ang+radtilt))/2.0));                    
                                        /// Translate y = (sfpa.ladderradius(ilad)*sin(ang)+(dthk*sin(ang+radtilt))/2.0)                    
                                        place.TranslateZ(0);                    
                                        /// Translate z = 0                    
                                        place.par("only")=AgPlacement::kMany;                    
                                        /// Overlap: agplacement::kmany                    
                                        place.par("ncopy")=nc;                    
                                        /// Ncopy: nc                    
                                        place.AlphaZ(sfpa.ladderangle(ilad)-90.0+sfpa.laddertilt(ilad));                    
                                        /// Rotate: AlphaZ = sfpa.ladderangle(ilad)-90.0+sfpa.laddertilt(ilad)                    
                                        /// G3 Reference: thetax = 90                    
                                        /// G3 Reference: phix = 0                    
                                        /// G3 Reference: thetay = 90                    
                                        /// G3 Reference: phiy = 90                    
                                        /// G3 Reference: thetaz = 0                    
                                        /// G3 Reference: phiz = 0                    
                                        _stacker -> Position( AgBlock::Find("SFLM"), place );                    
                                  } // end placement of SFLM                 
                            }              
                      }           
                      END_OF_SFMO:           
                      mCurrent = _save;           
                ///@}        
          } // End Block SFMO     
          // ---------------------------------------------------------------------------------------------------     
          void SFLM::Block( AgCreate create )     
          {         
                ///@addtogroup SFLM_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Air            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Air");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("SFLM");              
                            attr.par("seen")=1;              
                            attr.par("colo")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=sfpa.dmwid/2;              
                            shape.par("dy")=(sfpa.dmthk+sfpa.gpthk+sfpa.smthk)/2;              
                            shape.par("dz")=sfpa.smlen/2;              
                            /// Shape Bbox dx=sfpa.dmwid/2 dy=(sfpa.dmthk+sfpa.gpthk+sfpa.smthk)/2 dz=sfpa.smlen/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_SFLM;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("SFDM");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create SFDM              
                            Create("SFDM");               
                      }           
                      { AgPlacement place = AgPlacement("SFDM","SFLM");              
                            /// Add daughter volume SFDM to mother SFLM              
                            place.TranslateY(-(sfpa.smthk+sfpa.gpthk)/2);              
                            /// Translate y = -(sfpa.smthk+sfpa.gpthk)/2              
                            _stacker -> Position( AgBlock::Find("SFDM"), place );              
                      } // end placement of SFDM           
                      _create = AgCreate("SFSM");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create SFSM              
                            Create("SFSM");               
                      }           
                      { AgPlacement place = AgPlacement("SFSM","SFLM");              
                            /// Add daughter volume SFSM to mother SFLM              
                            place.TranslateY((sfpa.dmthk+sfpa.gpthk)/2);              
                            /// Translate y = (sfpa.dmthk+sfpa.gpthk)/2              
                            _stacker -> Position( AgBlock::Find("SFSM"), place );              
                      } // end placement of SFSM           
                      END_OF_SFLM:           
                      mCurrent = _save;           
                ///@}        
          } // End Block SFLM     
          // ---------------------------------------------------------------------------------------------------     
          void SFDM::Block( AgCreate create )     
          {         
                ///@addtogroup SFDM_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Air            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Air");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("SFDM");              
                            attr.par("seen")=0;              
                            attr.par("colo")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=sfpa.dmwid/2;              
                            shape.par("dy")=sfpa.dmthk/2;              
                            shape.par("dz")=sfpa.wplen/2;              
                            /// Shape Bbox dx=sfpa.dmwid/2 dy=sfpa.dmthk/2 dz=sfpa.wplen/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_SFDM;              
                            _stacker -> Build(this);              
                      }           
                      wafpcklen=sfpa.wplen/(sfpa.nssd*1.);           
                      /// Loop on iwaf from 1 to sfpa.nssd step=1           
                      for ( iwaf=1; (1>0)? (iwaf<=sfpa.nssd):(iwaf>=sfpa.nssd); iwaf+=1 )           
                      {              
                            _create = AgCreate("SFSW");              
                            {                 
                                  AgShape myshape; // undefined shape                 
                                  ///Create SFSW                 
                                  Create("SFSW");                  
                            }              
                            { AgPlacement place = AgPlacement("SFSW","SFDM");                 
                                  /// Add daughter volume SFSW to mother SFDM                 
                                  place.TranslateZ(-(sfpa.wplen+wafpcklen)/2+iwaf*wafpcklen);                 
                                  /// Translate z = -(sfpa.wplen+wafpcklen)/2+iwaf*wafpcklen                 
                                  _stacker -> Position( AgBlock::Find("SFSW"), place );                 
                            } // end placement of SFSW              
                      }           
                      END_OF_SFDM:           
                      mCurrent = _save;           
                ///@}        
          } // End Block SFDM     
          // ---------------------------------------------------------------------------------------------------     
          void SFSW::Block( AgCreate create )     
          {         
                ///@addtogroup SFSW_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("SFSW");              
                            attr.par("seen")=0;              
                            attr.par("colo")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=sfpa.dmwid/2;              
                            shape.par("dy")=sfpa.dmthk/2;              
                            shape.par("dz")=wafpcklen/2;              
                            /// Shape Bbox dx=sfpa.dmwid/2 dy=sfpa.dmthk/2 dz=wafpcklen/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_SFSW;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("SFSD");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create SFSD              
                            Create("SFSD");               
                      }           
                      { AgPlacement place = AgPlacement("SFSD","SFSW");              
                            /// Add daughter volume SFSD to mother SFSW              
                            _stacker -> Position( AgBlock::Find("SFSD"), place );              
                      } // end placement of SFSD           
                      END_OF_SFSW:           
                      mCurrent = _save;           
                ///@}        
          } // End Block SFSW     
          // ---------------------------------------------------------------------------------------------------     
          void SFSD::Block( AgCreate create )     
          {         
                ///@addtogroup SFSD_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Silicon            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Silicon");              
                            _material = mat;              
                      }           
                      /// Material Sensitive isvol=1            
                      { AgMaterial &mat = AgMaterial::Get("Sensitive");              
                            mat.par("isvol")=1;              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("SFSD");              
                            attr.par("seen")=2;              
                            attr.par("colo")=4;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=sfpa.dmwid/2;              
                            shape.par("dy")=sfpa.dmthk/2;              
                            shape.par("dz")=sfpa.sdlen/2;              
                            /// Shape Bbox dx=sfpa.dmwid/2 dy=sfpa.dmthk/2 dz=sfpa.sdlen/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_SFSD;              
                            _stacker -> Build(this);              
                      }           
                      /*{              
                            GSTPAR( %imed,"stra",1. );// CALL GSTPAR              
                      }*/           
                      END_OF_SFSD:           
                      mCurrent = _save;           
                ///@}        
          } // End Block SFSD     
          // ---------------------------------------------------------------------------------------------------     
          void SFSM::Block( AgCreate create )     
          {         
                ///@addtogroup SFSM_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Air            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Air");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("SFSM");              
                            attr.par("seen")=0;              
                            attr.par("colo")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=sfpa.smwid/2;              
                            shape.par("dy")=sfpa.smthk/2;              
                            shape.par("dz")=sfpa.smlen/2;              
                            /// Shape Bbox dx=sfpa.smwid/2 dy=sfpa.smthk/2 dz=sfpa.smlen/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_SFSM;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("SFSS");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create SFSS              
                            Create("SFSS");               
                      }           
                      END_OF_SFSM:           
                      mCurrent = _save;           
                ///@}        
          } // End Block SFSM     
          // ---------------------------------------------------------------------------------------------------     
          void SFSS::Block( AgCreate create )     
          {         
                ///@addtogroup SFSS_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Air            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Air");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("SFSS");              
                            attr.par("seen")=0;              
                            attr.par("colo")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Division");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("ndiv")=20;              
                            shape.par("iaxis")=3;              
                            /// Shape Division ndiv=20 iaxis=3               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_SFSS;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("SFCP");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create SFCP              
                            Create("SFCP");               
                      }           
                      { AgPlacement place = AgPlacement("SFCP","SFSS");              
                            /// Add daughter volume SFCP to mother SFSS              
                            place.TranslateX(sfpa.smwid/2-5.*sfpa.cprad);              
                            /// Translate x = sfpa.smwid/2-5.*sfpa.cprad              
                            place.TranslateY(-sfpa.smthk/2+sfpa.cprad);              
                            /// Translate y = -sfpa.smthk/2+sfpa.cprad              
                            _stacker -> Position( AgBlock::Find("SFCP"), place );              
                      } // end placement of SFCP           
                      { AgPlacement place = AgPlacement("SFCP","SFSS");              
                            /// Add daughter volume SFCP to mother SFSS              
                            place.TranslateX(-sfpa.smwid/2+5.*sfpa.cprad);              
                            /// Translate x = -sfpa.smwid/2+5.*sfpa.cprad              
                            place.TranslateY(-sfpa.smthk/2+sfpa.cprad);              
                            /// Translate y = -sfpa.smthk/2+sfpa.cprad              
                            _stacker -> Position( AgBlock::Find("SFCP"), place );              
                      } // end placement of SFCP           
                      _create = AgCreate("SFCF");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create SFCF              
                            Create("SFCF");               
                      }           
                      { AgPlacement place = AgPlacement("SFCF","SFSS");              
                            /// Add daughter volume SFCF to mother SFSS              
                            _stacker -> Position( AgBlock::Find("SFCF"), place );              
                      } // end placement of SFCF           
                      END_OF_SFSS:           
                      mCurrent = _save;           
                ///@}        
          } // End Block SFSS     
          // ---------------------------------------------------------------------------------------------------     
          void SFCP::Block( AgCreate create )     
          {         
                ///@addtogroup SFCP_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Carbon            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Carbon");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("SFCP");              
                            attr.par("seen")=1;              
                            attr.par("colo")=6;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=0;              
                            shape.par("rmax")=sfpa.cprad;              
                            shape.par("dz")=sfpa.sslen/2;              
                            /// Shape Tube rmin=0 rmax=sfpa.cprad dz=sfpa.sslen/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_SFCP;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("SFCW");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create SFCW              
                            Create("SFCW");               
                      }           
                      { AgPlacement place = AgPlacement("SFCW","SFCP");              
                            /// Add daughter volume SFCW to mother SFCP              
                            _stacker -> Position( AgBlock::Find("SFCW"), place );              
                      } // end placement of SFCW           
                      END_OF_SFCP:           
                      mCurrent = _save;           
                ///@}        
          } // End Block SFCP     
          // ---------------------------------------------------------------------------------------------------     
          void SFCW::Block( AgCreate create )     
          {         
                ///@addtogroup SFCW_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("SFCW");              
                            attr.par("seen")=1;              
                            attr.par("colo")=6;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material Water            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Water");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmax")=sfpa.cpral;              
                            /// Shape Tube rmax=sfpa.cpral               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_SFCW;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_SFCW:           
                      mCurrent = _save;           
                ///@}        
          } // End Block SFCW     
          // ---------------------------------------------------------------------------------------------------     
          void SFCF::Block( AgCreate create )     
          {         
                ///@addtogroup SFCF_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Air            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Air");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("SFCF");              
                            attr.par("seen")=0;              
                            attr.par("colo")=3;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=sfpa.smthk*tan(pi/6.);              
                            shape.par("dy")=sfpa.smthk/2.;              
                            shape.par("dz")=sfpa.sslen/2;              
                            /// Shape Bbox dx=sfpa.smthk*tan(pi/6.) dy=sfpa.smthk/2. dz=sfpa.sslen/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_SFCF;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("SFCT");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create SFCT              
                            Create("SFCT");               
                      }           
                      { AgPlacement place = AgPlacement("SFCT","SFCF");              
                            /// Add daughter volume SFCT to mother SFCF              
                            place.TranslateY(sfpa.smthk/2.-sfpa.cfrad);              
                            /// Translate y = sfpa.smthk/2.-sfpa.cfrad              
                            _stacker -> Position( AgBlock::Find("SFCT"), place );              
                      } // end placement of SFCT           
                      { AgPlacement place = AgPlacement("SFCT","SFCF");              
                            /// Add daughter volume SFCT to mother SFCF              
                            place.TranslateX(sfpa.smthk*tan(pi/6.)-sfpa.cfrad);              
                            /// Translate x = sfpa.smthk*tan(pi/6.)-sfpa.cfrad              
                            place.TranslateY(-sfpa.smthk/2.+sfpa.cfrad);              
                            /// Translate y = -sfpa.smthk/2.+sfpa.cfrad              
                            _stacker -> Position( AgBlock::Find("SFCT"), place );              
                      } // end placement of SFCT           
                      { AgPlacement place = AgPlacement("SFCT","SFCF");              
                            /// Add daughter volume SFCT to mother SFCF              
                            place.TranslateX(-sfpa.smthk*tan(pi/6.)+sfpa.cfrad);              
                            /// Translate x = -sfpa.smthk*tan(pi/6.)+sfpa.cfrad              
                            place.TranslateY(-sfpa.smthk/2.+sfpa.cfrad);              
                            /// Translate y = -sfpa.smthk/2.+sfpa.cfrad              
                            _stacker -> Position( AgBlock::Find("SFCT"), place );              
                      } // end placement of SFCT           
                      _create = AgCreate("SFCX");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create SFCX              
                            Create("SFCX");               
                      }           
                      { AgPlacement place = AgPlacement("SFCX","SFCF");              
                            /// Add daughter volume SFCX to mother SFCF              
                            place.TranslateY(-sfpa.smthk/2.+sfpa.cfrad);              
                            /// Translate y = -sfpa.smthk/2.+sfpa.cfrad              
                            /// G3 Reference: thetax = 90              
                            /// G3 Reference: phix = 0              
                            /// G3 Reference: thetay = 90              
                            /// G3 Reference: phiy = 90              
                            /// G3 Reference: thetaz = 0              
                            /// G3 Reference: phiz = 0              
                            place.Ortho( "yzx" ); // ORT=yzx              
                            /// Axis substitution: XYZ --> yzx              
                            _stacker -> Position( AgBlock::Find("SFCX"), place );              
                      } // end placement of SFCX           
                      { AgPlacement place = AgPlacement("SFCX","SFCF");              
                            /// Add daughter volume SFCX to mother SFCF              
                            place.TranslateX(sfpa.smthk*tan(pi/6.)/2.-sfpa.cfrad/3.);              
                            /// Translate x = sfpa.smthk*tan(pi/6.)/2.-sfpa.cfrad/3.              
                            /// G3 Reference: thetax = 90              
                            /// G3 Reference: phix = 0              
                            /// G3 Reference: thetay = 90              
                            /// G3 Reference: phiy = 90              
                            /// G3 Reference: thetaz = 0              
                            /// G3 Reference: phiz = 0              
                            place.Ortho( "yzx" ); // ORT=yzx              
                            /// Axis substitution: XYZ --> yzx              
                            place.AlphaZ(-60);              
                            /// Rotate: AlphaZ = -60              
                            /// G3 Reference: thetax = 90              
                            /// G3 Reference: phix = 0              
                            /// G3 Reference: thetay = 90              
                            /// G3 Reference: phiy = 90              
                            /// G3 Reference: thetaz = 0              
                            /// G3 Reference: phiz = 0              
                            _stacker -> Position( AgBlock::Find("SFCX"), place );              
                      } // end placement of SFCX           
                      { AgPlacement place = AgPlacement("SFCX","SFCF");              
                            /// Add daughter volume SFCX to mother SFCF              
                            place.TranslateX(-sfpa.smthk*tan(pi/6.)/2.+sfpa.cfrad/3.);              
                            /// Translate x = -sfpa.smthk*tan(pi/6.)/2.+sfpa.cfrad/3.              
                            /// G3 Reference: thetax = 90              
                            /// G3 Reference: phix = 0              
                            /// G3 Reference: thetay = 90              
                            /// G3 Reference: phiy = 90              
                            /// G3 Reference: thetaz = 0              
                            /// G3 Reference: phiz = 0              
                            place.Ortho( "yzx" ); // ORT=yzx              
                            /// Axis substitution: XYZ --> yzx              
                            place.AlphaZ(+60);              
                            /// Rotate: AlphaZ = +60              
                            /// G3 Reference: thetax = 90              
                            /// G3 Reference: phix = 0              
                            /// G3 Reference: thetay = 90              
                            /// G3 Reference: phiy = 90              
                            /// G3 Reference: thetaz = 0              
                            /// G3 Reference: phiz = 0              
                            _stacker -> Position( AgBlock::Find("SFCX"), place );              
                      } // end placement of SFCX           
                      END_OF_SFCF:           
                      mCurrent = _save;           
                ///@}        
          } // End Block SFCF     
          // ---------------------------------------------------------------------------------------------------     
          void SFCT::Block( AgCreate create )     
          {         
                ///@addtogroup SFCT_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Carbon            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Carbon");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("SFCT");              
                            attr.par("seen")=1;              
                            attr.par("colo")=6;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=0;              
                            shape.par("rmax")=sfpa.cfrad;              
                            shape.par("dz")=sfpa.sslen/2;              
                            /// Shape Tube rmin=0 rmax=sfpa.cfrad dz=sfpa.sslen/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_SFCT;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_SFCT:           
                      mCurrent = _save;           
                ///@}        
          } // End Block SFCT     
          // ---------------------------------------------------------------------------------------------------     
          void SFCX::Block( AgCreate create )     
          {         
                ///@addtogroup SFCX_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Carbon            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Carbon");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("SFCX");              
                            attr.par("seen")=1;              
                            attr.par("colo")=7;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=0;              
                            shape.par("rmax")=sfpa.cfrad;              
                            shape.par("dz")=sfpa.smthk*tan(pi/6.)-sfpa.cfrad;              
                            /// Shape Tube rmin=0 rmax=sfpa.cfrad dz=sfpa.smthk*tan(pi/6.)-sfpa.cfrad               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_SFCX;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_SFCX:           
                      mCurrent = _save;           
                ///@}        
          } // End Block SFCX     
    // ----------------------------------------------------------------------- geoctr
       void SisdGeo::ConstructGeometry( const Char_t *dummy )     
       {        
             ///@addtogroup SisdGeo_revision        
             ///@{           
                   /// Author: Maxim Potekhin           
             ///@}        
             ///@addtogroup SisdGeo_revision        
             ///@{           
                   /// Created:  17 Nov 03            
             ///@}        
             AddBlock("SFMO");        
             AddBlock("SFLM");        
             AddBlock("SFDM");        
             AddBlock("SFSW");        
             AddBlock("SFSD");        
             AddBlock("SFSM");        
             AddBlock("SFSS");        
             AddBlock("SFCP");        
             AddBlock("SFCW");        
             AddBlock("SFCF");        
             AddBlock("SFCT");        
             AddBlock("SFCX");        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup ssdp_doc        
             ///@{           
                   ++ssdp._index;           
                   ssdp . version = 1; //  Version            
                   /// ssdp . version = 1; //  Version            
                   ssdp . config = 1; //  There are a few configuraions possible            
                   /// ssdp . config = 1; //  There are a few configuraions possible            
                   ssdp . placement = 0; //  0=cave, 1=svtt            
                   /// ssdp . placement = 0; //  0=cave, 1=svtt            
                   //           
                   ssdp.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup sfpa_doc        
             ///@{           
                   ++sfpa._index;           
                   sfpa . version = 1; //  geometry version            
                   /// sfpa . version = 1; //  geometry version            
                   sfpa . rmin = 21.8; //  mother rmin            
                   /// sfpa . rmin = 21.8; //  mother rmin            
                   sfpa . rmax = 29.5; //  mother rmax            
                   /// sfpa . rmax = 29.5; //  mother rmax            
                   sfpa . len = 100.; //  mother Len            
                   /// sfpa . len = 100.; //  mother Len            
                   sfpa . rad = 23.; //  distance from beam axis to detector center            
                   /// sfpa . rad = 23.; //  distance from beam axis to detector center            
                   sfpa . nssd = 16; //  number of silicon strip detectors             
                   /// sfpa . nssd = 16; //  number of silicon strip detectors             
                   sfpa . dmwid = 7.5; //  detector mother width             
                   /// sfpa . dmwid = 7.5; //  detector mother width             
                   sfpa . dmthk = 0.03; //  detector mother thickness            
                   /// sfpa . dmthk = 0.03; //  detector mother thickness            
                   sfpa . dmlen = 90.; //  detector mother length (detectors + adc board)             
                   /// sfpa . dmlen = 90.; //  detector mother length (detectors + adc board)             
                   sfpa . smwid = 7.5; //  structure mother width            
                   /// sfpa . smwid = 7.5; //  structure mother width            
                   sfpa . smthk = 3.5; //  structure mother thickness            
                   /// sfpa . smthk = 3.5; //  structure mother thickness            
                   sfpa . smlen = 95.; //  structure mother length (cool. pipe+carbon fiber)            
                   /// sfpa . smlen = 95.; //  structure mother length (cool. pipe+carbon fiber)            
                   sfpa . sslen = 95./20.; //  length of a subvolume of the structure            
                   /// sfpa . sslen = 95./20.; //  length of a subvolume of the structure            
                   sfpa . wplen = 68.8; //  length of wafer pack            
                   /// sfpa . wplen = 68.8; //  length of wafer pack            
                   sfpa . sdlen = 4.2; //  lenght of one strip detector (along beam axis)            
                   /// sfpa . sdlen = 4.2; //  lenght of one strip detector (along beam axis)            
                   sfpa . tilt = 5.0; //  tiling angle (degrees)            
                   /// sfpa . tilt = 5.0; //  tiling angle (degrees)            
                   sfpa . cprad = 0.1; //  cooling pipe outer radius            
                   /// sfpa . cprad = 0.1; //  cooling pipe outer radius            
                   sfpa . cpral = 0.09; //  cooling pipe inner radius            
                   /// sfpa . cpral = 0.09; //  cooling pipe inner radius            
                   sfpa . cfrad = 0.1; //  carbon fiber tube radius (support structure)            
                   /// sfpa . cfrad = 0.1; //  carbon fiber tube radius (support structure)            
                   sfpa . gpthk = 0.5; //  gap between structure mother and detector            
                   /// sfpa . gpthk = 0.5; //  gap between structure mother and detector            
                   sfpa . laddermap.at(0) = 0; //  presence of ladders            
                   ///sfpa . laddermap.at(0) = 0; //  presence of ladders            
                   sfpa . laddermap.at(1) = 0; //  presence of ladders            
                   ///sfpa . laddermap.at(1) = 0; //  presence of ladders            
                   sfpa . laddermap.at(2) = 1; //  presence of ladders            
                   ///sfpa . laddermap.at(2) = 1; //  presence of ladders            
                   sfpa . laddermap.at(3) = 0; //  presence of ladders            
                   ///sfpa . laddermap.at(3) = 0; //  presence of ladders            
                   sfpa . laddermap.at(4) = 0; //  presence of ladders            
                   ///sfpa . laddermap.at(4) = 0; //  presence of ladders            
                   sfpa . laddermap.at(5) = 0; //  presence of ladders            
                   ///sfpa . laddermap.at(5) = 0; //  presence of ladders            
                   sfpa . laddermap.at(6) = 0; //  presence of ladders            
                   ///sfpa . laddermap.at(6) = 0; //  presence of ladders            
                   sfpa . laddermap.at(7) = 0; //  presence of ladders            
                   ///sfpa . laddermap.at(7) = 0; //  presence of ladders            
                   sfpa . laddermap.at(8) = 0; //  presence of ladders            
                   ///sfpa . laddermap.at(8) = 0; //  presence of ladders            
                   sfpa . laddermap.at(9) = 0; //  presence of ladders            
                   ///sfpa . laddermap.at(9) = 0; //  presence of ladders            
                   sfpa . laddermap.at(10) = 0; //  presence of ladders            
                   ///sfpa . laddermap.at(10) = 0; //  presence of ladders            
                   sfpa . laddermap.at(11) = 0; //  presence of ladders            
                   ///sfpa . laddermap.at(11) = 0; //  presence of ladders            
                   sfpa . laddermap.at(12) = 0; //  presence of ladders            
                   ///sfpa . laddermap.at(12) = 0; //  presence of ladders            
                   sfpa . laddermap.at(13) = 0; //  presence of ladders            
                   ///sfpa . laddermap.at(13) = 0; //  presence of ladders            
                   sfpa . laddermap.at(14) = 0; //  presence of ladders            
                   ///sfpa . laddermap.at(14) = 0; //  presence of ladders            
                   sfpa . laddermap.at(15) = 0; //  presence of ladders            
                   ///sfpa . laddermap.at(15) = 0; //  presence of ladders            
                   sfpa . laddermap.at(16) = 0; //  presence of ladders            
                   ///sfpa . laddermap.at(16) = 0; //  presence of ladders            
                   sfpa . laddermap.at(17) = 0; //  presence of ladders            
                   ///sfpa . laddermap.at(17) = 0; //  presence of ladders            
                   sfpa . laddermap.at(18) = 0; //  presence of ladders            
                   ///sfpa . laddermap.at(18) = 0; //  presence of ladders            
                   sfpa . laddermap.at(19) = 0; //  presence of ladders            
                   ///sfpa . laddermap.at(19) = 0; //  presence of ladders            
                   sfpa . ladderangle.at(0) = -1.0; //  individual angles            
                   ///sfpa . ladderangle.at(0) = -1.0; //  individual angles            
                   sfpa . ladderangle.at(1) = -1.0; //  individual angles            
                   ///sfpa . ladderangle.at(1) = -1.0; //  individual angles            
                   sfpa . ladderangle.at(2) = 45.0; //  individual angles            
                   ///sfpa . ladderangle.at(2) = 45.0; //  individual angles            
                   sfpa . ladderangle.at(3) = -1.0; //  individual angles            
                   ///sfpa . ladderangle.at(3) = -1.0; //  individual angles            
                   sfpa . ladderangle.at(4) = -1.0; //  individual angles            
                   ///sfpa . ladderangle.at(4) = -1.0; //  individual angles            
                   sfpa . ladderangle.at(5) = -1.0; //  individual angles            
                   ///sfpa . ladderangle.at(5) = -1.0; //  individual angles            
                   sfpa . ladderangle.at(6) = -1.0; //  individual angles            
                   ///sfpa . ladderangle.at(6) = -1.0; //  individual angles            
                   sfpa . ladderangle.at(7) = -1.0; //  individual angles            
                   ///sfpa . ladderangle.at(7) = -1.0; //  individual angles            
                   sfpa . ladderangle.at(8) = -1.0; //  individual angles            
                   ///sfpa . ladderangle.at(8) = -1.0; //  individual angles            
                   sfpa . ladderangle.at(9) = -1.0; //  individual angles            
                   ///sfpa . ladderangle.at(9) = -1.0; //  individual angles            
                   sfpa . ladderangle.at(10) = -1.0; //  individual angles            
                   ///sfpa . ladderangle.at(10) = -1.0; //  individual angles            
                   sfpa . ladderangle.at(11) = -1.0; //  individual angles            
                   ///sfpa . ladderangle.at(11) = -1.0; //  individual angles            
                   sfpa . ladderangle.at(12) = -1.0; //  individual angles            
                   ///sfpa . ladderangle.at(12) = -1.0; //  individual angles            
                   sfpa . ladderangle.at(13) = -1.0; //  individual angles            
                   ///sfpa . ladderangle.at(13) = -1.0; //  individual angles            
                   sfpa . ladderangle.at(14) = -1.0; //  individual angles            
                   ///sfpa . ladderangle.at(14) = -1.0; //  individual angles            
                   sfpa . ladderangle.at(15) = -1.0; //  individual angles            
                   ///sfpa . ladderangle.at(15) = -1.0; //  individual angles            
                   sfpa . ladderangle.at(16) = -1.0; //  individual angles            
                   ///sfpa . ladderangle.at(16) = -1.0; //  individual angles            
                   sfpa . ladderangle.at(17) = -1.0; //  individual angles            
                   ///sfpa . ladderangle.at(17) = -1.0; //  individual angles            
                   sfpa . ladderangle.at(18) = -1.0; //  individual angles            
                   ///sfpa . ladderangle.at(18) = -1.0; //  individual angles            
                   sfpa . ladderangle.at(19) = -1.0; //  individual angles            
                   ///sfpa . ladderangle.at(19) = -1.0; //  individual angles            
                   sfpa . laddertilt.at(0) = -1.0; //  individual tilts            
                   ///sfpa . laddertilt.at(0) = -1.0; //  individual tilts            
                   sfpa . laddertilt.at(1) = -1.0; //  individual tilts            
                   ///sfpa . laddertilt.at(1) = -1.0; //  individual tilts            
                   sfpa . laddertilt.at(2) = 0.0; //  individual tilts            
                   ///sfpa . laddertilt.at(2) = 0.0; //  individual tilts            
                   sfpa . laddertilt.at(3) = -1.0; //  individual tilts            
                   ///sfpa . laddertilt.at(3) = -1.0; //  individual tilts            
                   sfpa . laddertilt.at(4) = -1.0; //  individual tilts            
                   ///sfpa . laddertilt.at(4) = -1.0; //  individual tilts            
                   sfpa . laddertilt.at(5) = -1.0; //  individual tilts            
                   ///sfpa . laddertilt.at(5) = -1.0; //  individual tilts            
                   sfpa . laddertilt.at(6) = -1.0; //  individual tilts            
                   ///sfpa . laddertilt.at(6) = -1.0; //  individual tilts            
                   sfpa . laddertilt.at(7) = -1.0; //  individual tilts            
                   ///sfpa . laddertilt.at(7) = -1.0; //  individual tilts            
                   sfpa . laddertilt.at(8) = -1.0; //  individual tilts            
                   ///sfpa . laddertilt.at(8) = -1.0; //  individual tilts            
                   sfpa . laddertilt.at(9) = -1.0; //  individual tilts            
                   ///sfpa . laddertilt.at(9) = -1.0; //  individual tilts            
                   sfpa . laddertilt.at(10) = -1.0; //  individual tilts            
                   ///sfpa . laddertilt.at(10) = -1.0; //  individual tilts            
                   sfpa . laddertilt.at(11) = -1.0; //  individual tilts            
                   ///sfpa . laddertilt.at(11) = -1.0; //  individual tilts            
                   sfpa . laddertilt.at(12) = -1.0; //  individual tilts            
                   ///sfpa . laddertilt.at(12) = -1.0; //  individual tilts            
                   sfpa . laddertilt.at(13) = -1.0; //  individual tilts            
                   ///sfpa . laddertilt.at(13) = -1.0; //  individual tilts            
                   sfpa . laddertilt.at(14) = -1.0; //  individual tilts            
                   ///sfpa . laddertilt.at(14) = -1.0; //  individual tilts            
                   sfpa . laddertilt.at(15) = -1.0; //  individual tilts            
                   ///sfpa . laddertilt.at(15) = -1.0; //  individual tilts            
                   sfpa . laddertilt.at(16) = -1.0; //  individual tilts            
                   ///sfpa . laddertilt.at(16) = -1.0; //  individual tilts            
                   sfpa . laddertilt.at(17) = -1.0; //  individual tilts            
                   ///sfpa . laddertilt.at(17) = -1.0; //  individual tilts            
                   sfpa . laddertilt.at(18) = -1.0; //  individual tilts            
                   ///sfpa . laddertilt.at(18) = -1.0; //  individual tilts            
                   sfpa . laddertilt.at(19) = -1.0; //  individual tilts            
                   ///sfpa . laddertilt.at(19) = -1.0; //  individual tilts            
                   sfpa . ladderradius.at(0) = -1.0; //  individual radii            
                   ///sfpa . ladderradius.at(0) = -1.0; //  individual radii            
                   sfpa . ladderradius.at(1) = -1.0; //  individual radii            
                   ///sfpa . ladderradius.at(1) = -1.0; //  individual radii            
                   sfpa . ladderradius.at(2) = 23.000; //  individual radii            
                   ///sfpa . ladderradius.at(2) = 23.000; //  individual radii            
                   sfpa . ladderradius.at(3) = -1.0; //  individual radii            
                   ///sfpa . ladderradius.at(3) = -1.0; //  individual radii            
                   sfpa . ladderradius.at(4) = -1.0; //  individual radii            
                   ///sfpa . ladderradius.at(4) = -1.0; //  individual radii            
                   sfpa . ladderradius.at(5) = -1.0; //  individual radii            
                   ///sfpa . ladderradius.at(5) = -1.0; //  individual radii            
                   sfpa . ladderradius.at(6) = -1.0; //  individual radii            
                   ///sfpa . ladderradius.at(6) = -1.0; //  individual radii            
                   sfpa . ladderradius.at(7) = -1.0; //  individual radii            
                   ///sfpa . ladderradius.at(7) = -1.0; //  individual radii            
                   sfpa . ladderradius.at(8) = -1.0; //  individual radii            
                   ///sfpa . ladderradius.at(8) = -1.0; //  individual radii            
                   sfpa . ladderradius.at(9) = -1.0; //  individual radii            
                   ///sfpa . ladderradius.at(9) = -1.0; //  individual radii            
                   sfpa . ladderradius.at(10) = -1.0; //  individual radii            
                   ///sfpa . ladderradius.at(10) = -1.0; //  individual radii            
                   sfpa . ladderradius.at(11) = -1.0; //  individual radii            
                   ///sfpa . ladderradius.at(11) = -1.0; //  individual radii            
                   sfpa . ladderradius.at(12) = -1.0; //  individual radii            
                   ///sfpa . ladderradius.at(12) = -1.0; //  individual radii            
                   sfpa . ladderradius.at(13) = -1.0; //  individual radii            
                   ///sfpa . ladderradius.at(13) = -1.0; //  individual radii            
                   sfpa . ladderradius.at(14) = -1.0; //  individual radii            
                   ///sfpa . ladderradius.at(14) = -1.0; //  individual radii            
                   sfpa . ladderradius.at(15) = -1.0; //  individual radii            
                   ///sfpa . ladderradius.at(15) = -1.0; //  individual radii            
                   sfpa . ladderradius.at(16) = -1.0; //  individual radii            
                   ///sfpa . ladderradius.at(16) = -1.0; //  individual radii            
                   sfpa . ladderradius.at(17) = -1.0; //  individual radii            
                   ///sfpa . ladderradius.at(17) = -1.0; //  individual radii            
                   sfpa . ladderradius.at(18) = -1.0; //  individual radii            
                   ///sfpa . ladderradius.at(18) = -1.0; //  individual radii            
                   sfpa . ladderradius.at(19) = -1.0; //  individual radii            
                   ///sfpa . ladderradius.at(19) = -1.0; //  individual radii            
                   //           
                   sfpa.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup sfpa_doc        
             ///@{           
                   ++sfpa._index;           
                   sfpa . version = 2; //  geometry version            
                   /// sfpa . version = 2; //  geometry version            
                   sfpa . laddermap.at(0) = 1; //  presence of ladders            
                   ///sfpa . laddermap.at(0) = 1; //  presence of ladders            
                   sfpa . laddermap.at(1) = 1; //  presence of ladders            
                   ///sfpa . laddermap.at(1) = 1; //  presence of ladders            
                   sfpa . laddermap.at(2) = 1; //  presence of ladders            
                   ///sfpa . laddermap.at(2) = 1; //  presence of ladders            
                   sfpa . laddermap.at(3) = 0; //  presence of ladders            
                   ///sfpa . laddermap.at(3) = 0; //  presence of ladders            
                   sfpa . laddermap.at(4) = 0; //  presence of ladders            
                   ///sfpa . laddermap.at(4) = 0; //  presence of ladders            
                   sfpa . laddermap.at(5) = 0; //  presence of ladders            
                   ///sfpa . laddermap.at(5) = 0; //  presence of ladders            
                   sfpa . laddermap.at(6) = 0; //  presence of ladders            
                   ///sfpa . laddermap.at(6) = 0; //  presence of ladders            
                   sfpa . laddermap.at(7) = 0; //  presence of ladders            
                   ///sfpa . laddermap.at(7) = 0; //  presence of ladders            
                   sfpa . laddermap.at(8) = 1; //  presence of ladders            
                   ///sfpa . laddermap.at(8) = 1; //  presence of ladders            
                   sfpa . laddermap.at(9) = 1; //  presence of ladders            
                   ///sfpa . laddermap.at(9) = 1; //  presence of ladders            
                   sfpa . laddermap.at(10) = 1; //  presence of ladders            
                   ///sfpa . laddermap.at(10) = 1; //  presence of ladders            
                   sfpa . laddermap.at(11) = 1; //  presence of ladders            
                   ///sfpa . laddermap.at(11) = 1; //  presence of ladders            
                   sfpa . laddermap.at(12) = 1; //  presence of ladders            
                   ///sfpa . laddermap.at(12) = 1; //  presence of ladders            
                   sfpa . laddermap.at(13) = 0; //  presence of ladders            
                   ///sfpa . laddermap.at(13) = 0; //  presence of ladders            
                   sfpa . laddermap.at(14) = 0; //  presence of ladders            
                   ///sfpa . laddermap.at(14) = 0; //  presence of ladders            
                   sfpa . laddermap.at(15) = 0; //  presence of ladders            
                   ///sfpa . laddermap.at(15) = 0; //  presence of ladders            
                   sfpa . laddermap.at(16) = 0; //  presence of ladders            
                   ///sfpa . laddermap.at(16) = 0; //  presence of ladders            
                   sfpa . laddermap.at(17) = 0; //  presence of ladders            
                   ///sfpa . laddermap.at(17) = 0; //  presence of ladders            
                   sfpa . laddermap.at(18) = 1; //  presence of ladders            
                   ///sfpa . laddermap.at(18) = 1; //  presence of ladders            
                   sfpa . laddermap.at(19) = 1; //  presence of ladders            
                   ///sfpa . laddermap.at(19) = 1; //  presence of ladders            
                   sfpa . ladderangle.at(0) = 90.0; //  individual angles            
                   ///sfpa . ladderangle.at(0) = 90.0; //  individual angles            
                   sfpa . ladderangle.at(1) = 108.3; //  individual angles            
                   ///sfpa . ladderangle.at(1) = 108.3; //  individual angles            
                   sfpa . ladderangle.at(2) = 130.0; //  individual angles            
                   ///sfpa . ladderangle.at(2) = 130.0; //  individual angles            
                   sfpa . ladderangle.at(3) = -1.0; //  individual angles            
                   ///sfpa . ladderangle.at(3) = -1.0; //  individual angles            
                   sfpa . ladderangle.at(4) = -1.0; //  individual angles            
                   ///sfpa . ladderangle.at(4) = -1.0; //  individual angles            
                   sfpa . ladderangle.at(5) = -1.0; //  individual angles            
                   ///sfpa . ladderangle.at(5) = -1.0; //  individual angles            
                   sfpa . ladderangle.at(6) = -1.0; //  individual angles            
                   ///sfpa . ladderangle.at(6) = -1.0; //  individual angles            
                   sfpa . ladderangle.at(7) = -1.0; //  individual angles            
                   ///sfpa . ladderangle.at(7) = -1.0; //  individual angles            
                   sfpa . ladderangle.at(8) = 230.0; //  individual angles            
                   ///sfpa . ladderangle.at(8) = 230.0; //  individual angles            
                   sfpa . ladderangle.at(9) = 251.7; //  individual angles            
                   ///sfpa . ladderangle.at(9) = 251.7; //  individual angles            
                   sfpa . ladderangle.at(10) = 270.0; //  individual angles            
                   ///sfpa . ladderangle.at(10) = 270.0; //  individual angles            
                   sfpa . ladderangle.at(11) = 288.3; //  individual angles            
                   ///sfpa . ladderangle.at(11) = 288.3; //  individual angles            
                   sfpa . ladderangle.at(12) = 310.0; //  individual angles            
                   ///sfpa . ladderangle.at(12) = 310.0; //  individual angles            
                   sfpa . ladderangle.at(13) = -1.0; //  individual angles            
                   ///sfpa . ladderangle.at(13) = -1.0; //  individual angles            
                   sfpa . ladderangle.at(14) = -1.0; //  individual angles            
                   ///sfpa . ladderangle.at(14) = -1.0; //  individual angles            
                   sfpa . ladderangle.at(15) = -1.0; //  individual angles            
                   ///sfpa . ladderangle.at(15) = -1.0; //  individual angles            
                   sfpa . ladderangle.at(16) = -1.0; //  individual angles            
                   ///sfpa . ladderangle.at(16) = -1.0; //  individual angles            
                   sfpa . ladderangle.at(17) = -1.0; //  individual angles            
                   ///sfpa . ladderangle.at(17) = -1.0; //  individual angles            
                   sfpa . ladderangle.at(18) = 50.0; //  individual angles            
                   ///sfpa . ladderangle.at(18) = 50.0; //  individual angles            
                   sfpa . ladderangle.at(19) = 71.7; //  individual angles            
                   ///sfpa . ladderangle.at(19) = 71.7; //  individual angles            
                   sfpa . laddertilt.at(0) = 0.0; //  individual tilts            
                   ///sfpa . laddertilt.at(0) = 0.0; //  individual tilts            
                   sfpa . laddertilt.at(1) = -6.0; //  individual tilts            
                   ///sfpa . laddertilt.at(1) = -6.0; //  individual tilts            
                   sfpa . laddertilt.at(2) = 0.0; //  individual tilts            
                   ///sfpa . laddertilt.at(2) = 0.0; //  individual tilts            
                   sfpa . laddertilt.at(3) = -1.0; //  individual tilts            
                   ///sfpa . laddertilt.at(3) = -1.0; //  individual tilts            
                   sfpa . laddertilt.at(4) = -1.0; //  individual tilts            
                   ///sfpa . laddertilt.at(4) = -1.0; //  individual tilts            
                   sfpa . laddertilt.at(5) = -1.0; //  individual tilts            
                   ///sfpa . laddertilt.at(5) = -1.0; //  individual tilts            
                   sfpa . laddertilt.at(6) = -1.0; //  individual tilts            
                   ///sfpa . laddertilt.at(6) = -1.0; //  individual tilts            
                   sfpa . laddertilt.at(7) = 0.0; //  individual tilts            
                   ///sfpa . laddertilt.at(7) = 0.0; //  individual tilts            
                   sfpa . laddertilt.at(8) = 0.0; //  individual tilts            
                   ///sfpa . laddertilt.at(8) = 0.0; //  individual tilts            
                   sfpa . laddertilt.at(9) = 6.0; //  individual tilts            
                   ///sfpa . laddertilt.at(9) = 6.0; //  individual tilts            
                   sfpa . laddertilt.at(10) = 0.0; //  individual tilts            
                   ///sfpa . laddertilt.at(10) = 0.0; //  individual tilts            
                   sfpa . laddertilt.at(11) = -6.0; //  individual tilts            
                   ///sfpa . laddertilt.at(11) = -6.0; //  individual tilts            
                   sfpa . laddertilt.at(12) = 0.0; //  individual tilts            
                   ///sfpa . laddertilt.at(12) = 0.0; //  individual tilts            
                   sfpa . laddertilt.at(13) = -1.0; //  individual tilts            
                   ///sfpa . laddertilt.at(13) = -1.0; //  individual tilts            
                   sfpa . laddertilt.at(14) = -1.0; //  individual tilts            
                   ///sfpa . laddertilt.at(14) = -1.0; //  individual tilts            
                   sfpa . laddertilt.at(15) = -1.0; //  individual tilts            
                   ///sfpa . laddertilt.at(15) = -1.0; //  individual tilts            
                   sfpa . laddertilt.at(16) = -1.0; //  individual tilts            
                   ///sfpa . laddertilt.at(16) = -1.0; //  individual tilts            
                   sfpa . laddertilt.at(17) = -1.0; //  individual tilts            
                   ///sfpa . laddertilt.at(17) = -1.0; //  individual tilts            
                   sfpa . laddertilt.at(18) = 0.0; //  individual tilts            
                   ///sfpa . laddertilt.at(18) = 0.0; //  individual tilts            
                   sfpa . laddertilt.at(19) = 6.0; //  individual tilts            
                   ///sfpa . laddertilt.at(19) = 6.0; //  individual tilts            
                   sfpa . ladderradius.at(0) = 23.174; //  individual radii            
                   ///sfpa . ladderradius.at(0) = 23.174; //  individual radii            
                   sfpa . ladderradius.at(1) = 22.800; //  individual radii            
                   ///sfpa . ladderradius.at(1) = 22.800; //  individual radii            
                   sfpa . ladderradius.at(2) = 24.600; //  individual radii            
                   ///sfpa . ladderradius.at(2) = 24.600; //  individual radii            
                   sfpa . ladderradius.at(3) = -1.0; //  individual radii            
                   ///sfpa . ladderradius.at(3) = -1.0; //  individual radii            
                   sfpa . ladderradius.at(4) = -1.0; //  individual radii            
                   ///sfpa . ladderradius.at(4) = -1.0; //  individual radii            
                   sfpa . ladderradius.at(5) = -1.0; //  individual radii            
                   ///sfpa . ladderradius.at(5) = -1.0; //  individual radii            
                   sfpa . ladderradius.at(6) = -1.0; //  individual radii            
                   ///sfpa . ladderradius.at(6) = -1.0; //  individual radii            
                   sfpa . ladderradius.at(7) = -1.0; //  individual radii            
                   ///sfpa . ladderradius.at(7) = -1.0; //  individual radii            
                   sfpa . ladderradius.at(8) = 24.600; //  individual radii            
                   ///sfpa . ladderradius.at(8) = 24.600; //  individual radii            
                   sfpa . ladderradius.at(9) = 22.800; //  individual radii            
                   ///sfpa . ladderradius.at(9) = 22.800; //  individual radii            
                   sfpa . ladderradius.at(10) = 23.174; //  individual radii            
                   ///sfpa . ladderradius.at(10) = 23.174; //  individual radii            
                   sfpa . ladderradius.at(11) = 22.800; //  individual radii            
                   ///sfpa . ladderradius.at(11) = 22.800; //  individual radii            
                   sfpa . ladderradius.at(12) = 24.600; //  individual radii            
                   ///sfpa . ladderradius.at(12) = 24.600; //  individual radii            
                   sfpa . ladderradius.at(13) = -1.0; //  individual radii            
                   ///sfpa . ladderradius.at(13) = -1.0; //  individual radii            
                   sfpa . ladderradius.at(14) = -1.0; //  individual radii            
                   ///sfpa . ladderradius.at(14) = -1.0; //  individual radii            
                   sfpa . ladderradius.at(15) = -1.0; //  individual radii            
                   ///sfpa . ladderradius.at(15) = -1.0; //  individual radii            
                   sfpa . ladderradius.at(16) = -1.0; //  individual radii            
                   ///sfpa . ladderradius.at(16) = -1.0; //  individual radii            
                   sfpa . ladderradius.at(17) = -1.0; //  individual radii            
                   ///sfpa . ladderradius.at(17) = -1.0; //  individual radii            
                   sfpa . ladderradius.at(18) = 24.600; //  individual radii            
                   ///sfpa . ladderradius.at(18) = 24.600; //  individual radii            
                   sfpa . ladderradius.at(19) = 22.800; //  individual radii            
                   ///sfpa . ladderradius.at(19) = 22.800; //  individual radii            
                   //           
                   sfpa.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup sfpa_doc        
             ///@{           
                   ++sfpa._index;           
                   sfpa . version = 3; //  geometry version            
                   /// sfpa . version = 3; //  geometry version            
                   sfpa . laddermap.at(0) = 1; //  presence of ladders            
                   ///sfpa . laddermap.at(0) = 1; //  presence of ladders            
                   sfpa . laddermap.at(1) = 1; //  presence of ladders            
                   ///sfpa . laddermap.at(1) = 1; //  presence of ladders            
                   sfpa . laddermap.at(2) = 1; //  presence of ladders            
                   ///sfpa . laddermap.at(2) = 1; //  presence of ladders            
                   sfpa . laddermap.at(3) = 1; //  presence of ladders            
                   ///sfpa . laddermap.at(3) = 1; //  presence of ladders            
                   sfpa . laddermap.at(4) = 1; //  presence of ladders            
                   ///sfpa . laddermap.at(4) = 1; //  presence of ladders            
                   sfpa . laddermap.at(5) = 1; //  presence of ladders            
                   ///sfpa . laddermap.at(5) = 1; //  presence of ladders            
                   sfpa . laddermap.at(6) = 1; //  presence of ladders            
                   ///sfpa . laddermap.at(6) = 1; //  presence of ladders            
                   sfpa . laddermap.at(7) = 1; //  presence of ladders            
                   ///sfpa . laddermap.at(7) = 1; //  presence of ladders            
                   sfpa . laddermap.at(8) = 1; //  presence of ladders            
                   ///sfpa . laddermap.at(8) = 1; //  presence of ladders            
                   sfpa . laddermap.at(9) = 1; //  presence of ladders            
                   ///sfpa . laddermap.at(9) = 1; //  presence of ladders            
                   sfpa . laddermap.at(10) = 1; //  presence of ladders            
                   ///sfpa . laddermap.at(10) = 1; //  presence of ladders            
                   sfpa . laddermap.at(11) = 1; //  presence of ladders            
                   ///sfpa . laddermap.at(11) = 1; //  presence of ladders            
                   sfpa . laddermap.at(12) = 1; //  presence of ladders            
                   ///sfpa . laddermap.at(12) = 1; //  presence of ladders            
                   sfpa . laddermap.at(13) = 1; //  presence of ladders            
                   ///sfpa . laddermap.at(13) = 1; //  presence of ladders            
                   sfpa . laddermap.at(14) = 1; //  presence of ladders            
                   ///sfpa . laddermap.at(14) = 1; //  presence of ladders            
                   sfpa . laddermap.at(15) = 1; //  presence of ladders            
                   ///sfpa . laddermap.at(15) = 1; //  presence of ladders            
                   sfpa . laddermap.at(16) = 1; //  presence of ladders            
                   ///sfpa . laddermap.at(16) = 1; //  presence of ladders            
                   sfpa . laddermap.at(17) = 1; //  presence of ladders            
                   ///sfpa . laddermap.at(17) = 1; //  presence of ladders            
                   sfpa . laddermap.at(18) = 1; //  presence of ladders            
                   ///sfpa . laddermap.at(18) = 1; //  presence of ladders            
                   sfpa . laddermap.at(19) = 1; //  presence of ladders            
                   ///sfpa . laddermap.at(19) = 1; //  presence of ladders            
                   sfpa . ladderangle.at(0) = 90.0; //  individual angles            
                   ///sfpa . ladderangle.at(0) = 90.0; //  individual angles            
                   sfpa . ladderangle.at(1) = 108.3; //  individual angles            
                   ///sfpa . ladderangle.at(1) = 108.3; //  individual angles            
                   sfpa . ladderangle.at(2) = 126.6; //  individual angles            
                   ///sfpa . ladderangle.at(2) = 126.6; //  individual angles            
                   sfpa . ladderangle.at(3) = 144.4; //  individual angles            
                   ///sfpa . ladderangle.at(3) = 144.4; //  individual angles            
                   sfpa . ladderangle.at(4) = 162.2; //  individual angles            
                   ///sfpa . ladderangle.at(4) = 162.2; //  individual angles            
                   sfpa . ladderangle.at(5) = 180.0; //  individual angles            
                   ///sfpa . ladderangle.at(5) = 180.0; //  individual angles            
                   sfpa . ladderangle.at(6) = 197.8; //  individual angles            
                   ///sfpa . ladderangle.at(6) = 197.8; //  individual angles            
                   sfpa . ladderangle.at(7) = 215.6; //  individual angles            
                   ///sfpa . ladderangle.at(7) = 215.6; //  individual angles            
                   sfpa . ladderangle.at(8) = 233.4; //  individual angles            
                   ///sfpa . ladderangle.at(8) = 233.4; //  individual angles            
                   sfpa . ladderangle.at(9) = 251.7; //  individual angles            
                   ///sfpa . ladderangle.at(9) = 251.7; //  individual angles            
                   sfpa . ladderangle.at(10) = 270.0; //  individual angles            
                   ///sfpa . ladderangle.at(10) = 270.0; //  individual angles            
                   sfpa . ladderangle.at(11) = 288.3; //  individual angles            
                   ///sfpa . ladderangle.at(11) = 288.3; //  individual angles            
                   sfpa . ladderangle.at(12) = 306.6; //  individual angles            
                   ///sfpa . ladderangle.at(12) = 306.6; //  individual angles            
                   sfpa . ladderangle.at(13) = 324.4; //  individual angles            
                   ///sfpa . ladderangle.at(13) = 324.4; //  individual angles            
                   sfpa . ladderangle.at(14) = 342.2; //  individual angles            
                   ///sfpa . ladderangle.at(14) = 342.2; //  individual angles            
                   sfpa . ladderangle.at(15) = 0.0; //  individual angles            
                   ///sfpa . ladderangle.at(15) = 0.0; //  individual angles            
                   sfpa . ladderangle.at(16) = 17.8; //  individual angles            
                   ///sfpa . ladderangle.at(16) = 17.8; //  individual angles            
                   sfpa . ladderangle.at(17) = 35.6; //  individual angles            
                   ///sfpa . ladderangle.at(17) = 35.6; //  individual angles            
                   sfpa . ladderangle.at(18) = 53.4; //  individual angles            
                   ///sfpa . ladderangle.at(18) = 53.4; //  individual angles            
                   sfpa . ladderangle.at(19) = 71.7; //  individual angles            
                   ///sfpa . ladderangle.at(19) = 71.7; //  individual angles            
                   sfpa . laddertilt.at(0) = 0.0; //  individual tilts            
                   ///sfpa . laddertilt.at(0) = 0.0; //  individual tilts            
                   sfpa . laddertilt.at(1) = -6.0; //  individual tilts            
                   ///sfpa . laddertilt.at(1) = -6.0; //  individual tilts            
                   sfpa . laddertilt.at(2) = -7.0; //  individual tilts            
                   ///sfpa . laddertilt.at(2) = -7.0; //  individual tilts            
                   sfpa . laddertilt.at(3) = -7.0; //  individual tilts            
                   ///sfpa . laddertilt.at(3) = -7.0; //  individual tilts            
                   sfpa . laddertilt.at(4) = -7.0; //  individual tilts            
                   ///sfpa . laddertilt.at(4) = -7.0; //  individual tilts            
                   sfpa . laddertilt.at(5) = 0.0; //  individual tilts            
                   ///sfpa . laddertilt.at(5) = 0.0; //  individual tilts            
                   sfpa . laddertilt.at(6) = 7.0; //  individual tilts            
                   ///sfpa . laddertilt.at(6) = 7.0; //  individual tilts            
                   sfpa . laddertilt.at(7) = 7.0; //  individual tilts            
                   ///sfpa . laddertilt.at(7) = 7.0; //  individual tilts            
                   sfpa . laddertilt.at(8) = 7.0; //  individual tilts            
                   ///sfpa . laddertilt.at(8) = 7.0; //  individual tilts            
                   sfpa . laddertilt.at(9) = 6.0; //  individual tilts            
                   ///sfpa . laddertilt.at(9) = 6.0; //  individual tilts            
                   sfpa . laddertilt.at(10) = 0.0; //  individual tilts            
                   ///sfpa . laddertilt.at(10) = 0.0; //  individual tilts            
                   sfpa . laddertilt.at(11) = -6.0; //  individual tilts            
                   ///sfpa . laddertilt.at(11) = -6.0; //  individual tilts            
                   sfpa . laddertilt.at(12) = -7.0; //  individual tilts            
                   ///sfpa . laddertilt.at(12) = -7.0; //  individual tilts            
                   sfpa . laddertilt.at(13) = -7.0; //  individual tilts            
                   ///sfpa . laddertilt.at(13) = -7.0; //  individual tilts            
                   sfpa . laddertilt.at(14) = -7.0; //  individual tilts            
                   ///sfpa . laddertilt.at(14) = -7.0; //  individual tilts            
                   sfpa . laddertilt.at(15) = 0.0; //  individual tilts            
                   ///sfpa . laddertilt.at(15) = 0.0; //  individual tilts            
                   sfpa . laddertilt.at(16) = 7.0; //  individual tilts            
                   ///sfpa . laddertilt.at(16) = 7.0; //  individual tilts            
                   sfpa . laddertilt.at(17) = 7.0; //  individual tilts            
                   ///sfpa . laddertilt.at(17) = 7.0; //  individual tilts            
                   sfpa . laddertilt.at(18) = 7.0; //  individual tilts            
                   ///sfpa . laddertilt.at(18) = 7.0; //  individual tilts            
                   sfpa . laddertilt.at(19) = 6.0; //  individual tilts            
                   ///sfpa . laddertilt.at(19) = 6.0; //  individual tilts            
                   sfpa . ladderradius.at(0) = 23.177; //  individual radii            
                   ///sfpa . ladderradius.at(0) = 23.177; //  individual radii            
                   sfpa . ladderradius.at(1) = 22.800; //  individual radii            
                   ///sfpa . ladderradius.at(1) = 22.800; //  individual radii            
                   sfpa . ladderradius.at(2) = 22.800; //  individual radii            
                   ///sfpa . ladderradius.at(2) = 22.800; //  individual radii            
                   sfpa . ladderradius.at(3) = 22.800; //  individual radii            
                   ///sfpa . ladderradius.at(3) = 22.800; //  individual radii            
                   sfpa . ladderradius.at(4) = 23.800; //  individual radii            
                   ///sfpa . ladderradius.at(4) = 23.800; //  individual radii            
                   sfpa . ladderradius.at(5) = 22.500; //  individual radii            
                   ///sfpa . ladderradius.at(5) = 22.500; //  individual radii            
                   sfpa . ladderradius.at(6) = 23.800; //  individual radii            
                   ///sfpa . ladderradius.at(6) = 23.800; //  individual radii            
                   sfpa . ladderradius.at(7) = 22.800; //  individual radii            
                   ///sfpa . ladderradius.at(7) = 22.800; //  individual radii            
                   sfpa . ladderradius.at(8) = 22.800; //  individual radii            
                   ///sfpa . ladderradius.at(8) = 22.800; //  individual radii            
                   sfpa . ladderradius.at(9) = 22.800; //  individual radii            
                   ///sfpa . ladderradius.at(9) = 22.800; //  individual radii            
                   sfpa . ladderradius.at(10) = 23.177; //  individual radii            
                   ///sfpa . ladderradius.at(10) = 23.177; //  individual radii            
                   sfpa . ladderradius.at(11) = 22.800; //  individual radii            
                   ///sfpa . ladderradius.at(11) = 22.800; //  individual radii            
                   sfpa . ladderradius.at(12) = 22.800; //  individual radii            
                   ///sfpa . ladderradius.at(12) = 22.800; //  individual radii            
                   sfpa . ladderradius.at(13) = 22.800; //  individual radii            
                   ///sfpa . ladderradius.at(13) = 22.800; //  individual radii            
                   sfpa . ladderradius.at(14) = 23.800; //  individual radii            
                   ///sfpa . ladderradius.at(14) = 23.800; //  individual radii            
                   sfpa . ladderradius.at(15) = 22.500; //  individual radii            
                   ///sfpa . ladderradius.at(15) = 22.500; //  individual radii            
                   sfpa . ladderradius.at(16) = 23.800; //  individual radii            
                   ///sfpa . ladderradius.at(16) = 23.800; //  individual radii            
                   sfpa . ladderradius.at(17) = 22.800; //  individual radii            
                   ///sfpa . ladderradius.at(17) = 22.800; //  individual radii            
                   sfpa . ladderradius.at(18) = 22.800; //  individual radii            
                   ///sfpa . ladderradius.at(18) = 22.800; //  individual radii            
                   sfpa . ladderradius.at(19) = 22.800; //  individual radii            
                   ///sfpa . ladderradius.at(19) = 22.800; //  individual radii            
                   //           
                   sfpa.fill();           
             ///@}        
             //        
             /// USE ssdp _index=1;        
             ssdp.Use();        
             /// USE sfpa version=ssdp.config ;        
             sfpa.Use("version",(Float_t)ssdp.config );        
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
             if ( ssdp.placement==1 )        
             {           
                   _create = AgCreate("SFMO");           
                   {              
                         AgShape myshape; // undefined shape              
                         ///Create SFMO              
                         Create("SFMO");               
                   }           
                   { AgPlacement place = AgPlacement("SFMO","SVTT");              
                         /// Add daughter volume SFMO to mother SVTT              
                         _stacker -> Position( AgBlock::Find("SFMO"), place );              
                   } // end placement of SFMO           
             }        
             else        
             {           
                   _create = AgCreate("SFMO");           
                   {              
                         AgShape myshape; // undefined shape              
                         ///Create SFMO              
                         Create("SFMO");               
                   }           
                   { AgPlacement place = AgPlacement("SFMO","CAVE");              
                         /// Add daughter volume SFMO to mother CAVE              
                         _stacker -> Position( AgBlock::Find("SFMO"), place );              
                   } // end placement of SFMO           
             }        
       }; // SisdGeo     
 }; // namespace SisdGeo  
 