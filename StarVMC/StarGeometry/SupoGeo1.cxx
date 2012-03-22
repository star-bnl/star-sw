#include "SupoGeo1.h"  
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
 namespace SUPOGEO1 // $NMSPC  
 {     
       //     
       // ---------------------------------------------------------------------------------------------------     
          //  -----------------------------------------------------     
          /// @defgroup smai_doc     
          /// \class Smai_t     
          /// \brief User-defined structure     
          ///                             
          /// AgML structure members:     
          ///                             
          ///Float_t version;     
          ///Float_t rinner;     
          ///Float_t router;     
          ///Float_t zmin;     
          ///Float_t zmax;     
          ///Float_t phimid;     
          ///Float_t fixhei;     
          ///Float_t fixwid;     
          ///Float_t fixthk;     
          ///Int_t _index;     
          //     
          Smai_t smai;     
          //     
          //  -----------------------------------------------------     
          /// @defgroup sslo_doc     
          /// \class Sslo_t     
          /// \brief User-defined structure     
          ///                             
          /// AgML structure members:     
          ///                             
          ///Float_t version;     
          ///Float_t phimin;     
          ///Float_t phimax;     
          ///Float_t raillen;     
          ///Float_t railwin;     
          ///Float_t railwout;     
          ///Float_t railhei;     
          ///Float_t wallwid;     
          ///Float_t wallhei;     
          ///Float_t headthk;     
          ///Float_t headhei;     
          ///Float_t xwalthk;     
          ///Float_t xwal1pos;     
          ///Float_t xwal2pos;     
          ///Float_t endthk;     
          ///Float_t endhei;     
          ///Float_t boltpos;     
          ///Float_t boltoff;     
          ///Float_t boltrad;     
          ///Int_t _index;     
          //     
          Sslo_t sslo;     
          //     
          //  -----------------------------------------------------     
          /// @defgroup sshi_doc     
          /// \class Sshi_t     
          /// \brief User-defined structure     
          ///                             
          /// AgML structure members:     
          ///                             
          ///Float_t version;     
          ///Float_t phimin;     
          ///Float_t phimax;     
          ///Float_t xdist;     
          ///Float_t ydist;     
          ///Float_t railwid;     
          ///Float_t railhei;     
          ///Float_t platthk;     
          ///Float_t plathei;     
          ///Float_t barlen;     
          ///Float_t baroffz;     
          ///Float_t barhei;     
          ///Float_t topwid;     
          ///Float_t blochei;     
          ///Float_t bloclen;     
          ///Float_t boltoff;     
          ///Float_t stabwid;     
          ///Float_t stabthk;     
          ///Float_t stab1z;     
          ///Float_t stab2z;     
          ///Int_t _index;     
          //     
          Sshi_t sshi;     
          //     
          ///@addtogroup SupoGeo1_vars     
          ///@{        
          ///@}     
          ///@addtogroup SupoGeo1_vars     
          ///@{        
                Float_t pos,angle,shptpos,shbkpos;        
                //        
                /// Float_t pos,angle,shptpos,shbkpos        
          ///@}     
       SupoGeo1::SupoGeo1()     
         : AgModule("SupoGeo1","  is the geometry of the Forward TPC supports in STAR ")     
       {        
       }     
          // ---------------------------------------------------------------------------------------------------     
          void SUPO::Block( AgCreate create )     
          {         
                ///@addtogroup SUPO_doc        
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
                      { AgAttribute attr = AgAttribute("SUPO");              
                            attr.par("seen")=0;              
                            attr.par("colo")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=smai.rinner;              
                            shape.par("rmax")=smai.router;              
                            shape.par("dz")=(smai.zmax-smai.zmin)/2;              
                            /// Shape Tube rmin=smai.rinner rmax=smai.router dz=(smai.zmax-smai.zmin)/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_SUPO;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("SUPL");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create SUPL              
                            Create("SUPL");               
                      }           
                      { AgPlacement place = AgPlacement("SUPL","SUPO");              
                            /// Add daughter volume SUPL to mother SUPO              
                            /// G3 Reference: thetax = 90              
                            /// G3 Reference: phix = -smai.phimid              
                            /// G3 Reference: thetay = 90              
                            /// G3 Reference: phiy = -smai.phimid+90              
                            /// G3 Reference: thetaz = 0              
                            /// G3 Reference: phiz = 0              
                            Double_t _thetax=90,_phix=-smai.phimid,_thetay=90,_phiy=-smai.phimid+90,_thetaz=0,_phiz=0;              
                            place.Reference( _thetax, _phix, _thetay, _phiy, _thetaz, _phiz );              
                            _stacker -> Position( AgBlock::Find("SUPL"), place );              
                      } // end placement of SUPL           
                      { AgPlacement place = AgPlacement("SUPL","SUPO");              
                            /// Add daughter volume SUPL to mother SUPO              
                            /// G3 Reference: thetax = 270              
                            /// G3 Reference: phix = smai.phimid              
                            /// G3 Reference: thetay = 90              
                            /// G3 Reference: phiy = smai.phimid+90              
                            /// G3 Reference: thetaz = 0              
                            /// G3 Reference: phiz = 0              
                            Double_t _thetax=270,_phix=smai.phimid,_thetay=90,_phiy=smai.phimid+90,_thetaz=0,_phiz=0;              
                            place.Reference( _thetax, _phix, _thetay, _phiy, _thetaz, _phiz );              
                            _stacker -> Position( AgBlock::Find("SUPL"), place );              
                      } // end placement of SUPL           
                      _create = AgCreate("SUPH");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create SUPH              
                            Create("SUPH");               
                      }           
                      { AgPlacement place = AgPlacement("SUPH","SUPO");              
                            /// Add daughter volume SUPH to mother SUPO              
                            _stacker -> Position( AgBlock::Find("SUPH"), place );              
                      } // end placement of SUPH           
                      { AgPlacement place = AgPlacement("SUPH","SUPO");              
                            /// Add daughter volume SUPH to mother SUPO              
                            /// G3 Reference: thetax = 270              
                            /// G3 Reference: phix = 0              
                            /// G3 Reference: thetay = 90              
                            /// G3 Reference: phiy = 90              
                            /// G3 Reference: thetaz = 0              
                            /// G3 Reference: phiz = 0              
                            Double_t _thetax=270,_phix=0,_thetay=90,_phiy=90,_thetaz=0,_phiz=0;              
                            place.Reference( _thetax, _phix, _thetay, _phiy, _thetaz, _phiz );              
                            _stacker -> Position( AgBlock::Find("SUPH"), place );              
                      } // end placement of SUPH           
                      END_OF_SUPO:           
                      mCurrent = _save;           
                ///@}        
          } // End Block SUPO     
          // ---------------------------------------------------------------------------------------------------     
          void SUPL::Block( AgCreate create )     
          {         
                ///@addtogroup SUPL_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      {  AgShape shape = AgShape("Tubs");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=smai.rinner;              
                            shape.par("rmax")=smai.router;              
                            shape.par("phi1")=-sslo.phimax;              
                            shape.par("phi2")=-sslo.phimin;              
                            shape.par("dz")=(smai.zmax-smai.zmin)/2;              
                            /// Shape Tubs rmin=smai.rinner rmax=smai.router phi1=-sslo.phimax phi2=-sslo.phimin dz=(smai.zmax-smai.zmin)/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_SUPL;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("SLRL");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create SLRL              
                            Create("SLRL");               
                      }           
                      { AgPlacement place = AgPlacement("SLRL","SUPL");              
                            /// Add daughter volume SLRL to mother SUPL              
                            place.TranslateX((smai.rinner+sslo.railhei/2));              
                            /// Translate x = (smai.rinner+sslo.railhei/2)              
                            place.TranslateZ((smai.zmax-smai.zmin-sslo.raillen)/2);              
                            /// Translate z = (smai.zmax-smai.zmin-sslo.raillen)/2              
                            /// G3 Reference: thetax = 90              
                            /// G3 Reference: phix = 0              
                            /// G3 Reference: thetay = 90              
                            /// G3 Reference: phiy = 90              
                            /// G3 Reference: thetaz = 0              
                            /// G3 Reference: phiz = 0              
                            place.Ortho( "YZX" ); // ORT=YZX              
                            /// Axis substitution: XYZ --> YZX              
                            _stacker -> Position( AgBlock::Find("SLRL"), place );              
                      } // end placement of SLRL           
                      _create = AgCreate("SLWL");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create SLWL              
                            Create("SLWL");               
                      }           
                      { AgPlacement place = AgPlacement("SLWL","SUPL");              
                            /// Add daughter volume SLWL to mother SUPL              
                            place.TranslateX((smai.rinner+sslo.railhei+sslo.wallhei/2));              
                            /// Translate x = (smai.rinner+sslo.railhei+sslo.wallhei/2)              
                            place.TranslateY((sslo.railwout/2-sslo.wallwid/2));              
                            /// Translate y = (sslo.railwout/2-sslo.wallwid/2)              
                            place.TranslateZ((smai.zmax-smai.zmin-sslo.raillen+sslo.headthk)/2);              
                            /// Translate z = (smai.zmax-smai.zmin-sslo.raillen+sslo.headthk)/2              
                            _stacker -> Position( AgBlock::Find("SLWL"), place );              
                      } // end placement of SLWL           
                      { AgPlacement place = AgPlacement("SLWL","SUPL");              
                            /// Add daughter volume SLWL to mother SUPL              
                            place.TranslateX((smai.rinner+sslo.railhei+sslo.wallhei/2));              
                            /// Translate x = (smai.rinner+sslo.railhei+sslo.wallhei/2)              
                            place.TranslateY(-(sslo.railwout/2-sslo.wallwid/2));              
                            /// Translate y = -(sslo.railwout/2-sslo.wallwid/2)              
                            place.TranslateZ((smai.zmax-smai.zmin-sslo.raillen+sslo.headthk)/2);              
                            /// Translate z = (smai.zmax-smai.zmin-sslo.raillen+sslo.headthk)/2              
                            _stacker -> Position( AgBlock::Find("SLWL"), place );              
                      } // end placement of SLWL           
                      _create = AgCreate("SLHD");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create SLHD              
                            Create("SLHD");               
                      }           
                      { AgPlacement place = AgPlacement("SLHD","SUPL");              
                            /// Add daughter volume SLHD to mother SUPL              
                            place.TranslateX((smai.rinner+sslo.railhei+sslo.headhei/2));              
                            /// Translate x = (smai.rinner+sslo.railhei+sslo.headhei/2)              
                            place.TranslateZ((smai.zmax-smai.zmin+sslo.headthk)/2-sslo.raillen);              
                            /// Translate z = (smai.zmax-smai.zmin+sslo.headthk)/2-sslo.raillen              
                            _stacker -> Position( AgBlock::Find("SLHD"), place );              
                      } // end placement of SLHD           
                      _create = AgCreate("SLXW");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create SLXW              
                            Create("SLXW");               
                      }           
                      { AgPlacement place = AgPlacement("SLXW","SUPL");              
                            /// Add daughter volume SLXW to mother SUPL              
                            place.TranslateX((smai.rinner+sslo.railhei+sslo.wallhei/2));              
                            /// Translate x = (smai.rinner+sslo.railhei+sslo.wallhei/2)              
                            place.TranslateZ(sslo.xwal1pos);              
                            /// Translate z = sslo.xwal1pos              
                            _stacker -> Position( AgBlock::Find("SLXW"), place );              
                      } // end placement of SLXW           
                      { AgPlacement place = AgPlacement("SLXW","SUPL");              
                            /// Add daughter volume SLXW to mother SUPL              
                            place.TranslateX((smai.rinner+sslo.railhei+sslo.wallhei/2));              
                            /// Translate x = (smai.rinner+sslo.railhei+sslo.wallhei/2)              
                            place.TranslateZ(sslo.xwal2pos);              
                            /// Translate z = sslo.xwal2pos              
                            _stacker -> Position( AgBlock::Find("SLXW"), place );              
                      } // end placement of SLXW           
                      _create = AgCreate("SLEN");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create SLEN              
                            Create("SLEN");               
                      }           
                      { AgPlacement place = AgPlacement("SLEN","SUPL");              
                            /// Add daughter volume SLEN to mother SUPL              
                            place.TranslateX((smai.rinner+sslo.railhei+sslo.endhei/2));              
                            /// Translate x = (smai.rinner+sslo.railhei+sslo.endhei/2)              
                            place.TranslateZ((smai.zmax-smai.zmin-sslo.endthk)/2);              
                            /// Translate z = (smai.zmax-smai.zmin-sslo.endthk)/2              
                            _stacker -> Position( AgBlock::Find("SLEN"), place );              
                      } // end placement of SLEN           
                      _create = AgCreate("SLFX");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create SLFX              
                            Create("SLFX");               
                      }           
                      { AgPlacement place = AgPlacement("SLFX","SUPL");              
                            /// Add daughter volume SLFX to mother SUPL              
                            place.TranslateX(sslo.boltpos);              
                            /// Translate x = sslo.boltpos              
                            place.TranslateY(-sslo.boltoff);              
                            /// Translate y = -sslo.boltoff              
                            place.TranslateZ((smai.zmin-smai.zmax+smai.fixthk)/2);              
                            /// Translate z = (smai.zmin-smai.zmax+smai.fixthk)/2              
                            _stacker -> Position( AgBlock::Find("SLFX"), place );              
                      } // end placement of SLFX           
                      _create = AgCreate("SLBL");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create SLBL              
                            Create("SLBL");               
                      }           
                      { AgPlacement place = AgPlacement("SLBL","SUPL");              
                            /// Add daughter volume SLBL to mother SUPL              
                            place.TranslateX(sslo.boltpos);              
                            /// Translate x = sslo.boltpos              
                            place.TranslateZ((smai.fixthk-sslo.raillen)/2);              
                            /// Translate z = (smai.fixthk-sslo.raillen)/2              
                            _stacker -> Position( AgBlock::Find("SLBL"), place );              
                      } // end placement of SLBL           
                      END_OF_SUPL:           
                      mCurrent = _save;           
                ///@}        
          } // End Block SUPL     
          // ---------------------------------------------------------------------------------------------------     
          void SLRL::Block( AgCreate create )     
          {         
                ///@addtogroup SLRL_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material aluminium            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Aluminium");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("SLRL");              
                            attr.par("seen")=1;              
                            attr.par("colo")=2;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Trd1");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx1")=sslo.railwin/2;              
                            shape.par("dx2")=sslo.railwout/2;              
                            shape.par("dy")=sslo.raillen/2;              
                            shape.par("dz")=sslo.railhei/2;              
                            /// Shape Trd1 dx1=sslo.railwin/2 dx2=sslo.railwout/2 dy=sslo.raillen/2 dz=sslo.railhei/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_SLRL;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_SLRL:           
                      mCurrent = _save;           
                ///@}        
          } // End Block SLRL     
          // ---------------------------------------------------------------------------------------------------     
          void SLWL::Block( AgCreate create )     
          {         
                ///@addtogroup SLWL_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material aluminium            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Aluminium");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("SLRL");              
                            attr.par("seen")=1;              
                            attr.par("colo")=2;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=sslo.wallhei/2;              
                            shape.par("dy")=sslo.wallwid/2;              
                            shape.par("dz")=sslo.raillen/2-sslo.headthk/2;              
                            /// Shape Bbox dx=sslo.wallhei/2 dy=sslo.wallwid/2 dz=sslo.raillen/2-sslo.headthk/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_SLWL;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_SLWL:           
                      mCurrent = _save;           
                ///@}        
          } // End Block SLWL     
          // ---------------------------------------------------------------------------------------------------     
          void SLHD::Block( AgCreate create )     
          {         
                ///@addtogroup SLHD_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material aluminium            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Aluminium");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("SLHD");              
                            attr.par("seen")=1;              
                            attr.par("colo")=2;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=sslo.headhei/2;              
                            shape.par("dy")=sslo.railwout/2;              
                            shape.par("dz")=sslo.headthk/2;              
                            /// Shape Bbox dx=sslo.headhei/2 dy=sslo.railwout/2 dz=sslo.headthk/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_SLHD;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_SLHD:           
                      mCurrent = _save;           
                ///@}        
          } // End Block SLHD     
          // ---------------------------------------------------------------------------------------------------     
          void SLXW::Block( AgCreate create )     
          {         
                ///@addtogroup SLXW_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material aluminium            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Aluminium");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("SLXW");              
                            attr.par("seen")=1;              
                            attr.par("colo")=2;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=sslo.wallhei/2;              
                            shape.par("dy")=sslo.railwout/2-sslo.wallwid;              
                            shape.par("dz")=sslo.xwalthk/2;              
                            /// Shape Bbox dx=sslo.wallhei/2 dy=sslo.railwout/2-sslo.wallwid dz=sslo.xwalthk/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_SLXW;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_SLXW:           
                      mCurrent = _save;           
                ///@}        
          } // End Block SLXW     
          // ---------------------------------------------------------------------------------------------------     
          void SLEN::Block( AgCreate create )     
          {         
                ///@addtogroup SLEN_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material aluminium            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Aluminium");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("SLEN");              
                            attr.par("seen")=1;              
                            attr.par("colo")=2;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=sslo.endhei/2;              
                            shape.par("dy")=sslo.railwout/2-sslo.wallwid;              
                            shape.par("dz")=sslo.endthk/2;              
                            /// Shape Bbox dx=sslo.endhei/2 dy=sslo.railwout/2-sslo.wallwid dz=sslo.endthk/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_SLEN;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_SLEN:           
                      mCurrent = _save;           
                ///@}        
          } // End Block SLEN     
          // ---------------------------------------------------------------------------------------------------     
          void SLFX::Block( AgCreate create )     
          {         
                ///@addtogroup SLFX_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material aluminium            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Aluminium");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("SLFX");              
                            attr.par("seen")=1;              
                            attr.par("colo")=2;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=smai.fixwid/2;              
                            shape.par("dy")=smai.fixhei/2;              
                            shape.par("dz")=smai.fixthk/2;              
                            /// Shape Bbox dx=smai.fixwid/2 dy=smai.fixhei/2 dz=smai.fixthk/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_SLFX;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_SLFX:           
                      mCurrent = _save;           
                ///@}        
          } // End Block SLFX     
          // ---------------------------------------------------------------------------------------------------     
          void SLBL::Block( AgCreate create )     
          {         
                ///@addtogroup SLBL_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material aluminium            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Aluminium");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("SLBL");              
                            attr.par("seen")=1;              
                            attr.par("colo")=2;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=0;              
                            shape.par("rmax")=sslo.boltrad;              
                            shape.par("dz")=(smai.zmax-smai.zmin-sslo.raillen-smai.fixthk)/2;              
                            /// Shape Tube rmin=0 rmax=sslo.boltrad dz=(smai.zmax-smai.zmin-sslo.raillen-smai.fixthk)/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_SLBL;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_SLBL:           
                      mCurrent = _save;           
                ///@}        
          } // End Block SLBL     
          // ---------------------------------------------------------------------------------------------------     
          void SUPH::Block( AgCreate create )     
          {         
                ///@addtogroup SUPH_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      {  AgShape shape = AgShape("Tubs");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=smai.rinner;              
                            shape.par("rmax")=smai.router;              
                            shape.par("phi1")=90-sshi.phimax;              
                            shape.par("phi2")=90-sshi.phimin;              
                            shape.par("dz")=(smai.zmax-smai.zmin)/2;              
                            /// Shape Tubs rmin=smai.rinner rmax=smai.router phi1=90-sshi.phimax phi2=90-sshi.phimin dz=(smai.zmax-smai.zmin)/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_SUPH;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("SHRL");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create SHRL              
                            Create("SHRL");               
                      }           
                      { AgPlacement place = AgPlacement("SHRL","SUPH");              
                            /// Add daughter volume SHRL to mother SUPH              
                            place.TranslateX(sshi.xdist+sshi.platthk/2-sshi.railwid/2);              
                            /// Translate x = sshi.xdist+sshi.platthk/2-sshi.railwid/2              
                            place.TranslateY(sshi.ydist+sshi.railhei/2);              
                            /// Translate y = sshi.ydist+sshi.railhei/2              
                            _stacker -> Position( AgBlock::Find("SHRL"), place );              
                      } // end placement of SHRL           
                      _create = AgCreate("SHPT");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create SHPT              
                            Create("SHPT");               
                      }           
                      { AgPlacement place = AgPlacement("SHPT","SUPH");              
                            /// Add daughter volume SHPT to mother SUPH              
                            place.TranslateX(sshi.xdist);              
                            /// Translate x = sshi.xdist              
                            place.TranslateY(sshi.ydist+sshi.railhei+sshi.plathei/2);              
                            /// Translate y = sshi.ydist+sshi.railhei+sshi.plathei/2              
                            place.TranslateZ(-sshi.baroffz);              
                            /// Translate z = -sshi.baroffz              
                            /// G3 Reference: thetax = 90              
                            /// G3 Reference: phix = 0              
                            /// G3 Reference: thetay = 90              
                            /// G3 Reference: phiy = 90              
                            /// G3 Reference: thetaz = 0              
                            /// G3 Reference: phiz = 0              
                            place.Ortho( "XYZ" ); // ORT=XYZ              
                            /// Axis substitution: XYZ --> XYZ              
                            place.AlphaY(90);              
                            /// Rotate: AlphaY = 90              
                            /// G3 Reference: thetax = 90              
                            /// G3 Reference: phix = 0              
                            /// G3 Reference: thetay = 90              
                            /// G3 Reference: phiy = 90              
                            /// G3 Reference: thetaz = 0              
                            /// G3 Reference: phiz = 0              
                            _stacker -> Position( AgBlock::Find("SHPT"), place );              
                      } // end placement of SHPT           
                      _create = AgCreate("SHBR");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create SHBR              
                            Create("SHBR");               
                      }           
                      { AgPlacement place = AgPlacement("SHBR","SUPH");              
                            /// Add daughter volume SHBR to mother SUPH              
                            place.TranslateX(sshi.xdist);              
                            /// Translate x = sshi.xdist              
                            place.TranslateY(sshi.ydist+sshi.railhei+sshi.plathei+sshi.barhei/2);              
                            /// Translate y = sshi.ydist+sshi.railhei+sshi.plathei+sshi.barhei/2              
                            place.TranslateZ((smai.zmin-smai.zmax)/2+sshi.baroffz+sshi.barlen/2);              
                            /// Translate z = (smai.zmin-smai.zmax)/2+sshi.baroffz+sshi.barlen/2              
                            _stacker -> Position( AgBlock::Find("SHBR"), place );              
                      } // end placement of SHBR           
                      _create = AgCreate("SHBK");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create SHBK              
                            Create("SHBK");               
                      }           
                      pos=sshi.ydist+sshi.railhei+sshi.plathei+sshi.barhei;           
                      { AgPlacement place = AgPlacement("SHBK","SUPH");              
                            /// Add daughter volume SHBK to mother SUPH              
                            place.TranslateX(sshi.xdist);              
                            /// Translate x = sshi.xdist              
                            place.TranslateY(pos+sshi.blochei/2);              
                            /// Translate y = pos+sshi.blochei/2              
                            place.TranslateZ((smai.zmin-smai.zmax)/2+smai.fixthk+sshi.bloclen/2);              
                            /// Translate z = (smai.zmin-smai.zmax)/2+smai.fixthk+sshi.bloclen/2              
                            _stacker -> Position( AgBlock::Find("SHBK"), place );              
                      } // end placement of SHBK           
                      _create = AgCreate("SHFX");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create SHFX              
                            Create("SHFX");               
                      }           
                      { AgPlacement place = AgPlacement("SHFX","SUPH");              
                            /// Add daughter volume SHFX to mother SUPH              
                            place.TranslateX(sshi.xdist-sshi.boltoff*sin(degrad*45));              
                            /// Translate x = sshi.xdist-sshi.boltoff*sin(degrad*45)              
                            place.TranslateY(pos+sshi.blochei/2+sshi.boltoff*cos(degrad*45));              
                            /// Translate y = pos+sshi.blochei/2+sshi.boltoff*cos(degrad*45)              
                            place.TranslateZ((smai.zmin-smai.zmax)/2+smai.fixthk/2);              
                            /// Translate z = (smai.zmin-smai.zmax)/2+smai.fixthk/2              
                            place.AlphaZ(45);              
                            /// Rotate: AlphaZ = 45              
                            /// G3 Reference: thetax = 90              
                            /// G3 Reference: phix = 0              
                            /// G3 Reference: thetay = 90              
                            /// G3 Reference: phiy = 90              
                            /// G3 Reference: thetaz = 0              
                            /// G3 Reference: phiz = 0              
                            _stacker -> Position( AgBlock::Find("SHFX"), place );              
                      } // end placement of SHFX           
                      _create = AgCreate("SHST");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create SHST              
                            Create("SHST");               
                      }           
                      { AgPlacement place = AgPlacement("SHST","SUPH");              
                            /// Add daughter volume SHST to mother SUPH              
                            place.TranslateX(sshi.xdist-sshi.platthk/2-sshi.stabwid/2);              
                            /// Translate x = sshi.xdist-sshi.platthk/2-sshi.stabwid/2              
                            place.TranslateY(sshi.ydist+sshi.railhei+sshi.plathei/2);              
                            /// Translate y = sshi.ydist+sshi.railhei+sshi.plathei/2              
                            place.TranslateZ((smai.zmin-smai.zmax)/2+sshi.stab1z);              
                            /// Translate z = (smai.zmin-smai.zmax)/2+sshi.stab1z              
                            _stacker -> Position( AgBlock::Find("SHST"), place );              
                      } // end placement of SHST           
                      { AgPlacement place = AgPlacement("SHST","SUPH");              
                            /// Add daughter volume SHST to mother SUPH              
                            place.TranslateX(sshi.xdist+sshi.platthk/2+sshi.stabwid/2);              
                            /// Translate x = sshi.xdist+sshi.platthk/2+sshi.stabwid/2              
                            place.TranslateY(sshi.ydist+sshi.railhei+sshi.plathei/2);              
                            /// Translate y = sshi.ydist+sshi.railhei+sshi.plathei/2              
                            place.TranslateZ((smai.zmin-smai.zmax)/2+sshi.stab1z);              
                            /// Translate z = (smai.zmin-smai.zmax)/2+sshi.stab1z              
                            _stacker -> Position( AgBlock::Find("SHST"), place );              
                      } // end placement of SHST           
                      { AgPlacement place = AgPlacement("SHST","SUPH");              
                            /// Add daughter volume SHST to mother SUPH              
                            place.TranslateX(sshi.xdist-sshi.platthk/2-sshi.stabwid/2);              
                            /// Translate x = sshi.xdist-sshi.platthk/2-sshi.stabwid/2              
                            place.TranslateY(sshi.ydist+sshi.railhei+sshi.plathei/2);              
                            /// Translate y = sshi.ydist+sshi.railhei+sshi.plathei/2              
                            place.TranslateZ((smai.zmin-smai.zmax)/2+sshi.stab2z);              
                            /// Translate z = (smai.zmin-smai.zmax)/2+sshi.stab2z              
                            _stacker -> Position( AgBlock::Find("SHST"), place );              
                      } // end placement of SHST           
                      { AgPlacement place = AgPlacement("SHST","SUPH");              
                            /// Add daughter volume SHST to mother SUPH              
                            place.TranslateX(sshi.xdist+sshi.platthk/2+sshi.stabwid/2);              
                            /// Translate x = sshi.xdist+sshi.platthk/2+sshi.stabwid/2              
                            place.TranslateY(sshi.ydist+sshi.railhei+sshi.plathei/2);              
                            /// Translate y = sshi.ydist+sshi.railhei+sshi.plathei/2              
                            place.TranslateZ((smai.zmin-smai.zmax)/2+sshi.stab2z);              
                            /// Translate z = (smai.zmin-smai.zmax)/2+sshi.stab2z              
                            _stacker -> Position( AgBlock::Find("SHST"), place );              
                      } // end placement of SHST           
                      END_OF_SUPH:           
                      mCurrent = _save;           
                ///@}        
          } // End Block SUPH     
          // ---------------------------------------------------------------------------------------------------     
          void SHRL::Block( AgCreate create )     
          {         
                ///@addtogroup SHRL_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material aluminium            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Aluminium");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("SHRL");              
                            attr.par("seen")=1;              
                            attr.par("colo")=2;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=sshi.railwid/2;              
                            shape.par("dy")=sshi.railhei/2;              
                            shape.par("dz")=(smai.zmax-smai.zmin)/2;              
                            /// Shape Bbox dx=sshi.railwid/2 dy=sshi.railhei/2 dz=(smai.zmax-smai.zmin)/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_SHRL;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_SHRL:           
                      mCurrent = _save;           
                ///@}        
          } // End Block SHRL     
          // ---------------------------------------------------------------------------------------------------     
          void SHPT::Block( AgCreate create )     
          {         
                ///@addtogroup SHPT_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      shptpos=((smai.zmax-smai.zmin)/2-(sshi.baroffz+sshi.barlen/2));           
                      angle=atan(shptpos/sshi.plathei);           
                      /// Material aluminium            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Aluminium");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("SHPT");              
                            attr.par("seen")=1;              
                            attr.par("colo")=2;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Trap");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dz")=sshi.platthk/2;              
                            shape.par("thet")=0;              
                            shape.par("phi")=0;              
                            shape.par("h1")=sshi.plathei/2;              
                            shape.par("bl1")=(smai.zmax-smai.zmin)/2;              
                            shape.par("tl1")=sshi.barlen/2;              
                            shape.par("alp1")=angle/degrad;              
                            shape.par("h2")=sshi.plathei/2;              
                            shape.par("bl2")=(smai.zmax-smai.zmin)/2;              
                            shape.par("tl2")=sshi.barlen/2;              
                            shape.par("alp2")=angle/degrad;              
                            /// Shape Trap dz=sshi.platthk/2 thet=0 phi=0 h1=sshi.plathei/2 bl1=(smai.zmax-smai.zmin)/2 tl1=sshi.barlen/2 alp1=angle/degrad h2=sshi.plathei/2 bl2=(smai.zmax-smai.zmin)/2 tl2=sshi.barlen/2 alp2=angle/degrad               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_SHPT;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_SHPT:           
                      mCurrent = _save;           
                ///@}        
          } // End Block SHPT     
          // ---------------------------------------------------------------------------------------------------     
          void SHBR::Block( AgCreate create )     
          {         
                ///@addtogroup SHBR_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material aluminium            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Aluminium");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("SHBR");              
                            attr.par("seen")=1;              
                            attr.par("colo")=2;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=sshi.topwid/2;              
                            shape.par("dy")=sshi.barhei/2;              
                            shape.par("dz")=sshi.barlen/2;              
                            /// Shape Bbox dx=sshi.topwid/2 dy=sshi.barhei/2 dz=sshi.barlen/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_SHBR;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_SHBR:           
                      mCurrent = _save;           
                ///@}        
          } // End Block SHBR     
          // ---------------------------------------------------------------------------------------------------     
          void SHBK::Block( AgCreate create )     
          {         
                ///@addtogroup SHBK_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material aluminium            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Aluminium");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("SHBK");              
                            attr.par("seen")=1;              
                            attr.par("colo")=2;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=sshi.topwid/2;              
                            shape.par("dy")=sshi.blochei/2;              
                            shape.par("dz")=sshi.bloclen/2;              
                            /// Shape Bbox dx=sshi.topwid/2 dy=sshi.blochei/2 dz=sshi.bloclen/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_SHBK;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_SHBK:           
                      mCurrent = _save;           
                ///@}        
          } // End Block SHBK     
          // ---------------------------------------------------------------------------------------------------     
          void SHFX::Block( AgCreate create )     
          {         
                ///@addtogroup SHFX_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
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
                      { AgAttribute attr = AgAttribute("SHFX");              
                            attr.par("seen")=1;              
                            attr.par("colo")=3;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=smai.fixwid/2;              
                            shape.par("dy")=smai.fixhei/2;              
                            shape.par("dz")=smai.fixthk/2;              
                            /// Shape Bbox dx=smai.fixwid/2 dy=smai.fixhei/2 dz=smai.fixthk/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_SHFX;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_SHFX:           
                      mCurrent = _save;           
                ///@}        
          } // End Block SHFX     
          // ---------------------------------------------------------------------------------------------------     
          void SHST::Block( AgCreate create )     
          {         
                ///@addtogroup SHST_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material aluminium            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Aluminium");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("SHBK");              
                            attr.par("seen")=1;              
                            attr.par("colo")=2;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=sshi.stabwid/2;              
                            shape.par("dy")=sshi.plathei/2;              
                            shape.par("dz")=sshi.stabthk/2;              
                            /// Shape Bbox dx=sshi.stabwid/2 dy=sshi.plathei/2 dz=sshi.stabthk/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_SHST;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_SHST:           
                      mCurrent = _save;           
                ///@}        
          } // End Block SHST     
    // ----------------------------------------------------------------------- geoctr
       void SupoGeo1::ConstructGeometry( const Char_t *dummy )     
       {        
             ///@addtogroup SupoGeo1_revision        
             ///@{           
                   /// Author: Holm Huemmler           
             ///@}        
             ///@addtogroup SupoGeo1_revision        
             ///@{           
                   /// Created:   27-Okt-99            
             ///@}        
             AddBlock("SUPO");        
             AddBlock("SUPL");        
             AddBlock("SUPH");        
             AddBlock("SLRL");        
             AddBlock("SLWL");        
             AddBlock("SLHD");        
             AddBlock("SLXW");        
             AddBlock("SLEN");        
             AddBlock("SLFX");        
             AddBlock("SLBL");        
             AddBlock("SHRL");        
             AddBlock("SHPT");        
             AddBlock("SHBR");        
             AddBlock("SHBK");        
             AddBlock("SHFX");        
             AddBlock("SHST");        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup smai_doc        
             ///@{           
                   ++smai._index;           
                   smai . version = 1; //  geometry Version            
                   /// smai . version = 1; //  geometry Version            
                   smai . rinner = 36.4; //  envelope inner radius             
                   /// smai . rinner = 36.4; //  envelope inner radius             
                   smai . router = 47.5; //  envelope outer radius            
                   /// smai . router = 47.5; //  envelope outer radius            
                   smai . zmin = 230.69; //  envelope minimum z value            
                   /// smai . zmin = 230.69; //  envelope minimum z value            
                   smai . zmax = 259.7; //  envelope maximum z value            
                   /// smai . zmax = 259.7; //  envelope maximum z value            
                   smai . phimid = 30; //  angle of support center positions            
                   /// smai . phimid = 30; //  angle of support center positions            
                   smai . fixhei = 10.22; //  Height of support fixture plate (on TPC)            
                   /// smai . fixhei = 10.22; //  Height of support fixture plate (on TPC)            
                   smai . fixwid = 3.79; //  Width of support fixture plate             
                   /// smai . fixwid = 3.79; //  Width of support fixture plate             
                   smai . fixthk = 2.04; //  Thickness of support fixture plate            
                   /// smai . fixthk = 2.04; //  Thickness of support fixture plate            
                   //           
                   smai.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup sslo_doc        
             ///@{           
                   ++sslo._index;           
                   sslo . version = 1; //  geometry Version            
                   /// sslo . version = 1; //  geometry Version            
                   sslo . phimin = -6; //  envelope start angle            
                   /// sslo . phimin = -6; //  envelope start angle            
                   sslo . phimax = 11; //  envelope end angle            
                   /// sslo . phimax = 11; //  envelope end angle            
                   sslo . raillen = 24.5; //  support rail length            
                   /// sslo . raillen = 24.5; //  support rail length            
                   sslo . railwin = 4.07; //  support rail inner width            
                   /// sslo . railwin = 4.07; //  support rail inner width            
                   sslo . railwout = 7.03; //  support rail outer width            
                   /// sslo . railwout = 7.03; //  support rail outer width            
                   sslo . railhei = 2.96; //  support rail height            
                   /// sslo . railhei = 2.96; //  support rail height            
                   sslo . wallwid = 0.78; //  support wall thickness            
                   /// sslo . wallwid = 0.78; //  support wall thickness            
                   sslo . wallhei = 4.46; //  support wall height            
                   /// sslo . wallhei = 4.46; //  support wall height            
                   sslo . headthk = 2.68; //  support head thickness            
                   /// sslo . headthk = 2.68; //  support head thickness            
                   sslo . headhei = 7.82; //  support head height            
                   /// sslo . headhei = 7.82; //  support head height            
                   sslo . xwalthk = 0.82; //  cross support wall thickness            
                   /// sslo . xwalthk = 0.82; //  cross support wall thickness            
                   sslo . xwal1pos = 0.55; //  1st cross support wall pos (to env center)            
                   /// sslo . xwal1pos = 0.55; //  1st cross support wall pos (to env center)            
                   sslo . xwal2pos = 7.02; //  2nd cross support wall pos (to env center)            
                   /// sslo . xwal2pos = 7.02; //  2nd cross support wall pos (to env center)            
                   sslo . endthk = 2.6; //  support end block thickness            
                   /// sslo . endthk = 2.6; //  support end block thickness            
                   sslo . endhei = 1.49; //  support end block height            
                   /// sslo . endhei = 1.49; //  support end block height            
                   sslo . boltpos = 44.9; //  support bolt radial position            
                   /// sslo . boltpos = 44.9; //  support bolt radial position            
                   sslo . boltoff = 1.54; //  support bolt offset from fixture center            
                   /// sslo . boltoff = 1.54; //  support bolt offset from fixture center            
                   sslo . boltrad = 1.2; //  effective support bolt radius            
                   /// sslo . boltrad = 1.2; //  effective support bolt radius            
                   //           
                   sslo.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup sshi_doc        
             ///@{           
                   ++sshi._index;           
                   sshi . version = 1; //  geometry Version            
                   /// sshi . version = 1; //  geometry Version            
                   sshi . phimin = 38; //  envelope start angle            
                   /// sshi . phimin = 38; //  envelope start angle            
                   sshi . phimax = 65; //  envelope end angle            
                   /// sshi . phimax = 65; //  envelope end angle            
                   sshi . xdist = 34.19; //  distance of support to beam axis in x            
                   /// sshi . xdist = 34.19; //  distance of support to beam axis in x            
                   sshi . ydist = 16.39; //  distance of support bottom to axis in y            
                   /// sshi . ydist = 16.39; //  distance of support bottom to axis in y            
                   sshi . railwid = 1.75; //  Width of upper FTPC support rail            
                   /// sshi . railwid = 1.75; //  Width of upper FTPC support rail            
                   sshi . railhei = 1.93; //  Height of support rail             
                   /// sshi . railhei = 1.93; //  Height of support rail             
                   sshi . platthk = 0.41; //  Thickness of support plate            
                   /// sshi . platthk = 0.41; //  Thickness of support plate            
                   sshi . plathei = 8.69; //  Height of support plate            
                   /// sshi . plathei = 8.69; //  Height of support plate            
                   sshi . barlen = 10.58; //  Length of support top bar            
                   /// sshi . barlen = 10.58; //  Length of support top bar            
                   sshi . baroffz = 3.22; //  Offset of bar from support base in z            
                   /// sshi . baroffz = 3.22; //  Offset of bar from support base in z            
                   sshi . barhei = 0.81; //  Height of support top bar            
                   /// sshi . barhei = 0.81; //  Height of support top bar            
                   sshi . topwid = 2.58; //  Width of support top part            
                   /// sshi . topwid = 2.58; //  Width of support top part            
                   sshi . blochei = 2.74; //  Height of support top block            
                   /// sshi . blochei = 2.74; //  Height of support top block            
                   sshi . bloclen = 5.5; //  Length of support top block            
                   /// sshi . bloclen = 5.5; //  Length of support top block            
                   sshi . boltoff = 3.26; //  support bolt offset from fixture center            
                   /// sshi . boltoff = 3.26; //  support bolt offset from fixture center            
                   sshi . stabwid = 1.15; //  Width of support stabilizers            
                   /// sshi . stabwid = 1.15; //  Width of support stabilizers            
                   sshi . stabthk = 0.41; //  Thickness of support stabilizers            
                   /// sshi . stabthk = 0.41; //  Thickness of support stabilizers            
                   sshi . stab1z = 8.32; //  z-position of first stabilizer            
                   /// sshi . stab1z = 8.32; //  z-position of first stabilizer            
                   sshi . stab2z = 13.58; //  z-position of second stabilizer            
                   /// sshi . stab2z = 13.58; //  z-position of second stabilizer            
                   //           
                   sshi.fill();           
             ///@}        
             //        
             pos=(smai.zmin+smai.zmax)/2;        
             _create = AgCreate("SUPO");        
             {           
                   AgShape myshape; // undefined shape           
                   ///Create SUPO           
                   Create("SUPO");            
             }        
             pos=(smai.zmin+smai.zmax)/2;        
             { AgPlacement place = AgPlacement("SUPO","CAVE");           
                   /// Add daughter volume SUPO to mother CAVE           
                   place.TranslateZ(-pos);           
                   /// Translate z = -pos           
                   place.par("only")=AgPlacement::kMany;           
                   /// Overlap: agplacement::kmany           
                   _stacker -> Position( AgBlock::Find("SUPO"), place );           
             } // end placement of SUPO        
             { AgPlacement place = AgPlacement("SUPO","CAVE");           
                   /// Add daughter volume SUPO to mother CAVE           
                   place.TranslateZ(pos);           
                   /// Translate z = pos           
                   place.par("only")=AgPlacement::kMany;           
                   /// Overlap: agplacement::kmany           
                   /// G3 Reference: thetax = 90           
                   /// G3 Reference: phix = 0           
                   /// G3 Reference: thetay = 90           
                   /// G3 Reference: phiy = 90           
                   /// G3 Reference: thetaz = 180           
                   /// G3 Reference: phiz = 0           
                   Double_t _thetax=90,_phix=0,_thetay=90,_phiy=90,_thetaz=180,_phiz=0;           
                   place.Reference( _thetax, _phix, _thetay, _phiy, _thetaz, _phiz );           
                   _stacker -> Position( AgBlock::Find("SUPO"), place );           
             } // end placement of SUPO        
       }; // SupoGeo1     
 }; // namespace SupoGeo1  
 