#include "FtroGeo.h"  
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
 namespace FTROGEO // $NMSPC  
 {     
       //     
       // ---------------------------------------------------------------------------------------------------     
          ///@addtogroup FtroGeo_vars     
          ///@{        
                Float_t zpos,myraddeg,holeangle,strutangle,anglepos,angle;        
                //        
                /// Float_t zpos,myraddeg,holeangle,strutangle,anglepos,angle        
          ///@}     
          ///@addtogroup FtroGeo_vars     
          ///@{        
                Float_t strutrad,plankrad,strutholespacing,centerstrut,inflangez;        
                //        
                /// Float_t strutrad,plankrad,strutholespacing,centerstrut,inflangez        
          ///@}     
          ///@addtogroup FtroGeo_vars     
          ///@{        
                Float_t ftpoanglepos,ftpianglepos,a,reff;        
                //        
                /// Float_t ftpoanglepos,ftpianglepos,a,reff        
          ///@}     
          ///@addtogroup FtroGeo_vars     
          ///@{        
                Int_t nhole,nstrut;        
                //        
                /// Int_t nhole,nstrut        
          ///@}     
          //  -----------------------------------------------------     
          /// @defgroup ftrg_doc     
          /// \class Ftrg_t     
          /// \brief User-defined structure     
          ///                             
          /// AgML structure members:     
          ///                             
          ///Float_t version;     
          ///Float_t n;     
          ///Float_t ftpcz;     
          ///Float_t ftpclen;     
          ///Float_t length;     
          ///Float_t angoffset;     
          ///Float_t rin;     
          ///Float_t rout;     
          ///Float_t inrin;     
          ///Float_t inrout;     
          ///Float_t inrthk;     
          ///Float_t ofrin;     
          ///Float_t ofrout;     
          ///Float_t ofrthk;     
          ///Float_t ofz;     
          ///Float_t ofnholes;     
          ///Float_t ofholerad;     
          ///Float_t ofholer;     
          ///Float_t strutlen;     
          ///Float_t struthgt;     
          ///Float_t strutwth;     
          ///Float_t strutthk;     
          ///Float_t strutholer;     
          ///Float_t strutnholes;     
          ///Float_t ftpowth;     
          ///Float_t ftpothk;     
          ///Float_t ftpor;     
          ///Float_t ftpiwth;     
          ///Float_t ftpithk;     
          ///Float_t ftpir;     
          ///Float_t shellthk;     
          ///Float_t rimthk;     
          ///Float_t rimwth;     
          ///Float_t plankwth;     
          ///Float_t plankthk;     
          ///Int_t _index;     
          //     
          Ftrg_t ftrg;     
          //     
       FtroGeo::FtroGeo()     
         : AgModule("FtroGeo"," is the geometry of the readout structure of the FTPC ")     
       {        
       }     
          // ---------------------------------------------------------------------------------------------------     
          void FTMO::Block( AgCreate create )     
          {         
                ///@addtogroup FTMO_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Air            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Air");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("FTMO");              
                            attr.par("seen")=1;              
                            attr.par("colo")=6;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=ftrg.rin;              
                            shape.par("rmax")=ftrg.rout;              
                            shape.par("dz")=ftrg.length/2.0;              
                            /// Shape Tube rmin=ftrg.rin rmax=ftrg.rout dz=ftrg.length/2.0               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FTMO;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("FTCM");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FTCM              
                            Create("FTCM");               
                      }           
                      { AgPlacement place = AgPlacement("FTCM","FTMO");              
                            /// Add daughter volume FTCM to mother FTMO              
                            place.TranslateZ(centerstrut);              
                            /// Translate z = centerstrut              
                            _stacker -> Position( AgBlock::Find("FTCM"), place );              
                      } // end placement of FTCM           
                      _create = AgCreate("FTOF");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FTOF              
                            Create("FTOF");               
                      }           
                      { AgPlacement place = AgPlacement("FTOF","FTMO");              
                            /// Add daughter volume FTOF to mother FTMO              
                            place.TranslateX(0.0);              
                            /// Translate x = 0.0              
                            place.TranslateY(0.0);              
                            /// Translate y = 0.0              
                            place.TranslateZ(ftrg.ofz-ftrg.length/2.0+ftrg.ofrthk);              
                            /// Translate z = ftrg.ofz-ftrg.length/2.0+ftrg.ofrthk              
                            _stacker -> Position( AgBlock::Find("FTOF"), place );              
                      } // end placement of FTOF           
                      _create = AgCreate("FTIF");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FTIF              
                            Create("FTIF");               
                      }           
                      { AgPlacement place = AgPlacement("FTIF","FTMO");              
                            /// Add daughter volume FTIF to mother FTMO              
                            place.TranslateX(0.0);              
                            /// Translate x = 0.0              
                            place.TranslateY(0.0);              
                            /// Translate y = 0.0              
                            place.TranslateZ(inflangez-ftrg.inrthk/2.0);              
                            /// Translate z = inflangez-ftrg.inrthk/2.0              
                            _stacker -> Position( AgBlock::Find("FTIF"), place );              
                      } // end placement of FTIF           
                      _create = AgCreate("FTRM");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FTRM              
                            Create("FTRM");               
                      }           
                      { AgPlacement place = AgPlacement("FTRM","FTMO");              
                            /// Add daughter volume FTRM to mother FTMO              
                            place.TranslateX(0.0);              
                            /// Translate x = 0.0              
                            place.TranslateY(0.0);              
                            /// Translate y = 0.0              
                            place.TranslateZ(inflangez-ftrg.inrthk-ftrg.rimwth/2.0);              
                            /// Translate z = inflangez-ftrg.inrthk-ftrg.rimwth/2.0              
                            _stacker -> Position( AgBlock::Find("FTRM"), place );              
                      } // end placement of FTRM           
                      END_OF_FTMO:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FTMO     
          // ---------------------------------------------------------------------------------------------------     
          void FTCM::Block( AgCreate create )     
          {         
                ///@addtogroup FTCM_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("FTCM");              
                            attr.par("seen")=0;              
                            attr.par("colo")=0;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=ftrg.rin;              
                            shape.par("rmax")=ftrg.rout;              
                            shape.par("dz")=ftrg.strutlen/2.0;              
                            /// Shape Tube rmin=ftrg.rin rmax=ftrg.rout dz=ftrg.strutlen/2.0               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FTCM;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("FTCD");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FTCD              
                            Create("FTCD");               
                      }           
                      END_OF_FTCM:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FTCM     
          // ---------------------------------------------------------------------------------------------------     
          void FTCD::Block( AgCreate create )     
          {         
                ///@addtogroup FTCD_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      {  AgShape shape = AgShape("Division");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("ndiv")=ftrg.n;              
                            shape.par("iaxis")=2;              
                            /// Shape Division ndiv=ftrg.n iaxis=2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FTCD;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("FSMO");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FSMO              
                            Create("FSMO");               
                      }           
                      { AgPlacement place = AgPlacement("FSMO","FTCD");              
                            /// Add daughter volume FSMO to mother FTCD              
                            place.TranslateX(strutrad*cos(0.0));              
                            /// Translate x = strutrad*cos(0.0)              
                            place.TranslateY(strutrad*sin(0.0));              
                            /// Translate y = strutrad*sin(0.0)              
                            place.TranslateZ(0.0);              
                            /// Translate z = 0.0              
                            place.AlphaZ(90);              
                            /// Rotate: AlphaZ = 90              
                            /// G3 Reference: thetax = 90              
                            /// G3 Reference: phix = 0              
                            /// G3 Reference: thetay = 90              
                            /// G3 Reference: phiy = 90              
                            /// G3 Reference: thetaz = 0              
                            /// G3 Reference: phiz = 0              
                            _stacker -> Position( AgBlock::Find("FSMO"), place );              
                      } // end placement of FSMO           
                      _create = AgCreate("FTPL");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FTPL              
                            Create("FTPL");               
                      }           
                      { AgPlacement place = AgPlacement("FTPL","FTCD");              
                            /// Add daughter volume FTPL to mother FTCD              
                            place.TranslateX(plankrad*cos(0.0));              
                            /// Translate x = plankrad*cos(0.0)              
                            place.TranslateY(plankrad*sin(0.0));              
                            /// Translate y = plankrad*sin(0.0)              
                            place.TranslateZ(0.0);              
                            /// Translate z = 0.0              
                            place.AlphaZ(90);              
                            /// Rotate: AlphaZ = 90              
                            /// G3 Reference: thetax = 90              
                            /// G3 Reference: phix = 0              
                            /// G3 Reference: thetay = 90              
                            /// G3 Reference: phiy = 90              
                            /// G3 Reference: thetaz = 0              
                            /// G3 Reference: phiz = 0              
                            _stacker -> Position( AgBlock::Find("FTPL"), place );              
                      } // end placement of FTPL           
                      a=myraddeg*strutangle/2.0-ftpoanglepos;           
                      reff=(ftrg.ftpor+ftrg.ftpothk/2.0)/cos(ftpoanglepos);           
                      _create = AgCreate("FTPO");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FTPO              
                            Create("FTPO");               
                      }           
                      { AgPlacement place = AgPlacement("FTPO","FTCD");              
                            /// Add daughter volume FTPO to mother FTCD              
                            place.TranslateX(reff*cos(a));              
                            /// Translate x = reff*cos(a)              
                            place.TranslateY(reff*sin(a));              
                            /// Translate y = reff*sin(a)              
                            place.TranslateZ(0.0);              
                            /// Translate z = 0.0              
                            place.AlphaZ(90+strutangle/2.0);              
                            /// Rotate: AlphaZ = 90+strutangle/2.0              
                            /// G3 Reference: thetax = 90              
                            /// G3 Reference: phix = 0              
                            /// G3 Reference: thetay = 90              
                            /// G3 Reference: phiy = 90              
                            /// G3 Reference: thetaz = 0              
                            /// G3 Reference: phiz = 0              
                            _stacker -> Position( AgBlock::Find("FTPO"), place );              
                      } // end placement of FTPO           
                      { AgPlacement place = AgPlacement("FTPO","FTCD");              
                            /// Add daughter volume FTPO to mother FTCD              
                            place.TranslateX(reff*cos(-a));              
                            /// Translate x = reff*cos(-a)              
                            place.TranslateY(reff*sin(-a));              
                            /// Translate y = reff*sin(-a)              
                            place.TranslateZ(0.0);              
                            /// Translate z = 0.0              
                            place.AlphaZ(-90-strutangle/2.0);              
                            /// Rotate: AlphaZ = -90-strutangle/2.0              
                            /// G3 Reference: thetax = 90              
                            /// G3 Reference: phix = 0              
                            /// G3 Reference: thetay = 90              
                            /// G3 Reference: phiy = 90              
                            /// G3 Reference: thetaz = 0              
                            /// G3 Reference: phiz = 0              
                            _stacker -> Position( AgBlock::Find("FTPO"), place );              
                      } // end placement of FTPO           
                      a=myraddeg*strutangle/2.0-ftpianglepos;           
                      reff=(ftrg.ftpir+ftrg.ftpithk/2.0)/cos(ftpianglepos);           
                      _create = AgCreate("FTPI");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FTPI              
                            Create("FTPI");               
                      }           
                      { AgPlacement place = AgPlacement("FTPI","FTCD");              
                            /// Add daughter volume FTPI to mother FTCD              
                            place.TranslateX(reff*cos(a));              
                            /// Translate x = reff*cos(a)              
                            place.TranslateY(reff*sin(a));              
                            /// Translate y = reff*sin(a)              
                            place.TranslateZ(0.0);              
                            /// Translate z = 0.0              
                            place.AlphaZ(90+strutangle/2.0);              
                            /// Rotate: AlphaZ = 90+strutangle/2.0              
                            /// G3 Reference: thetax = 90              
                            /// G3 Reference: phix = 0              
                            /// G3 Reference: thetay = 90              
                            /// G3 Reference: phiy = 90              
                            /// G3 Reference: thetaz = 0              
                            /// G3 Reference: phiz = 0              
                            _stacker -> Position( AgBlock::Find("FTPI"), place );              
                      } // end placement of FTPI           
                      { AgPlacement place = AgPlacement("FTPI","FTCD");              
                            /// Add daughter volume FTPI to mother FTCD              
                            place.TranslateX(reff*cos(-a));              
                            /// Translate x = reff*cos(-a)              
                            place.TranslateY(reff*sin(-a));              
                            /// Translate y = reff*sin(-a)              
                            place.TranslateZ(0.0);              
                            /// Translate z = 0.0              
                            place.AlphaZ(-90-strutangle/2.0);              
                            /// Rotate: AlphaZ = -90-strutangle/2.0              
                            /// G3 Reference: thetax = 90              
                            /// G3 Reference: phix = 0              
                            /// G3 Reference: thetay = 90              
                            /// G3 Reference: phiy = 90              
                            /// G3 Reference: thetaz = 0              
                            /// G3 Reference: phiz = 0              
                            _stacker -> Position( AgBlock::Find("FTPI"), place );              
                      } // end placement of FTPI           
                      _create = AgCreate("FTSH");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FTSH              
                            Create("FTSH");               
                      }           
                      { AgPlacement place = AgPlacement("FTSH","FTCD");              
                            /// Add daughter volume FTSH to mother FTCD              
                            _stacker -> Position( AgBlock::Find("FTSH"), place );              
                      } // end placement of FTSH           
                      END_OF_FTCD:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FTCD     
          // ---------------------------------------------------------------------------------------------------     
          void FTOF::Block( AgCreate create )     
          {         
                ///@addtogroup FTOF_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Aluminium            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Aluminium");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("FTOF");              
                            attr.par("seen")=1;              
                            attr.par("colo")=2;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=ftrg.ofrin;              
                            shape.par("rmax")=ftrg.ofrout;              
                            shape.par("dz")=ftrg.ofrthk/2.0;              
                            /// Shape Tube rmin=ftrg.ofrin rmax=ftrg.ofrout dz=ftrg.ofrthk/2.0               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FTOF;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("FTOH");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FTOH              
                            Create("FTOH");               
                      }           
                      /// Loop on nhole from 1 to ftrg.ofnholes step=1           
                      for ( nhole=1; (1>0)? (nhole<=ftrg.ofnholes):(nhole>=ftrg.ofnholes); nhole+=1 )           
                      {              
                            angle=(nhole-1)*holeangle;              
                            anglepos = angle*myraddeg;              
                            { AgPlacement place = AgPlacement("FTOH","FTOF");                 
                                  /// Add daughter volume FTOH to mother FTOF                 
                                  place.TranslateX(ftrg.ofholerad*cos(anglepos));                 
                                  /// Translate x = ftrg.ofholerad*cos(anglepos)                 
                                  place.TranslateY(ftrg.ofholerad*sin(anglepos));                 
                                  /// Translate y = ftrg.ofholerad*sin(anglepos)                 
                                  _stacker -> Position( AgBlock::Find("FTOH"), place );                 
                            } // end placement of FTOH              
                      }           
                      END_OF_FTOF:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FTOF     
          // ---------------------------------------------------------------------------------------------------     
          void FTIF::Block( AgCreate create )     
          {         
                ///@addtogroup FTIF_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Aluminium            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Aluminium");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("FTIF");              
                            attr.par("seen")=1;              
                            attr.par("colo")=2;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=ftrg.inrin;              
                            shape.par("rmax")=ftrg.inrout;              
                            shape.par("dz")=ftrg.inrthk/2.0;              
                            /// Shape Tube rmin=ftrg.inrin rmax=ftrg.inrout dz=ftrg.inrthk/2.0               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FTIF;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_FTIF:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FTIF     
          // ---------------------------------------------------------------------------------------------------     
          void FTRM::Block( AgCreate create )     
          {         
                ///@addtogroup FTRM_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Aluminium            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Aluminium");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("FTRM");              
                            attr.par("seen")=1;              
                            attr.par("colo")=2;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=ftrg.inrin;              
                            shape.par("rmax")=ftrg.inrin+ftrg.rimthk;              
                            shape.par("dz")=ftrg.rimwth/2.0;              
                            /// Shape Tube rmin=ftrg.inrin rmax=ftrg.inrin+ftrg.rimthk dz=ftrg.rimwth/2.0               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FTRM;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_FTRM:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FTRM     
          // ---------------------------------------------------------------------------------------------------     
          void FTOH::Block( AgCreate create )     
          {         
                ///@addtogroup FTOH_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Air            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Air");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("FTOF");              
                            attr.par("seen")=1;              
                            attr.par("colo")=2;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=0.0;              
                            shape.par("rmax")=ftrg.ofholer;              
                            shape.par("dz")=ftrg.ofrthk/2.0;              
                            /// Shape Tube rmin=0.0 rmax=ftrg.ofholer dz=ftrg.ofrthk/2.0               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FTOH;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_FTOH:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FTOH     
          // ---------------------------------------------------------------------------------------------------     
          void FSMO::Block( AgCreate create )     
          {         
                ///@addtogroup FSMO_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Air            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Air");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("FSMO");              
                            attr.par("seen")=0;              
                            attr.par("colo")=0;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=ftrg.strutwth/2.0;              
                            shape.par("dy")=ftrg.struthgt/2.0;              
                            shape.par("dz")=ftrg.strutlen/2.0;              
                            /// Shape Bbox dx=ftrg.strutwth/2.0 dy=ftrg.struthgt/2.0 dz=ftrg.strutlen/2.0               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FSMO;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("FSTL");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FSTL              
                            Create("FSTL");               
                      }           
                      { AgPlacement place = AgPlacement("FSTL","FSMO");              
                            /// Add daughter volume FSTL to mother FSMO              
                            place.TranslateX(0.0);              
                            /// Translate x = 0.0              
                            place.TranslateY(-ftrg.struthgt/2.0+ftrg.strutthk/2.0);              
                            /// Translate y = -ftrg.struthgt/2.0+ftrg.strutthk/2.0              
                            place.TranslateZ(0.0);              
                            /// Translate z = 0.0              
                            _stacker -> Position( AgBlock::Find("FSTL"), place );              
                      } // end placement of FSTL           
                      { AgPlacement place = AgPlacement("FSTL","FSMO");              
                            /// Add daughter volume FSTL to mother FSMO              
                            place.TranslateX(0.0);              
                            /// Translate x = 0.0              
                            place.TranslateY(+ftrg.struthgt/2.0-ftrg.strutthk/2.0);              
                            /// Translate y = +ftrg.struthgt/2.0-ftrg.strutthk/2.0              
                            place.TranslateZ(0.0);              
                            /// Translate z = 0.0              
                            _stacker -> Position( AgBlock::Find("FSTL"), place );              
                      } // end placement of FSTL           
                      _create = AgCreate("FSTC");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FSTC              
                            Create("FSTC");               
                      }           
                      { AgPlacement place = AgPlacement("FSTC","FSMO");              
                            /// Add daughter volume FSTC to mother FSMO              
                            place.TranslateX(0.0);              
                            /// Translate x = 0.0              
                            place.TranslateY(0.0);              
                            /// Translate y = 0.0              
                            place.TranslateZ(0.0);              
                            /// Translate z = 0.0              
                            _stacker -> Position( AgBlock::Find("FSTC"), place );              
                      } // end placement of FSTC           
                      END_OF_FSMO:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FSMO     
          // ---------------------------------------------------------------------------------------------------     
          void FTPL::Block( AgCreate create )     
          {         
                ///@addtogroup FTPL_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Aluminium            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Aluminium");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("FTPL");              
                            attr.par("seen")=1;              
                            attr.par("colo")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=ftrg.plankwth/2.0;              
                            shape.par("dy")=ftrg.plankthk/2.0;              
                            shape.par("dz")=ftrg.strutlen/2.0;              
                            /// Shape Bbox dx=ftrg.plankwth/2.0 dy=ftrg.plankthk/2.0 dz=ftrg.strutlen/2.0               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FTPL;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_FTPL:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FTPL     
          // ---------------------------------------------------------------------------------------------------     
          void FSTL::Block( AgCreate create )     
          {         
                ///@addtogroup FSTL_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Aluminium            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Aluminium");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("FSTL");              
                            attr.par("seen")=1;              
                            attr.par("colo")=2;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=ftrg.strutwth/2.0;              
                            shape.par("dy")=ftrg.strutthk/2.0;              
                            shape.par("dz")=ftrg.strutlen/2.0;              
                            /// Shape Bbox dx=ftrg.strutwth/2.0 dy=ftrg.strutthk/2.0 dz=ftrg.strutlen/2.0               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FSTL;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_FSTL:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FSTL     
          // ---------------------------------------------------------------------------------------------------     
          void FSTC::Block( AgCreate create )     
          {         
                ///@addtogroup FSTC_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Aluminium            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Aluminium");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("FSTC");              
                            attr.par("seen")=1;              
                            attr.par("colo")=2;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=ftrg.strutthk/2.0;              
                            shape.par("dy")=ftrg.struthgt/2.0-ftrg.strutthk;              
                            shape.par("dz")=ftrg.strutlen/2.0;              
                            /// Shape Bbox dx=ftrg.strutthk/2.0 dy=ftrg.struthgt/2.0-ftrg.strutthk dz=ftrg.strutlen/2.0               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FSTC;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("FSHL");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FSHL              
                            Create("FSHL");               
                      }           
                      /// Loop on nhole from 1 to ftrg.strutnholes step=1           
                      for ( nhole=1; (1>0)? (nhole<=ftrg.strutnholes):(nhole>=ftrg.strutnholes); nhole+=1 )           
                      {              
                            { AgPlacement place = AgPlacement("FSHL","FSTC");                 
                                  /// Add daughter volume FSHL to mother FSTC                 
                                  place.TranslateX(0.0);                 
                                  /// Translate x = 0.0                 
                                  place.TranslateY(0.0);                 
                                  /// Translate y = 0.0                 
                                  place.TranslateZ(-ftrg.strutlen/2.0+nhole*strutholespacing);                 
                                  /// Translate z = -ftrg.strutlen/2.0+nhole*strutholespacing                 
                                  place.AlphaY(90);                 
                                  /// Rotate: AlphaY = 90                 
                                  /// G3 Reference: thetax = 90                 
                                  /// G3 Reference: phix = 0                 
                                  /// G3 Reference: thetay = 90                 
                                  /// G3 Reference: phiy = 90                 
                                  /// G3 Reference: thetaz = 0                 
                                  /// G3 Reference: phiz = 0                 
                                  _stacker -> Position( AgBlock::Find("FSHL"), place );                 
                            } // end placement of FSHL              
                      }           
                      END_OF_FSTC:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FSTC     
          // ---------------------------------------------------------------------------------------------------     
          void FSHL::Block( AgCreate create )     
          {         
                ///@addtogroup FSHL_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Air            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Air");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("FSHL");              
                            attr.par("seen")=1;              
                            attr.par("colo")=2;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=0.0;              
                            shape.par("rmax")=ftrg.strutholer;              
                            shape.par("dz")=ftrg.strutthk/2.0;              
                            /// Shape Tube rmin=0.0 rmax=ftrg.strutholer dz=ftrg.strutthk/2.0               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FSHL;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_FSHL:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FSHL     
          // ---------------------------------------------------------------------------------------------------     
          void FTPO::Block( AgCreate create )     
          {         
                ///@addtogroup FTPO_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material G10            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("G10");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("FTPO");              
                            attr.par("seen")=1;              
                            attr.par("colo")=3;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=ftrg.ftpowth/2.0;              
                            shape.par("dy")=ftrg.ftpothk/2.0;              
                            shape.par("dz")=ftrg.strutlen/2.0;              
                            /// Shape Bbox dx=ftrg.ftpowth/2.0 dy=ftrg.ftpothk/2.0 dz=ftrg.strutlen/2.0               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FTPO;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_FTPO:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FTPO     
          // ---------------------------------------------------------------------------------------------------     
          void FTPI::Block( AgCreate create )     
          {         
                ///@addtogroup FTPI_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material G10            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("G10");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("FTPI");              
                            attr.par("seen")=1;              
                            attr.par("colo")=3;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=ftrg.ftpiwth/2.0;              
                            shape.par("dy")=ftrg.ftpithk/2.0;              
                            shape.par("dz")=ftrg.strutlen/2.0;              
                            /// Shape Bbox dx=ftrg.ftpiwth/2.0 dy=ftrg.ftpithk/2.0 dz=ftrg.strutlen/2.0               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FTPI;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_FTPI:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FTPI     
          // ---------------------------------------------------------------------------------------------------     
          void FTSH::Block( AgCreate create )     
          {         
                ///@addtogroup FTSH_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Aluminium            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Aluminium");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("FTSH");              
                            attr.par("seen")=1;              
                            attr.par("colo")=2;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tubs");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=ftrg.ofrout-ftrg.shellthk;              
                            shape.par("rmax")=ftrg.ofrout;              
                            shape.par("phi1")=-strutangle/2.0;              
                            shape.par("phi2")=strutangle/2.0;              
                            shape.par("dz")=ftrg.strutlen/2.0;              
                            /// Shape Tubs rmin=ftrg.ofrout-ftrg.shellthk rmax=ftrg.ofrout phi1=-strutangle/2.0 phi2=strutangle/2.0 dz=ftrg.strutlen/2.0               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FTSH;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_FTSH:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FTSH     
    // ----------------------------------------------------------------------- geoctr
       void FtroGeo::ConstructGeometry( const Char_t *dummy )     
       {        
             ///@addtogroup FtroGeo_revision        
             ///@{           
                   /// Created:   07/22/04            
             ///@}        
             ///@addtogroup FtroGeo_revision        
             ///@{           
                   /// Author: Maxim Potekhin           
             ///@}        
             AddBlock("FTMO");        
             AddBlock("FTOF");        
             AddBlock("FTOH");        
             AddBlock("FSMO");        
             AddBlock("FTCM");        
             AddBlock("FTCD");        
             AddBlock("FSTL");        
             AddBlock("FSTC");        
             AddBlock("FSHL");        
             AddBlock("FTIF");        
             AddBlock("FTPO");        
             AddBlock("FTPI");        
             AddBlock("FTSH");        
             AddBlock("FTRM");        
             AddBlock("FTPL");        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup ftrg_doc        
             ///@{           
                   ++ftrg._index;           
                   ftrg . version = 1; //  version            
                   /// ftrg . version = 1; //  version            
                   ftrg . n = 5; //  number of sectors            
                   /// ftrg . n = 5; //  number of sectors            
                   ftrg . ftpcz = 150.0; //  FTPC face from the interaction point            
                   /// ftrg . ftpcz = 150.0; //  FTPC face from the interaction point            
                   ftrg . ftpclen = 119.0; //  FTPC overall length            
                   /// ftrg . ftpclen = 119.0; //  FTPC overall length            
                   ftrg . length = 80.0; //  RO barrel length            
                   /// ftrg . length = 80.0; //  RO barrel length            
                   ftrg . angoffset = 0.0; //  theta angle offset for the whle structure            
                   /// ftrg . angoffset = 0.0; //  theta angle offset for the whle structure            
                   ftrg . rin = 15.0; //  innermost radius of envelope            
                   /// ftrg . rin = 15.0; //  innermost radius of envelope            
                   ftrg . rout = 30.0; //  outermost radius of envelope            
                   /// ftrg . rout = 30.0; //  outermost radius of envelope            
                   ftrg . inrin = 22.0; //  innermost radius of the inner flange            
                   /// ftrg . inrin = 22.0; //  innermost radius of the inner flange            
                   ftrg . inrout = 30.0; //  outermost radius of the inner flange            
                   /// ftrg . inrout = 30.0; //  outermost radius of the inner flange            
                   ftrg . inrthk = 0.15; //  thickness of the inner flange            
                   /// ftrg . inrthk = 0.15; //  thickness of the inner flange            
                   ftrg . ofrin = 20.0; //  innermost radius of the outer flange            
                   /// ftrg . ofrin = 20.0; //  innermost radius of the outer flange            
                   ftrg . ofrout = 30.0; //  outermost radius of the outer flange            
                   /// ftrg . ofrout = 30.0; //  outermost radius of the outer flange            
                   ftrg . ofrthk = 0.15; //  thickness of the outer flange            
                   /// ftrg . ofrthk = 0.15; //  thickness of the outer flange            
                   ftrg . ofz = 79.0; //  Z of the outer flange inner face            
                   /// ftrg . ofz = 79.0; //  Z of the outer flange inner face            
                   ftrg . ofnholes = 15.0; //  number of holes the outer flange            
                   /// ftrg . ofnholes = 15.0; //  number of holes the outer flange            
                   ftrg . ofholerad = 26.0; //  radius at which the holes the outer flange are located            
                   /// ftrg . ofholerad = 26.0; //  radius at which the holes the outer flange are located            
                   ftrg . ofholer = 2.5; //  radius of the holes the outer flange            
                   /// ftrg . ofholer = 2.5; //  radius of the holes the outer flange            
                   ftrg . strutlen = 50.0; //  length of the strut            
                   /// ftrg . strutlen = 50.0; //  length of the strut            
                   ftrg . struthgt = 6.5; //  length of the strut            
                   /// ftrg . struthgt = 6.5; //  length of the strut            
                   ftrg . strutwth = 2.0; //  width of the strut            
                   /// ftrg . strutwth = 2.0; //  width of the strut            
                   ftrg . strutthk = 0.25; //  thickness of the strut material            
                   /// ftrg . strutthk = 0.25; //  thickness of the strut material            
                   ftrg . strutholer = 1.0; //  strut hole radius            
                   /// ftrg . strutholer = 1.0; //  strut hole radius            
                   ftrg . strutnholes = 5; //  number of holes in the strut            
                   /// ftrg . strutnholes = 5; //  number of holes in the strut            
                   ftrg . ftpowth = 13.0; //  outer half-PCB width            
                   /// ftrg . ftpowth = 13.0; //  outer half-PCB width            
                   ftrg . ftpothk = 0.3; //  outer half-PCB thickness            
                   /// ftrg . ftpothk = 0.3; //  outer half-PCB thickness            
                   ftrg . ftpor = 22.0; //  outer half-PCB radial position (to surface)            
                   /// ftrg . ftpor = 22.0; //  outer half-PCB radial position (to surface)            
                   ftrg . ftpiwth = 12.0; //  inner half-PCB width            
                   /// ftrg . ftpiwth = 12.0; //  inner half-PCB width            
                   ftrg . ftpithk = 0.3; //  inner half-PCB thickness            
                   /// ftrg . ftpithk = 0.3; //  inner half-PCB thickness            
                   ftrg . ftpir = 20.0; //  inner half-PCB radial position (to surface)            
                   /// ftrg . ftpir = 20.0; //  inner half-PCB radial position (to surface)            
                   ftrg . shellthk = 0.1; //  outer protective shell thickness            
                   /// ftrg . shellthk = 0.1; //  outer protective shell thickness            
                   ftrg . rimthk = 0.1; //  cylindrical tim thickness            
                   /// ftrg . rimthk = 0.1; //  cylindrical tim thickness            
                   ftrg . rimwth = 5.0; //  cylindrical tim width            
                   /// ftrg . rimwth = 5.0; //  cylindrical tim width            
                   ftrg . plankwth = 8.0; //  the width of the plank covering the strut            
                   /// ftrg . plankwth = 8.0; //  the width of the plank covering the strut            
                   ftrg . plankthk = 0.15; //  the thickness of the plank covering the strut            
                   /// ftrg . plankthk = 0.15; //  the thickness of the plank covering the strut            
                   //           
                   ftrg.fill();           
             ///@}        
             //        
             /// USE ftrg _index=1;        
             ftrg.Use();        
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
             myraddeg     = 3.14159265/180.0;        
             holeangle  = 360.0/ftrg.ofnholes;        
             strutangle = 360.0/ftrg.n;        
             strutrad   = ftrg.ofrin+ftrg.struthgt/2.0;        
             plankrad   = strutrad+ftrg.struthgt/2.0+ftrg.plankthk/2.0;        
             strutholespacing = ftrg.strutlen/(ftrg.strutnholes+1);        
             zpos       = ftrg.ftpcz+ftrg.ftpclen+ftrg.length/2.0;        
             centerstrut= ftrg.ofz-ftrg.length/2.0-ftrg.strutlen/2.0;        
             inflangez  = centerstrut-ftrg.strutlen/2.0;        
             ftpoanglepos=atan(ftrg.ftpowth/((ftrg.ftpor+ftrg.ftpothk/2.0)*2.0));        
             ftpianglepos=atan(ftrg.ftpiwth/((ftrg.ftpir+ftrg.ftpithk/2.0)*2.0));        
             _create = AgCreate("FTMO");        
             {           
                   AgShape myshape; // undefined shape           
                   ///Create FTMO           
                   Create("FTMO");            
             }        
             { AgPlacement place = AgPlacement("FTMO","CAVE");           
                   /// Add daughter volume FTMO to mother CAVE           
                   place.TranslateZ(zpos);           
                   /// Translate z = zpos           
                   _stacker -> Position( AgBlock::Find("FTMO"), place );           
             } // end placement of FTMO        
             { AgPlacement place = AgPlacement("FTMO","CAVE");           
                   /// Add daughter volume FTMO to mother CAVE           
                   place.TranslateZ(-zpos);           
                   /// Translate z = -zpos           
                   place.AlphaZ(180.);           
                   /// Rotate: AlphaZ = 180.           
                   /// G3 Reference: thetax = 90           
                   /// G3 Reference: phix = 0           
                   /// G3 Reference: thetay = 90           
                   /// G3 Reference: phiy = 90           
                   /// G3 Reference: thetaz = 0           
                   /// G3 Reference: phiz = 0           
                   _stacker -> Position( AgBlock::Find("FTMO"), place );           
             } // end placement of FTMO        
       }; // FtroGeo     
 }; // namespace FtroGeo  
 