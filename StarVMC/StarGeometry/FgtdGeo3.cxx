#include "FgtdGeo3.h"  
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
 namespace FGTDGEO3 // $NMSPC  
 {     
       //     
       // ---------------------------------------------------------------------------------------------------     
          ///@addtogroup FgtdGeo3_vars     
          ///@{        
                float cooltubeinr=10.25;        
                //        
                /// float cooltubeinr=10.25        
          ///@}     
          ///@addtogroup FgtdGeo3_vars     
          ///@{        
                float cooltubedz=150;        
                //        
                /// float cooltubedz=150        
          ///@}     
          ///@addtogroup FgtdGeo3_vars     
          ///@{        
                float diskinr=10.35;        
                //        
                /// float diskinr=10.35        
          ///@}     
          ///@addtogroup FgtdGeo3_vars     
          ///@{        
                float diskoutr=39.4;        
                //        
                /// float diskoutr=39.4        
          ///@}     
          ///@addtogroup FgtdGeo3_vars     
          ///@{        
                float flatoutr=37.0;        
                //        
                /// float flatoutr=37.0        
          ///@}     
          ///@addtogroup FgtdGeo3_vars     
          ///@{        
                float flatperpang=31.0;        
                //        
                /// float flatperpang=31.0        
          ///@}     
          ///@addtogroup FgtdGeo3_vars     
          ///@{        
                float quadtiltang=-15.0;        
                //        
                /// float quadtiltang=-15.0        
          ///@}     
          ///@addtogroup FgtdGeo3_vars     
          ///@{        
                float gridarcbigr=29.15;        
                //        
                /// float gridarcbigr=29.15        
          ///@}     
          ///@addtogroup FgtdGeo3_vars     
          ///@{        
                float gridarcsmallr=20.63;        
                //        
                /// float gridarcsmallr=20.63        
          ///@}     
          ///@addtogroup FgtdGeo3_vars     
          ///@{        
                float gridraylongphi=60.;        
                //        
                /// float gridraylongphi=60.        
          ///@}     
          ///@addtogroup FgtdGeo3_vars     
          ///@{        
                float gridrayshortphi=30.;        
                //        
                /// float gridrayshortphi=30.        
          ///@}     
          ///@addtogroup FgtdGeo3_vars     
          ///@{        
                float gridwidth=0.2;        
                //        
                /// float gridwidth=0.2        
          ///@}     
          ///@addtogroup FgtdGeo3_vars     
          ///@{        
                float framewidth=1.15;        
                //        
                /// float framewidth=1.15        
          ///@}     
          ///@addtogroup FgtdGeo3_vars     
          ///@{        
                float framexyoff=0.05;        
                //        
                /// float framexyoff=0.05        
          ///@}     
          ///@addtogroup FgtdGeo3_vars     
          ///@{        
                float framedz=0.198;        
                //        
                /// float framedz=0.198        
          ///@}     
          ///@addtogroup FgtdGeo3_vars     
          ///@{        
                float volnomexdz=1.288;        
                //        
                /// float volnomexdz=1.288        
          ///@}     
          ///@addtogroup FgtdGeo3_vars     
          ///@{        
                float volsensdz=0.312;        
                //        
                /// float volsensdz=0.312        
          ///@}     
          ///@addtogroup FgtdGeo3_vars     
          ///@{        
                float volgemdz=0.612;        
                //        
                /// float volgemdz=0.612        
          ///@}     
          ///@addtogroup FgtdGeo3_vars     
          ///@{        
                float volreaddz=0.246;        
                //        
                /// float volreaddz=0.246        
          ///@}     
          ///@addtogroup FgtdGeo3_vars     
          ///@{        
                float volelecdz=4.3;        
                //        
                /// float volelecdz=4.3        
          ///@}     
          ///@addtogroup FgtdGeo3_vars     
          ///@{        
                float elecwidth=0.16;        
                //        
                /// float elecwidth=0.16        
          ///@}     
          ///@addtogroup FgtdGeo3_vars     
          ///@{        
                float elecady=1.4;        
                //        
                /// float elecady=1.4        
          ///@}     
          ///@addtogroup FgtdGeo3_vars     
          ///@{        
                float eleccdy=4.5;        
                //        
                /// float eleccdy=4.5        
          ///@}     
          ///@addtogroup FgtdGeo3_vars     
          ///@{        
                float elecdang=9.;        
                //        
                /// float elecdang=9.        
          ///@}     
          ///@addtogroup FgtdGeo3_vars     
          ///@{        
                float elecelen=22.5;        
                //        
                /// float elecelen=22.5        
          ///@}     
          ///@addtogroup FgtdGeo3_vars     
          ///@{        
                Float_t centerz,fgtlenz,disklenz,flatang1,flatang2,quad,disk,xx,diskz,k,zzfgz,xxfgx,xxfgq,xxfgz;        
                //        
                /// Float_t centerz,fgtlenz,disklenz,flatang1,flatang2,quad,disk,xx,diskz,k,zzfgz,xxfgx,xxfgq,xxfgz        
          ///@}     
          //  -----------------------------------------------------     
          /// @defgroup fggg_doc     
          /// \class Fggg_t     
          /// \brief User-defined structure     
          ///                             
          /// AgML structure members:     
          ///                             
          ///Float_t fgstconfig;     
          ///Int_t _index;     
          //     
          Fggg_t fggg;     
          //     
          //  -----------------------------------------------------     
          /// @defgroup fgst_doc     
          /// \class Fgst_t     
          /// \brief User-defined structure     
          ///                             
          /// AgML structure members:     
          ///                             
          ///Float_t config;     
          ///Int_t ndisk;     
          ///Int_t nquad;     
          ///Array_t<Float_t> zdisca;     
          ///Int_t _index;     
          //     
          Fgst_t fgst;     
          //     
       FgtdGeo3::FgtdGeo3()     
         : AgModule("FgtdGeo3"," forward GEM tracking detector for 2012 , 2013")     
       {        
       }     
          // ---------------------------------------------------------------------------------------------------     
          void FGTM::Block( AgCreate create )     
          {         
                ///@addtogroup FGTM_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("FGTM");              
                            attr.par("seen")=0;              
                            attr.par("colo")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material Air            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Air");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=cooltubeinr;              
                            shape.par("rmax")=diskoutr;              
                            shape.par("dz")=fgtlenz/2.0;              
                            /// Shape Tube rmin=cooltubeinr rmax=diskoutr dz=fgtlenz/2.0               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FGTM;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("FGCT");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FGCT              
                            Create("FGCT");               
                      }           
                      { AgPlacement place = AgPlacement("FGCT","FGTM");              
                            /// Add daughter volume FGCT to mother FGTM              
                            _stacker -> Position( AgBlock::Find("FGCT"), place );              
                      } // end placement of FGCT           
                      _create = AgCreate("FGCN");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FGCN              
                            Create("FGCN");               
                      }           
                      { AgPlacement place = AgPlacement("FGCN","FGTM");              
                            /// Add daughter volume FGCN to mother FGTM              
                            place.TranslateZ(-fgtlenz/2.0+0.64/2.);              
                            /// Translate z = -fgtlenz/2.0+0.64/2.              
                            _stacker -> Position( AgBlock::Find("FGCN"), place );              
                      } // end placement of FGCN           
                      /// Loop on disk from 1 to fgst.ndisk step=1           
                      for ( disk=1; (1>0)? (disk<=fgst.ndisk):(disk>=fgst.ndisk); disk+=1 )           
                      {              
                            diskz=-fgtlenz/2.+1.0 +disklenz/2. + fgst.zdisca(disk)- fgst.zdisca(1);              
                            if ( (fgst.config==2.0)||(disk<=1) )              
                            {                 
                                  _create = AgCreate("FGTD");                 
                                  {                    
                                        AgShape myshape; // undefined shape                    
                                        ///Create FGTD                    
                                        Create("FGTD");                     
                                  }                 
                                  { AgPlacement place = AgPlacement("FGTD","FGTM");                    
                                        /// Add daughter volume FGTD to mother FGTM                    
                                        place.TranslateZ(diskz);                    
                                        /// Translate z = diskz                    
                                        place.par("ncopy")=disk;                    
                                        /// Ncopy: disk                    
                                        place.AlphaZ(quadtiltang);                    
                                        /// Rotate: AlphaZ = quadtiltang                    
                                        /// G3 Reference: thetax = 90                    
                                        /// G3 Reference: phix = 0                    
                                        /// G3 Reference: thetay = 90                    
                                        /// G3 Reference: phiy = 90                    
                                        /// G3 Reference: thetaz = 0                    
                                        /// G3 Reference: phiz = 0                    
                                        _stacker -> Position( AgBlock::Find("FGTD"), place );                    
                                  } // end placement of FGTD                 
                            }              
                            else              
                            {                 
                                  _create = AgCreate("FGTH");                 
                                  {                    
                                        AgShape myshape; // undefined shape                    
                                        ///Create FGTH                    
                                        Create("FGTH");                     
                                  }                 
                                  { AgPlacement place = AgPlacement("FGTH","FGTM");                    
                                        /// Add daughter volume FGTH to mother FGTM                    
                                        place.TranslateZ(diskz);                    
                                        /// Translate z = diskz                    
                                        place.par("ncopy")=disk;                    
                                        /// Ncopy: disk                    
                                        place.AlphaZ(quadtiltang);                    
                                        /// Rotate: AlphaZ = quadtiltang                    
                                        /// G3 Reference: thetax = 90                    
                                        /// G3 Reference: phix = 0                    
                                        /// G3 Reference: thetay = 90                    
                                        /// G3 Reference: phiy = 90                    
                                        /// G3 Reference: thetaz = 0                    
                                        /// G3 Reference: phiz = 0                    
                                        _stacker -> Position( AgBlock::Find("FGTH"), place );                    
                                  } // end placement of FGTH                 
                            }              
                      }           
                      /// Loop on disk from 2 to fgst.ndisk step=1           
                      for ( disk=2; (1>0)? (disk<=fgst.ndisk):(disk>=fgst.ndisk); disk+=1 )           
                      {              
                            diskz=-fgtlenz/2. + 1.0 + fgst.zdisca(disk)- fgst.zdisca(1) -1.28/2.;              
                            _create = AgCreate("FGCM");              
                            {                 
                                  AgShape myshape; // undefined shape                 
                                  ///Create FGCM                 
                                  Create("FGCM");                  
                            }              
                            { AgPlacement place = AgPlacement("FGCM","FGTM");                 
                                  /// Add daughter volume FGCM to mother FGTM                 
                                  place.TranslateZ(diskz);                 
                                  /// Translate z = diskz                 
                                  _stacker -> Position( AgBlock::Find("FGCM"), place );                 
                            } // end placement of FGCM              
                      }           
                      END_OF_FGTM:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FGTM     
          // ---------------------------------------------------------------------------------------------------     
          void FGCT::Block( AgCreate create )     
          {         
                ///@addtogroup FGCT_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("FGCT");              
                            attr.par("seen")=1;              
                            attr.par("colo")=6;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material CFRPMix            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Cfrpmix");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=cooltubeinr;              
                            shape.par("rmax")=cooltubeinr+0.05;              
                            shape.par("dz")=cooltubedz/2.;              
                            /// Shape Tube rmin=cooltubeinr rmax=cooltubeinr+0.05 dz=cooltubedz/2.               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FGCT;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_FGCT:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FGCT     
          // ---------------------------------------------------------------------------------------------------     
          void FGCN::Block( AgCreate create )     
          {         
                ///@addtogroup FGCN_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("FGCN");              
                            attr.par("seen")=1;              
                            attr.par("colo")=6;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material Nylon            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Nylon");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=10.35;              
                            shape.par("rmax")=11.5;              
                            shape.par("dz")=0.64/2.;              
                            /// Shape Tube rmin=10.35 rmax=11.5 dz=0.64/2.               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FGCN;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_FGCN:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FGCN     
          // ---------------------------------------------------------------------------------------------------     
          void FGCM::Block( AgCreate create )     
          {         
                ///@addtogroup FGCM_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("FGCM");              
                            attr.par("seen")=1;              
                            attr.par("colo")=6;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material CoolMix            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Coolmix");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=10.35;              
                            shape.par("rmax")=11.54;              
                            shape.par("dz")=1.28/2.;              
                            /// Shape Tube rmin=10.35 rmax=11.54 dz=1.28/2.               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FGCM;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_FGCM:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FGCM     
          // ---------------------------------------------------------------------------------------------------     
          void FGTD::Block( AgCreate create )     
          {         
                ///@addtogroup FGTD_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("FGTD");              
                            attr.par("seen")=1;              
                            attr.par("colo")=5;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material Air            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Air");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=diskinr;              
                            shape.par("rmax")=diskoutr;              
                            shape.par("dz")=disklenz/2.0;              
                            /// Shape Tube rmin=diskinr rmax=diskoutr dz=disklenz/2.0               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FGTD;              
                            _stacker -> Build(this);              
                      }           
                      /// Loop on quad from 1 to fgst.nquad step=1           
                      for ( quad=1; (1>0)? (quad<=fgst.nquad):(quad>=fgst.nquad); quad+=1 )           
                      {              
                            _create = AgCreate("FGTQ");              
                            {                 
                                  AgShape myshape; // undefined shape                 
                                  ///Create FGTQ                 
                                  Create("FGTQ");                  
                            }              
                            { AgPlacement place = AgPlacement("FGTQ","FGTD");                 
                                  /// Add daughter volume FGTQ to mother FGTD                 
                                  place.TranslateZ(0.);                 
                                  /// Translate z = 0.                 
                                  place.AlphaZ((1-quad)*90.);                 
                                  /// Rotate: AlphaZ = (1-quad)*90.                 
                                  /// G3 Reference: thetax = 90                 
                                  /// G3 Reference: phix = 0                 
                                  /// G3 Reference: thetay = 90                 
                                  /// G3 Reference: phiy = 90                 
                                  /// G3 Reference: thetaz = 0                 
                                  /// G3 Reference: phiz = 0                 
                                  _stacker -> Position( AgBlock::Find("FGTQ"), place );                 
                            } // end placement of FGTQ              
                      }           
                      END_OF_FGTD:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FGTD     
          // ---------------------------------------------------------------------------------------------------     
          void FGTH::Block( AgCreate create )     
          {         
                ///@addtogroup FGTH_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("FGTH");              
                            attr.par("seen")=1;              
                            attr.par("colo")=7;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material Air            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Air");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=diskinr;              
                            shape.par("rmax")=diskoutr;              
                            shape.par("dz")=disklenz/2.0;              
                            /// Shape Tube rmin=diskinr rmax=diskoutr dz=disklenz/2.0               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FGTH;              
                            _stacker -> Build(this);              
                      }           
                      /// Loop on quad from 1 to 2 step=1           
                      for ( quad=1; (1>0)? (quad<=2):(quad>=2); quad+=1 )           
                      {              
                            _create = AgCreate("FGTQ");              
                            {                 
                                  AgShape myshape; // undefined shape                 
                                  ///Create FGTQ                 
                                  Create("FGTQ");                  
                            }              
                            { AgPlacement place = AgPlacement("FGTQ","FGTH");                 
                                  /// Add daughter volume FGTQ to mother FGTH                 
                                  place.TranslateZ(0.);                 
                                  /// Translate z = 0.                 
                                  place.AlphaZ((1-quad)*90.);                 
                                  /// Rotate: AlphaZ = (1-quad)*90.                 
                                  /// G3 Reference: thetax = 90                 
                                  /// G3 Reference: phix = 0                 
                                  /// G3 Reference: thetay = 90                 
                                  /// G3 Reference: phiy = 90                 
                                  /// G3 Reference: thetaz = 0                 
                                  /// G3 Reference: phiz = 0                 
                                  _stacker -> Position( AgBlock::Find("FGTQ"), place );                 
                            } // end placement of FGTQ              
                      }           
                      END_OF_FGTH:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FGTH     
          // ---------------------------------------------------------------------------------------------------     
          void FGTQ::Block( AgCreate create )     
          {         
                ///@addtogroup FGTQ_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("FGTQ");              
                            attr.par("seen")=0;              
                            attr.par("colo")=4;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material Air            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Air");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Tubs");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=diskinr;              
                            shape.par("rmax")=diskoutr;              
                            shape.par("phi1")=0.;              
                            shape.par("phi2")=90.;              
                            shape.par("dz")=disklenz/2.;              
                            /// Shape Tubs rmin=diskinr rmax=diskoutr phi1=0. phi2=90. dz=disklenz/2.               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FGTQ;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("FGVN");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FGVN              
                            Create("FGVN");               
                      }           
                      { AgPlacement place = AgPlacement("FGVN","FGTQ");              
                            /// Add daughter volume FGVN to mother FGTQ              
                            place.TranslateZ(-disklenz/2.+volnomexdz/2.);              
                            /// Translate z = -disklenz/2.+volnomexdz/2.              
                            _stacker -> Position( AgBlock::Find("FGVN"), place );              
                      } // end placement of FGVN           
                      _create = AgCreate("FGZC");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FGZC              
                            Create("FGZC");               
                      }           
                      { AgPlacement place = AgPlacement("FGZC","FGTQ");              
                            /// Add daughter volume FGZC to mother FGTQ              
                            place.TranslateZ(-disklenz/2.+volnomexdz+volsensdz/2.);              
                            /// Translate z = -disklenz/2.+volnomexdz+volsensdz/2.              
                            _stacker -> Position( AgBlock::Find("FGZC"), place );              
                      } // end placement of FGZC           
                      _create = AgCreate("FGVG");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FGVG              
                            Create("FGVG");               
                      }           
                      { AgPlacement place = AgPlacement("FGVG","FGTQ");              
                            /// Add daughter volume FGVG to mother FGTQ              
                            place.TranslateZ(-disklenz/2.+volnomexdz+volsensdz+volgemdz/2.);              
                            /// Translate z = -disklenz/2.+volnomexdz+volsensdz+volgemdz/2.              
                            _stacker -> Position( AgBlock::Find("FGVG"), place );              
                      } // end placement of FGVG           
                      _create = AgCreate("FGVR");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FGVR              
                            Create("FGVR");               
                      }           
                      { AgPlacement place = AgPlacement("FGVR","FGTQ");              
                            /// Add daughter volume FGVR to mother FGTQ              
                            place.TranslateZ(-disklenz/2.+volnomexdz+volsensdz+volgemdz+volreaddz/2.);              
                            /// Translate z = -disklenz/2.+volnomexdz+volsensdz+volgemdz+volreaddz/2.              
                            _stacker -> Position( AgBlock::Find("FGVR"), place );              
                      } // end placement of FGVR           
                      _create = AgCreate("FGVE");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FGVE              
                            Create("FGVE");               
                      }           
                      { AgPlacement place = AgPlacement("FGVE","FGTQ");              
                            /// Add daughter volume FGVE to mother FGTQ              
                            place.TranslateZ(-disklenz/2.+volnomexdz+volsensdz+volgemdz+volreaddz+volelecdz/2.);              
                            /// Translate z = -disklenz/2.+volnomexdz+volsensdz+volgemdz+volreaddz+volelecdz/2.              
                            _stacker -> Position( AgBlock::Find("FGVE"), place );              
                      } // end placement of FGVE           
                      END_OF_FGTQ:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FGTQ     
          // ---------------------------------------------------------------------------------------------------     
          void FGVN::Block( AgCreate create )     
          {         
                ///@addtogroup FGVN_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("FGVN");              
                            attr.par("seen")=0;              
                            attr.par("colo")=5;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material NomexMix            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Nomexmix");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Tubs");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=diskinr;              
                            shape.par("rmax")=diskoutr;              
                            shape.par("phi1")=0.;              
                            shape.par("phi2")=90.;              
                            shape.par("dz")=volnomexdz/2.;              
                            /// Shape Tubs rmin=diskinr rmax=diskoutr phi1=0. phi2=90. dz=volnomexdz/2.               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FGVN;              
                            _stacker -> Build(this);              
                      }           
                      /// Loop on k from 0 to 2 step=1           
                      for ( k=0; (1>0)? (k<=2):(k>=2); k+=1 )           
                      {              
                            zzfgz=(-volnomexdz+framedz)/2.;              
                            if ( k==1 )              
                            {                 
                                  zzfgz=(volnomexdz-framedz)/2.-0.007;                 
                            }              
                            if ( k==2 )              
                            {                 
                                  zzfgz=(volnomexdz-framedz)/2.-0.014 -  framedz;                 
                            }              
                            _create = AgCreate("FGQA");              
                            {                 
                                  AgShape myshape; // undefined shape                 
                                  ///Create FGQA                 
                                  Create("FGQA");                  
                            }              
                            { AgPlacement place = AgPlacement("FGQA","FGVN");                 
                                  /// Add daughter volume FGQA to mother FGVN                 
                                  place.TranslateX(framewidth/2.+framexyoff);                 
                                  /// Translate x = framewidth/2.+framexyoff                 
                                  place.TranslateY((diskoutr+diskinr)/2.);                 
                                  /// Translate y = (diskoutr+diskinr)/2.                 
                                  place.TranslateZ(zzfgz);                 
                                  /// Translate z = zzfgz                 
                                  _stacker -> Position( AgBlock::Find("FGQA"), place );                 
                            } // end placement of FGQA              
                            _create = AgCreate("FGQB");              
                            {                 
                                  AgShape myshape; // undefined shape                 
                                  ///Create FGQB                 
                                  Create("FGQB");                  
                            }              
                            { AgPlacement place = AgPlacement("FGQB","FGVN");                 
                                  /// Add daughter volume FGQB to mother FGVN                 
                                  place.TranslateZ(zzfgz);                 
                                  /// Translate z = zzfgz                 
                                  _stacker -> Position( AgBlock::Find("FGQB"), place );                 
                            } // end placement of FGQB              
                            _create = AgCreate("FGQC");              
                            {                 
                                  AgShape myshape; // undefined shape                 
                                  ///Create FGQC                 
                                  Create("FGQC");                  
                            }              
                            { AgPlacement place = AgPlacement("FGQC","FGVN");                 
                                  /// Add daughter volume FGQC to mother FGVN                 
                                  place.TranslateX((diskoutr+diskinr)/2.);                 
                                  /// Translate x = (diskoutr+diskinr)/2.                 
                                  place.TranslateY(framewidth/2.+framexyoff);                 
                                  /// Translate y = framewidth/2.+framexyoff                 
                                  place.TranslateZ(zzfgz);                 
                                  /// Translate z = zzfgz                 
                                  _stacker -> Position( AgBlock::Find("FGQC"), place );                 
                            } // end placement of FGQC              
                            _create = AgCreate("FGQD");              
                            {                 
                                  AgShape myshape; // undefined shape                 
                                  ///Create FGQD                 
                                  Create("FGQD");                  
                            }              
                            { AgPlacement place = AgPlacement("FGQD","FGVN");                 
                                  /// Add daughter volume FGQD to mother FGVN                 
                                  place.TranslateZ(zzfgz);                 
                                  /// Translate z = zzfgz                 
                                  _stacker -> Position( AgBlock::Find("FGQD"), place );                 
                            } // end placement of FGQD              
                            _create = AgCreate("FGQE");              
                            {                 
                                  AgShape myshape; // undefined shape                 
                                  ///Create FGQE                 
                                  Create("FGQE");                  
                            }              
                            { AgPlacement place = AgPlacement("FGQE","FGVN");                 
                                  /// Add daughter volume FGQE to mother FGVN                 
                                  place.TranslateX((flatoutr-framewidth/2.)*cos(flatperpang*degrad));                 
                                  /// Translate x = (flatoutr-framewidth/2.)*cos(flatperpang*degrad)                 
                                  place.TranslateY((flatoutr-framewidth/2.)*sin(flatperpang*degrad));                 
                                  /// Translate y = (flatoutr-framewidth/2.)*sin(flatperpang*degrad)                 
                                  place.TranslateZ(zzfgz);                 
                                  /// Translate z = zzfgz                 
                                  place.AlphaZ(flatperpang-90.);                 
                                  /// Rotate: AlphaZ = flatperpang-90.                 
                                  /// G3 Reference: thetax = 90                 
                                  /// G3 Reference: phix = 0                 
                                  /// G3 Reference: thetay = 90                 
                                  /// G3 Reference: phiy = 90                 
                                  /// G3 Reference: thetaz = 0                 
                                  /// G3 Reference: phiz = 0                 
                                  _stacker -> Position( AgBlock::Find("FGQE"), place );                 
                            } // end placement of FGQE              
                            _create = AgCreate("FGQF");              
                            {                 
                                  AgShape myshape; // undefined shape                 
                                  ///Create FGQF                 
                                  Create("FGQF");                  
                            }              
                            { AgPlacement place = AgPlacement("FGQF","FGVN");                 
                                  /// Add daughter volume FGQF to mother FGVN                 
                                  place.TranslateZ(zzfgz);                 
                                  /// Translate z = zzfgz                 
                                  _stacker -> Position( AgBlock::Find("FGQF"), place );                 
                            } // end placement of FGQF              
                      }           
                      END_OF_FGVN:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FGVN     
          // ---------------------------------------------------------------------------------------------------     
          void FGZC::Block( AgCreate create )     
          {         
                ///@addtogroup FGZC_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("FGZC");              
                            attr.par("seen")=1;              
                            attr.par("colo")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material ArCO2Mix            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Arco2mix");              
                            _material = mat;              
                      }           
                      /// Material Sensitive_fgt_gas isvol=1            
                      { AgMaterial &mat = AgMaterial::Get("Sensitive_fgt_gas");              
                            mat.par("isvol")=1;              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Tubs");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=diskinr;              
                            shape.par("rmax")=diskoutr;              
                            shape.par("phi1")=0.;              
                            shape.par("phi2")=90.;              
                            shape.par("dz")=volsensdz/2.;              
                            /// Shape Tubs rmin=diskinr rmax=diskoutr phi1=0. phi2=90. dz=volsensdz/2.               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FGZC;              
                            _stacker -> Build(this);              
                      }           
                      /// Loop on k from 0 to 1 step=1           
                      for ( k=0; (1>0)? (k<=1):(k>=1); k+=1 )           
                      {              
                            zzfgz=(-volsensdz+framedz)/2. * (1.-2.*k);              
                            _create = AgCreate("FGQA");              
                            {                 
                                  AgShape myshape; // undefined shape                 
                                  ///Create FGQA                 
                                  Create("FGQA");                  
                            }              
                            { AgPlacement place = AgPlacement("FGQA","FGZC");                 
                                  /// Add daughter volume FGQA to mother FGZC                 
                                  place.TranslateX(framewidth/2.+framexyoff);                 
                                  /// Translate x = framewidth/2.+framexyoff                 
                                  place.TranslateY((diskoutr+diskinr)/2.);                 
                                  /// Translate y = (diskoutr+diskinr)/2.                 
                                  place.TranslateZ(zzfgz);                 
                                  /// Translate z = zzfgz                 
                                  _stacker -> Position( AgBlock::Find("FGQA"), place );                 
                            } // end placement of FGQA              
                            _create = AgCreate("FGQB");              
                            {                 
                                  AgShape myshape; // undefined shape                 
                                  ///Create FGQB                 
                                  Create("FGQB");                  
                            }              
                            { AgPlacement place = AgPlacement("FGQB","FGZC");                 
                                  /// Add daughter volume FGQB to mother FGZC                 
                                  place.TranslateZ(zzfgz);                 
                                  /// Translate z = zzfgz                 
                                  _stacker -> Position( AgBlock::Find("FGQB"), place );                 
                            } // end placement of FGQB              
                            _create = AgCreate("FGQC");              
                            {                 
                                  AgShape myshape; // undefined shape                 
                                  ///Create FGQC                 
                                  Create("FGQC");                  
                            }              
                            { AgPlacement place = AgPlacement("FGQC","FGZC");                 
                                  /// Add daughter volume FGQC to mother FGZC                 
                                  place.TranslateX((diskoutr+diskinr)/2.);                 
                                  /// Translate x = (diskoutr+diskinr)/2.                 
                                  place.TranslateY(framewidth/2.+framexyoff);                 
                                  /// Translate y = framewidth/2.+framexyoff                 
                                  place.TranslateZ(zzfgz);                 
                                  /// Translate z = zzfgz                 
                                  _stacker -> Position( AgBlock::Find("FGQC"), place );                 
                            } // end placement of FGQC              
                            _create = AgCreate("FGQD");              
                            {                 
                                  AgShape myshape; // undefined shape                 
                                  ///Create FGQD                 
                                  Create("FGQD");                  
                            }              
                            { AgPlacement place = AgPlacement("FGQD","FGZC");                 
                                  /// Add daughter volume FGQD to mother FGZC                 
                                  place.TranslateZ(zzfgz);                 
                                  /// Translate z = zzfgz                 
                                  _stacker -> Position( AgBlock::Find("FGQD"), place );                 
                            } // end placement of FGQD              
                            _create = AgCreate("FGQE");              
                            {                 
                                  AgShape myshape; // undefined shape                 
                                  ///Create FGQE                 
                                  Create("FGQE");                  
                            }              
                            { AgPlacement place = AgPlacement("FGQE","FGZC");                 
                                  /// Add daughter volume FGQE to mother FGZC                 
                                  place.TranslateX((flatoutr-framewidth/2.)*cos(flatperpang*degrad));                 
                                  /// Translate x = (flatoutr-framewidth/2.)*cos(flatperpang*degrad)                 
                                  place.TranslateY((flatoutr-framewidth/2.)*sin(flatperpang*degrad));                 
                                  /// Translate y = (flatoutr-framewidth/2.)*sin(flatperpang*degrad)                 
                                  place.TranslateZ(zzfgz);                 
                                  /// Translate z = zzfgz                 
                                  place.AlphaZ(flatperpang-90.);                 
                                  /// Rotate: AlphaZ = flatperpang-90.                 
                                  /// G3 Reference: thetax = 90                 
                                  /// G3 Reference: phix = 0                 
                                  /// G3 Reference: thetay = 90                 
                                  /// G3 Reference: phiy = 90                 
                                  /// G3 Reference: thetaz = 0                 
                                  /// G3 Reference: phiz = 0                 
                                  _stacker -> Position( AgBlock::Find("FGQE"), place );                 
                            } // end placement of FGQE              
                            _create = AgCreate("FGQF");              
                            {                 
                                  AgShape myshape; // undefined shape                 
                                  ///Create FGQF                 
                                  Create("FGQF");                  
                            }              
                            { AgPlacement place = AgPlacement("FGQF","FGZC");                 
                                  /// Add daughter volume FGQF to mother FGZC                 
                                  place.TranslateZ(zzfgz);                 
                                  /// Translate z = zzfgz                 
                                  _stacker -> Position( AgBlock::Find("FGQF"), place );                 
                            } // end placement of FGQF              
                            _create = AgCreate("FGXA");              
                            {                 
                                  AgShape myshape; // undefined shape                 
                                  ///Create FGXA                 
                                  Create("FGXA");                  
                            }              
                            { AgPlacement place = AgPlacement("FGXA","FGZC");                 
                                  /// Add daughter volume FGXA to mother FGZC                 
                                  place.TranslateZ(zzfgz);                 
                                  /// Translate z = zzfgz                 
                                  _stacker -> Position( AgBlock::Find("FGXA"), place );                 
                            } // end placement of FGXA              
                            _create = AgCreate("FGXB");              
                            {                 
                                  AgShape myshape; // undefined shape                 
                                  ///Create FGXB                 
                                  Create("FGXB");                  
                            }              
                            { AgPlacement place = AgPlacement("FGXB","FGZC");                 
                                  /// Add daughter volume FGXB to mother FGZC                 
                                  place.TranslateZ(zzfgz);                 
                                  /// Translate z = zzfgz                 
                                  _stacker -> Position( AgBlock::Find("FGXB"), place );                 
                            } // end placement of FGXB              
                            _create = AgCreate("FGXC");              
                            {                 
                                  AgShape myshape; // undefined shape                 
                                  ///Create FGXC                 
                                  Create("FGXC");                  
                            }              
                            xxfgz=(diskoutr+diskinr)/2.;              
                            { AgPlacement place = AgPlacement("FGXC","FGZC");                 
                                  /// Add daughter volume FGXC to mother FGZC                 
                                  place.TranslateX(xxfgz*cos(gridraylongphi*degrad));                 
                                  /// Translate x = xxfgz*cos(gridraylongphi*degrad)                 
                                  place.TranslateY(xxfgz*sin(gridraylongphi*degrad));                 
                                  /// Translate y = xxfgz*sin(gridraylongphi*degrad)                 
                                  place.TranslateZ(zzfgz);                 
                                  /// Translate z = zzfgz                 
                                  place.AlphaZ(gridraylongphi);                 
                                  /// Rotate: AlphaZ = gridraylongphi                 
                                  /// G3 Reference: thetax = 90                 
                                  /// G3 Reference: phix = 0                 
                                  /// G3 Reference: thetay = 90                 
                                  /// G3 Reference: phiy = 90                 
                                  /// G3 Reference: thetaz = 0                 
                                  /// G3 Reference: phiz = 0                 
                                  _stacker -> Position( AgBlock::Find("FGXC"), place );                 
                            } // end placement of FGXC              
                            _create = AgCreate("FGXD");              
                            {                 
                                  AgShape myshape; // undefined shape                 
                                  ///Create FGXD                 
                                  Create("FGXD");                  
                            }              
                            xxfgz=(flatoutr+diskinr)/2.;              
                            { AgPlacement place = AgPlacement("FGXD","FGZC");                 
                                  /// Add daughter volume FGXD to mother FGZC                 
                                  place.TranslateX(xxfgz*cos(gridrayshortphi*degrad));                 
                                  /// Translate x = xxfgz*cos(gridrayshortphi*degrad)                 
                                  place.TranslateY(xxfgz*sin(gridrayshortphi*degrad));                 
                                  /// Translate y = xxfgz*sin(gridrayshortphi*degrad)                 
                                  place.TranslateZ(zzfgz);                 
                                  /// Translate z = zzfgz                 
                                  place.AlphaZ(gridrayshortphi);                 
                                  /// Rotate: AlphaZ = gridrayshortphi                 
                                  /// G3 Reference: thetax = 90                 
                                  /// G3 Reference: phix = 0                 
                                  /// G3 Reference: thetay = 90                 
                                  /// G3 Reference: phiy = 90                 
                                  /// G3 Reference: thetaz = 0                 
                                  /// G3 Reference: phiz = 0                 
                                  _stacker -> Position( AgBlock::Find("FGXD"), place );                 
                            } // end placement of FGXD              
                      }           
                      END_OF_FGZC:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FGZC     
          // ---------------------------------------------------------------------------------------------------     
          void FGVG::Block( AgCreate create )     
          {         
                ///@addtogroup FGVG_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("FGVG");              
                            attr.par("seen")=0;              
                            attr.par("colo")=6;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material GemMix            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Gemmix");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Tubs");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=diskinr;              
                            shape.par("rmax")=diskoutr;              
                            shape.par("phi1")=0.;              
                            shape.par("phi2")=90.;              
                            shape.par("dz")=volgemdz/2.;              
                            /// Shape Tubs rmin=diskinr rmax=diskoutr phi1=0. phi2=90. dz=volgemdz/2.               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FGVG;              
                            _stacker -> Build(this);              
                      }           
                      /// Loop on k from 0 to 2 step=1           
                      for ( k=0; (1>0)? (k<=2):(k>=2); k+=1 )           
                      {              
                            zzfgz=(-volgemdz+framedz-0.01)/2. * (-1+k);              
                            _create = AgCreate("FGQA");              
                            {                 
                                  AgShape myshape; // undefined shape                 
                                  ///Create FGQA                 
                                  Create("FGQA");                  
                            }              
                            { AgPlacement place = AgPlacement("FGQA","FGVG");                 
                                  /// Add daughter volume FGQA to mother FGVG                 
                                  place.TranslateX(framewidth/2.+framexyoff);                 
                                  /// Translate x = framewidth/2.+framexyoff                 
                                  place.TranslateY((diskoutr+diskinr)/2.);                 
                                  /// Translate y = (diskoutr+diskinr)/2.                 
                                  place.TranslateZ(zzfgz);                 
                                  /// Translate z = zzfgz                 
                                  _stacker -> Position( AgBlock::Find("FGQA"), place );                 
                            } // end placement of FGQA              
                            _create = AgCreate("FGQB");              
                            {                 
                                  AgShape myshape; // undefined shape                 
                                  ///Create FGQB                 
                                  Create("FGQB");                  
                            }              
                            { AgPlacement place = AgPlacement("FGQB","FGVG");                 
                                  /// Add daughter volume FGQB to mother FGVG                 
                                  place.TranslateZ(zzfgz);                 
                                  /// Translate z = zzfgz                 
                                  _stacker -> Position( AgBlock::Find("FGQB"), place );                 
                            } // end placement of FGQB              
                            _create = AgCreate("FGQC");              
                            {                 
                                  AgShape myshape; // undefined shape                 
                                  ///Create FGQC                 
                                  Create("FGQC");                  
                            }              
                            { AgPlacement place = AgPlacement("FGQC","FGVG");                 
                                  /// Add daughter volume FGQC to mother FGVG                 
                                  place.TranslateX((diskoutr+diskinr)/2.);                 
                                  /// Translate x = (diskoutr+diskinr)/2.                 
                                  place.TranslateY(framewidth/2.+framexyoff);                 
                                  /// Translate y = framewidth/2.+framexyoff                 
                                  place.TranslateZ(zzfgz);                 
                                  /// Translate z = zzfgz                 
                                  _stacker -> Position( AgBlock::Find("FGQC"), place );                 
                            } // end placement of FGQC              
                            _create = AgCreate("FGQD");              
                            {                 
                                  AgShape myshape; // undefined shape                 
                                  ///Create FGQD                 
                                  Create("FGQD");                  
                            }              
                            { AgPlacement place = AgPlacement("FGQD","FGVG");                 
                                  /// Add daughter volume FGQD to mother FGVG                 
                                  place.TranslateZ(zzfgz);                 
                                  /// Translate z = zzfgz                 
                                  _stacker -> Position( AgBlock::Find("FGQD"), place );                 
                            } // end placement of FGQD              
                            _create = AgCreate("FGQE");              
                            {                 
                                  AgShape myshape; // undefined shape                 
                                  ///Create FGQE                 
                                  Create("FGQE");                  
                            }              
                            { AgPlacement place = AgPlacement("FGQE","FGVG");                 
                                  /// Add daughter volume FGQE to mother FGVG                 
                                  place.TranslateX((flatoutr-framewidth/2.)*cos(flatperpang*degrad));                 
                                  /// Translate x = (flatoutr-framewidth/2.)*cos(flatperpang*degrad)                 
                                  place.TranslateY((flatoutr-framewidth/2.)*sin(flatperpang*degrad));                 
                                  /// Translate y = (flatoutr-framewidth/2.)*sin(flatperpang*degrad)                 
                                  place.TranslateZ(zzfgz);                 
                                  /// Translate z = zzfgz                 
                                  place.AlphaZ(flatperpang-90.);                 
                                  /// Rotate: AlphaZ = flatperpang-90.                 
                                  /// G3 Reference: thetax = 90                 
                                  /// G3 Reference: phix = 0                 
                                  /// G3 Reference: thetay = 90                 
                                  /// G3 Reference: phiy = 90                 
                                  /// G3 Reference: thetaz = 0                 
                                  /// G3 Reference: phiz = 0                 
                                  _stacker -> Position( AgBlock::Find("FGQE"), place );                 
                            } // end placement of FGQE              
                            _create = AgCreate("FGQF");              
                            {                 
                                  AgShape myshape; // undefined shape                 
                                  ///Create FGQF                 
                                  Create("FGQF");                  
                            }              
                            { AgPlacement place = AgPlacement("FGQF","FGVG");                 
                                  /// Add daughter volume FGQF to mother FGVG                 
                                  place.TranslateZ(zzfgz);                 
                                  /// Translate z = zzfgz                 
                                  _stacker -> Position( AgBlock::Find("FGQF"), place );                 
                            } // end placement of FGQF              
                            _create = AgCreate("FGXA");              
                            {                 
                                  AgShape myshape; // undefined shape                 
                                  ///Create FGXA                 
                                  Create("FGXA");                  
                            }              
                            { AgPlacement place = AgPlacement("FGXA","FGVG");                 
                                  /// Add daughter volume FGXA to mother FGVG                 
                                  place.TranslateZ(zzfgz);                 
                                  /// Translate z = zzfgz                 
                                  _stacker -> Position( AgBlock::Find("FGXA"), place );                 
                            } // end placement of FGXA              
                            _create = AgCreate("FGXB");              
                            {                 
                                  AgShape myshape; // undefined shape                 
                                  ///Create FGXB                 
                                  Create("FGXB");                  
                            }              
                            { AgPlacement place = AgPlacement("FGXB","FGVG");                 
                                  /// Add daughter volume FGXB to mother FGVG                 
                                  place.TranslateZ(zzfgz);                 
                                  /// Translate z = zzfgz                 
                                  _stacker -> Position( AgBlock::Find("FGXB"), place );                 
                            } // end placement of FGXB              
                            _create = AgCreate("FGXC");              
                            {                 
                                  AgShape myshape; // undefined shape                 
                                  ///Create FGXC                 
                                  Create("FGXC");                  
                            }              
                            xxfgz=(diskoutr+diskinr)/2.;              
                            { AgPlacement place = AgPlacement("FGXC","FGVG");                 
                                  /// Add daughter volume FGXC to mother FGVG                 
                                  place.TranslateX(xxfgz*cos(gridraylongphi*degrad));                 
                                  /// Translate x = xxfgz*cos(gridraylongphi*degrad)                 
                                  place.TranslateY(xxfgz*sin(gridraylongphi*degrad));                 
                                  /// Translate y = xxfgz*sin(gridraylongphi*degrad)                 
                                  place.TranslateZ(zzfgz);                 
                                  /// Translate z = zzfgz                 
                                  place.AlphaZ(gridraylongphi);                 
                                  /// Rotate: AlphaZ = gridraylongphi                 
                                  /// G3 Reference: thetax = 90                 
                                  /// G3 Reference: phix = 0                 
                                  /// G3 Reference: thetay = 90                 
                                  /// G3 Reference: phiy = 90                 
                                  /// G3 Reference: thetaz = 0                 
                                  /// G3 Reference: phiz = 0                 
                                  _stacker -> Position( AgBlock::Find("FGXC"), place );                 
                            } // end placement of FGXC              
                            _create = AgCreate("FGXD");              
                            {                 
                                  AgShape myshape; // undefined shape                 
                                  ///Create FGXD                 
                                  Create("FGXD");                  
                            }              
                            xxfgz=(flatoutr+diskinr)/2.;              
                            { AgPlacement place = AgPlacement("FGXD","FGVG");                 
                                  /// Add daughter volume FGXD to mother FGVG                 
                                  place.TranslateX(xxfgz*cos(gridrayshortphi*degrad));                 
                                  /// Translate x = xxfgz*cos(gridrayshortphi*degrad)                 
                                  place.TranslateY(xxfgz*sin(gridrayshortphi*degrad));                 
                                  /// Translate y = xxfgz*sin(gridrayshortphi*degrad)                 
                                  place.TranslateZ(zzfgz);                 
                                  /// Translate z = zzfgz                 
                                  place.AlphaZ(gridrayshortphi);                 
                                  /// Rotate: AlphaZ = gridrayshortphi                 
                                  /// G3 Reference: thetax = 90                 
                                  /// G3 Reference: phix = 0                 
                                  /// G3 Reference: thetay = 90                 
                                  /// G3 Reference: phiy = 90                 
                                  /// G3 Reference: thetaz = 0                 
                                  /// G3 Reference: phiz = 0                 
                                  _stacker -> Position( AgBlock::Find("FGXD"), place );                 
                            } // end placement of FGXD              
                      }           
                      END_OF_FGVG:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FGVG     
          // ---------------------------------------------------------------------------------------------------     
          void FGVR::Block( AgCreate create )     
          {         
                ///@addtogroup FGVR_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("FGVR");              
                            attr.par("seen")=0;              
                            attr.par("colo")=5;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material ReadMix            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Readmix");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Tubs");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=diskinr;              
                            shape.par("rmax")=diskoutr;              
                            shape.par("phi1")=0.;              
                            shape.par("phi2")=90.;              
                            shape.par("dz")=volreaddz/2.;              
                            /// Shape Tubs rmin=diskinr rmax=diskoutr phi1=0. phi2=90. dz=volreaddz/2.               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FGVR;              
                            _stacker -> Build(this);              
                      }           
                      zzfgz=-volreaddz/2. +framedz/1.5;           
                      _create = AgCreate("FGQA");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FGQA              
                            Create("FGQA");               
                      }           
                      { AgPlacement place = AgPlacement("FGQA","FGVR");              
                            /// Add daughter volume FGQA to mother FGVR              
                            place.TranslateX(framewidth/2.+framexyoff);              
                            /// Translate x = framewidth/2.+framexyoff              
                            place.TranslateY((diskoutr+diskinr)/2.);              
                            /// Translate y = (diskoutr+diskinr)/2.              
                            place.TranslateZ(zzfgz);              
                            /// Translate z = zzfgz              
                            _stacker -> Position( AgBlock::Find("FGQA"), place );              
                      } // end placement of FGQA           
                      _create = AgCreate("FGQB");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FGQB              
                            Create("FGQB");               
                      }           
                      { AgPlacement place = AgPlacement("FGQB","FGVR");              
                            /// Add daughter volume FGQB to mother FGVR              
                            place.TranslateZ(zzfgz);              
                            /// Translate z = zzfgz              
                            _stacker -> Position( AgBlock::Find("FGQB"), place );              
                      } // end placement of FGQB           
                      _create = AgCreate("FGQC");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FGQC              
                            Create("FGQC");               
                      }           
                      { AgPlacement place = AgPlacement("FGQC","FGVR");              
                            /// Add daughter volume FGQC to mother FGVR              
                            place.TranslateX((diskoutr+diskinr)/2.);              
                            /// Translate x = (diskoutr+diskinr)/2.              
                            place.TranslateY(framewidth/2.+framexyoff);              
                            /// Translate y = framewidth/2.+framexyoff              
                            place.TranslateZ(zzfgz);              
                            /// Translate z = zzfgz              
                            _stacker -> Position( AgBlock::Find("FGQC"), place );              
                      } // end placement of FGQC           
                      _create = AgCreate("FGQD");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FGQD              
                            Create("FGQD");               
                      }           
                      { AgPlacement place = AgPlacement("FGQD","FGVR");              
                            /// Add daughter volume FGQD to mother FGVR              
                            place.TranslateZ(zzfgz);              
                            /// Translate z = zzfgz              
                            _stacker -> Position( AgBlock::Find("FGQD"), place );              
                      } // end placement of FGQD           
                      _create = AgCreate("FGQE");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FGQE              
                            Create("FGQE");               
                      }           
                      { AgPlacement place = AgPlacement("FGQE","FGVR");              
                            /// Add daughter volume FGQE to mother FGVR              
                            place.TranslateX((flatoutr-framewidth/2.)*cos(flatperpang*degrad));              
                            /// Translate x = (flatoutr-framewidth/2.)*cos(flatperpang*degrad)              
                            place.TranslateY((flatoutr-framewidth/2.)*sin(flatperpang*degrad));              
                            /// Translate y = (flatoutr-framewidth/2.)*sin(flatperpang*degrad)              
                            place.TranslateZ(zzfgz);              
                            /// Translate z = zzfgz              
                            place.AlphaZ(flatperpang-90.);              
                            /// Rotate: AlphaZ = flatperpang-90.              
                            /// G3 Reference: thetax = 90              
                            /// G3 Reference: phix = 0              
                            /// G3 Reference: thetay = 90              
                            /// G3 Reference: phiy = 90              
                            /// G3 Reference: thetaz = 0              
                            /// G3 Reference: phiz = 0              
                            _stacker -> Position( AgBlock::Find("FGQE"), place );              
                      } // end placement of FGQE           
                      _create = AgCreate("FGQF");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FGQF              
                            Create("FGQF");               
                      }           
                      { AgPlacement place = AgPlacement("FGQF","FGVR");              
                            /// Add daughter volume FGQF to mother FGVR              
                            place.TranslateZ(zzfgz);              
                            /// Translate z = zzfgz              
                            _stacker -> Position( AgBlock::Find("FGQF"), place );              
                      } // end placement of FGQF           
                      END_OF_FGVR:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FGVR     
          // ---------------------------------------------------------------------------------------------------     
          void FGVE::Block( AgCreate create )     
          {         
                ///@addtogroup FGVE_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("FGVE");              
                            attr.par("seen")=0;              
                            attr.par("colo")=7;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material Air            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Air");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Tubs");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=diskinr;              
                            shape.par("rmax")=diskoutr;              
                            shape.par("phi1")=0.;              
                            shape.par("phi2")=90.;              
                            shape.par("dz")=volelecdz/2.;              
                            /// Shape Tubs rmin=diskinr rmax=diskoutr phi1=0. phi2=90. dz=volelecdz/2.               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FGVE;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("FGWA");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FGWA              
                            Create("FGWA");               
                      }           
                      { AgPlacement place = AgPlacement("FGWA","FGVE");              
                            /// Add daughter volume FGWA to mother FGVE              
                            place.TranslateX(diskinr+framewidth*0.7);              
                            /// Translate x = diskinr+framewidth*0.7              
                            place.TranslateY(elecady/2.);              
                            /// Translate y = elecady/2.              
                            _stacker -> Position( AgBlock::Find("FGWA"), place );              
                      } // end placement of FGWA           
                      _create = AgCreate("FGWA");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FGWA              
                            Create("FGWA");               
                      }           
                      { AgPlacement place = AgPlacement("FGWA","FGVE");              
                            /// Add daughter volume FGWA to mother FGVE              
                            place.TranslateX(elecady/2.);              
                            /// Translate x = elecady/2.              
                            place.TranslateY(diskinr+framewidth*0.7);              
                            /// Translate y = diskinr+framewidth*0.7              
                            place.AlphaZ(90.);              
                            /// Rotate: AlphaZ = 90.              
                            /// G3 Reference: thetax = 90              
                            /// G3 Reference: phix = 0              
                            /// G3 Reference: thetay = 90              
                            /// G3 Reference: phiy = 90              
                            /// G3 Reference: thetaz = 0              
                            /// G3 Reference: phiz = 0              
                            _stacker -> Position( AgBlock::Find("FGWA"), place );              
                      } // end placement of FGWA           
                      _create = AgCreate("FGWB");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FGWB              
                            Create("FGWB");               
                      }           
                      { AgPlacement place = AgPlacement("FGWB","FGVE");              
                            /// Add daughter volume FGWB to mother FGVE              
                            place.TranslateX((diskoutr+diskinr)/2.);              
                            /// Translate x = (diskoutr+diskinr)/2.              
                            place.TranslateY(framexyoff+elecwidth/2.);              
                            /// Translate y = framexyoff+elecwidth/2.              
                            _stacker -> Position( AgBlock::Find("FGWB"), place );              
                      } // end placement of FGWB           
                      _create = AgCreate("FGWB");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FGWB              
                            Create("FGWB");               
                      }           
                      { AgPlacement place = AgPlacement("FGWB","FGVE");              
                            /// Add daughter volume FGWB to mother FGVE              
                            place.TranslateX(framexyoff+elecwidth/2.);              
                            /// Translate x = framexyoff+elecwidth/2.              
                            place.TranslateY((diskoutr+diskinr)/2.);              
                            /// Translate y = (diskoutr+diskinr)/2.              
                            place.AlphaZ(90.);              
                            /// Rotate: AlphaZ = 90.              
                            /// G3 Reference: thetax = 90              
                            /// G3 Reference: phix = 0              
                            /// G3 Reference: thetay = 90              
                            /// G3 Reference: phiy = 90              
                            /// G3 Reference: thetaz = 0              
                            /// G3 Reference: phiz = 0              
                            _stacker -> Position( AgBlock::Find("FGWB"), place );              
                      } // end placement of FGWB           
                      _create = AgCreate("FGWC");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FGWC              
                            Create("FGWC");               
                      }           
                      { AgPlacement place = AgPlacement("FGWC","FGVE");              
                            /// Add daughter volume FGWC to mother FGVE              
                            place.TranslateX(diskoutr-framewidth*0.7);              
                            /// Translate x = diskoutr-framewidth*0.7              
                            place.TranslateY(eleccdy/2.);              
                            /// Translate y = eleccdy/2.              
                            _stacker -> Position( AgBlock::Find("FGWC"), place );              
                      } // end placement of FGWC           
                      _create = AgCreate("FGWC");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FGWC              
                            Create("FGWC");               
                      }           
                      { AgPlacement place = AgPlacement("FGWC","FGVE");              
                            /// Add daughter volume FGWC to mother FGVE              
                            place.TranslateX(eleccdy/2.);              
                            /// Translate x = eleccdy/2.              
                            place.TranslateY(diskoutr-framewidth*0.7);              
                            /// Translate y = diskoutr-framewidth*0.7              
                            place.AlphaZ(90.);              
                            /// Rotate: AlphaZ = 90.              
                            /// G3 Reference: thetax = 90              
                            /// G3 Reference: phix = 0              
                            /// G3 Reference: thetay = 90              
                            /// G3 Reference: phiy = 90              
                            /// G3 Reference: thetaz = 0              
                            /// G3 Reference: phiz = 0              
                            _stacker -> Position( AgBlock::Find("FGWC"), place );              
                      } // end placement of FGWC           
                      _create = AgCreate("FGWD");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FGWD              
                            Create("FGWD");               
                      }           
                      { AgPlacement place = AgPlacement("FGWD","FGVE");              
                            /// Add daughter volume FGWD to mother FGVE              
                            place.TranslateX((diskoutr-framewidth/2.)*cos(elecdang*degrad));              
                            /// Translate x = (diskoutr-framewidth/2.)*cos(elecdang*degrad)              
                            place.TranslateY((diskoutr-framewidth/2.)*sin(elecdang*degrad));              
                            /// Translate y = (diskoutr-framewidth/2.)*sin(elecdang*degrad)              
                            place.AlphaX(90.);              
                            /// Rotate: AlphaX = 90.              
                            /// G3 Reference: thetax = 90              
                            /// G3 Reference: phix = 0              
                            /// G3 Reference: thetay = 90              
                            /// G3 Reference: phiy = 90              
                            /// G3 Reference: thetaz = 0              
                            /// G3 Reference: phiz = 0              
                            _stacker -> Position( AgBlock::Find("FGWD"), place );              
                      } // end placement of FGWD           
                      _create = AgCreate("FGWD");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FGWD              
                            Create("FGWD");               
                      }           
                      { AgPlacement place = AgPlacement("FGWD","FGVE");              
                            /// Add daughter volume FGWD to mother FGVE              
                            place.TranslateX((diskoutr-framewidth/2.)*sin(elecdang*degrad));              
                            /// Translate x = (diskoutr-framewidth/2.)*sin(elecdang*degrad)              
                            place.TranslateY((diskoutr-framewidth/2.)*cos(elecdang*degrad));              
                            /// Translate y = (diskoutr-framewidth/2.)*cos(elecdang*degrad)              
                            place.AlphaX(90.);              
                            /// Rotate: AlphaX = 90.              
                            /// G3 Reference: thetax = 90              
                            /// G3 Reference: phix = 0              
                            /// G3 Reference: thetay = 90              
                            /// G3 Reference: phiy = 90              
                            /// G3 Reference: thetaz = 0              
                            /// G3 Reference: phiz = 0              
                            place.AlphaZ(90.);              
                            /// Rotate: AlphaZ = 90.              
                            /// G3 Reference: thetax = 90              
                            /// G3 Reference: phix = 0              
                            /// G3 Reference: thetay = 90              
                            /// G3 Reference: phiy = 90              
                            /// G3 Reference: thetaz = 0              
                            /// G3 Reference: phiz = 0              
                            _stacker -> Position( AgBlock::Find("FGWD"), place );              
                      } // end placement of FGWD           
                      _create = AgCreate("FGWE");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FGWE              
                            Create("FGWE");               
                      }           
                      { AgPlacement place = AgPlacement("FGWE","FGVE");              
                            /// Add daughter volume FGWE to mother FGVE              
                            place.TranslateX((flatoutr-framewidth/2.)*cos(flatperpang*degrad));              
                            /// Translate x = (flatoutr-framewidth/2.)*cos(flatperpang*degrad)              
                            place.TranslateY((flatoutr-framewidth/2.)*sin(flatperpang*degrad));              
                            /// Translate y = (flatoutr-framewidth/2.)*sin(flatperpang*degrad)              
                            place.AlphaZ(flatperpang-90.);              
                            /// Rotate: AlphaZ = flatperpang-90.              
                            /// G3 Reference: thetax = 90              
                            /// G3 Reference: phix = 0              
                            /// G3 Reference: thetay = 90              
                            /// G3 Reference: phiy = 90              
                            /// G3 Reference: thetaz = 0              
                            /// G3 Reference: phiz = 0              
                            _stacker -> Position( AgBlock::Find("FGWE"), place );              
                      } // end placement of FGWE           
                      END_OF_FGVE:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FGVE     
          // ---------------------------------------------------------------------------------------------------     
          void FGQA::Block( AgCreate create )     
          {         
                ///@addtogroup FGQA_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("FGQA");              
                            attr.par("seen")=1;              
                            attr.par("colo")=4;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material FR4            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Fr4");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=framewidth/2.;              
                            shape.par("dy")=(diskoutr-diskinr-framewidth)/2.;              
                            shape.par("dz")=framedz/2.;              
                            /// Shape Bbox dx=framewidth/2. dy=(diskoutr-diskinr-framewidth)/2. dz=framedz/2.               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FGQA;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_FGQA:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FGQA     
          // ---------------------------------------------------------------------------------------------------     
          void FGQB::Block( AgCreate create )     
          {         
                ///@addtogroup FGQB_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("FGQB");              
                            attr.par("seen")=1;              
                            attr.par("colo")=4;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material FR4            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Fr4");              
                            _material = mat;              
                      }           
                      xxfgq=framexyoff/diskinr/degrad;           
                      {  AgShape shape = AgShape("Tubs");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=diskinr;              
                            shape.par("rmax")=diskinr+framewidth;              
                            shape.par("phi1")=xxfgq;              
                            shape.par("phi2")=90.-xxfgq;              
                            shape.par("dz")=framedz/2.;              
                            /// Shape Tubs rmin=diskinr rmax=diskinr+framewidth phi1=xxfgq phi2=90.-xxfgq dz=framedz/2.               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FGQB;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_FGQB:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FGQB     
          // ---------------------------------------------------------------------------------------------------     
          void FGQC::Block( AgCreate create )     
          {         
                ///@addtogroup FGQC_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("FGQC");              
                            attr.par("seen")=1;              
                            attr.par("colo")=4;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material FR4            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Fr4");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=(diskoutr-diskinr-framewidth)/2.;              
                            shape.par("dy")=framewidth/2.;              
                            shape.par("dz")=framedz/2.;              
                            /// Shape Bbox dx=(diskoutr-diskinr-framewidth)/2. dy=framewidth/2. dz=framedz/2.               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FGQC;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_FGQC:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FGQC     
          // ---------------------------------------------------------------------------------------------------     
          void FGQD::Block( AgCreate create )     
          {         
                ///@addtogroup FGQD_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("FGQD");              
                            attr.par("seen")=1;              
                            attr.par("colo")=4;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material FR4            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Fr4");              
                            _material = mat;              
                      }           
                      xxfgq=framexyoff/diskoutr/degrad;           
                      {  AgShape shape = AgShape("Tubs");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=diskoutr-framewidth;              
                            shape.par("rmax")=diskoutr;              
                            shape.par("phi1")=xxfgq;              
                            shape.par("phi2")=flatang1;              
                            shape.par("dz")=framedz/2.;              
                            /// Shape Tubs rmin=diskoutr-framewidth rmax=diskoutr phi1=xxfgq phi2=flatang1 dz=framedz/2.               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FGQD;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_FGQD:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FGQD     
          // ---------------------------------------------------------------------------------------------------     
          void FGQE::Block( AgCreate create )     
          {         
                ///@addtogroup FGQE_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("FGQE");              
                            attr.par("seen")=1;              
                            attr.par("colo")=4;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material FR4            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Fr4");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=diskoutr*sin(( flatang2-flatang1)*degrad/2.);              
                            shape.par("dy")=framewidth/2.;              
                            shape.par("dz")=framedz/2.;              
                            /// Shape Bbox dx=diskoutr*sin(( flatang2-flatang1)*degrad/2.) dy=framewidth/2. dz=framedz/2.               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FGQE;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_FGQE:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FGQE     
          // ---------------------------------------------------------------------------------------------------     
          void FGQF::Block( AgCreate create )     
          {         
                ///@addtogroup FGQF_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("FGQF");              
                            attr.par("seen")=1;              
                            attr.par("colo")=4;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material FR4            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Fr4");              
                            _material = mat;              
                      }           
                      xxfgq=framexyoff/diskoutr/degrad;           
                      {  AgShape shape = AgShape("Tubs");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=diskoutr-framewidth;              
                            shape.par("rmax")=diskoutr;              
                            shape.par("phi1")=flatang2;              
                            shape.par("phi2")=90.-xxfgq;              
                            shape.par("dz")=framedz/2.;              
                            /// Shape Tubs rmin=diskoutr-framewidth rmax=diskoutr phi1=flatang2 phi2=90.-xxfgq dz=framedz/2.               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FGQF;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_FGQF:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FGQF     
          // ---------------------------------------------------------------------------------------------------     
          void FGXA::Block( AgCreate create )     
          {         
                ///@addtogroup FGXA_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("FGXB");              
                            attr.par("seen")=1;              
                            attr.par("colo")=3;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material FR4            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Fr4");              
                            _material = mat;              
                      }           
                      xxfgx=framexyoff/gridarcbigr/degrad;           
                      {  AgShape shape = AgShape("Tubs");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=gridarcbigr;              
                            shape.par("rmax")=gridarcbigr+gridwidth;              
                            shape.par("phi1")=xxfgx;              
                            shape.par("phi2")=90.-xxfgx;              
                            shape.par("dz")=framedz/2.;              
                            /// Shape Tubs rmin=gridarcbigr rmax=gridarcbigr+gridwidth phi1=xxfgx phi2=90.-xxfgx dz=framedz/2.               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FGXA;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_FGXA:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FGXA     
          // ---------------------------------------------------------------------------------------------------     
          void FGXB::Block( AgCreate create )     
          {         
                ///@addtogroup FGXB_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("FGXB");              
                            attr.par("seen")=1;              
                            attr.par("colo")=3;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material FR4            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Fr4");              
                            _material = mat;              
                      }           
                      xxfgx=framexyoff/gridarcsmallr/degrad;           
                      {  AgShape shape = AgShape("Tubs");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=gridarcsmallr;              
                            shape.par("rmax")=gridarcsmallr+gridwidth;              
                            shape.par("phi1")=xxfgx;              
                            shape.par("phi2")=90.-xxfgx;              
                            shape.par("dz")=framedz/2.;              
                            /// Shape Tubs rmin=gridarcsmallr rmax=gridarcsmallr+gridwidth phi1=xxfgx phi2=90.-xxfgx dz=framedz/2.               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FGXB;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_FGXB:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FGXB     
          // ---------------------------------------------------------------------------------------------------     
          void FGXC::Block( AgCreate create )     
          {         
                ///@addtogroup FGXC_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("FGXC");              
                            attr.par("seen")=1;              
                            attr.par("colo")=3;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material FR4            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Fr4");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=(diskoutr-diskinr-framewidth)/2.;              
                            shape.par("dy")=gridwidth/2.;              
                            shape.par("dz")=framedz/2.;              
                            /// Shape Bbox dx=(diskoutr-diskinr-framewidth)/2. dy=gridwidth/2. dz=framedz/2.               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FGXC;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_FGXC:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FGXC     
          // ---------------------------------------------------------------------------------------------------     
          void FGXD::Block( AgCreate create )     
          {         
                ///@addtogroup FGXD_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("FGXD");              
                            attr.par("seen")=1;              
                            attr.par("colo")=3;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material FR4            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Fr4");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=(flatoutr-diskinr-framewidth)/2.;              
                            shape.par("dy")=gridwidth/2.;              
                            shape.par("dz")=framedz/2.;              
                            /// Shape Bbox dx=(flatoutr-diskinr-framewidth)/2. dy=gridwidth/2. dz=framedz/2.               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FGXD;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_FGXD:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FGXD     
          // ---------------------------------------------------------------------------------------------------     
          void FGWA::Block( AgCreate create )     
          {         
                ///@addtogroup FGWA_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("FGWA");              
                            attr.par("seen")=1;              
                            attr.par("colo")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material TermMix            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Termmix");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=0.3/2.;              
                            shape.par("dy")=elecady/2.;              
                            shape.par("dz")=2.7/2.;              
                            /// Shape Bbox dx=0.3/2. dy=elecady/2. dz=2.7/2.               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FGWA;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_FGWA:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FGWA     
          // ---------------------------------------------------------------------------------------------------     
          void FGWB::Block( AgCreate create )     
          {         
                ///@addtogroup FGWB_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("FGWB");              
                            attr.par("seen")=1;              
                            attr.par("colo")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material APVMix            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Apvmix");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=25.92/2.;              
                            shape.par("dy")=elecwidth/2.;              
                            shape.par("dz")=4.04/2.;              
                            /// Shape Bbox dx=25.92/2. dy=elecwidth/2. dz=4.04/2.               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FGWB;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_FGWB:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FGWB     
          // ---------------------------------------------------------------------------------------------------     
          void FGWC::Block( AgCreate create )     
          {         
                ///@addtogroup FGWC_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("FGWC");              
                            attr.par("seen")=1;              
                            attr.par("colo")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material ConMix            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Conmix");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=0.3/2.;              
                            shape.par("dy")=eleccdy/2.;              
                            shape.par("dz")=3.15/2.;              
                            /// Shape Bbox dx=0.3/2. dy=eleccdy/2. dz=3.15/2.               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FGWC;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_FGWC:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FGWC     
          // ---------------------------------------------------------------------------------------------------     
          void FGWD::Block( AgCreate create )     
          {         
                ///@addtogroup FGWD_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("FGWD");              
                            attr.par("seen")=1;              
                            attr.par("colo")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material Brass            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Brass");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=0.2;              
                            shape.par("rmax")=0.5;              
                            shape.par("dz")=2.1/2.;              
                            /// Shape Tube rmin=0.2 rmax=0.5 dz=2.1/2.               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FGWD;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_FGWD:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FGWD     
          // ---------------------------------------------------------------------------------------------------     
          void FGWE::Block( AgCreate create )     
          {         
                ///@addtogroup FGWE_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("FGWE");              
                            attr.par("seen")=1;              
                            attr.par("colo")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material HVMix            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Hvmix");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=elecelen/2.;              
                            shape.par("dy")=elecwidth/2.;              
                            shape.par("dz")=volelecdz/2.;              
                            /// Shape Bbox dx=elecelen/2. dy=elecwidth/2. dz=volelecdz/2.               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FGWE;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_FGWE:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FGWE     
    // ----------------------------------------------------------------------- geoctr
       void FgtdGeo3::ConstructGeometry( const Char_t *dummy )     
       {        
             ///@addtogroup FgtdGeo3_revision        
             ///@{           
                   /// Created:   12/20/2011            
             ///@}        
             ///@addtogroup FgtdGeo3_revision        
             ///@{           
                   /// Author: Jan Balewski MIT, Wei-Ming Zhang KSU, Willie Leight MIT (material mixes)           
             ///@}        
             AddBlock("FGTM");        
             AddBlock("FGTD");        
             AddBlock("FGTQ");        
             AddBlock("FGTH");        
             AddBlock("FGVN");        
             AddBlock("FGZC");        
             AddBlock("FGVG");        
             AddBlock("FGVR");        
             AddBlock("FGVE");        
             AddBlock("FGQA");        
             AddBlock("FGQB");        
             AddBlock("FGQC");        
             AddBlock("FGQD");        
             AddBlock("FGQE");        
             AddBlock("FGQF");        
             AddBlock("FGXA");        
             AddBlock("FGXB");        
             AddBlock("FGXC");        
             AddBlock("FGXD");        
             AddBlock("FGWA");        
             AddBlock("FGWB");        
             AddBlock("FGWC");        
             AddBlock("FGWD");        
             AddBlock("FGWE");        
             AddBlock("FGCT");        
             AddBlock("FGCN");        
             AddBlock("FGCM");        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup fggg_doc        
             ///@{           
                   ++fggg._index;           
                   fggg . fgstconfig = 1.0; // selection of no. of disks and quadrants            
                   /// fggg . fgstconfig = 1.0; // selection of no. of disks and quadrants            
                   //           
                   fggg.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup fgst_doc        
             ///@{           
                   ++fgst._index;           
                   fgst . config = 1.0; // versioning of the FGST geometry data           
                   /// fgst . config = 1.0; // versioning of the FGST geometry data           
                   fgst . ndisk = 6; // number of disks           
                   /// fgst . ndisk = 6; // number of disks           
                   fgst . nquad = 4; // number quadrants in a disks           
                   /// fgst . nquad = 4; // number quadrants in a disks           
                   fgst . zdisca.at(0) = 67.3990; //  z-center of disk senstive volume from center in STAR , as measured in 2011-10             
                   ///fgst . zdisca.at(0) = 67.3990; //  z-center of disk senstive volume from center in STAR , as measured in 2011-10             
                   fgst . zdisca.at(1) = 77.8765; //  z-center of disk senstive volume from center in STAR , as measured in 2011-10             
                   ///fgst . zdisca.at(1) = 77.8765; //  z-center of disk senstive volume from center in STAR , as measured in 2011-10             
                   fgst . zdisca.at(2) = 87.0840; //  z-center of disk senstive volume from center in STAR , as measured in 2011-10             
                   ///fgst . zdisca.at(2) = 87.0840; //  z-center of disk senstive volume from center in STAR , as measured in 2011-10             
                   fgst . zdisca.at(3) = 97.4821; //  z-center of disk senstive volume from center in STAR , as measured in 2011-10             
                   ///fgst . zdisca.at(3) = 97.4821; //  z-center of disk senstive volume from center in STAR , as measured in 2011-10             
                   fgst . zdisca.at(4) = 108.9121; //  z-center of disk senstive volume from center in STAR , as measured in 2011-10             
                   ///fgst . zdisca.at(4) = 108.9121; //  z-center of disk senstive volume from center in STAR , as measured in 2011-10             
                   fgst . zdisca.at(5) = 118.9927; //  z-center of disk senstive volume from center in STAR , as measured in 2011-10             
                   ///fgst . zdisca.at(5) = 118.9927; //  z-center of disk senstive volume from center in STAR , as measured in 2011-10             
                   //           
                   fgst.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup fgst_doc        
             ///@{           
                   ++fgst._index;           
                   fgst . config = 2.0; // versioning of the FGST geometry data           
                   /// fgst . config = 2.0; // versioning of the FGST geometry data           
                   fgst . ndisk = 6; // number of disks           
                   /// fgst . ndisk = 6; // number of disks           
                   fgst . nquad = 4; // number quadrants in a disks           
                   /// fgst . nquad = 4; // number quadrants in a disks           
                   fgst . zdisca.at(0) = 70.; //  z-center of disk senstive volume from center in STAR , ideal location            
                   ///fgst . zdisca.at(0) = 70.; //  z-center of disk senstive volume from center in STAR , ideal location            
                   fgst . zdisca.at(1) = 80.; //  z-center of disk senstive volume from center in STAR , ideal location            
                   ///fgst . zdisca.at(1) = 80.; //  z-center of disk senstive volume from center in STAR , ideal location            
                   fgst . zdisca.at(2) = 90.; //  z-center of disk senstive volume from center in STAR , ideal location            
                   ///fgst . zdisca.at(2) = 90.; //  z-center of disk senstive volume from center in STAR , ideal location            
                   fgst . zdisca.at(3) = 100.; //  z-center of disk senstive volume from center in STAR , ideal location            
                   ///fgst . zdisca.at(3) = 100.; //  z-center of disk senstive volume from center in STAR , ideal location            
                   fgst . zdisca.at(4) = 110.; //  z-center of disk senstive volume from center in STAR , ideal location            
                   ///fgst . zdisca.at(4) = 110.; //  z-center of disk senstive volume from center in STAR , ideal location            
                   fgst . zdisca.at(5) = 120.; //  z-center of disk senstive volume from center in STAR , ideal location            
                   ///fgst . zdisca.at(5) = 120.; //  z-center of disk senstive volume from center in STAR , ideal location            
                   //           
                   fgst.fill();           
             ///@}        
             //        
             /// Component Si	a=28.08	z=14	w=0.281        
             /// Component O	a=16	z=8	w=0.467        
             /// Component C	a=12	z=6	w=0.220        
             /// Component H	a=1	z=1	w=0.032        
             /// Mixture FR4 dens=1.80        
             {  AgMaterial &mix = AgMaterial::Get("Fr4");           
                   mix.Component("Si",28.08,14,0.281);           
                   mix.Component("O",16,8,0.467);           
                   mix.Component("C",12,6,0.220);           
                   mix.Component("H",1,1,0.032);           
                   mix.par("dens")=1.80;           
                   mix.lock();           
                   _material = mix;           
                   _material.lock();           
             }        
             /// Component O	a=16	z=8	w=0.062        
             /// Component C	a=12	z=6	w=0.892        
             /// Component H	a=1	z=1	w=0.019        
             /// Component Cl	a=35.5	z=17	w=0.027        
             /// Mixture CFRPMix dens=1.78        
             {  AgMaterial &mix = AgMaterial::Get("Cfrpmix");           
                   mix.Component("O",16,8,0.062);           
                   mix.Component("C",12,6,0.892);           
                   mix.Component("H",1,1,0.019);           
                   mix.Component("Cl",35.5,17,0.027);           
                   mix.par("dens")=1.78;           
                   mix.lock();           
                   _material = mix;           
                   _material.lock();           
             }        
             /// Component O	a=16	z=8	w=0.142        
             /// Component C	a=12	z=6	w=0.637        
             /// Component H	a=1	z=1	w=0.097        
             /// Component N	a=14	z=7	w=0.124        
             /// Mixture Nylon dens=1.15        
             {  AgMaterial &mix = AgMaterial::Get("Nylon");           
                   mix.Component("O",16,8,0.142);           
                   mix.Component("C",12,6,0.637);           
                   mix.Component("H",1,1,0.097);           
                   mix.Component("N",14,7,0.124);           
                   mix.par("dens")=1.15;           
                   mix.lock();           
                   _material = mix;           
                   _material.lock();           
             }        
             /// Component O	a=16	z=8	w=0.129        
             /// Component C	a=12	z=6	w=0.579        
             /// Component H	a=1	z=1	w=0.088        
             /// Component N	a=14	z=7	w=0.112        
             /// Component Al	a=27	z=13	w=0.092        
             /// Mixture CoolMix dens=1.214        
             {  AgMaterial &mix = AgMaterial::Get("Coolmix");           
                   mix.Component("O",16,8,0.129);           
                   mix.Component("C",12,6,0.579);           
                   mix.Component("H",1,1,0.088);           
                   mix.Component("N",14,7,0.112);           
                   mix.Component("Al",27,13,0.092);           
                   mix.par("dens")=1.214;           
                   mix.lock();           
                   _material = mix;           
                   _material.lock();           
             }        
             /// Component O	a=16	z=8	w=0.090        
             /// Component C	a=12	z=6	w=0.796        
             /// Component H	a=1	z=1	w=0.023        
             /// Component Cl	a=35.5	z=17	w=0.019        
             /// Component N	a=14	z=7	w=0.025        
             /// Component Al	a=27	z=13	w=0.002        
             /// Component Cu	a=63.5	z=29	w=0.039        
             /// Component Ar	a=39.9	z=18	w=0.006        
             /// Mixture NomexMix dens=0.090        
             {  AgMaterial &mix = AgMaterial::Get("Nomexmix");           
                   mix.Component("O",16,8,0.090);           
                   mix.Component("C",12,6,0.796);           
                   mix.Component("H",1,1,0.023);           
                   mix.Component("Cl",35.5,17,0.019);           
                   mix.Component("N",14,7,0.025);           
                   mix.Component("Al",27,13,0.002);           
                   mix.Component("Cu",63.5,29,0.039);           
                   mix.Component("Ar",39.9,18,0.006);           
                   mix.par("dens")=0.090;           
                   mix.lock();           
                   _material = mix;           
                   _material.lock();           
             }        
             /// Component O	a=16	z=8	w=0.095        
             /// Component C	a=12	z=6	w=0.301        
             /// Component H	a=1	z=1	w=0.011        
             /// Component N	a=14	z=7	w=0.032        
             /// Component Cu	a=63.5	z=29	w=0.547        
             /// Component Ar	a=39.9	z=18	w=0.014        
             /// Mixture GemMix dens=0.079        
             {  AgMaterial &mix = AgMaterial::Get("Gemmix");           
                   mix.Component("O",16,8,0.095);           
                   mix.Component("C",12,6,0.301);           
                   mix.Component("H",1,1,0.011);           
                   mix.Component("N",14,7,0.032);           
                   mix.Component("Cu",63.5,29,0.547);           
                   mix.Component("Ar",39.9,18,0.014);           
                   mix.par("dens")=0.079;           
                   mix.lock();           
                   _material = mix;           
                   _material.lock();           
             }        
             /// Component O	a=16	z=8	w=0.160        
             /// Component C	a=12	z=6	w=0.508        
             /// Component H	a=1	z=1	w=0.020        
             /// Component N	a=14	z=7	w=0.051        
             /// Component Al	a=27	z=13	w=0.003        
             /// Component Cu	a=63.5	z=29	w=0.255        
             /// Component Ar	a=39.9	z=18	w=0.003        
             /// Mixture ReadMix dens=0.356        
             {  AgMaterial &mix = AgMaterial::Get("Readmix");           
                   mix.Component("O",16,8,0.160);           
                   mix.Component("C",12,6,0.508);           
                   mix.Component("H",1,1,0.020);           
                   mix.Component("N",14,7,0.051);           
                   mix.Component("Al",27,13,0.003);           
                   mix.Component("Cu",63.5,29,0.255);           
                   mix.Component("Ar",39.9,18,0.003);           
                   mix.par("dens")=0.356;           
                   mix.lock();           
                   _material = mix;           
                   _material.lock();           
             }        
             /// Component Si	a=28.1	z=14	w=0.191        
             /// Component O	a=16	z=8	w=0.339        
             /// Component C	a=12	z=6	w=0.213        
             /// Component H	a=1	z=1	w=0.028        
             /// Component Cl	a=35.5	z=17	w=0.009        
             /// Component Cu	a=63.5	z=29	w=0.122        
             /// Component Pb	a=207	z=82	w=0.039        
             /// Component Sn	a=118.7	z=50	w=0.059        
             /// Mixture APVMix dens=2.535        
             {  AgMaterial &mix = AgMaterial::Get("Apvmix");           
                   mix.Component("Si",28.1,14,0.191);           
                   mix.Component("O",16,8,0.339);           
                   mix.Component("C",12,6,0.213);           
                   mix.Component("H",1,1,0.028);           
                   mix.Component("Cl",35.5,17,0.009);           
                   mix.Component("Cu",63.5,29,0.122);           
                   mix.Component("Pb",207,82,0.039);           
                   mix.Component("Sn",118.7,50,0.059);           
                   mix.par("dens")=2.535;           
                   mix.lock();           
                   _material = mix;           
                   _material.lock();           
             }        
             /// Component Si	a=28.1	z=14	w=0.113        
             /// Component O	a=16	z=8	w=0.241        
             /// Component C	a=12	z=6	w=0.248        
             /// Component H	a=1	z=1	w=0.023        
             /// Component Cu	a=63.5	z=29	w=0.109        
             /// Component Fe	a=55.8	z=26	w=0.019        
             /// Component Cr	a=52.0	z=24	w=0.008        
             /// Component Pb	a=207	z=82	w=0.096        
             /// Component Sn	a=118.7	z=50	w=0.143        
             /// Mixture HVMix dens=3.009        
             {  AgMaterial &mix = AgMaterial::Get("Hvmix");           
                   mix.Component("Si",28.1,14,0.113);           
                   mix.Component("O",16,8,0.241);           
                   mix.Component("C",12,6,0.248);           
                   mix.Component("H",1,1,0.023);           
                   mix.Component("Cu",63.5,29,0.109);           
                   mix.Component("Fe",55.8,26,0.019);           
                   mix.Component("Cr",52.0,24,0.008);           
                   mix.Component("Pb",207,82,0.096);           
                   mix.Component("Sn",118.7,50,0.143);           
                   mix.par("dens")=3.009;           
                   mix.lock();           
                   _material = mix;           
                   _material.lock();           
             }        
             /// Component Si	a=28.1	z=14	w=0.122        
             /// Component O	a=16	z=8	w=0.225        
             /// Component C	a=12	z=6	w=0.297        
             /// Component H	a=1	z=1	w=0.023        
             /// Component N	a=14	z=7	w=0.009        
             /// Component Cu	a=63.5	z=29	w=0.045        
             /// Component Fe	a=55.8	z=26	w=0.055        
             /// Component Ni	a=58.7	z=28	w=0.022        
             /// Component Pb	a=207	z=82	w=0.081        
             /// Component Sn	a=118.7	z=50	w=0.121        
             /// Mixture ConMix dens=1.816        
             {  AgMaterial &mix = AgMaterial::Get("Conmix");           
                   mix.Component("Si",28.1,14,0.122);           
                   mix.Component("O",16,8,0.225);           
                   mix.Component("C",12,6,0.297);           
                   mix.Component("H",1,1,0.023);           
                   mix.Component("N",14,7,0.009);           
                   mix.Component("Cu",63.5,29,0.045);           
                   mix.Component("Fe",55.8,26,0.055);           
                   mix.Component("Ni",58.7,28,0.022);           
                   mix.Component("Pb",207,82,0.081);           
                   mix.Component("Sn",118.7,50,0.121);           
                   mix.par("dens")=1.816;           
                   mix.lock();           
                   _material = mix;           
                   _material.lock();           
             }        
             /// Component Si	a=28.1	z=14	w=0.141        
             /// Component O	a=16	z=8	w=0.196        
             /// Component C	a=12	z=6	w=0.419        
             /// Component H	a=1	z=1	w=0.008        
             /// Component Cu	a=63.5	z=29	w=0.156        
             /// Component Pb	a=207	z=82	w=0.032        
             /// Component Sn	a=118.7	z=50	w=0.048        
             /// Mixture TermMix dens=2.55        
             {  AgMaterial &mix = AgMaterial::Get("Termmix");           
                   mix.Component("Si",28.1,14,0.141);           
                   mix.Component("O",16,8,0.196);           
                   mix.Component("C",12,6,0.419);           
                   mix.Component("H",1,1,0.008);           
                   mix.Component("Cu",63.5,29,0.156);           
                   mix.Component("Pb",207,82,0.032);           
                   mix.Component("Sn",118.7,50,0.048);           
                   mix.par("dens")=2.55;           
                   mix.lock();           
                   _material = mix;           
                   _material.lock();           
             }        
             /// Component Cu	a=63.5	z=29	w=0.63        
             /// Component Zn	a=65.4	z=30	w=0.37        
             /// Mixture Brass dens=8.4        
             {  AgMaterial &mix = AgMaterial::Get("Brass");           
                   mix.Component("Cu",63.5,29,0.63);           
                   mix.Component("Zn",65.4,30,0.37);           
                   mix.par("dens")=8.4;           
                   mix.lock();           
                   _material = mix;           
                   _material.lock();           
             }        
             /// Component Ar	a=39.95	z=18.	w=0.700        
             /// Component O	a=16.	z=8.	w=0.218        
             /// Component C	a=12.01	z=6.	w=0.082        
             /// Mixture ArCO2Mix isvol=1 dens=0.0018015        
             {  AgMaterial &mix = AgMaterial::Get("Arco2mix");           
                   mix.Component("Ar",39.95,18.,0.700);           
                   mix.Component("O",16.,8.,0.218);           
                   mix.Component("C",12.01,6.,0.082);           
                   mix.par("isvol")=1;           
                   mix.par("dens")=0.0018015;           
                   mix.lock();           
                   _material = mix;           
                   _material.lock();           
             }        
             /// USE fggg _index=1;        
             fggg.Use();        
             /// USE fgst config=fggg.fgstconfig;        
             fgst.Use("config",(Float_t)fggg.fgstconfig);        
             disklenz=volnomexdz+volsensdz+volgemdz+volreaddz+volelecdz;        
             fgtlenz = cooltubedz;        
             centerz = fgst.zdisca(1) +fgtlenz/2.0 -( volnomexdz+volsensdz/2.);        
             xx=acos(flatoutr/diskoutr);        
             flatang1=flatperpang-xx/degrad;        
             flatang2=flatperpang+xx/degrad;        
             _create = AgCreate("FGTM");        
             {           
                   AgShape myshape; // undefined shape           
                   ///Create FGTM           
                   Create("FGTM");            
             }        
             { AgPlacement place = AgPlacement("FGTM","IDSM");           
                   /// Add daughter volume FGTM to mother IDSM           
                   place.TranslateZ(centerz);           
                   /// Translate z = centerz           
                   _stacker -> Position( AgBlock::Find("FGTM"), place );           
             } // end placement of FGTM        
       }; // FgtdGeo3     
 }; // namespace FgtdGeo3  
 