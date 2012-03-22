#include "FpdmGeo2.h"  
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
 namespace FPDMGEO2 // $NMSPC  
 {     
       //     
       // ---------------------------------------------------------------------------------------------------     
          //  -----------------------------------------------------     
          /// @defgroup fmcg_doc     
          /// \class Fmcg_t     
          /// \brief User-defined structure     
          ///                             
          /// AgML structure members:     
          ///                             
          ///Float_t version;     
          ///Float_t chkvsim;     
          ///Float_t pbplate;     
          ///Int_t _index;     
          //     
          Fmcg_t fmcg;     
          //     
          //  -----------------------------------------------------     
          /// @defgroup fpos_doc     
          /// \class Fpos_t     
          /// \brief User-defined structure     
          ///                             
          /// AgML structure members:     
          ///                             
          ///Float_t imod;     
          ///Float_t itype;     
          ///Float_t x;     
          ///Float_t y;     
          ///Float_t z;     
          ///Float_t ay;     
          ///Int_t _index;     
          //     
          Fpos_t fpos;     
          //     
          //  -----------------------------------------------------     
          /// @defgroup fbxd_doc     
          /// \class Fbxd_t     
          /// \brief User-defined structure     
          ///                             
          /// AgML structure members:     
          ///                             
          ///Float_t type;     
          ///Float_t height;     
          ///Float_t depth;     
          ///Float_t nx;     
          ///Float_t ny;     
          ///Float_t xoffset;     
          ///Float_t zoffset;     
          ///Float_t psoffset;     
          ///Float_t smdoff;     
          ///Int_t _index;     
          //     
          Fbxd_t fbxd;     
          //     
          //  -----------------------------------------------------     
          /// @defgroup flgg_doc     
          /// \class Flgg_t     
          /// \brief User-defined structure     
          ///                             
          /// AgML structure members:     
          ///                             
          ///Float_t type;     
          ///Float_t width;     
          ///Float_t depth;     
          ///Float_t dgap;     
          ///Float_t althick;     
          ///Float_t phcathdz;     
          ///Float_t phcathr;     
          ///Float_t mumetdz;     
          ///Float_t mumetr;     
          ///Int_t _index;     
          //     
          Flgg_t flgg;     
          //     
          //  -----------------------------------------------------     
          /// @defgroup flgm_doc     
          /// \class Flgm_t     
          /// \brief User-defined structure     
          ///                             
          /// AgML structure members:     
          ///                             
          ///Int_t type;     
          ///Float_t density;     
          ///Float_t radlen;     
          ///Float_t pbcont;     
          ///Float_t critene;     
          ///Float_t molierer;     
          ///Int_t _index;     
          //     
          Flgm_t flgm;     
          //     
          //  -----------------------------------------------------     
          /// @defgroup pbpd_doc     
          /// \class Pbpd_t     
          /// \brief User-defined structure     
          ///                             
          /// AgML structure members:     
          ///                             
          ///Float_t z;     
          ///Float_t width;     
          ///Float_t height;     
          ///Float_t thick;     
          ///Int_t _index;     
          //     
          Pbpd_t pbpd;     
          //     
          //  -----------------------------------------------------     
          /// @defgroup fmxg_doc     
          /// \class Fmxg_t     
          /// \brief User-defined structure     
          ///                             
          /// AgML structure members:     
          ///                             
          ///Float_t version;     
          ///Float_t sapex;     
          ///Float_t sbase;     
          ///Float_t sgap;     
          ///Float_t nstrip;     
          ///Float_t g10width;     
          ///Float_t g10hgt;     
          ///Float_t g10thick;     
          ///Int_t _index;     
          //     
          Fmxg_t fmxg;     
          //     
          ///@addtogroup FpdmGeo2_vars     
          ///@{        
                Int_t chkvsim,imod,itype,type,pbplate;        
                //        
                /// Int_t chkvsim,imod,itype,type,pbplate        
          ///@}     
          ///@addtogroup FpdmGeo2_vars     
          ///@{        
                Int_t i,j,m;        
                //        
                /// Int_t i,j,m        
          ///@}     
          ///@addtogroup FpdmGeo2_vars     
          ///@{        
                Float_t xx,yy,zz,x1,y1,z1,ztot,rtot,wid,bwid,x0;        
                //        
                /// Float_t xx,yy,zz,x1,y1,z1,ztot,rtot,wid,bwid,x0        
          ///@}     
          ///@addtogroup FpdmGeo2_vars     
          ///@{        
                Float_t ztotsmd,wtotsmd,xxx,yyy,zzz,wsmd;        
                //        
                /// Float_t ztotsmd,wtotsmd,xxx,yyy,zzz,wsmd        
          ///@}     
          ///@addtogroup FpdmGeo2_vars     
          ///@{        
                Int_t n;        
                //        
                /// Int_t n        
          ///@}     
          ///@addtogroup FpdmGeo2_vars     
          ///@{        
                Array_t<Float_t> e(n);        
                /// e(n) : array of Float_t        
          ///@}     
          ///@addtogroup FpdmGeo2_vars     
          ///@{        
                Array_t<Float_t> rindex_pbg(n);        
                /// rindex_pbg(n) : array of Float_t        
          ///@}     
          ///@addtogroup FpdmGeo2_vars     
          ///@{        
                Array_t<Float_t> rindex_sirub(n);        
                /// rindex_sirub(n) : array of Float_t        
          ///@}     
          ///@addtogroup FpdmGeo2_vars     
          ///@{        
                Array_t<Float_t> rindex_phcath(n);        
                /// rindex_phcath(n) : array of Float_t        
          ///@}     
          ///@addtogroup FpdmGeo2_vars     
          ///@{        
                Array_t<Float_t> rindex_alm(n);        
                /// rindex_alm(n) : array of Float_t        
          ///@}     
          ///@addtogroup FpdmGeo2_vars     
          ///@{        
                Array_t<Float_t> rindex_mumet(n);        
                /// rindex_mumet(n) : array of Float_t        
          ///@}     
          ///@addtogroup FpdmGeo2_vars     
          ///@{        
                Array_t<Float_t> absco_pbg(n);        
                /// absco_pbg(n) : array of Float_t        
          ///@}     
          ///@addtogroup FpdmGeo2_vars     
          ///@{        
                Array_t<Float_t> absco_sirub(n);        
                /// absco_sirub(n) : array of Float_t        
          ///@}     
          ///@addtogroup FpdmGeo2_vars     
          ///@{        
                Array_t<Float_t> absco_phcath(n);        
                /// absco_phcath(n) : array of Float_t        
          ///@}     
          ///@addtogroup FpdmGeo2_vars     
          ///@{        
                Array_t<Float_t> absco_alm(n);        
                /// absco_alm(n) : array of Float_t        
          ///@}     
          ///@addtogroup FpdmGeo2_vars     
          ///@{        
                Array_t<Float_t> absco_mumet(n);        
                /// absco_mumet(n) : array of Float_t        
          ///@}     
          ///@addtogroup FpdmGeo2_vars     
          ///@{        
                Array_t<Float_t> effic_phcath(n);        
                /// effic_phcath(n) : array of Float_t        
          ///@}     
          ///@addtogroup FpdmGeo2_vars     
          ///@{        
                Array_t<Float_t> effic_all(n);        
                /// effic_all(n) : array of Float_t        
          ///@}     
       FpdmGeo2::FpdmGeo2()     
         : AgModule("FpdmGeo2"," is the Forward Pion Detector Modules GEOmetry ")     
       {        
       }     
          // ---------------------------------------------------------------------------------------------------     
          void FBOX::Block( AgCreate create )     
          {         
                ///@addtogroup FBOX_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Air            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Air");              
                            _material = mat;              
                      }           
                      /// Medium standard           
                      {  AgMedium med = AgMedium::CopyMedium("Standard");              
                            _medium = med;              
                      }           
                      { AgAttribute attr = AgAttribute("FBOX");              
                            attr.par("seen")=1;              
                            attr.par("colo")=2;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=bwid;              
                            shape.par("dy")=fbxd.height/2;              
                            shape.par("dz")=fbxd.depth/2;              
                            /// Shape Bbox dx=bwid dy=fbxd.height/2 dz=fbxd.depth/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FBOX;              
                            _stacker -> Build(this);              
                      }           
                      /// USE flgg type=1 ;           
                      flgg.Use("type",(Float_t)1 );           
                      /// USE flgm type=1 ;           
                      flgm.Use("type",(Int_t)1 );           
                      wid  =  flgg.width + flgg.dgap + flgg.althick*2;           
                      ztot = (flgg.depth + flgg.althick + flgg.mumetdz)/2.0;           
                      rtot = fbxd.nx*wid/2.0;           
                      bwid = rtot+fbxd.xoffset;           
                      x0 =  bwid - fbxd.xoffset - wid/2;           
                      y1 =  rtot - wid/2;           
                      z1 = -fbxd.depth/2 + fbxd.zoffset + ztot;           
                      /// Loop on i from 1 to fbxd.ny step=1           
                      for ( i=1; (1>0)? (i<=fbxd.ny):(i>=fbxd.ny); i+=1 )           
                      {              
                            x1=x0;              
                            /// Loop on j from 1 to fbxd.nx step=1              
                            for ( j=1; (1>0)? (j<=fbxd.nx):(j>=fbxd.nx); j+=1 )              
                            {                 
                                  _create = AgCreate("FLGT");                 
                                  {                    
                                        AgShape myshape; // undefined shape                    
                                        ///Create FLGT                    
                                        Create("FLGT");                     
                                  }                 
                                  { AgPlacement place = AgPlacement("FLGT","FBOX");                    
                                        /// Add daughter volume FLGT to mother FBOX                    
                                        place.TranslateX(x1);                    
                                        /// Translate x = x1                    
                                        place.TranslateY(y1);                    
                                        /// Translate y = y1                    
                                        place.TranslateZ(z1);                    
                                        /// Translate z = z1                    
                                        _stacker -> Position( AgBlock::Find("FLGT"), place );                    
                                  } // end placement of FLGT                 
                                  x1=x1-wid;                 
                            }              
                            y1=y1-wid;              
                      }           
                      if ( fbxd.type==1 )           
                      {              
                            x1=x0;              
                            y1= -rtot + ztot;              
                            z1=-fbxd.depth/2  + fbxd.psoffset + wid/2;              
                            /// Loop on j from 1 to fbxd.nx step=1              
                            for ( j=1; (1>0)? (j<=fbxd.nx):(j>=fbxd.nx); j+=1 )              
                            {                 
                                  _create = AgCreate("FLGT");                 
                                  {                    
                                        AgShape myshape; // undefined shape                    
                                        ///Create FLGT                    
                                        Create("FLGT");                     
                                  }                 
                                  { AgPlacement place = AgPlacement("FLGT","FBOX");                    
                                        /// Add daughter volume FLGT to mother FBOX                    
                                        place.TranslateX(x1);                    
                                        /// Translate x = x1                    
                                        place.TranslateY(y1);                    
                                        /// Translate y = y1                    
                                        place.TranslateZ(z1);                    
                                        /// Translate z = z1                    
                                        place.AlphaX(90);                    
                                        /// Rotate: AlphaX = 90                    
                                        /// G3 Reference: thetax = 90                    
                                        /// G3 Reference: phix = 0                    
                                        /// G3 Reference: thetay = 90                    
                                        /// G3 Reference: phiy = 90                    
                                        /// G3 Reference: thetaz = 0                    
                                        /// G3 Reference: phiz = 0                    
                                        _stacker -> Position( AgBlock::Find("FLGT"), place );                    
                                  } // end placement of FLGT                 
                                  x1=x1-wid;                 
                            }              
                            if ( fmcg.pbplate==1 )              
                            {                 
                                  _create = AgCreate("PBPT");                 
                                  {                    
                                        AgShape myshape; // undefined shape                    
                                        ///Create PBPT                    
                                        Create("PBPT");                     
                                  }                 
                                  { AgPlacement place = AgPlacement("PBPT","FBOX");                    
                                        /// Add daughter volume PBPT to mother FBOX                    
                                        place.TranslateX(0);                    
                                        /// Translate x = 0                    
                                        place.TranslateY(0);                    
                                        /// Translate y = 0                    
                                        place.TranslateZ(pbpd.thick/2.0-fbxd.depth/2);                    
                                        /// Translate z = pbpd.thick/2.0-fbxd.depth/2                    
                                        _stacker -> Position( AgBlock::Find("PBPT"), place );                    
                                  } // end placement of PBPT                 
                            }              
                            ztotsmd=fmxg.g10thick+fmxg.sapex;              
                            _create = AgCreate("FSHM");              
                            {                 
                                  AgShape myshape; // undefined shape                 
                                  ///Create FSHM                 
                                  Create("FSHM");                  
                            }              
                            { AgPlacement place = AgPlacement("FSHM","FBOX");                 
                                  /// Add daughter volume FSHM to mother FBOX                 
                                  place.TranslateX(0);                 
                                  /// Translate x = 0                 
                                  place.TranslateY(0);                 
                                  /// Translate y = 0                 
                                  place.TranslateZ(fbxd.smdoff+ztotsmd-fbxd.depth/2);                 
                                  /// Translate z = fbxd.smdoff+ztotsmd-fbxd.depth/2                 
                                  _stacker -> Position( AgBlock::Find("FSHM"), place );                 
                            } // end placement of FSHM              
                      }           
                      if ( fbxd.type==3 )           
                      {              
                            /// USE flgg type=2 ;              
                            flgg.Use("type",(Float_t)2 );              
                            /// USE flgm type=2 ;              
                            flgm.Use("type",(Int_t)2 );              
                            wid  = flgg.width + flgg.dgap;              
                            ztot = flgg.depth/2.0;              
                            rtot = 14*wid/2.0;              
                            bwid = rtot;              
                            x0 =  bwid - wid/2;              
                            y1 =  rtot - wid/2;              
                            z1 = -fbxd.depth/2 + fbxd.zoffset + ztot;              
                            /// Loop on i from 1 to 14 step=1              
                            for ( i=1; (1>0)? (i<=14):(i>=14); i+=1 )              
                            {                 
                                  x1=x0;                 
                                  /// Loop on j from 1 to 14 step=1                 
                                  for ( j=1; (1>0)? (j<=14):(j>=14); j+=1 )                 
                                  {                    
                                        if ( i>5&&i<10&&j>5&&j<10 )                    
                                        {                       
                                              x1=x1-wid;                       
                                        }                    
                                        else                    
                                        {                       
                                              _create = AgCreate("FLGF");                       
                                              {                          
                                                    AgShape myshape; // undefined shape                          
                                                    ///Create FLGF                          
                                                    Create("FLGF");                           
                                              }                       
                                              { AgPlacement place = AgPlacement("FLGF","FBOX");                          
                                                    /// Add daughter volume FLGF to mother FBOX                          
                                                    place.TranslateX(x1);                          
                                                    /// Translate x = x1                          
                                                    place.TranslateY(y1);                          
                                                    /// Translate y = y1                          
                                                    place.TranslateZ(z1);                          
                                                    /// Translate z = z1                          
                                                    _stacker -> Position( AgBlock::Find("FLGF"), place );                          
                                              } // end placement of FLGF                       
                                              x1=x1-wid;                       
                                        }                    
                                  }                 
                                  y1=y1-wid;                 
                            }              
                      }           
                      END_OF_FBOX:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FBOX     
          // ---------------------------------------------------------------------------------------------------     
          void FLGT::Block( AgCreate create )     
          {         
                ///@addtogroup FLGT_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Air            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Air");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("FLGT");              
                            attr.par("seen")=1;              
                            attr.par("colo")=2;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=wid/2;              
                            shape.par("dy")=wid/2;              
                            shape.par("dz")=ztot;              
                            /// Shape Bbox dx=wid/2 dy=wid/2 dz=ztot               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FLGT;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("FWAL");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FWAL              
                            Create("FWAL");               
                      }           
                      { AgPlacement place = AgPlacement("FWAL","FLGT");              
                            /// Add daughter volume FWAL to mother FLGT              
                            place.TranslateZ(-ztot+(flgg.althick+flgg.depth)/2.0);              
                            /// Translate z = -ztot+(flgg.althick+flgg.depth)/2.0              
                            _stacker -> Position( AgBlock::Find("FWAL"), place );              
                      } // end placement of FWAL           
                      _create = AgCreate("FUMT");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FUMT              
                            Create("FUMT");               
                      }           
                      { AgPlacement place = AgPlacement("FUMT","FLGT");              
                            /// Add daughter volume FUMT to mother FLGT              
                            place.TranslateZ(-ztot+flgg.althick+flgg.depth+flgg.mumetdz/2.0);              
                            /// Translate z = -ztot+flgg.althick+flgg.depth+flgg.mumetdz/2.0              
                            _stacker -> Position( AgBlock::Find("FUMT"), place );              
                      } // end placement of FUMT           
                      _create = AgCreate("FPCT");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FPCT              
                            Create("FPCT");               
                      }           
                      { AgPlacement place = AgPlacement("FPCT","FLGT");              
                            /// Add daughter volume FPCT to mother FLGT              
                            place.TranslateZ(-ztot+flgg.althick+flgg.depth+flgg.phcathdz/2.0);              
                            /// Translate z = -ztot+flgg.althick+flgg.depth+flgg.phcathdz/2.0              
                            _stacker -> Position( AgBlock::Find("FPCT"), place );              
                      } // end placement of FPCT           
                      END_OF_FLGT:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FLGT     
          // ---------------------------------------------------------------------------------------------------     
          void FWAL::Block( AgCreate create )     
          {         
                ///@addtogroup FWAL_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Aluminium            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Aluminium");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("FWAL");              
                            attr.par("seen")=1;              
                            attr.par("colo")=3;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=flgg.width/2.0+flgg.althick;              
                            shape.par("dy")=flgg.width/2.0+flgg.althick;              
                            shape.par("dz")=flgg.depth/2.0+flgg.althick/2.0;              
                            /// Shape Bbox dx=flgg.width/2.0+flgg.althick dy=flgg.width/2.0+flgg.althick dz=flgg.depth/2.0+flgg.althick/2.0               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FWAL;              
                            _stacker -> Build(this);              
                      }           
                      if ( fmcg.chkvsim==1 )           
                      {              
                            /*{                 
                                  GSCKOV( %imed,n,e,absco_alm,effic_all,rindex_alm );// CALL GSCKOV                 
                            }*/              
                      }           
                      _create = AgCreate("FLGR");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FLGR              
                            Create("FLGR");               
                      }           
                      { AgPlacement place = AgPlacement("FLGR","FWAL");              
                            /// Add daughter volume FLGR to mother FWAL              
                            place.TranslateZ(+flgg.althick/2.0);              
                            /// Translate z = +flgg.althick/2.0              
                            _stacker -> Position( AgBlock::Find("FLGR"), place );              
                      } // end placement of FLGR           
                      END_OF_FWAL:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FWAL     
          // ---------------------------------------------------------------------------------------------------     
          void FLGR::Block( AgCreate create )     
          {         
                ///@addtogroup FLGR_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Component Pb	a=207.19	z=82	w=.60712           
                      /// Component K	a=39.102	z=19	w=.02324           
                      /// Component Si	a=28.088	z=14	w=.14771           
                      /// Component O	a=15.999	z=8	w=.22041           
                      /// Component As	a=74.922	z=33	w=.00152           
                      /// Mixture PbG dens=flgm_density radl=flgm_radlen           
                      {  AgMaterial &mix = AgMaterial::Get("Pbg");              
                            mix.Component("Pb",207.19,82,.60712);              
                            mix.Component("K",39.102,19,.02324);              
                            mix.Component("Si",28.088,14,.14771);              
                            mix.Component("O",15.999,8,.22041);              
                            mix.Component("As",74.922,33,.00152);              
                            mix.par("dens")=flgm.density;              
                            mix.par("radl")=flgm.radlen;              
                            mix.lock();              
                            _material = mix;              
                            _material.lock();              
                      }           
                      /// Medium leadglass           
                      ///  isvol = 1            
                      {  AgMedium &med = AgMedium::Get("Leadglass");              
                               med.Inherit(this);              
                            med.par("isvol")=1 ;              
                            _medium = med;              
                      }           
                      { AgAttribute attr = AgAttribute("FLGR");              
                            attr.par("seen")=1;              
                            attr.par("colo")=4;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=flgg.width/2;              
                            shape.par("dy")=flgg.width/2;              
                            shape.par("dz")=flgg.depth/2;              
                            /// Shape Bbox dx=flgg.width/2 dy=flgg.width/2 dz=flgg.depth/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FLGR;              
                            _stacker -> Build(this);              
                      }           
                      if ( fmcg.chkvsim==1 )           
                      {              
                            /*{                 
                                  GSCKOV( %imed,n,e,absco_pbg,effic_all,rindex_pbg );// CALL GSCKOV                 
                            }*/              
                      }           
                      END_OF_FLGR:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FLGR     
          // ---------------------------------------------------------------------------------------------------     
          void FLGF::Block( AgCreate create )     
          {         
                ///@addtogroup FLGF_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Component Pb	a=207.19	z=82	w=.60712           
                      /// Component K	a=39.102	z=19	w=.02324           
                      /// Component Si	a=28.088	z=14	w=.14771           
                      /// Component O	a=15.999	z=8	w=.22041           
                      /// Component As	a=74.922	z=33	w=.00152           
                      /// Mixture PbG dens=flgm_density           
                      {  AgMaterial &mix = AgMaterial::Get("Pbg");              
                            mix.Component("Pb",207.19,82,.60712);              
                            mix.Component("K",39.102,19,.02324);              
                            mix.Component("Si",28.088,14,.14771);              
                            mix.Component("O",15.999,8,.22041);              
                            mix.Component("As",74.922,33,.00152);              
                            mix.par("dens")=flgm.density;              
                            mix.lock();              
                            _material = mix;              
                            _material.lock();              
                      }           
                      /// Medium leadglass           
                      ///  isvol = 1            
                      {  AgMedium &med = AgMedium::Get("Leadglass");              
                               med.Inherit(this);              
                            med.par("isvol")=1 ;              
                            _medium = med;              
                      }           
                      { AgAttribute attr = AgAttribute("FLGF");              
                            attr.par("seen")=1;              
                            attr.par("colo")=4;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=flgg.width/2;              
                            shape.par("dy")=flgg.width/2;              
                            shape.par("dz")=flgg.depth/2;              
                            /// Shape Bbox dx=flgg.width/2 dy=flgg.width/2 dz=flgg.depth/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FLGF;              
                            _stacker -> Build(this);              
                      }           
                      /*{              
                            GSTPAR( ag_imed,"cutele",flgm.critene );// CALL GSTPAR              
                      }*/           
                      END_OF_FLGF:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FLGF     
          // ---------------------------------------------------------------------------------------------------     
          void FPCT::Block( AgCreate create )     
          {         
                ///@addtogroup FPCT_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Air            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Air");              
                            _material = mat;              
                      }           
                      /// Medium PhotCath           
                      ///  isvol = 1            
                      {  AgMedium &med = AgMedium::Get("Photcath");              
                               med.Inherit(this);              
                            med.par("isvol")=1 ;              
                            _medium = med;              
                      }           
                      { AgAttribute attr = AgAttribute("LPCT");              
                            attr.par("seen")=1;              
                            attr.par("colo")=6;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=0;              
                            shape.par("rmax")=flgg.phcathr;              
                            shape.par("dz")=flgg.phcathdz/2.0;              
                            /// Shape Tube rmin=0 rmax=flgg.phcathr dz=flgg.phcathdz/2.0               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FPCT;              
                            _stacker -> Build(this);              
                      }           
                      if ( fmcg.chkvsim==1 )           
                      {              
                            /*{                 
                                  GSCKOV( %imed,n,e,absco_phcath,effic_phcath,rindex_phcath );// CALL GSCKOV                 
                            }*/              
                      }           
                      END_OF_FPCT:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FPCT     
          // ---------------------------------------------------------------------------------------------------     
          void FUMT::Block( AgCreate create )     
          {         
                ///@addtogroup FUMT_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material iron            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Iron");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("LUMT");              
                            attr.par("seen")=1;              
                            attr.par("colo")=5;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=flgg.phcathr;              
                            shape.par("rmax")=flgg.mumetr;              
                            shape.par("dz")=flgg.mumetdz/2.0;              
                            /// Shape Tube rmin=flgg.phcathr rmax=flgg.mumetr dz=flgg.mumetdz/2.0               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FUMT;              
                            _stacker -> Build(this);              
                      }           
                      if ( fmcg.chkvsim==1 )           
                      {              
                            /*{                 
                                  GSCKOV( %imed,n,e,absco_mumet,effic_all,rindex_mumet );// CALL GSCKOV                 
                            }*/              
                      }           
                      END_OF_FUMT:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FUMT     
          // ---------------------------------------------------------------------------------------------------     
          void PBPT::Block( AgCreate create )     
          {         
                ///@addtogroup PBPT_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Lead            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Lead");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("PBPT");              
                            attr.par("seen")=1;              
                            attr.par("colo")=7;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=pbpd.width/2.0;              
                            shape.par("dy")=pbpd.height/2.0;              
                            shape.par("dz")=pbpd.thick/2.0;              
                            /// Shape Bbox dx=pbpd.width/2.0 dy=pbpd.height/2.0 dz=pbpd.thick/2.0               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_PBPT;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_PBPT:           
                      mCurrent = _save;           
                ///@}        
          } // End Block PBPT     
          // ---------------------------------------------------------------------------------------------------     
          void FSHM::Block( AgCreate create )     
          {         
                ///@addtogroup FSHM_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Air            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Air");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("FSHM");              
                            attr.par("seen")=1;              
                            attr.par("colo")=4;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=fmxg.g10width/2;              
                            shape.par("dy")=fmxg.g10hgt/2;              
                            shape.par("dz")=ztotsmd;              
                            /// Shape Bbox dx=fmxg.g10width/2 dy=fmxg.g10hgt/2 dz=ztotsmd               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FSHM;              
                            _stacker -> Build(this);              
                      }           
                      wsmd=fmxg.sbase/2+fmxg.sgap;           
                      wtotsmd=(fmxg.nstrip+1)*wsmd;           
                      zzz=-ztotsmd+fmxg.g10thick/2;           
                      _create = AgCreate("FXGT");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FXGT              
                            Create("FXGT");               
                      }           
                      { AgPlacement place = AgPlacement("FXGT","FSHM");              
                            /// Add daughter volume FXGT to mother FSHM              
                            place.TranslateX(0);              
                            /// Translate x = 0              
                            place.TranslateY(0);              
                            /// Translate y = 0              
                            place.TranslateZ(zzz);              
                            /// Translate z = zzz              
                            _stacker -> Position( AgBlock::Find("FXGT"), place );              
                      } // end placement of FXGT           
                      xxx=-wtotsmd/2-fmxg.sgap/2+wsmd;           
                      yyy=0.0;           
                      zzz=zzz+fmxg.g10thick/2+fmxg.sapex/2;           
                      /// Loop on i from 1 to fmxg.nstrip step=1           
                      for ( i=1; (1>0)? (i<=fmxg.nstrip):(i>=fmxg.nstrip); i+=1 )           
                      {              
                            if ( mod(i,2)!=0 )              
                            {                 
                                  _create = AgCreate("FHMS");                 
                                  {                    
                                        AgShape myshape; // undefined shape                    
                                        ///Create FHMS                    
                                        Create("FHMS");                     
                                  }                 
                                  { AgPlacement place = AgPlacement("FHMS","FSHM");                    
                                        /// Add daughter volume FHMS to mother FSHM                    
                                        place.TranslateX(xxx);                    
                                        /// Translate x = xxx                    
                                        place.TranslateY(yyy);                    
                                        /// Translate y = yyy                    
                                        place.TranslateZ(zzz);                    
                                        /// Translate z = zzz                    
                                        _stacker -> Position( AgBlock::Find("FHMS"), place );                    
                                  } // end placement of FHMS                 
                            }              
                            else              
                            {                 
                                  _create = AgCreate("FHMS");                 
                                  {                    
                                        AgShape myshape; // undefined shape                    
                                        ///Create FHMS                    
                                        Create("FHMS");                     
                                  }                 
                                  { AgPlacement place = AgPlacement("FHMS","FSHM");                    
                                        /// Add daughter volume FHMS to mother FSHM                    
                                        place.TranslateX(xxx);                    
                                        /// Translate x = xxx                    
                                        place.TranslateY(yyy);                    
                                        /// Translate y = yyy                    
                                        place.TranslateZ(zzz);                    
                                        /// Translate z = zzz                    
                                        place.AlphaX(180);                    
                                        /// Rotate: AlphaX = 180                    
                                        /// G3 Reference: thetax = 90                    
                                        /// G3 Reference: phix = 0                    
                                        /// G3 Reference: thetay = 90                    
                                        /// G3 Reference: phiy = 90                    
                                        /// G3 Reference: thetaz = 0                    
                                        /// G3 Reference: phiz = 0                    
                                        _stacker -> Position( AgBlock::Find("FHMS"), place );                    
                                  } // end placement of FHMS                 
                            }              
                            xxx=xxx+wsmd;              
                      }           
                      zzz=zzz+fmxg.g10thick/2+fmxg.sapex/2;           
                      _create = AgCreate("FXGT");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FXGT              
                            Create("FXGT");               
                      }           
                      { AgPlacement place = AgPlacement("FXGT","FSHM");              
                            /// Add daughter volume FXGT to mother FSHM              
                            place.TranslateX(0);              
                            /// Translate x = 0              
                            place.TranslateY(0);              
                            /// Translate y = 0              
                            place.TranslateZ(zzz);              
                            /// Translate z = zzz              
                            _stacker -> Position( AgBlock::Find("FXGT"), place );              
                      } // end placement of FXGT           
                      xxx=0.0;           
                      yyy=-wtotsmd/2-fmxg.sgap/2+wsmd;           
                      zzz=zzz+fmxg.g10thick/2+fmxg.sapex/2;           
                      /// Loop on i from 1 to fmxg.nstrip step=1           
                      for ( i=1; (1>0)? (i<=fmxg.nstrip):(i>=fmxg.nstrip); i+=1 )           
                      {              
                            if ( mod(i,2)!=0 )              
                            {                 
                                  _create = AgCreate("FHMS");                 
                                  {                    
                                        AgShape myshape; // undefined shape                    
                                        ///Create FHMS                    
                                        Create("FHMS");                     
                                  }                 
                                  { AgPlacement place = AgPlacement("FHMS","FSHM");                    
                                        /// Add daughter volume FHMS to mother FSHM                    
                                        place.TranslateX(xxx);                    
                                        /// Translate x = xxx                    
                                        place.TranslateY(yyy);                    
                                        /// Translate y = yyy                    
                                        place.TranslateZ(zzz);                    
                                        /// Translate z = zzz                    
                                        place.AlphaZ(90);                    
                                        /// Rotate: AlphaZ = 90                    
                                        /// G3 Reference: thetax = 90                    
                                        /// G3 Reference: phix = 0                    
                                        /// G3 Reference: thetay = 90                    
                                        /// G3 Reference: phiy = 90                    
                                        /// G3 Reference: thetaz = 0                    
                                        /// G3 Reference: phiz = 0                    
                                        _stacker -> Position( AgBlock::Find("FHMS"), place );                    
                                  } // end placement of FHMS                 
                            }              
                            else              
                            {                 
                                  _create = AgCreate("FHMS");                 
                                  {                    
                                        AgShape myshape; // undefined shape                    
                                        ///Create FHMS                    
                                        Create("FHMS");                     
                                  }                 
                                  { AgPlacement place = AgPlacement("FHMS","FSHM");                    
                                        /// Add daughter volume FHMS to mother FSHM                    
                                        place.TranslateX(xxx);                    
                                        /// Translate x = xxx                    
                                        place.TranslateY(yyy);                    
                                        /// Translate y = yyy                    
                                        place.TranslateZ(zzz);                    
                                        /// Translate z = zzz                    
                                        /// G3 Reference: thetax = 90                    
                                        /// G3 Reference: phix = 0                    
                                        /// G3 Reference: thetay = 90                    
                                        /// G3 Reference: phiy = 90                    
                                        /// G3 Reference: thetaz = 0                    
                                        /// G3 Reference: phiz = 0                    
                                        place.Ortho( "YX-Z" ); // ORT=YX-Z                    
                                        /// Axis substitution: XYZ --> YX-Z                    
                                        _stacker -> Position( AgBlock::Find("FHMS"), place );                    
                                  } // end placement of FHMS                 
                            }              
                            yyy=yyy+wsmd;              
                      }           
                      END_OF_FSHM:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FSHM     
          // ---------------------------------------------------------------------------------------------------     
          void FXGT::Block( AgCreate create )     
          {         
                ///@addtogroup FXGT_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Component Si	a=28.08	z=14	w=0.6*1*28./60.           
                      /// Component O	a=16	z=8	w=0.6*2*16./60.           
                      /// Component C	a=12	z=6	w=0.4*8*12./174.           
                      /// Component H	a=1	z=1	w=0.4*14*1./174.           
                      /// Component O	a=16	z=8	w=0.4*4*16./174.           
                      /// Mixture g10 dens=1.7           
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
                      { AgAttribute attr = AgAttribute("FXGT");              
                            attr.par("seen")=1;              
                            attr.par("colo")=7;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=fmxg.g10width/2;              
                            shape.par("dy")=fmxg.g10hgt/2;              
                            shape.par("dz")=fmxg.g10thick/2;              
                            /// Shape Bbox dx=fmxg.g10width/2 dy=fmxg.g10hgt/2 dz=fmxg.g10thick/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FXGT;              
                            _stacker -> Build(this);              
                      }           
                      // _medium.par("CUTGAM") = 0.00001;           
                      // _medium.par("CUTELE") = 0.00001;           
                      END_OF_FXGT:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FXGT     
          // ---------------------------------------------------------------------------------------------------     
          void FHMS::Block( AgCreate create )     
          {         
                ///@addtogroup FHMS_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material POLYSTYREN            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Polystyren");              
                            _material = mat;              
                      }           
                      /// Material Cpolystyren isvol=1            
                      { AgMaterial &mat = AgMaterial::Get("Cpolystyren");              
                            mat.par("isvol")=1;              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("FHMS");              
                            attr.par("seen")=1;              
                            attr.par("colo")=2;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Trd1");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx1")=0;              
                            shape.par("dx2")=fmxg.sbase/2;              
                            shape.par("dy")=fmxg.g10hgt/2;              
                            shape.par("dz")=fmxg.sapex/2;              
                            /// Shape Trd1 dx1=0 dx2=fmxg.sbase/2 dy=fmxg.g10hgt/2 dz=fmxg.sapex/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FHMS;              
                            _stacker -> Build(this);              
                      }           
                      // _medium.par("CUTGAM") = 0.00008;           
                      // _medium.par("CUTELE") = 0.001;           
                      // _medium.par("BCUTE") = 0.0001;           
                      // _medium.par("BIRK1") = 1.;           
                      // _medium.par("BIRK2") = 0.0130;           
                      // _medium.par("BIRK3") = 9.6E-6;           
                      END_OF_FHMS:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FHMS     
    // ----------------------------------------------------------------------- geoctr
       void FpdmGeo2::ConstructGeometry( const Char_t *dummy )     
       {        
             ///@addtogroup FpdmGeo2_revision        
             ///@{           
                   /// Created:    12 Jun 2006            
             ///@}        
             ///@addtogroup FpdmGeo2_revision        
             ///@{           
                   /// Author: Akio Ogawa           
             ///@}        
             AddBlock("FBOX");        
             AddBlock("FLGT");        
             AddBlock("FLGF");        
             AddBlock("FWAL");        
             AddBlock("FLGR");        
             AddBlock("FPCT");        
             AddBlock("FUMT");        
             AddBlock("PBPT");        
             AddBlock("FSHM");        
             AddBlock("FHMS");        
             AddBlock("FXGT");        
             n=12;        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup fmcg_doc        
             ///@{           
                   ++fmcg._index;           
                   fmcg . version = 7.0; //  Geometry version             
                   /// fmcg . version = 7.0; //  Geometry version             
                   fmcg . chkvsim = 0; //  = 0 dE, = 1 Cherenkov simulation for PbG            
                   /// fmcg . chkvsim = 0; //  = 0 dE, = 1 Cherenkov simulation for PbG            
                   fmcg . pbplate = 0; //  =0 no plate, =1 with plate            
                   /// fmcg . pbplate = 0; //  =0 no plate, =1 with plate            
                   //           
                   fmcg.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup fpos_doc        
             ///@{           
                   ++fpos._index;           
                   fpos . imod = 1; //  Module# (EN=1, ES=2, ET=3, EB=4, WN=5...)            
                   /// fpos . imod = 1; //  Module# (EN=1, ES=2, ET=3, EB=4, WN=5...)            
                   fpos . itype = 1; //  Type (1=7*7+SMD+PreShower, 2=5*5, 3=14*14+6*6)            
                   /// fpos . itype = 1; //  Type (1=7*7+SMD+PreShower, 2=5*5, 3=14*14+6*6)            
                   fpos . x = -48.19; //  X distance from beam to edge of detector            
                   /// fpos . x = -48.19; //  X distance from beam to edge of detector            
                   fpos . y = 0.0; //  Y distance from beam to edge of detector            
                   /// fpos . y = 0.0; //  Y distance from beam to edge of detector            
                   fpos . z = -779.0; //  Z distance from IP to surface of detector            
                   /// fpos . z = -779.0; //  Z distance from IP to surface of detector            
                   fpos . ay = 180; //  Angle aroound Y (0 for west, 180 for east)            
                   /// fpos . ay = 180; //  Angle aroound Y (0 for west, 180 for east)            
                   //           
                   fpos.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup fpos_doc        
             ///@{           
                   ++fpos._index;           
                   fpos . imod = 2; //  Module# (EN=1, ES=2, ET=3, EB=4, WN=5...)            
                   /// fpos . imod = 2; //  Module# (EN=1, ES=2, ET=3, EB=4, WN=5...)            
                   fpos . itype = 1; //  Type (1=7*7+SMD+PreShower, 2=5*5, 3=14*14+6*6)            
                   /// fpos . itype = 1; //  Type (1=7*7+SMD+PreShower, 2=5*5, 3=14*14+6*6)            
                   fpos . x = 48.19; //  X distance from beam to edge of detector            
                   /// fpos . x = 48.19; //  X distance from beam to edge of detector            
                   fpos . y = 0.0; //  Y distance from beam to edge of detector            
                   /// fpos . y = 0.0; //  Y distance from beam to edge of detector            
                   fpos . z = -779.0; //  Z distance from IP to surface of detector            
                   /// fpos . z = -779.0; //  Z distance from IP to surface of detector            
                   fpos . ay = 180; //  Angle aroound Y (0 for west, 180 for east)            
                   /// fpos . ay = 180; //  Angle aroound Y (0 for west, 180 for east)            
                   //           
                   fpos.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup fpos_doc        
             ///@{           
                   ++fpos._index;           
                   fpos . imod = 3; //  Module# (EN=1, ES=2, ET=3, EB=4, WN=5...)            
                   /// fpos . imod = 3; //  Module# (EN=1, ES=2, ET=3, EB=4, WN=5...)            
                   fpos . itype = 2; //  Type (1=7*7+SMD+PreShower, 2=5*5, 3=14*14+6*6)            
                   /// fpos . itype = 2; //  Type (1=7*7+SMD+PreShower, 2=5*5, 3=14*14+6*6)            
                   fpos . x = 0.0; //  X distance from beam to edge of detector            
                   /// fpos . x = 0.0; //  X distance from beam to edge of detector            
                   fpos . y = 30.8; //  Y distance from beam to edge of detector            
                   /// fpos . y = 30.8; //  Y distance from beam to edge of detector            
                   fpos . z = -590.2; //  Z distance from IP to surface of detector            
                   /// fpos . z = -590.2; //  Z distance from IP to surface of detector            
                   fpos . ay = 180; //  Angle aroound Y (0 for west, 180 for east)            
                   /// fpos . ay = 180; //  Angle aroound Y (0 for west, 180 for east)            
                   //           
                   fpos.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup fpos_doc        
             ///@{           
                   ++fpos._index;           
                   fpos . imod = 4; //  Module# (EN=1, ES=2, ET=3, EB=4, WN=5...)            
                   /// fpos . imod = 4; //  Module# (EN=1, ES=2, ET=3, EB=4, WN=5...)            
                   fpos . itype = 2; //  Type (1=7*7+SMD+PreShower, 2=5*5, 3=14*14+6*6)            
                   /// fpos . itype = 2; //  Type (1=7*7+SMD+PreShower, 2=5*5, 3=14*14+6*6)            
                   fpos . x = 0.0; //  X distance from beam to edge of detector            
                   /// fpos . x = 0.0; //  X distance from beam to edge of detector            
                   fpos . y = -30.2; //  Y distance from beam to edge of detector            
                   /// fpos . y = -30.2; //  Y distance from beam to edge of detector            
                   fpos . z = -590.2; //  Z distance from IP to surface of detector            
                   /// fpos . z = -590.2; //  Z distance from IP to surface of detector            
                   fpos . ay = 180; //  Angle aroound Y (0 for west, 180 for east)            
                   /// fpos . ay = 180; //  Angle aroound Y (0 for west, 180 for east)            
                   //           
                   fpos.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup fpos_doc        
             ///@{           
                   ++fpos._index;           
                   fpos . imod = 5; //  Module# (EN=1, ES=2, ET=3, EB=4, WN=5...)            
                   /// fpos . imod = 5; //  Module# (EN=1, ES=2, ET=3, EB=4, WN=5...)            
                   fpos . itype = 3; //  Type (1=7*7+SMD+PreShower, 2=5*5, 3=14*14+6*6)            
                   /// fpos . itype = 3; //  Type (1=7*7+SMD+PreShower, 2=5*5, 3=14*14+6*6)            
                   fpos . x = -20.00; //  X distance from beam to edge of detector            
                   /// fpos . x = -20.00; //  X distance from beam to edge of detector            
                   fpos . y = 0.0; //  Y distance from beam to edge of detector            
                   /// fpos . y = 0.0; //  Y distance from beam to edge of detector            
                   fpos . z = 700.0; //  Z distance from IP to surface of detector            
                   /// fpos . z = 700.0; //  Z distance from IP to surface of detector            
                   fpos . ay = 0; //  Angle aroound Y (0 for west, 180 for east)            
                   /// fpos . ay = 0; //  Angle aroound Y (0 for west, 180 for east)            
                   //           
                   fpos.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup fpos_doc        
             ///@{           
                   ++fpos._index;           
                   fpos . imod = 6; //  Module# (EN=1, ES=2, ET=3, EB=4, WN=5...)            
                   /// fpos . imod = 6; //  Module# (EN=1, ES=2, ET=3, EB=4, WN=5...)            
                   fpos . itype = 3; //  Type (1=7*7+SMD+PreShower, 2=5*5, 3=14*14+6*6)            
                   /// fpos . itype = 3; //  Type (1=7*7+SMD+PreShower, 2=5*5, 3=14*14+6*6)            
                   fpos . x = 20.00; //  X distance from beam to edge of detector            
                   /// fpos . x = 20.00; //  X distance from beam to edge of detector            
                   fpos . y = 0.0; //  Y distance from beam to edge of detector            
                   /// fpos . y = 0.0; //  Y distance from beam to edge of detector            
                   fpos . z = 700.0; //  Z distance from IP to surface of detector            
                   /// fpos . z = 700.0; //  Z distance from IP to surface of detector            
                   fpos . ay = 0; //  Angle aroound Y (0 for west, 180 for east)            
                   /// fpos . ay = 0; //  Angle aroound Y (0 for west, 180 for east)            
                   //           
                   fpos.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup fpos_doc        
             ///@{           
                   ++fpos._index;           
                   fpos . imod = 7; //  Module# (EN=1, ES=2, ET=3, EB=4, WN=5...)            
                   /// fpos . imod = 7; //  Module# (EN=1, ES=2, ET=3, EB=4, WN=5...)            
                   fpos . itype = 2; //  Type (1=7*7+SMD+PreShower, 2=5*5, 3=14*14+6*6)            
                   /// fpos . itype = 2; //  Type (1=7*7+SMD+PreShower, 2=5*5, 3=14*14+6*6)            
                   fpos . x = 0.0; //  X distance from beam to edge of detector            
                   /// fpos . x = 0.0; //  X distance from beam to edge of detector            
                   fpos . y = 30.8; //  Y distance from beam to edge of detector            
                   /// fpos . y = 30.8; //  Y distance from beam to edge of detector            
                   fpos . z = 590.2; //  Z distance from IP to surface of detector            
                   /// fpos . z = 590.2; //  Z distance from IP to surface of detector            
                   fpos . ay = 0; //  Angle aroound Y (0 for west, 180 for east)            
                   /// fpos . ay = 0; //  Angle aroound Y (0 for west, 180 for east)            
                   //           
                   fpos.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup fpos_doc        
             ///@{           
                   ++fpos._index;           
                   fpos . imod = 8; //  Module# (EN=1, ES=2, ET=3, EB=4, WN=5...)            
                   /// fpos . imod = 8; //  Module# (EN=1, ES=2, ET=3, EB=4, WN=5...)            
                   fpos . itype = 2; //  Type (1=7*7+SMD+PreShower, 2=5*5, 3=14*14+6*6)            
                   /// fpos . itype = 2; //  Type (1=7*7+SMD+PreShower, 2=5*5, 3=14*14+6*6)            
                   fpos . x = 0.0; //  X distance from beam to edge of detector            
                   /// fpos . x = 0.0; //  X distance from beam to edge of detector            
                   fpos . y = -30.2; //  Y distance from beam to edge of detector            
                   /// fpos . y = -30.2; //  Y distance from beam to edge of detector            
                   fpos . z = 590.2; //  Z distance from IP to surface of detector            
                   /// fpos . z = 590.2; //  Z distance from IP to surface of detector            
                   fpos . ay = 0; //  Angle aroound Y (0 for west, 180 for east)            
                   /// fpos . ay = 0; //  Angle aroound Y (0 for west, 180 for east)            
                   //           
                   fpos.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup fbxd_doc        
             ///@{           
                   ++fbxd._index;           
                   fbxd . type = 1; //  Type (1=7*7+SMD+PreShower, 2=5*5, 3=14*14+6*6)            
                   /// fbxd . type = 1; //  Type (1=7*7+SMD+PreShower, 2=5*5, 3=14*14+6*6)            
                   fbxd . height = 100; //  Box height            
                   /// fbxd . height = 100; //  Box height            
                   fbxd . depth = 96; //  Box Depth            
                   /// fbxd . depth = 96; //  Box Depth            
                   fbxd . nx = 7; //  Number of pbg in x            
                   /// fbxd . nx = 7; //  Number of pbg in x            
                   fbxd . ny = 7; //  number of pbg in y            
                   /// fbxd . ny = 7; //  number of pbg in y            
                   fbxd . xoffset = 2.54; //  tower x offset from box edge to PbG edge            
                   /// fbxd . xoffset = 2.54; //  tower x offset from box edge to PbG edge            
                   fbxd . zoffset = 19; //  tower z offset from box edge to PbG edge                  
                   /// fbxd . zoffset = 19; //  tower z offset from box edge to PbG edge                  
                   fbxd . psoffset = 2.0; //  PreShower z offset from box edge to PbG edge            
                   /// fbxd . psoffset = 2.0; //  PreShower z offset from box edge to PbG edge            
                   fbxd . smdoff = 8.0; //  SMD V-plane z offset from box edge            
                   /// fbxd . smdoff = 8.0; //  SMD V-plane z offset from box edge            
                   //           
                   fbxd.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup fbxd_doc        
             ///@{           
                   ++fbxd._index;           
                   fbxd . type = 2; //  Type (1=7*7+SMD+PreShower, 2=5*5, 3=14*14+6*6)            
                   /// fbxd . type = 2; //  Type (1=7*7+SMD+PreShower, 2=5*5, 3=14*14+6*6)            
                   fbxd . height = 20; //  Box height            
                   /// fbxd . height = 20; //  Box height            
                   fbxd . depth = 65; //  Box Depth            
                   /// fbxd . depth = 65; //  Box Depth            
                   fbxd . nx = 5; //  Number of pbg in x            
                   /// fbxd . nx = 5; //  Number of pbg in x            
                   fbxd . ny = 5; //  number of pbg in y            
                   /// fbxd . ny = 5; //  number of pbg in y            
                   fbxd . xoffset = 0.0; //  tower x offset from box edge to PbG edge            
                   /// fbxd . xoffset = 0.0; //  tower x offset from box edge to PbG edge            
                   fbxd . zoffset = 1; //  tower z offset from box edge to PbG edge            
                   /// fbxd . zoffset = 1; //  tower z offset from box edge to PbG edge            
                   fbxd . psoffset = 0; //  PreShower z offset from box edge to PbG edge            
                   /// fbxd . psoffset = 0; //  PreShower z offset from box edge to PbG edge            
                   fbxd . smdoff = 0.0; //  SMD z offset from box edge            
                   /// fbxd . smdoff = 0.0; //  SMD z offset from box edge            
                   //           
                   fbxd.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup fbxd_doc        
             ///@{           
                   ++fbxd._index;           
                   fbxd . type = 3; //  Type (1=7*7+SMD+PreShower, 2=5*5, 3=14*14+6*6)            
                   /// fbxd . type = 3; //  Type (1=7*7+SMD+PreShower, 2=5*5, 3=14*14+6*6)            
                   fbxd . height = 82; //  Box height            
                   /// fbxd . height = 82; //  Box height            
                   fbxd . depth = 82; //  Box Depth            
                   /// fbxd . depth = 82; //  Box Depth            
                   fbxd . nx = 6; //  Number of pbg in x            
                   /// fbxd . nx = 6; //  Number of pbg in x            
                   fbxd . ny = 6; //  number of pbg in y            
                   /// fbxd . ny = 6; //  number of pbg in y            
                   fbxd . xoffset = 29.0; //  tower x offset from box edge to PbG edge            
                   /// fbxd . xoffset = 29.0; //  tower x offset from box edge to PbG edge            
                   fbxd . zoffset = 1; //  tower z offset from box edge to PbG edge            
                   /// fbxd . zoffset = 1; //  tower z offset from box edge to PbG edge            
                   fbxd . psoffset = 0; //  PreShower z offset from box edge to PbG edge            
                   /// fbxd . psoffset = 0; //  PreShower z offset from box edge to PbG edge            
                   fbxd . smdoff = 0.0; //  SMD z offset from box edge            
                   /// fbxd . smdoff = 0.0; //  SMD z offset from box edge            
                   //           
                   fbxd.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup flgg_doc        
             ///@{           
                   ++flgg._index;           
                   flgg . type = 1; //  Type (1=Protovino Cell, 2=FLab Cell)            
                   /// flgg . type = 1; //  Type (1=Protovino Cell, 2=FLab Cell)            
                   flgg . width = 3.81; //  PbG width             
                   /// flgg . width = 3.81; //  PbG width             
                   flgg . depth = 45.0; //  PbG depth            
                   /// flgg . depth = 45.0; //  PbG depth            
                   flgg . dgap = 0.01; //  Gap between PbG            
                   /// flgg . dgap = 0.01; //  Gap between PbG            
                   flgg . althick = 0.001; //  almunim wrap thickness (real)            
                   /// flgg . althick = 0.001; //  almunim wrap thickness (real)            
                   flgg . phcathdz = 2.0; //  Photo Cathode thickness            
                   /// flgg . phcathdz = 2.0; //  Photo Cathode thickness            
                   flgg . phcathr = 1.8; //  Photo Cathode radius  (real)            
                   /// flgg . phcathr = 1.8; //  Photo Cathode radius  (real)            
                   flgg . mumetdz = 11.0; //  Mu Metal Length            
                   /// flgg . mumetdz = 11.0; //  Mu Metal Length            
                   flgg . mumetr = 1.9; //  Mu metal outer Radius  (real)            
                   /// flgg . mumetr = 1.9; //  Mu metal outer Radius  (real)            
                   //           
                   flgg.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup flgg_doc        
             ///@{           
                   ++flgg._index;           
                   flgg . type = 2; //  Type (1=Protovino Cell, 2=FLab Cell)            
                   /// flgg . type = 2; //  Type (1=Protovino Cell, 2=FLab Cell)            
                   flgg . width = 5.8; //  PbG width             
                   /// flgg . width = 5.8; //  PbG width             
                   flgg . depth = 60.2; //  PbG depth            
                   /// flgg . depth = 60.2; //  PbG depth            
                   flgg . dgap = 0.01; //  Gap between PbG            
                   /// flgg . dgap = 0.01; //  Gap between PbG            
                   flgg . althick = 0.001; //  almunim wrap thickness (real)            
                   /// flgg . althick = 0.001; //  almunim wrap thickness (real)            
                   flgg . phcathdz = 2.0; //  Photo Cathode thickness            
                   /// flgg . phcathdz = 2.0; //  Photo Cathode thickness            
                   flgg . phcathr = 1.8; //  Photo Cathode radius  (real)            
                   /// flgg . phcathr = 1.8; //  Photo Cathode radius  (real)            
                   flgg . mumetdz = 11.0; //  Mu Metal Length            
                   /// flgg . mumetdz = 11.0; //  Mu Metal Length            
                   flgg . mumetr = 1.9; //  Mu metal outer Radius  (real)            
                   /// flgg . mumetr = 1.9; //  Mu metal outer Radius  (real)            
                   //           
                   flgg.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup flgm_doc        
             ///@{           
                   ++flgm._index;           
                   flgm . type = 1; //  Type (1=Protovino Cell, 2=FLab Cell)            
                   /// flgm . type = 1; //  Type (1=Protovino Cell, 2=FLab Cell)            
                   flgm . density = 3.86; //  gdensity [/cm^3]            
                   /// flgm . density = 3.86; //  gdensity [/cm^3]            
                   flgm . radlen = 2.5; //  radiation length [cm]            
                   /// flgm . radlen = 2.5; //  radiation length [cm]            
                   flgm . pbcont = 65.4; //  PbO content [%]            
                   /// flgm . pbcont = 65.4; //  PbO content [%]            
                   flgm . critene = 0.0158; //  critical energy [GeV]            
                   /// flgm . critene = 0.0158; //  critical energy [GeV]            
                   flgm . molierer = 3.32; //  Moliere radius [cm]            
                   /// flgm . molierer = 3.32; //  Moliere radius [cm]            
                   //           
                   flgm.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup flgm_doc        
             ///@{           
                   ++flgm._index;           
                   flgm . type = 2; //  Type (1=Protovino Cell, 2=FLab Cell)            
                   /// flgm . type = 2; //  Type (1=Protovino Cell, 2=FLab Cell)            
                   flgm . density = 3.61; //  gdensity [/cm^3]            
                   /// flgm . density = 3.61; //  gdensity [/cm^3]            
                   flgm . radlen = 2.5; //  radiation length [cm]            
                   /// flgm . radlen = 2.5; //  radiation length [cm]            
                   flgm . pbcont = 65.4; //  PbO content [%]            
                   /// flgm . pbcont = 65.4; //  PbO content [%]            
                   flgm . critene = 0.0158; //  critical energy [GeV]            
                   /// flgm . critene = 0.0158; //  critical energy [GeV]            
                   flgm . molierer = 3.32; //  Moliere radius [cm]            
                   /// flgm . molierer = 3.32; //  Moliere radius [cm]            
                   //           
                   flgm.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup pbpd_doc        
             ///@{           
                   ++pbpd._index;           
                   pbpd . width = 33.02; //  Width            
                   /// pbpd . width = 33.02; //  Width            
                   pbpd . height = 33.02; //  Height            
                   /// pbpd . height = 33.02; //  Height            
                   pbpd . thick = 1.27; //  Thickness            
                   /// pbpd . thick = 1.27; //  Thickness            
                   //           
                   pbpd.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup fmxg_doc        
             ///@{           
                   ++fmxg._index;           
                   fmxg . version = 2; //  Geometry version            
                   /// fmxg . version = 2; //  Geometry version            
                   fmxg . sapex = 0.7; //  Scintillator strip apex            
                   /// fmxg . sapex = 0.7; //  Scintillator strip apex            
                   fmxg . sbase = 1.0; //  Scintillator strip base            
                   /// fmxg . sbase = 1.0; //  Scintillator strip base            
                   fmxg . sgap = 0.0064; //  Gap between strips            
                   /// fmxg . sgap = 0.0064; //  Gap between strips            
                   fmxg . nstrip = 50; //  # of strips            
                   /// fmxg . nstrip = 50; //  # of strips            
                   fmxg . g10width = 27.0; //  G10 plate width            
                   /// fmxg . g10width = 27.0; //  G10 plate width            
                   fmxg . g10hgt = 27.0; //  G10 plate height            
                   /// fmxg . g10hgt = 27.0; //  G10 plate height            
                   fmxg . g10thick = 0.15; //  G10 plate thickness            
                   /// fmxg . g10thick = 0.15; //  G10 plate thickness            
                   //           
                   fmxg.fill();           
             ///@}        
             //        
             /// USE fmcg _index=1;        
             fmcg.Use();        
             /// Loop on m from 1 to 8 step=1        
             for ( m=1; (1>0)? (m<=8):(m>=8); m+=1 )        
             {           
                   /// USE fpos imod=m ;           
                   fpos.Use("imod",(Float_t)m );           
                   /// USE fbxd type=fpos.itype ;           
                   fbxd.Use("type",(Float_t)fpos.itype );           
                   if ( fbxd.type<3 )           
                   {              
                         /// USE flgg type=1 ;              
                         flgg.Use("type",(Float_t)1 );              
                         wid  =  flgg.width + flgg.dgap + flgg.althick*2;              
                         ztot = (flgg.depth + flgg.althick + flgg.mumetdz)/2.0;              
                         rtot = fbxd.nx*wid/2.0;              
                         bwid = rtot+fbxd.xoffset;              
                   }           
                   else           
                   {              
                         /// USE flgg type=2 ;              
                         flgg.Use("type",(Float_t)2 );              
                         wid  =  flgg.width + flgg.dgap + flgg.althick*2;              
                         ztot = (flgg.depth + flgg.althick + flgg.mumetdz)/2.0;              
                         rtot = 14*wid/2.0;              
                         bwid = rtot;              
                   }           
                   if ( fpos.x>0.0 )           
                   {              
                         xx=fpos.x+bwid;              
                   }           
                   else if ( fpos.x==0.0 )           
                   {              
                         xx=0.0;              
                   }           
                   else           
                   {              
                         xx=fpos.x-bwid;              
                   }           
                   if ( fpos.y>0.0 )           
                   {              
                         yy=fpos.y+fbxd.height/2.0;              
                   }           
                   else if ( fpos.y==0.0 )           
                   {              
                         yy=0.0;              
                   }           
                   else           
                   {              
                         yy=fpos.y-fbxd.height/2.0;              
                   }           
                   if ( fpos.z>0.0 )           
                   {              
                         zz=fpos.z+fbxd.depth/2.0;              
                   }           
                   else           
                   {              
                         zz=fpos.z-fbxd.depth/2.0;              
                   }           
                   _create = AgCreate("FBOX");           
                   {              
                         AgShape myshape; // undefined shape              
                         ///Create FBOX              
                         Create("FBOX");               
                   }           
                   { AgPlacement place = AgPlacement("FBOX","CAVE");              
                         /// Add daughter volume FBOX to mother CAVE              
                         place.TranslateX(xx);              
                         /// Translate x = xx              
                         place.TranslateY(yy);              
                         /// Translate y = yy              
                         place.TranslateZ(zz);              
                         /// Translate z = zz              
                         place.AlphaY(fpos.ay);              
                         /// Rotate: AlphaY = fpos.ay              
                         /// G3 Reference: thetax = 90              
                         /// G3 Reference: phix = 0              
                         /// G3 Reference: thetay = 90              
                         /// G3 Reference: phiy = 90              
                         /// G3 Reference: thetaz = 0              
                         /// G3 Reference: phiz = 0              
                         _stacker -> Position( AgBlock::Find("FBOX"), place );              
                   } // end placement of FBOX           
             }        
       }; // FpdmGeo2     
 }; // namespace FpdmGeo2  
 