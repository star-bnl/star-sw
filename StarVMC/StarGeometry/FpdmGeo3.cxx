#include "FpdmGeo3.h"  
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
 namespace FPDMGEO3 // $NMSPC  
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
          ///Float_t fmsnorthx;     
          ///Float_t fmssouthx;     
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
          ///Float_t az;     
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
          ///Float_t width;     
          ///Float_t nx;     
          ///Float_t ny;     
          ///Float_t nxl;     
          ///Float_t nyl;     
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
          ///Float_t type;     
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
          //  -----------------------------------------------------     
          /// @defgroup inse_doc     
          /// \class Inse_t     
          /// \brief User-defined structure     
          ///                             
          /// AgML structure members:     
          ///                             
          ///Float_t width;     
          ///Float_t depth;     
          ///Float_t height;     
          ///Float_t sheetdpt;     
          ///Float_t holegap;     
          ///Float_t holedepth;     
          ///Float_t holeheight;     
          ///Float_t gapdepth;     
          ///Float_t gapheight;     
          ///Float_t gatedepth;     
          ///Float_t ra;     
          ///Float_t rb;     
          ///Float_t diam;     
          ///Float_t rmax;     
          ///Float_t gategap;     
          ///Int_t _index;     
          //     
          Inse_t inse;     
          //     
          ///@addtogroup FpdmGeo3_vars     
          ///@{        
                Int_t chkvsim,imod,itype,type,pbplate;        
                //        
                /// Int_t chkvsim,imod,itype,type,pbplate        
          ///@}     
          ///@addtogroup FpdmGeo3_vars     
          ///@{        
                Int_t i,j,m,sern;        
                //        
                /// Int_t i,j,m,sern        
          ///@}     
          ///@addtogroup FpdmGeo3_vars     
          ///@{        
                Float_t xx,yy,zz,x1,y1,z1,ztot,rtot,wid,widx,widy,bwid,x0,widl;        
                //        
                /// Float_t xx,yy,zz,x1,y1,z1,ztot,rtot,wid,widx,widy,bwid,x0,widl        
          ///@}     
          ///@addtogroup FpdmGeo3_vars     
          ///@{        
                Float_t ztotsmd,wtotsmd,zsmd,zsmd2,wsmd;        
                //        
                /// Float_t ztotsmd,wtotsmd,zsmd,zsmd2,wsmd        
          ///@}     
          ///@addtogroup FpdmGeo3_vars     
          ///@{        
                Float_t xsmdh,ysmdh,zsmdh,xsmdv,ysmdv,zsmdv;        
                //        
                /// Float_t xsmdh,ysmdh,zsmdh,xsmdv,ysmdv,zsmdv        
          ///@}     
          ///@addtogroup FpdmGeo3_vars     
          ///@{        
                Float_t xlcoffset,bzoffset;        
                //        
                /// Float_t xlcoffset,bzoffset        
          ///@}     
          ///@addtogroup FpdmGeo3_vars     
          ///@{        
                Float_t basewidth,distancer,xofffecc,xofffedc,xshift;        
                //        
                /// Float_t basewidth,distancer,xofffecc,xofffedc,xshift        
          ///@}     
          ///@addtogroup FpdmGeo3_vars     
          ///@{        
                Float_t xofffenc,yofffenc,zofffenc,zofffecc;        
                //        
                /// Float_t xofffenc,yofffenc,zofffenc,zofffecc        
          ///@}     
          ///@addtogroup FpdmGeo3_vars     
          ///@{        
                Array_t<Float_t> tmp(7);        
                /// tmp(7) : array of Float_t        
          ///@}     
          ///@addtogroup FpdmGeo3_vars     
          ///@{        
                Int_t n;        
                //        
                /// Int_t n        
          ///@}     
          ///@addtogroup FpdmGeo3_vars     
          ///@{        
                Array_t<Float_t> e(n);        
                /// e(n) : array of Float_t        
          ///@}     
          ///@addtogroup FpdmGeo3_vars     
          ///@{        
                Array_t<Float_t> rindex_pbg(n);        
                /// rindex_pbg(n) : array of Float_t        
          ///@}     
          ///@addtogroup FpdmGeo3_vars     
          ///@{        
                Array_t<Float_t> rindex_sirub(n);        
                /// rindex_sirub(n) : array of Float_t        
          ///@}     
          ///@addtogroup FpdmGeo3_vars     
          ///@{        
                Array_t<Float_t> rindex_phcath(n);        
                /// rindex_phcath(n) : array of Float_t        
          ///@}     
          ///@addtogroup FpdmGeo3_vars     
          ///@{        
                Array_t<Float_t> rindex_alm(n);        
                /// rindex_alm(n) : array of Float_t        
          ///@}     
          ///@addtogroup FpdmGeo3_vars     
          ///@{        
                Array_t<Float_t> rindex_mumet(n);        
                /// rindex_mumet(n) : array of Float_t        
          ///@}     
          ///@addtogroup FpdmGeo3_vars     
          ///@{        
                Array_t<Float_t> absco_pbg(n);        
                /// absco_pbg(n) : array of Float_t        
          ///@}     
          ///@addtogroup FpdmGeo3_vars     
          ///@{        
                Array_t<Float_t> absco_sirub(n);        
                /// absco_sirub(n) : array of Float_t        
          ///@}     
          ///@addtogroup FpdmGeo3_vars     
          ///@{        
                Array_t<Float_t> absco_phcath(n);        
                /// absco_phcath(n) : array of Float_t        
          ///@}     
          ///@addtogroup FpdmGeo3_vars     
          ///@{        
                Array_t<Float_t> absco_alm(n);        
                /// absco_alm(n) : array of Float_t        
          ///@}     
          ///@addtogroup FpdmGeo3_vars     
          ///@{        
                Array_t<Float_t> absco_mumet(n);        
                /// absco_mumet(n) : array of Float_t        
          ///@}     
          ///@addtogroup FpdmGeo3_vars     
          ///@{        
                Array_t<Float_t> effic_phcath(n);        
                /// effic_phcath(n) : array of Float_t        
          ///@}     
          ///@addtogroup FpdmGeo3_vars     
          ///@{        
                Array_t<Float_t> effic_all(n);        
                /// effic_all(n) : array of Float_t        
          ///@}     
       FpdmGeo3::FpdmGeo3()     
         : AgModule("FpdmGeo3"," is the Forward Pion Detector Modules GEOmetry ")     
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
                            attr.par("serial")=sern;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      if ( fbxd.type==2 )           
                      {              
                            {  AgShape shape = AgShape("Bbox");                 
                                  shape     .Inherit( AgBlock::previous() );                 
                                  create     .SetParameters(shape);                 
                                  shape.par("dx")=fbxd.width/2.0;                 
                                  shape.par("dy")=fbxd.height/2.0;                 
                                  shape.par("dz")=fbxd.depth/2.0;                 
                                  /// Shape Bbox dx=fbxd.width/2.0 dy=fbxd.height/2.0 dz=fbxd.depth/2.0                  
                                  _same_shape &= _stacker->SearchVolume( shape, _attribute );                 
                                  _shape = shape;                 
                                  if (_same_shape) goto END_OF_FBOX;                 
                                  _stacker -> Build(this);                 
                            }              
                      }           
                      else           
                      {              
                            {  AgShape shape = AgShape("Bbox");                 
                                  shape     .Inherit( AgBlock::previous() );                 
                                  create     .SetParameters(shape);                 
                                  shape.par("dx")=bwid;                 
                                  shape.par("dy")=fbxd.height/2.0;                 
                                  shape.par("dz")=fbxd.depth/2.0;                 
                                  /// Shape Bbox dx=bwid dy=fbxd.height/2.0 dz=fbxd.depth/2.0                  
                                  _same_shape &= _stacker->SearchVolume( shape, _attribute );                 
                                  _shape = shape;                 
                                  if (_same_shape) goto END_OF_FBOX;                 
                                  _stacker -> Build(this);                 
                            }              
                      }           
                      /// USE flgg type=2 ;           
                      flgg.Use("type",(Float_t)2 );           
                      widl = flgg.width + flgg.dgap + flgg.althick*2.0              ;//large cell width;           
                      /// USE flgg type=1 ;           
                      flgg.Use("type",(Float_t)1 );           
                      /// USE flgm type=1 ;           
                      flgm.Use("type",(Float_t)1 );           
                      wid  =  flgg.width + flgg.dgap + flgg.althick*2.0             ;//small cell width;           
                      ztot = (flgg.depth + flgg.althick + flgg.mumetdz)/2.0;           
                      rtot = fbxd.nx*wid/2.0;           
                      bwid = rtot-fbxd.xoffset;           
                      _create = AgCreate("FTOW");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FTOW              
                            Create("FTOW");               
                      }           
                      _create = AgCreate("PBPT");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create PBPT              
                            Create("PBPT");               
                      }           
                      _create = AgCreate("FSHM");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FSHM              
                            Create("FSHM");               
                      }           
                      if ( fbxd.type==2&&fpos.imod==4 )           
                      {              
                            x0 = - rtot - fbxd.xoffset + wid/2.0                        ;// x0 start from north (near beam) for;              
                            widx = wid                                                ;// ws (fms-south) module;              
                      }           
                      else           
                      {              
                            x0 =  rtot + fbxd.xoffset - wid/2.0                         ;// x0 start from south for north/top/bottom module;              
                            widx = -wid                                               ;// since fpd is symmetric, true for es module too;              
                      }           
                      if ( fbxd.type==2 )           
                      {              
                            y1 =  fbxd.ny*wid/2.0 - wid/2.0 + (16*widl-fbxd.ny*wid)/2.0   ;//in order to correct the differences between;              
                            widy = wid+(16.0*widl-fbxd.ny*wid)/23.0                       ;//3 small cells and 2 large ones (see again in 17 lines);              
                      }           
                      else           
                      {              
                            y1 =  fbxd.ny*wid/2.0 - wid/2.0;              
                            widy = wid;              
                      }           
                      z1 = -fbxd.depth/2.0 + fbxd.zoffset + ztot;           
                      /// Loop on i from 1 to fbxd.ny step=1           
                      for ( i=1; (1>0)? (i<=fbxd.ny):(i>=fbxd.ny); i+=1 )           
                      {              
                            x1=x0;              
                            /// Loop on j from 1 to fbxd.nx step=1              
                            for ( j=1; (1>0)? (j<=fbxd.nx):(j>=fbxd.nx); j+=1 )              
                            {                 
                                  if ( fbxd.type==2&&j<6&&i>7&&i<18 )                 
                                  {                    
                                        x1=x1+widx;                    
                                  }                 
                                  else                 
                                  {                    
                                        _create = AgCreate("FTOW");                    
                                        {                       
                                              AgShape myshape; // undefined shape                       
                                              ///Create FTOW                       
                                              Create("FTOW");                        
                                        }                    
                                        { AgPlacement place = AgPlacement("FTOW","FBOX");                       
                                              /// Add daughter volume FTOW to mother FBOX                       
                                              place.TranslateX(x1);                       
                                              /// Translate x = x1                       
                                              place.TranslateY(y1);                       
                                              /// Translate y = y1                       
                                              place.TranslateZ(z1);                       
                                              /// Translate z = z1                       
                                              _stacker -> Position( AgBlock::Find("FTOW"), place );                       
                                        } // end placement of FTOW                    
                                        x1=x1+widx;                    
                                  }                 
                            }              
                            y1 =  y1-widy;              
                      }           
                      if ( fbxd.type==1 )           
                      {              
                            x1=x0;              
                            y1= -rtot + ztot;              
                            z1=-fbxd.depth/2.0  + fbxd.psoffset + wid/2.0;              
                            /// Loop on j from 1 to fbxd.nx step=1              
                            for ( j=1; (1>0)? (j<=fbxd.nx):(j>=fbxd.nx); j+=1 )              
                            {                 
                                  _create = AgCreate("FTOW");                 
                                  {                    
                                        AgShape myshape; // undefined shape                    
                                        ///Create FTOW                    
                                        Create("FTOW");                     
                                  }                 
                                  { AgPlacement place = AgPlacement("FTOW","FBOX");                    
                                        /// Add daughter volume FTOW to mother FBOX                    
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
                                        _stacker -> Position( AgBlock::Find("FTOW"), place );                    
                                  } // end placement of FTOW                 
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
                                        place.TranslateZ(pbpd.thick/2.0-fbxd.depth/2.0);                    
                                        /// Translate z = pbpd.thick/2.0-fbxd.depth/2.0                    
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
                                  place.TranslateZ(fbxd.smdoff+ztotsmd-fbxd.depth/2.0);                 
                                  /// Translate z = fbxd.smdoff+ztotsmd-fbxd.depth/2.0                 
                                  _stacker -> Position( AgBlock::Find("FSHM"), place );                 
                            } // end placement of FSHM              
                      }           
                      if ( fbxd.type>=2 )           
                      {              
                            /// USE flgg type=2 ;              
                            flgg.Use("type",(Float_t)2 );              
                            /// USE flgm type=2 ;              
                            flgm.Use("type",(Float_t)2 );              
                            wid  = flgg.width + flgg.dgap +  flgg.althick*2.0;              
                            ztot = flgg.depth/2.0;              
                            rtot = fbxd.nxl*wid/2.0;              
                            bwid = rtot;              
                            xlcoffset = (fbxd.width-fbxd.nxl*wid)/2.0          ;// large cells offset in x;              
                            if ( fpos.imod==4 )              
                            {                 
                                  x0 =  -bwid + wid/2.0 - xlcoffset;                 
                                  widx = wid;                 
                            }              
                            else if ( fpos.imod==3 )              
                            {                 
                                  x0 =  +bwid - wid/2.0 + xlcoffset;                 
                                  widx = -wid;                 
                            }              
                            y1 =  fbxd.nyl*wid/2.0 - wid/2.0;              
                            z1 = -fbxd.depth/2.0 + fbxd.zoffset + ztot;              
                            /// Loop on i from 1 to fbxd.nyl step=1              
                            for ( i=1; (1>0)? (i<=fbxd.nyl):(i>=fbxd.nyl); i+=1 )              
                            {                 
                                  x1=x0;                 
                                  /// Loop on j from 1 to fbxd.nxl step=1                 
                                  for ( j=1; (1>0)? (j<=fbxd.nxl):(j>=fbxd.nxl); j+=1 )                 
                                  {                    
                                        if ( j<9&&i>9&&i<26 )                    
                                        {                       
                                              x1=x1+widx;                       
                                        }                    
                                        else if ( (i+j)>=45 )                    
                                        {                       
                                              _create = AgCreate("FALU");                       
                                              {                          
                                                    AgShape myshape; // undefined shape                          
                                                    ///Create FALU                          
                                                    Create("FALU");                           
                                              }                       
                                              { AgPlacement place = AgPlacement("FALU","FBOX");                          
                                                    /// Add daughter volume FALU to mother FBOX                          
                                                    place.TranslateX(x1);                          
                                                    /// Translate x = x1                          
                                                    place.TranslateY(y1);                          
                                                    /// Translate y = y1                          
                                                    place.TranslateZ(z1);                          
                                                    /// Translate z = z1                          
                                                    _stacker -> Position( AgBlock::Find("FALU"), place );                          
                                              } // end placement of FALU                       
                                              x1=x1+widx;                       
                                        }                    
                                        else if ( (j-i)>=10 )                    
                                        {                       
                                              x1=x1+widx;                       
                                        }                    
                                        else                    
                                        {                       
                                              _create = AgCreate("FLXF");                       
                                              {                          
                                                    AgShape myshape; // undefined shape                          
                                                    ///Create FLXF                          
                                                    Create("FLXF");                           
                                              }                       
                                              { AgPlacement place = AgPlacement("FLXF","FBOX");                          
                                                    /// Add daughter volume FLXF to mother FBOX                          
                                                    place.TranslateX(x1);                          
                                                    /// Translate x = x1                          
                                                    place.TranslateY(y1);                          
                                                    /// Translate y = y1                          
                                                    place.TranslateZ(z1);                          
                                                    /// Translate z = z1                          
                                                    _stacker -> Position( AgBlock::Find("FLXF"), place );                          
                                              } // end placement of FLXF                       
                                              x1=x1+widx;                       
                                        }                    
                                  }                 
                                  y1=y1-wid;                 
                            }              
                      }           
                      if ( fbxd.type==2 )           
                      {              
                            /// USE flgg type=2 ;              
                            flgg.Use("type",(Float_t)2 );              
                            /// USE flgm type=2 ;              
                            flgm.Use("type",(Float_t)2 );              
                            bzoffset=0.0;              
                            wid  = flgg.width + flgg.dgap;              
                            _create = AgCreate("FBAS");              
                            {                 
                                  AgShape myshape; // undefined shape                 
                                  ///Create FBAS                 
                                  Create("FBAS");                  
                            }              
                            { AgPlacement place = AgPlacement("FBAS","FBOX");                 
                                  /// Add daughter volume FBAS to mother FBOX                 
                                  place.TranslateX(fpos.x);                 
                                  /// Translate x = fpos.x                 
                                  place.TranslateY(-(fbxd.nxl*wid+basewidth/2.0));                 
                                  /// Translate y = -(fbxd.nxl*wid+basewidth/2.0)                 
                                  place.TranslateZ(-fbxd.depth/2.0+bzoffset+inse.depth/2.0);                 
                                  /// Translate z = -fbxd.depth/2.0+bzoffset+inse.depth/2.0                 
                                  _stacker -> Position( AgBlock::Find("FBAS"), place );                 
                            } // end placement of FBAS              
                      }           
                      _create = AgCreate("FENC");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FENC              
                            Create("FENC");               
                      }           
                      _create = AgCreate("FEAC");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FEAC              
                            Create("FEAC");               
                      }           
                      _create = AgCreate("FECC");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FECC              
                            Create("FECC");               
                      }           
                      _create = AgCreate("FEDC");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FEDC              
                            Create("FEDC");               
                      }           
                      _create = AgCreate("FEEC");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FEEC              
                            Create("FEEC");               
                      }           
                      distancer=inse.gapheight-(inse.height-2.0*inse.sheetdpt);           
                      xofffecc=(inse.sheetdpt-fbxd.width)/2.0;           
                      zofffecc=-fbxd.depth/2.0+bzoffset+inse.depth-inse.gategap;           
                      xofffedc=inse.width - inse.sheetdpt;           
                      xofffenc=(inse.width-fbxd.width)/2.0;           
                      yofffenc=(inse.height-inse.sheetdpt)/2.0;           
                      zofffenc=-fbxd.depth/2.0 + bzoffset + inse.depth/2.0;           
                      xshift=8*5.812-12*3.822+5*3.822-inse.width    ;//this is to move insert to small cell edge;           
                      if ( fbxd.type==2 )           
                      {              
                            if ( fpos.imod==4 )              
                            {                 
                                  { AgPlacement place = AgPlacement("FENC","FBOX");                    
                                        /// Add daughter volume FENC to mother FBOX                    
                                        place.TranslateX(xofffenc+xshift);                    
                                        /// Translate x = xofffenc+xshift                    
                                        place.TranslateY(-yofffenc);                    
                                        /// Translate y = -yofffenc                    
                                        place.TranslateZ(zofffenc);                    
                                        /// Translate z = zofffenc                    
                                        _stacker -> Position( AgBlock::Find("FENC"), place );                    
                                  } // end placement of FENC                 
                                  { AgPlacement place = AgPlacement("FENC","FBOX");                    
                                        /// Add daughter volume FENC to mother FBOX                    
                                        place.TranslateX(xofffenc+xshift);                    
                                        /// Translate x = xofffenc+xshift                    
                                        place.TranslateY(yofffenc);                    
                                        /// Translate y = yofffenc                    
                                        place.TranslateZ(zofffenc);                    
                                        /// Translate z = zofffenc                    
                                        _stacker -> Position( AgBlock::Find("FENC"), place );                    
                                  } // end placement of FENC                 
                                  { AgPlacement place = AgPlacement("FEAC","FBOX");                    
                                        /// Add daughter volume FEAC to mother FBOX                    
                                        place.TranslateX(inse.width+xshift-(fbxd.width+inse.sheetdpt)/2.0);                    
                                        /// Translate x = inse.width+xshift-(fbxd.width+inse.sheetdpt)/2.0                    
                                        place.TranslateY(0);                    
                                        /// Translate y = 0                    
                                        place.TranslateZ(-fbxd.depth/2.0+bzoffset+inse.depth/2.0);                    
                                        /// Translate z = -fbxd.depth/2.0+bzoffset+inse.depth/2.0                    
                                        _stacker -> Position( AgBlock::Find("FEAC"), place );                    
                                  } // end placement of FEAC                 
                                  { AgPlacement place = AgPlacement("FECC","FBOX");                    
                                        /// Add daughter volume FECC to mother FBOX                    
                                        place.TranslateX(xofffecc+xshift);                    
                                        /// Translate x = xofffecc+xshift                    
                                        place.TranslateY(distancer/2.0);                    
                                        /// Translate y = distancer/2.0                    
                                        place.TranslateZ(zofffecc-inse.gatedepth-inse.gapdepth/2.0);                    
                                        /// Translate z = zofffecc-inse.gatedepth-inse.gapdepth/2.0                    
                                        _stacker -> Position( AgBlock::Find("FECC"), place );                    
                                  } // end placement of FECC                 
                                  { AgPlacement place = AgPlacement("FECC","FBOX");                    
                                        /// Add daughter volume FECC to mother FBOX                    
                                        place.TranslateX(xofffecc+xshift);                    
                                        /// Translate x = xofffecc+xshift                    
                                        place.TranslateY(distancer/2.0);                    
                                        /// Translate y = distancer/2.0                    
                                        place.TranslateZ(zofffecc-2.0*inse.gatedepth-3.0*inse.gapdepth/2.0);                    
                                        /// Translate z = zofffecc-2.0*inse.gatedepth-3.0*inse.gapdepth/2.0                    
                                        _stacker -> Position( AgBlock::Find("FECC"), place );                    
                                  } // end placement of FECC                 
                                  { AgPlacement place = AgPlacement("FECC","FBOX");                    
                                        /// Add daughter volume FECC to mother FBOX                    
                                        place.TranslateX(xofffecc+xshift);                    
                                        /// Translate x = xofffecc+xshift                    
                                        place.TranslateY(-distancer/2.0);                    
                                        /// Translate y = -distancer/2.0                    
                                        place.TranslateZ(zofffecc-inse.gatedepth-inse.gapdepth/2.0);                    
                                        /// Translate z = zofffecc-inse.gatedepth-inse.gapdepth/2.0                    
                                        _stacker -> Position( AgBlock::Find("FECC"), place );                    
                                  } // end placement of FECC                 
                                  { AgPlacement place = AgPlacement("FECC","FBOX");                    
                                        /// Add daughter volume FECC to mother FBOX                    
                                        place.TranslateX(xofffecc+xshift);                    
                                        /// Translate x = xofffecc+xshift                    
                                        place.TranslateY(-distancer/2.0);                    
                                        /// Translate y = -distancer/2.0                    
                                        place.TranslateZ(zofffecc-2.0*inse.gatedepth-3.0*inse.gapdepth/2.0);                    
                                        /// Translate z = zofffecc-2.0*inse.gatedepth-3.0*inse.gapdepth/2.0                    
                                        _stacker -> Position( AgBlock::Find("FECC"), place );                    
                                  } // end placement of FECC                 
                                  { AgPlacement place = AgPlacement("FEDC","FBOX");                    
                                        /// Add daughter volume FEDC to mother FBOX                    
                                        place.TranslateX(xshift+(xofffedc-fbxd.width)/2.0);                    
                                        /// Translate x = xshift+(xofffedc-fbxd.width)/2.0                    
                                        place.TranslateY(0);                    
                                        /// Translate y = 0                    
                                        place.TranslateZ(zofffecc-inse.gatedepth/2.0);                    
                                        /// Translate z = zofffecc-inse.gatedepth/2.0                    
                                        _stacker -> Position( AgBlock::Find("FEDC"), place );                    
                                  } // end placement of FEDC                 
                                  { AgPlacement place = AgPlacement("FEDC","FBOX");                    
                                        /// Add daughter volume FEDC to mother FBOX                    
                                        place.TranslateX(xshift+(xofffedc-fbxd.width)/2.0);                    
                                        /// Translate x = xshift+(xofffedc-fbxd.width)/2.0                    
                                        place.TranslateY(0);                    
                                        /// Translate y = 0                    
                                        place.TranslateZ(zofffecc-inse.gatedepth-inse.gapdepth-inse.gatedepth/2.0);                    
                                        /// Translate z = zofffecc-inse.gatedepth-inse.gapdepth-inse.gatedepth/2.0                    
                                        _stacker -> Position( AgBlock::Find("FEDC"), place );                    
                                  } // end placement of FEDC                 
                                  { AgPlacement place = AgPlacement("FEDC","FBOX");                    
                                        /// Add daughter volume FEDC to mother FBOX                    
                                        place.TranslateX(xshift+(xofffedc-fbxd.width)/2.0);                    
                                        /// Translate x = xshift+(xofffedc-fbxd.width)/2.0                    
                                        place.TranslateY(0);                    
                                        /// Translate y = 0                    
                                        place.TranslateZ(zofffecc-2.0*(inse.gatedepth+inse.gapdepth)-inse.gatedepth/2.0);                    
                                        /// Translate z = zofffecc-2.0*(inse.gatedepth+inse.gapdepth)-inse.gatedepth/2.0                    
                                        _stacker -> Position( AgBlock::Find("FEDC"), place );                    
                                  } // end placement of FEDC                 
                            }              
                            else if ( fpos.imod==3 )              
                            {                 
                                  { AgPlacement place = AgPlacement("FENC","FBOX");                    
                                        /// Add daughter volume FENC to mother FBOX                    
                                        place.TranslateX(-xofffenc-xshift);                    
                                        /// Translate x = -xofffenc-xshift                    
                                        place.TranslateY(-yofffenc);                    
                                        /// Translate y = -yofffenc                    
                                        place.TranslateZ(zofffenc);                    
                                        /// Translate z = zofffenc                    
                                        _stacker -> Position( AgBlock::Find("FENC"), place );                    
                                  } // end placement of FENC                 
                                  { AgPlacement place = AgPlacement("FENC","FBOX");                    
                                        /// Add daughter volume FENC to mother FBOX                    
                                        place.TranslateX(-xofffenc-xshift);                    
                                        /// Translate x = -xofffenc-xshift                    
                                        place.TranslateY(yofffenc);                    
                                        /// Translate y = yofffenc                    
                                        place.TranslateZ(zofffenc);                    
                                        /// Translate z = zofffenc                    
                                        _stacker -> Position( AgBlock::Find("FENC"), place );                    
                                  } // end placement of FENC                 
                                  { AgPlacement place = AgPlacement("FEAC","FBOX");                    
                                        /// Add daughter volume FEAC to mother FBOX                    
                                        place.TranslateX(-xshift-inse.width+(fbxd.width+inse.sheetdpt)/2.0);                    
                                        /// Translate x = -xshift-inse.width+(fbxd.width+inse.sheetdpt)/2.0                    
                                        place.TranslateY(0);                    
                                        /// Translate y = 0                    
                                        place.TranslateZ(-fbxd.depth/2.0+bzoffset+inse.depth/2.0);                    
                                        /// Translate z = -fbxd.depth/2.0+bzoffset+inse.depth/2.0                    
                                        _stacker -> Position( AgBlock::Find("FEAC"), place );                    
                                  } // end placement of FEAC                 
                                  { AgPlacement place = AgPlacement("FECC","FBOX");                    
                                        /// Add daughter volume FECC to mother FBOX                    
                                        place.TranslateX(-xofffecc-xshift);                    
                                        /// Translate x = -xofffecc-xshift                    
                                        place.TranslateY(distancer/2.0);                    
                                        /// Translate y = distancer/2.0                    
                                        place.TranslateZ(zofffecc-inse.gatedepth-inse.gapdepth/2.0);                    
                                        /// Translate z = zofffecc-inse.gatedepth-inse.gapdepth/2.0                    
                                        _stacker -> Position( AgBlock::Find("FECC"), place );                    
                                  } // end placement of FECC                 
                                  { AgPlacement place = AgPlacement("FECC","FBOX");                    
                                        /// Add daughter volume FECC to mother FBOX                    
                                        place.TranslateX(-xofffecc-xshift);                    
                                        /// Translate x = -xofffecc-xshift                    
                                        place.TranslateY(distancer/2.0);                    
                                        /// Translate y = distancer/2.0                    
                                        place.TranslateZ(zofffecc-2.0*inse.gatedepth-3.0*inse.gapdepth/2.0);                    
                                        /// Translate z = zofffecc-2.0*inse.gatedepth-3.0*inse.gapdepth/2.0                    
                                        _stacker -> Position( AgBlock::Find("FECC"), place );                    
                                  } // end placement of FECC                 
                                  { AgPlacement place = AgPlacement("FECC","FBOX");                    
                                        /// Add daughter volume FECC to mother FBOX                    
                                        place.TranslateX(-xofffecc-xshift);                    
                                        /// Translate x = -xofffecc-xshift                    
                                        place.TranslateY(-distancer/2.0);                    
                                        /// Translate y = -distancer/2.0                    
                                        place.TranslateZ(zofffecc-inse.gatedepth-inse.gapdepth/2.0);                    
                                        /// Translate z = zofffecc-inse.gatedepth-inse.gapdepth/2.0                    
                                        _stacker -> Position( AgBlock::Find("FECC"), place );                    
                                  } // end placement of FECC                 
                                  { AgPlacement place = AgPlacement("FECC","FBOX");                    
                                        /// Add daughter volume FECC to mother FBOX                    
                                        place.TranslateX(-xofffecc-xshift);                    
                                        /// Translate x = -xofffecc-xshift                    
                                        place.TranslateY(-distancer/2.0);                    
                                        /// Translate y = -distancer/2.0                    
                                        place.TranslateZ(zofffecc-2.0*inse.gatedepth-3.0*inse.gapdepth/2.0);                    
                                        /// Translate z = zofffecc-2.0*inse.gatedepth-3.0*inse.gapdepth/2.0                    
                                        _stacker -> Position( AgBlock::Find("FECC"), place );                    
                                  } // end placement of FECC                 
                                  { AgPlacement place = AgPlacement("FEEC","FBOX");                    
                                        /// Add daughter volume FEEC to mother FBOX                    
                                        place.TranslateX(-xshift-(xofffedc-fbxd.width)/2.0);                    
                                        /// Translate x = -xshift-(xofffedc-fbxd.width)/2.0                    
                                        place.TranslateY(0);                    
                                        /// Translate y = 0                    
                                        place.TranslateZ(zofffecc-inse.gatedepth/2.0);                    
                                        /// Translate z = zofffecc-inse.gatedepth/2.0                    
                                        _stacker -> Position( AgBlock::Find("FEEC"), place );                    
                                  } // end placement of FEEC                 
                                  { AgPlacement place = AgPlacement("FEEC","FBOX");                    
                                        /// Add daughter volume FEEC to mother FBOX                    
                                        place.TranslateX(-xshift-(xofffedc-fbxd.width)/2.0);                    
                                        /// Translate x = -xshift-(xofffedc-fbxd.width)/2.0                    
                                        place.TranslateY(0);                    
                                        /// Translate y = 0                    
                                        place.TranslateZ(zofffecc-inse.gatedepth-inse.gapdepth-inse.gatedepth/2.0);                    
                                        /// Translate z = zofffecc-inse.gatedepth-inse.gapdepth-inse.gatedepth/2.0                    
                                        _stacker -> Position( AgBlock::Find("FEEC"), place );                    
                                  } // end placement of FEEC                 
                                  { AgPlacement place = AgPlacement("FEEC","FBOX");                    
                                        /// Add daughter volume FEEC to mother FBOX                    
                                        place.TranslateX(-xshift-(xofffedc-fbxd.width)/2.0);                    
                                        /// Translate x = -xshift-(xofffedc-fbxd.width)/2.0                    
                                        place.TranslateY(0);                    
                                        /// Translate y = 0                    
                                        place.TranslateZ(zofffecc-2.0*(inse.gatedepth+inse.gapdepth)-inse.gatedepth/2.0);                    
                                        /// Translate z = zofffecc-2.0*(inse.gatedepth+inse.gapdepth)-inse.gatedepth/2.0                    
                                        _stacker -> Position( AgBlock::Find("FEEC"), place );                    
                                  } // end placement of FEEC                 
                            }              
                      }           
                      END_OF_FBOX:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FBOX     
          // ---------------------------------------------------------------------------------------------------     
          void FTOW::Block( AgCreate create )     
          {         
                ///@addtogroup FTOW_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Air            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Air");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("FTOW");              
                            attr.par("seen")=1;              
                            attr.par("colo")=2;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=wid/2.0;              
                            shape.par("dy")=wid/2.0;              
                            shape.par("dz")=ztot;              
                            /// Shape Bbox dx=wid/2.0 dy=wid/2.0 dz=ztot               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FTOW;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("FWAL");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FWAL              
                            Create("FWAL");               
                      }           
                      { AgPlacement place = AgPlacement("FWAL","FTOW");              
                            /// Add daughter volume FWAL to mother FTOW              
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
                      { AgPlacement place = AgPlacement("FUMT","FTOW");              
                            /// Add daughter volume FUMT to mother FTOW              
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
                      { AgPlacement place = AgPlacement("FPCT","FTOW");              
                            /// Add daughter volume FPCT to mother FTOW              
                            place.TranslateZ(-ztot+flgg.althick+flgg.depth+flgg.phcathdz/2.0);              
                            /// Translate z = -ztot+flgg.althick+flgg.depth+flgg.phcathdz/2.0              
                            _stacker -> Position( AgBlock::Find("FPCT"), place );              
                      } // end placement of FPCT           
                      END_OF_FTOW:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FTOW     
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
                            shape.par("dx")=flgg.width/2.0;              
                            shape.par("dy")=flgg.width/2.0;              
                            shape.par("dz")=flgg.depth/2.0;              
                            /// Shape Bbox dx=flgg.width/2.0 dy=flgg.width/2.0 dz=flgg.depth/2.0               
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
          void FLXF::Block( AgCreate create )     
          {         
                ///@addtogroup FLXF_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Component Pb	a=207.19	z=82	w=.41774           
                      /// Component K	a=39.102	z=19	w=.04151           
                      /// Component Si	a=28.088	z=14	w=.21047           
                      /// Component O	a=15.999	z=8	w=.29330           
                      /// Component Na	a=22.990	z=11	w=.03710           
                      /// Mixture F2 dens=flgm_density radl=flgm_radlen           
                      {  AgMaterial &mix = AgMaterial::Get("F2");              
                            mix.Component("Pb",207.19,82,.41774);              
                            mix.Component("K",39.102,19,.04151);              
                            mix.Component("Si",28.088,14,.21047);              
                            mix.Component("O",15.999,8,.29330);              
                            mix.Component("Na",22.990,11,.03710);              
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
                      { AgAttribute attr = AgAttribute("FLXF");              
                            attr.par("seen")=1;              
                            attr.par("colo")=4;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=flgg.width/2.0;              
                            shape.par("dy")=flgg.width/2.0;              
                            shape.par("dz")=flgg.depth/2.0;              
                            /// Shape Bbox dx=flgg.width/2.0 dy=flgg.width/2.0 dz=flgg.depth/2.0               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FLXF;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_FLXF:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FLXF     
          // ---------------------------------------------------------------------------------------------------     
          void FALU::Block( AgCreate create )     
          {         
                ///@addtogroup FALU_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Aluminium            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Aluminium");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("FALU");              
                            attr.par("seen")=1;              
                            attr.par("colo")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=flgg.width/2.0;              
                            shape.par("dy")=flgg.width/2.0;              
                            shape.par("dz")=flgg.depth/2.0;              
                            /// Shape Bbox dx=flgg.width/2.0 dy=flgg.width/2.0 dz=flgg.depth/2.0               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FALU;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_FALU:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FALU     
          // ---------------------------------------------------------------------------------------------------     
          void FBAS::Block( AgCreate create )     
          {         
                ///@addtogroup FBAS_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Iron            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Iron");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("FBAS");              
                            attr.par("seen")=1;              
                            attr.par("colo")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=fbxd.width/2.0;              
                            shape.par("dy")=basewidth/2.0;              
                            shape.par("dz")=inse.depth/2.0;              
                            /// Shape Bbox dx=fbxd.width/2.0 dy=basewidth/2.0 dz=inse.depth/2.0               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FBAS;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_FBAS:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FBAS     
          // ---------------------------------------------------------------------------------------------------     
          void FENC::Block( AgCreate create )     
          {         
                ///@addtogroup FENC_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Iron            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Iron");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("FENC");              
                            attr.par("seen")=1;              
                            attr.par("colo")=5;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=inse.width/2.0;              
                            shape.par("dy")=inse.sheetdpt/2.0;              
                            shape.par("dz")=inse.depth/2.0;              
                            /// Shape Bbox dx=inse.width/2.0 dy=inse.sheetdpt/2.0 dz=inse.depth/2.0               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FENC;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_FENC:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FENC     
          // ---------------------------------------------------------------------------------------------------     
          void FEAC::Block( AgCreate create )     
          {         
                ///@addtogroup FEAC_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Iron            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Iron");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("FEAC");              
                            attr.par("seen")=1;              
                            attr.par("colo")=5;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=inse.sheetdpt/2.0;              
                            shape.par("dy")=(inse.height-2.0*inse.sheetdpt)/2.0;              
                            shape.par("dz")=inse.depth/2.0;              
                            /// Shape Bbox dx=inse.sheetdpt/2.0 dy=(inse.height-2.0*inse.sheetdpt)/2.0 dz=inse.depth/2.0               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FEAC;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("FEBC");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FEBC              
                            Create("FEBC");               
                      }           
                      { AgPlacement place = AgPlacement("FEBC","FEAC");              
                            /// Add daughter volume FEBC to mother FEAC              
                            place.TranslateX(0.0);              
                            /// Translate x = 0.0              
                            place.TranslateY(0);              
                            /// Translate y = 0              
                            place.TranslateZ(-inse.depth/2.0+inse.holedepth/2.0+inse.holegap);              
                            /// Translate z = -inse.depth/2.0+inse.holedepth/2.0+inse.holegap              
                            _stacker -> Position( AgBlock::Find("FEBC"), place );              
                      } // end placement of FEBC           
                      END_OF_FEAC:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FEAC     
          // ---------------------------------------------------------------------------------------------------     
          void FEBC::Block( AgCreate create )     
          {         
                ///@addtogroup FEBC_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Air            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Air");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("FEBC");              
                            attr.par("seen")=1;              
                            attr.par("colo")=5;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=inse.sheetdpt/2.0;              
                            shape.par("dy")=inse.holeheight/2.0;              
                            shape.par("dz")=inse.holedepth/2.0;              
                            /// Shape Bbox dx=inse.sheetdpt/2.0 dy=inse.holeheight/2.0 dz=inse.holedepth/2.0               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FEBC;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_FEBC:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FEBC     
          // ---------------------------------------------------------------------------------------------------     
          void FECC::Block( AgCreate create )     
          {         
                ///@addtogroup FECC_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Iron            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Iron");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("FECC");              
                            attr.par("seen")=1;              
                            attr.par("colo")=5;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=inse.sheetdpt/2.0;              
                            shape.par("dy")=inse.gapheight/2.0;              
                            shape.par("dz")=inse.gapdepth/2.0;              
                            /// Shape Bbox dx=inse.sheetdpt/2.0 dy=inse.gapheight/2.0 dz=inse.gapdepth/2.0               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FECC;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_FECC:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FECC     
          // ---------------------------------------------------------------------------------------------------     
          void FEDC::Block( AgCreate create )     
          {         
                ///@addtogroup FEDC_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Iron            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Iron");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("FEDC");              
                            attr.par("seen")=1;              
                            attr.par("colo")=5;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=(xofffedc)/2.0;              
                            shape.par("dy")=(inse.height-2.0*inse.sheetdpt)/2.0;              
                            shape.par("dz")=inse.gatedepth/2.0;              
                            /// Shape Bbox dx=(xofffedc)/2.0 dy=(inse.height-2.0*inse.sheetdpt)/2.0 dz=inse.gatedepth/2.0               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FEDC;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("FERC");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FERC              
                            Create("FERC");               
                      }           
                      { AgPlacement place = AgPlacement("FERC","FEDC");              
                            /// Add daughter volume FERC to mother FEDC              
                            place.TranslateX(-(xofffedc)/2.0);              
                            /// Translate x = -(xofffedc)/2.0              
                            place.TranslateY(0);              
                            /// Translate y = 0              
                            place.TranslateZ(0.0);              
                            /// Translate z = 0.0              
                            _stacker -> Position( AgBlock::Find("FERC"), place );              
                      } // end placement of FERC           
                      _create = AgCreate("FESC");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FESC              
                            Create("FESC");               
                      }           
                      { AgPlacement place = AgPlacement("FESC","FEDC");              
                            /// Add daughter volume FESC to mother FEDC              
                            place.TranslateX(-(xofffedc)/2.0+inse.ra*cos(pi*5.0/12.0));              
                            /// Translate x = -(xofffedc)/2.0+inse.ra*cos(pi*5.0/12.0)              
                            place.TranslateY(inse.ra*sin(pi*5.0/12.0));              
                            /// Translate y = inse.ra*sin(pi*5.0/12.0)              
                            place.TranslateZ(0.0);              
                            /// Translate z = 0.0              
                            _stacker -> Position( AgBlock::Find("FESC"), place );              
                      } // end placement of FESC           
                      { AgPlacement place = AgPlacement("FESC","FEDC");              
                            /// Add daughter volume FESC to mother FEDC              
                            place.TranslateX(-(xofffedc)/2.0+inse.ra*cos(pi/4.0));              
                            /// Translate x = -(xofffedc)/2.0+inse.ra*cos(pi/4.0)              
                            place.TranslateY(inse.ra*sin(pi/4.0));              
                            /// Translate y = inse.ra*sin(pi/4.0)              
                            place.TranslateZ(0.0);              
                            /// Translate z = 0.0              
                            _stacker -> Position( AgBlock::Find("FESC"), place );              
                      } // end placement of FESC           
                      { AgPlacement place = AgPlacement("FESC","FEDC");              
                            /// Add daughter volume FESC to mother FEDC              
                            place.TranslateX(-(xofffedc)/2.0+inse.ra*cos(pi/12.0));              
                            /// Translate x = -(xofffedc)/2.0+inse.ra*cos(pi/12.0)              
                            place.TranslateY(inse.ra*sin(pi/12.0));              
                            /// Translate y = inse.ra*sin(pi/12.0)              
                            place.TranslateZ(0.0);              
                            /// Translate z = 0.0              
                            _stacker -> Position( AgBlock::Find("FESC"), place );              
                      } // end placement of FESC           
                      { AgPlacement place = AgPlacement("FESC","FEDC");              
                            /// Add daughter volume FESC to mother FEDC              
                            place.TranslateX(-(xofffedc)/2.0+inse.ra*cos(pi/12.0));              
                            /// Translate x = -(xofffedc)/2.0+inse.ra*cos(pi/12.0)              
                            place.TranslateY(-inse.ra*sin(pi/12.0));              
                            /// Translate y = -inse.ra*sin(pi/12.0)              
                            place.TranslateZ(0.0);              
                            /// Translate z = 0.0              
                            _stacker -> Position( AgBlock::Find("FESC"), place );              
                      } // end placement of FESC           
                      { AgPlacement place = AgPlacement("FESC","FEDC");              
                            /// Add daughter volume FESC to mother FEDC              
                            place.TranslateX(-(xofffedc)/2.0+inse.ra*cos(pi/4.0));              
                            /// Translate x = -(xofffedc)/2.0+inse.ra*cos(pi/4.0)              
                            place.TranslateY(-inse.ra*sin(pi/4.0));              
                            /// Translate y = -inse.ra*sin(pi/4.0)              
                            place.TranslateZ(0.0);              
                            /// Translate z = 0.0              
                            _stacker -> Position( AgBlock::Find("FESC"), place );              
                      } // end placement of FESC           
                      { AgPlacement place = AgPlacement("FESC","FEDC");              
                            /// Add daughter volume FESC to mother FEDC              
                            place.TranslateX(-(xofffedc)/2.0+inse.ra*cos(pi*5.0/12.0));              
                            /// Translate x = -(xofffedc)/2.0+inse.ra*cos(pi*5.0/12.0)              
                            place.TranslateY(-inse.ra*sin(pi*5.0/12.0));              
                            /// Translate y = -inse.ra*sin(pi*5.0/12.0)              
                            place.TranslateZ(0.0);              
                            /// Translate z = 0.0              
                            _stacker -> Position( AgBlock::Find("FESC"), place );              
                      } // end placement of FESC           
                      { AgPlacement place = AgPlacement("FESC","FEDC");              
                            /// Add daughter volume FESC to mother FEDC              
                            place.TranslateX(-(xofffedc)/2.0+inse.rb*cos(pi/4.0));              
                            /// Translate x = -(xofffedc)/2.0+inse.rb*cos(pi/4.0)              
                            place.TranslateY(inse.rb*sin(pi/4.0));              
                            /// Translate y = inse.rb*sin(pi/4.0)              
                            place.TranslateZ(0.0);              
                            /// Translate z = 0.0              
                            _stacker -> Position( AgBlock::Find("FESC"), place );              
                      } // end placement of FESC           
                      { AgPlacement place = AgPlacement("FESC","FEDC");              
                            /// Add daughter volume FESC to mother FEDC              
                            place.TranslateX(-(xofffedc)/2.0+inse.rb*cos(pi/4.0));              
                            /// Translate x = -(xofffedc)/2.0+inse.rb*cos(pi/4.0)              
                            place.TranslateY(-inse.rb*sin(pi/4.0));              
                            /// Translate y = -inse.rb*sin(pi/4.0)              
                            place.TranslateZ(0.0);              
                            /// Translate z = 0.0              
                            _stacker -> Position( AgBlock::Find("FESC"), place );              
                      } // end placement of FESC           
                      END_OF_FEDC:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FEDC     
          // ---------------------------------------------------------------------------------------------------     
          void FEEC::Block( AgCreate create )     
          {         
                ///@addtogroup FEEC_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Iron            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Iron");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("FEEC");              
                            attr.par("seen")=1;              
                            attr.par("colo")=5;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=(xofffedc)/2.0;              
                            shape.par("dy")=(inse.height-2.0*inse.sheetdpt)/2.0;              
                            shape.par("dz")=inse.gatedepth/2.0;              
                            /// Shape Bbox dx=(xofffedc)/2.0 dy=(inse.height-2.0*inse.sheetdpt)/2.0 dz=inse.gatedepth/2.0               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FEEC;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("FETC");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FETC              
                            Create("FETC");               
                      }           
                      { AgPlacement place = AgPlacement("FETC","FEEC");              
                            /// Add daughter volume FETC to mother FEEC              
                            place.TranslateX((xofffedc)/2.0);              
                            /// Translate x = (xofffedc)/2.0              
                            place.TranslateY(0);              
                            /// Translate y = 0              
                            place.TranslateZ(0.0);              
                            /// Translate z = 0.0              
                            _stacker -> Position( AgBlock::Find("FETC"), place );              
                      } // end placement of FETC           
                      _create = AgCreate("FESC");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FESC              
                            Create("FESC");               
                      }           
                      { AgPlacement place = AgPlacement("FESC","FEEC");              
                            /// Add daughter volume FESC to mother FEEC              
                            place.TranslateX((xofffedc)/2.0-inse.ra*cos(pi*5.0/12.0));              
                            /// Translate x = (xofffedc)/2.0-inse.ra*cos(pi*5.0/12.0)              
                            place.TranslateY(inse.ra*sin(pi*5.0/12.0));              
                            /// Translate y = inse.ra*sin(pi*5.0/12.0)              
                            place.TranslateZ(0.0);              
                            /// Translate z = 0.0              
                            _stacker -> Position( AgBlock::Find("FESC"), place );              
                      } // end placement of FESC           
                      { AgPlacement place = AgPlacement("FESC","FEEC");              
                            /// Add daughter volume FESC to mother FEEC              
                            place.TranslateX((xofffedc)/2.0-inse.ra*cos(pi/4.0));              
                            /// Translate x = (xofffedc)/2.0-inse.ra*cos(pi/4.0)              
                            place.TranslateY(inse.ra*sin(pi/4.0));              
                            /// Translate y = inse.ra*sin(pi/4.0)              
                            place.TranslateZ(0.0);              
                            /// Translate z = 0.0              
                            _stacker -> Position( AgBlock::Find("FESC"), place );              
                      } // end placement of FESC           
                      { AgPlacement place = AgPlacement("FESC","FEEC");              
                            /// Add daughter volume FESC to mother FEEC              
                            place.TranslateX((xofffedc)/2.0-inse.ra*cos(pi/12.0));              
                            /// Translate x = (xofffedc)/2.0-inse.ra*cos(pi/12.0)              
                            place.TranslateY(inse.ra*sin(pi/12.0));              
                            /// Translate y = inse.ra*sin(pi/12.0)              
                            place.TranslateZ(0.0);              
                            /// Translate z = 0.0              
                            _stacker -> Position( AgBlock::Find("FESC"), place );              
                      } // end placement of FESC           
                      { AgPlacement place = AgPlacement("FESC","FEEC");              
                            /// Add daughter volume FESC to mother FEEC              
                            place.TranslateX((xofffedc)/2.0-inse.ra*cos(pi/12.0));              
                            /// Translate x = (xofffedc)/2.0-inse.ra*cos(pi/12.0)              
                            place.TranslateY(-inse.ra*sin(pi/12.0));              
                            /// Translate y = -inse.ra*sin(pi/12.0)              
                            place.TranslateZ(0.0);              
                            /// Translate z = 0.0              
                            _stacker -> Position( AgBlock::Find("FESC"), place );              
                      } // end placement of FESC           
                      { AgPlacement place = AgPlacement("FESC","FEEC");              
                            /// Add daughter volume FESC to mother FEEC              
                            place.TranslateX((xofffedc)/2.0-inse.ra*cos(pi/4.0));              
                            /// Translate x = (xofffedc)/2.0-inse.ra*cos(pi/4.0)              
                            place.TranslateY(-inse.ra*sin(pi/4.0));              
                            /// Translate y = -inse.ra*sin(pi/4.0)              
                            place.TranslateZ(0.0);              
                            /// Translate z = 0.0              
                            _stacker -> Position( AgBlock::Find("FESC"), place );              
                      } // end placement of FESC           
                      { AgPlacement place = AgPlacement("FESC","FEEC");              
                            /// Add daughter volume FESC to mother FEEC              
                            place.TranslateX((xofffedc)/2.0-inse.ra*cos(pi*5.0/12.0));              
                            /// Translate x = (xofffedc)/2.0-inse.ra*cos(pi*5.0/12.0)              
                            place.TranslateY(-inse.ra*sin(pi*5.0/12.0));              
                            /// Translate y = -inse.ra*sin(pi*5.0/12.0)              
                            place.TranslateZ(0.0);              
                            /// Translate z = 0.0              
                            _stacker -> Position( AgBlock::Find("FESC"), place );              
                      } // end placement of FESC           
                      { AgPlacement place = AgPlacement("FESC","FEEC");              
                            /// Add daughter volume FESC to mother FEEC              
                            place.TranslateX((xofffedc)/2.0-inse.rb*cos(pi/4.0));              
                            /// Translate x = (xofffedc)/2.0-inse.rb*cos(pi/4.0)              
                            place.TranslateY(inse.rb*sin(pi/4.0));              
                            /// Translate y = inse.rb*sin(pi/4.0)              
                            place.TranslateZ(0.0);              
                            /// Translate z = 0.0              
                            _stacker -> Position( AgBlock::Find("FESC"), place );              
                      } // end placement of FESC           
                      { AgPlacement place = AgPlacement("FESC","FEEC");              
                            /// Add daughter volume FESC to mother FEEC              
                            place.TranslateX((xofffedc)/2.0-inse.rb*cos(pi/4.0));              
                            /// Translate x = (xofffedc)/2.0-inse.rb*cos(pi/4.0)              
                            place.TranslateY(-inse.rb*sin(pi/4.0));              
                            /// Translate y = -inse.rb*sin(pi/4.0)              
                            place.TranslateZ(0.0);              
                            /// Translate z = 0.0              
                            _stacker -> Position( AgBlock::Find("FESC"), place );              
                      } // end placement of FESC           
                      END_OF_FEEC:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FEEC     
          // ---------------------------------------------------------------------------------------------------     
          void FETC::Block( AgCreate create )     
          {         
                ///@addtogroup FETC_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Air            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Air");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("FETC");              
                            attr.par("seen")=1;              
                            attr.par("colo")=6;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tubs");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=0.0;              
                            shape.par("rmax")=inse.rmax;              
                            shape.par("phi1")=90;              
                            shape.par("phi2")=-90;              
                            shape.par("dz")=inse.gatedepth/2.0;              
                            /// Shape Tubs rmin=0.0 rmax=inse.rmax phi1=90 phi2=-90 dz=inse.gatedepth/2.0               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FETC;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_FETC:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FETC     
          // ---------------------------------------------------------------------------------------------------     
          void FERC::Block( AgCreate create )     
          {         
                ///@addtogroup FERC_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Air            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Air");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("FERC");              
                            attr.par("seen")=1;              
                            attr.par("colo")=6;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tubs");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=0.0;              
                            shape.par("rmax")=inse.rmax;              
                            shape.par("phi1")=-90;              
                            shape.par("phi2")=90;              
                            shape.par("dz")=inse.gatedepth/2.0;              
                            /// Shape Tubs rmin=0.0 rmax=inse.rmax phi1=-90 phi2=90 dz=inse.gatedepth/2.0               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FERC;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_FERC:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FERC     
          // ---------------------------------------------------------------------------------------------------     
          void FESC::Block( AgCreate create )     
          {         
                ///@addtogroup FESC_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Air            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Air");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("FESC");              
                            attr.par("seen")=1;              
                            attr.par("colo")=6;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=0.0;              
                            shape.par("rmax")=inse.diam/2.0;              
                            shape.par("dz")=inse.gatedepth/2.0;              
                            /// Shape Tube rmin=0.0 rmax=inse.diam/2.0 dz=inse.gatedepth/2.0               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FESC;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_FESC:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FESC     
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
                            shape.par("dx")=fmxg.g10width/2.0;              
                            shape.par("dy")=fmxg.g10hgt/2.0;              
                            shape.par("dz")=ztotsmd;              
                            /// Shape Bbox dx=fmxg.g10width/2.0 dy=fmxg.g10hgt/2.0 dz=ztotsmd               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FSHM;              
                            _stacker -> Build(this);              
                      }           
                      wsmd=fmxg.sbase/2.0+fmxg.sgap;           
                      wtotsmd=(fmxg.nstrip+1)*wsmd;           
                      zsmd=-ztotsmd+fmxg.g10thick/2.0;           
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
                            place.TranslateZ(zsmd);              
                            /// Translate z = zsmd              
                            _stacker -> Position( AgBlock::Find("FXGT"), place );              
                      } // end placement of FXGT           
                      xsmdv=-wtotsmd/2.0-fmxg.sgap/2.0+wsmd;           
                      ysmdv=0.0;           
                      zsmdv=zsmd+fmxg.g10thick/2.0+fmxg.sapex/2.0;           
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
                                        place.TranslateX(xsmdv);                    
                                        /// Translate x = xsmdv                    
                                        place.TranslateY(ysmdv);                    
                                        /// Translate y = ysmdv                    
                                        place.TranslateZ(zsmdv);                    
                                        /// Translate z = zsmdv                    
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
                                        place.TranslateX(xsmdv);                    
                                        /// Translate x = xsmdv                    
                                        place.TranslateY(ysmdv);                    
                                        /// Translate y = ysmdv                    
                                        place.TranslateZ(zsmdv);                    
                                        /// Translate z = zsmdv                    
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
                            xsmdv=xsmdv+wsmd;              
                      }           
                      zsmd2=zsmdv+fmxg.g10thick/2.0+fmxg.sapex/2.0;           
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
                            place.TranslateZ(zsmd2);              
                            /// Translate z = zsmd2              
                            _stacker -> Position( AgBlock::Find("FXGT"), place );              
                      } // end placement of FXGT           
                      xsmdh=0.0;           
                      ysmdh=-wtotsmd/2.0-fmxg.sgap/2.0+wsmd;           
                      zsmdh=zsmd2+fmxg.g10thick/2.0+fmxg.sapex/2.0;           
                      _create = AgCreate("FHMS");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FHMS              
                            Create("FHMS");               
                      }           
                      /// Loop on i from 1 to fmxg.nstrip step=1           
                      for ( i=1; (1>0)? (i<=fmxg.nstrip):(i>=fmxg.nstrip); i+=1 )           
                      {              
                            if ( mod(i,2)!=0 )              
                            {                 
                                  { AgPlacement place = AgPlacement("FHMS","FSHM");                    
                                        /// Add daughter volume FHMS to mother FSHM                    
                                        place.TranslateX(xsmdh);                    
                                        /// Translate x = xsmdh                    
                                        place.TranslateY(ysmdh);                    
                                        /// Translate y = ysmdh                    
                                        place.TranslateZ(zsmdh);                    
                                        /// Translate z = zsmdh                    
                                        /// G3 Reference: thetax = 90                    
                                        /// G3 Reference: phix = 0                    
                                        /// G3 Reference: thetay = 90                    
                                        /// G3 Reference: phiy = 90                    
                                        /// G3 Reference: thetaz = 0                    
                                        /// G3 Reference: phiz = 0                    
                                        place.Ortho( "Y-XZ" ); // ORT=Y-XZ                    
                                        /// Axis substitution: XYZ --> Y-XZ                    
                                        _stacker -> Position( AgBlock::Find("FHMS"), place );                    
                                  } // end placement of FHMS                 
                            }              
                            else              
                            {                 
                                  { AgPlacement place = AgPlacement("FHMS","FSHM");                    
                                        /// Add daughter volume FHMS to mother FSHM                    
                                        place.TranslateX(xsmdh);                    
                                        /// Translate x = xsmdh                    
                                        place.TranslateY(ysmdh);                    
                                        /// Translate y = ysmdh                    
                                        place.TranslateZ(zsmdh);                    
                                        /// Translate z = zsmdh                    
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
                            ysmdh=ysmdh+wsmd;              
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
                            shape.par("dx2")=fmxg.sbase/2.0;              
                            shape.par("dy")=fmxg.g10hgt/2.0;              
                            shape.par("dz")=fmxg.sapex/2.0;              
                            /// Shape Trd1 dx1=0 dx2=fmxg.sbase/2.0 dy=fmxg.g10hgt/2.0 dz=fmxg.sapex/2.0               
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
       void FpdmGeo3::ConstructGeometry( const Char_t *dummy )     
       {        
             ///@addtogroup FpdmGeo3_revision        
             ///@{           
                   /// Created:    27 Nov 2006            
             ///@}        
             ///@addtogroup FpdmGeo3_revision        
             ///@{           
                   /// Author: Akio Ogawa           
             ///@}        
             AddBlock("FBOX");        
             AddBlock("FTOW");        
             AddBlock("FLXF");        
             AddBlock("FWAL");        
             AddBlock("FLGR");        
             AddBlock("FPCT");        
             AddBlock("FUMT");        
             AddBlock("PBPT");        
             AddBlock("FSHM");        
             AddBlock("FHMS");        
             AddBlock("FXGT");        
             AddBlock("FALU");        
             AddBlock("FBAS");        
             AddBlock("FENC");        
             AddBlock("FEAC");        
             AddBlock("FEBC");        
             AddBlock("FECC");        
             AddBlock("FEDC");        
             AddBlock("FETC");        
             AddBlock("FERC");        
             AddBlock("FESC");        
             AddBlock("FEEC");        
             n=12;        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup fmcg_doc        
             ///@{           
                   ++fmcg._index;           
                   fmcg . version = 8.0; //  Geometry version             
                   /// fmcg . version = 8.0; //  Geometry version             
                   fmcg . chkvsim = 0; //  = 0 dE, = 1 Cherenkov simulation for PbG            
                   /// fmcg . chkvsim = 0; //  = 0 dE, = 1 Cherenkov simulation for PbG            
                   fmcg . pbplate = 0; //  =0 no plate, =1 with plate            
                   /// fmcg . pbplate = 0; //  =0 no plate, =1 with plate            
                   fmcg . fmsnorthx = -0.3; // Default x-position           
                   /// fmcg . fmsnorthx = -0.3; // Default x-position           
                   fmcg . fmssouthx = +0.3; // Default x-position           
                   /// fmcg . fmssouthx = +0.3; // Default x-position           
                   //           
                   fmcg.fill();           
             ///@}        
             //        
             /// USE fmcg version=8.0;        
             fmcg.Use("version",(Float_t)8.0);        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup fpos_doc        
             ///@{           
                   ++fpos._index;           
                   fpos . imod = 1; //  Module# (EN=1, ES=2, WN=3, WS=4, ...)            
                   /// fpos . imod = 1; //  Module# (EN=1, ES=2, WN=3, WS=4, ...)            
                   fpos . itype = 1; //  Type (1=7*7+SMD+PreShower, 2=17*34+14*28)            
                   /// fpos . itype = 1; //  Type (1=7*7+SMD+PreShower, 2=17*34+14*28)            
                   fpos . x = -48.19; //  X distance from beam to edge of detector            
                   /// fpos . x = -48.19; //  X distance from beam to edge of detector            
                   fpos . y = 0.0; //  Y distance from beam to edge of detector            
                   /// fpos . y = 0.0; //  Y distance from beam to edge of detector            
                   fpos . z = -779.0; //  Z distance from IP to surface of detector            
                   /// fpos . z = -779.0; //  Z distance from IP to surface of detector            
                   fpos . ay = 180; //  Angle aroound Y (0 for west, 180 for east)            
                   /// fpos . ay = 180; //  Angle aroound Y (0 for west, 180 for east)            
                   fpos . az = 0; //  Angle around Z            
                   /// fpos . az = 0; //  Angle around Z            
                   //           
                   fpos.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup fpos_doc        
             ///@{           
                   ++fpos._index;           
                   fpos . imod = 2; //  Module# (EN=1, ES=2, WS=3, WS=4, ...)            
                   /// fpos . imod = 2; //  Module# (EN=1, ES=2, WS=3, WS=4, ...)            
                   fpos . itype = 1; //  Type (1=7*7+SMD+PreShower, 2=17*34+14*28)            
                   /// fpos . itype = 1; //  Type (1=7*7+SMD+PreShower, 2=17*34+14*28)            
                   fpos . x = 48.19; //  X distance from beam to edge of detector            
                   /// fpos . x = 48.19; //  X distance from beam to edge of detector            
                   fpos . y = 0.0; //  Y distance from beam to edge of detector            
                   /// fpos . y = 0.0; //  Y distance from beam to edge of detector            
                   fpos . z = -779.0; //  Z distance from IP to surface of detector            
                   /// fpos . z = -779.0; //  Z distance from IP to surface of detector            
                   fpos . ay = 180; //  Angle aroound Y (0 for west, 180 for east)            
                   /// fpos . ay = 180; //  Angle aroound Y (0 for west, 180 for east)            
                   fpos . az = 0; //  Angle around Z            
                   /// fpos . az = 0; //  Angle around Z            
                   //           
                   fpos.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup fpos_doc        
             ///@{           
                   ++fpos._index;           
                   fpos . imod = 3; //  Module# (EN=1, ES=2, WN=3, WS=4, ...)            
                   /// fpos . imod = 3; //  Module# (EN=1, ES=2, WN=3, WS=4, ...)            
                   fpos . itype = 2; //  Type (1=7*7+SMD+PreShower, 2=17*34+14*28)            
                   /// fpos . itype = 2; //  Type (1=7*7+SMD+PreShower, 2=17*34+14*28)            
                   fpos . x = fmcg.fmsnorthx; //  X distance from beam to edge of detector            
                   /// fpos . x = fmcg.fmsnorthx; //  X distance from beam to edge of detector            
                   fpos . y = 0.0; //  Y distance from beam to edge of detector            
                   /// fpos . y = 0.0; //  Y distance from beam to edge of detector            
                   fpos . z = 706.3; //  Z distance from IP to surface of detector            
                   /// fpos . z = 706.3; //  Z distance from IP to surface of detector            
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
                   fpos . imod = 4; //  Module# (EN=1, ES=2, WN=3, WS=4, ...)            
                   /// fpos . imod = 4; //  Module# (EN=1, ES=2, WN=3, WS=4, ...)            
                   fpos . itype = 2; //  Type (1=7*7+SMD+PreShower, 2=17*34+14*28)            
                   /// fpos . itype = 2; //  Type (1=7*7+SMD+PreShower, 2=17*34+14*28)            
                   fpos . x = fmcg.fmssouthx; //  X distance from beam to edge of detector            
                   /// fpos . x = fmcg.fmssouthx; //  X distance from beam to edge of detector            
                   fpos . y = 0.0; //  Y distance from beam to edge of detector            
                   /// fpos . y = 0.0; //  Y distance from beam to edge of detector            
                   fpos . z = 706.3; //  Z distance from IP to surface of detector            
                   /// fpos . z = 706.3; //  Z distance from IP to surface of detector            
                   fpos . ay = 0; //  Angle aroound Y (0 for west, 180 for east)            
                   /// fpos . ay = 0; //  Angle aroound Y (0 for west, 180 for east)            
                   fpos . az = 0; //  Angle around Z            
                   /// fpos . az = 0; //  Angle around Z            
                   //           
                   fpos.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup fbxd_doc        
             ///@{           
                   ++fbxd._index;           
                   fbxd . type = 1; //  Type (1=7*7+SMD+PreShower, 2=17*34+14*28)            
                   /// fbxd . type = 1; //  Type (1=7*7+SMD+PreShower, 2=17*34+14*28)            
                   fbxd . height = 100; //  Box height            
                   /// fbxd . height = 100; //  Box height            
                   fbxd . depth = 96; //  Box Depth            
                   /// fbxd . depth = 96; //  Box Depth            
                   fbxd . width = 0.0; //  Box Width (only for FMS)            
                   /// fbxd . width = 0.0; //  Box Width (only for FMS)            
                   fbxd . nx = 7; //  Number of pbg in x            
                   /// fbxd . nx = 7; //  Number of pbg in x            
                   fbxd . ny = 7; //  number of pbg in y            
                   /// fbxd . ny = 7; //  number of pbg in y            
                   fbxd . nxl = 0; //  Number of large pbg in x            
                   /// fbxd . nxl = 0; //  Number of large pbg in x            
                   fbxd . nyl = 0; //  number of large pbg in y            
                   /// fbxd . nyl = 0; //  number of large pbg in y            
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
                   fbxd . type = 2; //  Type (1=7*7+SMD+PreShower, 2=17*34+14*28)            
                   /// fbxd . type = 2; //  Type (1=7*7+SMD+PreShower, 2=17*34+14*28)            
                   fbxd . height = 210; //  Box height            
                   /// fbxd . height = 210; //  Box height            
                   fbxd . depth = 98.425; //  Box Depth            
                   /// fbxd . depth = 98.425; //  Box Depth            
                   fbxd . width = 127.0; //  Box Width (only for FMS)            
                   /// fbxd . width = 127.0; //  Box Width (only for FMS)            
                   fbxd . nx = 12; //  Number of pbg in x            
                   /// fbxd . nx = 12; //  Number of pbg in x            
                   fbxd . ny = 24; //  number of pbg in y            
                   /// fbxd . ny = 24; //  number of pbg in y            
                   fbxd . nxl = 17; //  Number of large pbg in x            
                   /// fbxd . nxl = 17; //  Number of large pbg in x            
                   fbxd . nyl = 34; //  number of large pbg in y            
                   /// fbxd . nyl = 34; //  number of large pbg in y            
                   fbxd . xoffset = (6*3.822+0.5*5.812)+(127.0-17*5.812)/2.0; //  tower x offset from box edge to PbG edge            
                   /// fbxd . xoffset = (6*3.822+0.5*5.812)+(127.0-17*5.812)/2.0; //  tower x offset from box edge to PbG edge            
                   fbxd . zoffset = 10.4; //  tower z offset from box edge to PbG edge            
                   /// fbxd . zoffset = 10.4; //  tower z offset from box edge to PbG edge            
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
                   flgm . radlen = 3.21; //  radiation length [cm]            
                   /// flgm . radlen = 3.21; //  radiation length [cm]            
                   flgm . pbcont = 45.0; //  PbO content [%]            
                   /// flgm . pbcont = 45.0; //  PbO content [%]            
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
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup inse_doc        
             ///@{           
                   ++inse._index;           
                   inse . width = 19.30908; //  Width of the insert (x)            
                   /// inse . width = 19.30908; //  Width of the insert (x)            
                   inse . depth = 98.425; //  Depth of the insert (z)            
                   /// inse . depth = 98.425; //  Depth of the insert (z)            
                   inse . height = 38.608; //  Height of the insert (y)              
                   /// inse . height = 38.608; //  Height of the insert (y)              
                   inse . sheetdpt = 1.27; //  Depth of the steel parts (x,y)            
                   /// inse . sheetdpt = 1.27; //  Depth of the steel parts (x,y)            
                   inse . holegap = 5.08; //  Distance between edge of INSERT and square hole            
                   /// inse . holegap = 5.08; //  Distance between edge of INSERT and square hole            
                   inse . holedepth = 25.4; //  Depth of the square hole (z)            
                   /// inse . holedepth = 25.4; //  Depth of the square hole (z)            
                   inse . holeheight = 30.48; //  Height of the square hole (y)            
                   /// inse . holeheight = 30.48; //  Height of the square hole (y)            
                   inse . gapdepth = 19.685; //  Depth of the iron distancer (z)            
                   /// inse . gapdepth = 19.685; //  Depth of the iron distancer (z)            
                   inse . gapheight = 7.874; //  Height of the iron distancer (y)            
                   /// inse . gapheight = 7.874; //  Height of the iron distancer (y)            
                   inse . gatedepth = 1.905; //  Depth of one of the three Gates (z)            
                   /// inse . gatedepth = 1.905; //  Depth of one of the three Gates (z)            
                   inse . gategap = 12.7; //  Distance between the back edge of the box and last gate            
                   /// inse . gategap = 12.7; //  Distance between the back edge of the box and last gate            
                   inse . ra = 13.97; //  Radius of the inner circle of tubes            
                   /// inse . ra = 13.97; //  Radius of the inner circle of tubes            
                   inse . rb = 20.6375; //  Radius of the outer circle of tubes             
                   /// inse . rb = 20.6375; //  Radius of the outer circle of tubes             
                   inse . diam = 6.0325; //  Diameter of the tubes            
                   /// inse . diam = 6.0325; //  Diameter of the tubes            
                   inse . rmax = 10.16; //  Radius of the inner tube for beampipe            
                   /// inse . rmax = 10.16; //  Radius of the inner tube for beampipe            
                   //           
                   inse.fill();           
             ///@}        
             //        
             /// USE fmcg _index=1;        
             fmcg.Use();        
             // Print<level=%i> fmt=%s fortran format statements not supported        
             /// Loop on m from 1 to 4 step=1        
             for ( m=1; (1>0)? (m<=4):(m>=4); m+=1 )        
             {           
                   /// USE fpos imod=m ;           
                   fpos.Use("imod",(Float_t)m );           
                   /// USE fbxd type=fpos.itype ;           
                   fbxd.Use("type",(Float_t)fpos.itype );           
                   if ( fbxd.type==1 )           
                   {              
                         /// USE flgg type=1 ;              
                         flgg.Use("type",(Float_t)1 );              
                         wid  =  flgg.width + flgg.dgap + flgg.althick*2.0;              
                         ztot = (flgg.depth + flgg.althick + flgg.mumetdz)/2.0;              
                         rtot = fbxd.nx*wid/2.0;              
                         bwid = rtot+fbxd.xoffset;              
                   }           
                   else           
                   {              
                         /// USE flgg type=2 ;              
                         flgg.Use("type",(Float_t)2 );              
                         wid  =  flgg.width + flgg.dgap + flgg.althick*2.0;              
                         ztot = (flgg.depth + flgg.althick + flgg.mumetdz)/2.0;              
                         rtot = fbxd.nxl*wid/2.0;              
                         bwid = rtot;              
                   }           
                   if ( m>=3 )           
                   {              
                         bwid=fbxd.width/2.0;              
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
                   }           
                   else           
                   {              
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
                   sern=0;           
                   if ( m==4 )           
                   {              
                         sern=1;              
                   }           
                   if ( m!=7 )           
                   {              
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
                               place.par("only")=AgPlacement::kMany;                 
                               /// Overlap: agplacement::kmany                 
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
             }        
       }; // FpdmGeo3     
 }; // namespace FpdmGeo3  
 