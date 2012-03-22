#include "FsceGeo.h"  
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
 namespace FSCEGEO // $NMSPC  
 {     
       //     
       // ---------------------------------------------------------------------------------------------------     
          //  -----------------------------------------------------     
          /// @defgroup fscp_doc     
          /// \class Fscp_t     
          /// \brief User-defined structure     
          ///                             
          /// AgML structure members:     
          ///                             
          ///Float_t version;     
          ///Float_t towerwidth;     
          ///Float_t towerlength;     
          ///Float_t ntowersx;     
          ///Float_t ntowersy;     
          ///Float_t nempty;     
          ///Float_t distfromvtx;     
          ///Int_t _index;     
          //     
          Fscp_t fscp;     
          //     
          ///@addtogroup FsceGeo_vars     
          ///@{        
                Int_t xdim,ydim,xposmin,xposmax,yposmin,yposmax;        
                //        
                /// Int_t xdim,ydim,xposmin,xposmax,yposmin,yposmax        
          ///@}     
       FsceGeo::FsceGeo()     
         : AgModule("FsceGeo"," is the geometry of the Fiber Sampling Calorimeter ")     
       {        
       }     
          // ---------------------------------------------------------------------------------------------------     
          void FSCE::Block( AgCreate create )     
          {         
                ///@addtogroup FSCE_doc        
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
                      { AgAttribute attr = AgAttribute("FSCE");              
                            attr.par("seen")=0;              
                            attr.par("colo")=7;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=fscp.towerwidth*fscp.ntowersx;              
                            shape.par("dy")=fscp.towerwidth*fscp.ntowersy;              
                            shape.par("dz")=fscp.towerlength;              
                            /// Shape Bbox dx=fscp.towerwidth*fscp.ntowersx dy=fscp.towerwidth*fscp.ntowersy dz=fscp.towerlength               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FSCE;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("FSCT");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FSCT              
                            Create("FSCT");               
                      }           
                      xposmin = fscp.ntowersx - fscp.nempty;           
                      xposmax = fscp.ntowersx + fscp.nempty + 1;           
                      yposmin = fscp.ntowersy - fscp.nempty;           
                      yposmax = fscp.ntowersy + fscp.nempty + 1;           
                      /// Loop on xdim from 1 to fscp.ntowersx*2 step=1           
                      for ( xdim=1; (1>0)? (xdim<=fscp.ntowersx*2):(xdim>=fscp.ntowersx*2); xdim+=1 )           
                      {              
                            /// Loop on ydim from 1 to fscp.ntowersy*2 step=1              
                            for ( ydim=1; (1>0)? (ydim<=fscp.ntowersy*2):(ydim>=fscp.ntowersy*2); ydim+=1 )              
                            {                 
                                  if ( xdim>xposmin&&xdim<xposmax&&ydim>yposmin&&ydim<yposmax )                 
                                  {                    
                                  }                 
                                  else                 
                                  {                    
                                        { AgPlacement place = AgPlacement("FSCT","FSCE");                       
                                              /// Add daughter volume FSCT to mother FSCE                       
                                              place.TranslateX(-fscp.towerwidth*2*fscp.ntowersx-fscp.towerwidth+xdim*fscp.towerwidth*2);                       
                                              /// Translate x = -fscp.towerwidth*2*fscp.ntowersx-fscp.towerwidth+xdim*fscp.towerwidth*2                       
                                              place.TranslateY(-fscp.towerwidth*2*fscp.ntowersy-fscp.towerwidth+ydim*fscp.towerwidth*2);                       
                                              /// Translate y = -fscp.towerwidth*2*fscp.ntowersy-fscp.towerwidth+ydim*fscp.towerwidth*2                       
                                              place.TranslateZ(0);                       
                                              /// Translate z = 0                       
                                              _stacker -> Position( AgBlock::Find("FSCT"), place );                       
                                        } // end placement of FSCT                    
                                  }                 
                            }              
                      }           
                      END_OF_FSCE:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FSCE     
          // ---------------------------------------------------------------------------------------------------     
          void FSCT::Block( AgCreate create )     
          {         
                ///@addtogroup FSCT_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Component W	a=183.84	z=74	w=0.923           
                      /// Component C	a=12.01	z=6	w=0.071           
                      /// Component H	a=1	z=1	w=0.006           
                      /// Mixture WSE dens=8.297           
                      {  AgMaterial &mix = AgMaterial::Get("Wse");              
                            mix.Component("W",183.84,74,0.923);              
                            mix.Component("C",12.01,6,0.071);              
                            mix.Component("H",1,1,0.006);              
                            mix.par("dens")=8.297;              
                            mix.lock();              
                            _material = mix;              
                            _material.lock();              
                      }           
                      /// Medium sensitive           
                      ///  isvol = 1            
                      {  AgMedium &med = AgMedium::Get("Sensitive");              
                               med.Inherit(this);              
                            med.par("isvol")=1 ;              
                            _medium = med;              
                      }           
                      { AgAttribute attr = AgAttribute("FSCT");              
                            attr.par("seen")=1;              
                            attr.par("colo")=7;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=fscp.towerwidth;              
                            shape.par("dy")=fscp.towerwidth;              
                            shape.par("dz")=fscp.towerlength;              
                            /// Shape Bbox dx=fscp.towerwidth dy=fscp.towerwidth dz=fscp.towerlength               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FSCT;              
                            _stacker -> Build(this);              
                      }           
                      // _medium.par("CUTGAM") = 0.0005;           
                      // _medium.par("CUTELE") = 0.00015;           
                      END_OF_FSCT:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FSCT     
    // ----------------------------------------------------------------------- geoctr
       void FsceGeo::ConstructGeometry( const Char_t *dummy )     
       {        
             ///@addtogroup FsceGeo_revision        
             ///@{           
                   /// Created:  04-Jul-2011            
             ///@}        
             ///@addtogroup FsceGeo_revision        
             ///@{           
                   /// Author: D.Arkhipkin <arkhipkin@bnl.gov>           
             ///@}        
             AddBlock("FSCE");        
             AddBlock("FSCT");        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup fscp_doc        
             ///@{           
                   ++fscp._index;           
                   fscp . version = 1; //  Geometry version number            
                   /// fscp . version = 1; //  Geometry version number            
                   fscp . towerwidth = 1.25; //  Half width of tower, cm            
                   /// fscp . towerwidth = 1.25; //  Half width of tower, cm            
                   fscp . towerlength = 45.0; //  Half length of tower, cm            
                   /// fscp . towerlength = 45.0; //  Half length of tower, cm            
                   fscp . ntowersx = 20; //  Half number of towers in X dimension            
                   /// fscp . ntowersx = 20; //  Half number of towers in X dimension            
                   fscp . ntowersy = 40; //  Half number of towers in Y dimension            
                   /// fscp . ntowersy = 40; //  Half number of towers in Y dimension            
                   fscp . nempty = 3; //  Half number of towers in a center (hole), like 3 for 6 * 6 hole            
                   /// fscp . nempty = 3; //  Half number of towers in a center (hole), like 3 for 6 * 6 hole            
                   fscp . distfromvtx = 716.7; //  Distance from event vertex (0,0) in cm, should equal to FMS            
                   /// fscp . distfromvtx = 716.7; //  Distance from event vertex (0,0) in cm, should equal to FMS            
                   //           
                   fscp.fill();           
             ///@}        
             //        
             /// USE fscp version= 1 ;        
             fscp.Use("version",(Float_t) 1 );        
             _create = AgCreate("FSCE");        
             {           
                   AgShape myshape; // undefined shape           
                   ///Create FSCE           
                   Create("FSCE");            
             }        
             { AgPlacement place = AgPlacement("FSCE","CAVE");           
                   /// Add daughter volume FSCE to mother CAVE           
                   place.TranslateZ(fscp.distfromvtx+fscp.towerlength);           
                   /// Translate z = fscp.distfromvtx+fscp.towerlength           
                   _stacker -> Position( AgBlock::Find("FSCE"), place );           
             } // end placement of FSCE        
       }; // FsceGeo     
 }; // namespace FsceGeo  
 