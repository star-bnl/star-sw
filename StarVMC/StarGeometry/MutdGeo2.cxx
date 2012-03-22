#include "MutdGeo2.h"  
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
 namespace MUTDGEO2 // $NMSPC  
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
          ///@addtogroup MutdGeo2_vars     
          ///@{        
                Int_t ntray,iwid;        
                //        
                /// Int_t ntray,iwid        
          ///@}     
          ///@addtogroup MutdGeo2_vars     
          ///@{        
                Int_t is;        
                //        
                /// Int_t is        
          ///@}     
          ///@addtogroup MutdGeo2_vars     
          ///@{        
                Float_t barphi,xpos,ypos,zpos,sublen,subcen,totlen;        
                //        
                /// Float_t barphi,xpos,ypos,zpos,sublen,subcen,totlen        
          ///@}     
       MutdGeo2::MutdGeo2()     
         : AgModule("MutdGeo2"," is the geometry of the STAR muon trigger system ")     
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
                      /// Loop on is from 1 to 60 step=1           
                      for ( is=1; (1>0)? (is<=60):(is>=60); is+=1 )           
                      {              
                            _create = AgCreate("MUSC");              
                            {                 
                                  AgShape myshape; // undefined shape                 
                                  ///Create MUSC                 
                                  Create("MUSC");                  
                            }              
                            { AgPlacement place = AgPlacement("MUSC","MUTD");                 
                                  /// Add daughter volume MUSC to mother MUTD                 
                                  place.AlphaZ(barphi/2.0+barphi*is);                 
                                  /// Rotate: AlphaZ = barphi/2.0+barphi*is                 
                                  /// G3 Reference: thetax = 90                 
                                  /// G3 Reference: phix = 0                 
                                  /// G3 Reference: thetay = 90                 
                                  /// G3 Reference: phiy = 90                 
                                  /// G3 Reference: thetaz = 0                 
                                  /// G3 Reference: phiz = 0                 
                                  _stacker -> Position( AgBlock::Find("MUSC"), place );                 
                            } // end placement of MUSC              
                      }           
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
                            place.TranslateX(2.50/2.+mtdg.rpmtout);              
                            /// Translate x = 2.50/2.+mtdg.rpmtout              
                            place.TranslateY(0);              
                            /// Translate y = 0              
                            place.TranslateZ(0);              
                            /// Translate z = 0              
                            _stacker -> Position( AgBlock::Find("MMRP"), place );              
                      } // end placement of MMRP           
                      END_OF_MUSC:           
                      mCurrent = _save;           
                ///@}        
          } // End Block MUSC     
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
                            shape.par("dz")=mtdg.length/2;              
                            /// Shape Bbox dx=2.50/2. dy=57.20/2. dz=mtdg.length/2               
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
       void MutdGeo2::ConstructGeometry( const Char_t *dummy )     
       {        
             ///@addtogroup MutdGeo2_revision        
             ///@{           
                   /// Author: Maxim Potekhin           
             ///@}        
             ///@addtogroup MutdGeo2_revision        
             ///@{           
                   /// Created:    21 March 2006            
             ///@}        
             AddBlock("MUTD");        
             AddBlock("MUSC");        
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
             /// USE mtdg _index=1;        
             mtdg.Use();        
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
       }; // MutdGeo2     
 }; // namespace MutdGeo2  
 