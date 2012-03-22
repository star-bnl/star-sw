#include "CaveGeo.h"  
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
 namespace CAVEGEO // $NMSPC  
 {     
       //     
       // ---------------------------------------------------------------------------------------------------     
          //  -----------------------------------------------------     
          /// @defgroup cvcf_doc     
          /// \class Cvcf_t     
          /// \brief User-defined structure     
          ///                             
          /// AgML structure members:     
          ///                             
          ///Float_t version;     
          ///Int_t config;     
          ///Int_t _index;     
          //     
          Cvcf_t cvcf;     
          //     
          //  -----------------------------------------------------     
          /// @defgroup cave_doc     
          /// \class Cave_t     
          /// \brief User-defined structure     
          ///                             
          /// AgML structure members:     
          ///                             
          ///Float_t version;     
          ///Float_t rmin;     
          ///Array_t<Float_t> rmax;     
          ///Array_t<Float_t> dz;     
          ///Float_t dconc;     
          ///Int_t _index;     
          //     
          Cave_t cave;     
          //     
          ///@addtogroup CaveGeo_vars     
          ///@{        
                Float_t d1,d2,z1;        
                //        
                /// Float_t d1,d2,z1        
          ///@}     
       CaveGeo::CaveGeo()     
         : AgModule("CaveGeo","  builds CAVE for GSTAR ")     
       {        
       }     
          // ---------------------------------------------------------------------------------------------------     
          void HALL::Block( AgCreate create )     
          {         
                ///@addtogroup HALL_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Component Si	a=28.08	z=14	w=1           
                      /// Component O2	a=16	z=8	w=2           
                      /// Mixture Concrete dens=2.5           
                      {  AgMaterial &mix = AgMaterial::Get("Concrete");              
                            mix.Component("Si",28.08,14,1);              
                            mix.Component("O2",16,8,2);              
                            mix.par("dens")=2.5;              
                            mix.lock();              
                            _material = mix;              
                            _material.lock();              
                      }           
                      /// Medium Standard           
                      {  AgMedium med = AgMedium::CopyMedium("Standard");              
                            _medium = med;              
                      }           
                      { AgAttribute attr = AgAttribute("HALL");              
                            attr.par("seen")=1;              
                            attr.par("colo")=2;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      d1=cave.rmax(1)+cave.dconc;           
                      d2=cave.rmax(2)+cave.dconc;           
                      z1=cave.dz(1)+cave.dconc;           
                      if ( cvcf.config>=5 )           
                      {              
                            {  AgShape shape = AgShape("Pgon");                 
                                  shape     .Inherit( AgBlock::previous() );                 
                                  create     .SetParameters(shape);                 
                                  shape.par("phi1")=45;                 
                                  shape.par("dphi")=360;                 
                                  shape.par("npdiv")=4;                 
                                  shape.par("nz")=6;                 
                                  shape.Z(0)=-cave.dz(2);                 
                                  shape.Z(1)=-z1;                 
                                  shape.Z(2)=-z1;                 
                                  shape.Z(3)=z1;                 
                                  shape.Z(4)=z1;                 
                                  shape.Z(5)=cave.dz(2);                 
                                  shape.Rmin(0)=cave.rmin;                 
                                  shape.Rmin(1)=cave.rmin;                 
                                  shape.Rmin(2)=cave.rmin;                 
                                  shape.Rmin(3)=cave.rmin;                 
                                  shape.Rmin(4)=cave.rmin;                 
                                  shape.Rmin(5)=cave.rmin;                 
                                  shape.Rmax(0)=d2;                 
                                  shape.Rmax(1)=d2;                 
                                  shape.Rmax(2)=d1;                 
                                  shape.Rmax(3)=d1;                 
                                  shape.Rmax(4)=d2;                 
                                  shape.Rmax(5)=d2;                 
                                  /// Shape Pgon phi1=45 dphi=360 npdiv=4 nz=6                  
                                  _same_shape &= _stacker->SearchVolume( shape, _attribute );                 
                                  _shape = shape;                 
                                  if (_same_shape) goto END_OF_HALL;                 
                                  _stacker -> Build(this);                 
                            }              
                      }           
                      else           
                      {              
                            {  AgShape shape = AgShape("Pcon");                 
                                  shape     .Inherit( AgBlock::previous() );                 
                                  create     .SetParameters(shape);                 
                                  shape.par("phi1")=0;                 
                                  shape.par("dphi")=360;                 
                                  shape.par("nz")=6;                 
                                  shape.Z(0)=-cave.dz(2);                 
                                  shape.Z(1)=-z1;                 
                                  shape.Z(2)=-z1;                 
                                  shape.Z(3)=z1;                 
                                  shape.Z(4)=z1;                 
                                  shape.Z(5)=cave.dz(2);                 
                                  shape.Rmin(0)=cave.rmin;                 
                                  shape.Rmin(1)=cave.rmin;                 
                                  shape.Rmin(2)=cave.rmin;                 
                                  shape.Rmin(3)=cave.rmin;                 
                                  shape.Rmin(4)=cave.rmin;                 
                                  shape.Rmin(5)=cave.rmin;                 
                                  shape.Rmax(0)=d2;                 
                                  shape.Rmax(1)=d2;                 
                                  shape.Rmax(2)=d1;                 
                                  shape.Rmax(3)=d1;                 
                                  shape.Rmax(4)=d2;                 
                                  shape.Rmax(5)=d2;                 
                                  /// Shape Pcon phi1=0 dphi=360 nz=6                  
                                  _same_shape &= _stacker->SearchVolume( shape, _attribute );                 
                                  _shape = shape;                 
                                  if (_same_shape) goto END_OF_HALL;                 
                                  _stacker -> Build(this);                 
                            }              
                      }           
                      _create = AgCreate("CAVE");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create CAVE              
                            Create("CAVE");               
                      }           
                      { AgPlacement place = AgPlacement("CAVE","HALL");              
                            /// Add daughter volume CAVE to mother HALL              
                            _stacker -> Position( AgBlock::Find("CAVE"), place );              
                      } // end placement of CAVE           
                      END_OF_HALL:           
                      mCurrent = _save;           
                ///@}        
          } // End Block HALL     
          // ---------------------------------------------------------------------------------------------------     
          void CAVE::Block( AgCreate create )     
          {         
                ///@addtogroup CAVE_doc        
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
                      /// Medium something           
                      ///  stemax = 100            
                      {  AgMedium &med = AgMedium::Get("Something");              
                               med.Inherit(this);              
                            med.par("stemax")=100 ;              
                            _medium = med;              
                      }           
                      { AgAttribute attr = AgAttribute("CAVE");              
                            attr.par("seen")=1;              
                            attr.par("colo")=2;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      if ( cvcf.config>=5 )           
                      {              
                            {  AgShape shape = AgShape("Pgon");                 
                                  shape     .Inherit( AgBlock::previous() );                 
                                  create     .SetParameters(shape);                 
                                  shape.par("phi1")=45;                 
                                  shape.par("dphi")=360;                 
                                  shape.par("npdiv")=4;                 
                                  shape.Z(0)=-cave.dz(2);                 
                                  shape.Z(1)=-cave.dz(1);                 
                                  shape.Z(2)=-cave.dz(1);                 
                                  shape.Z(3)=+cave.dz(1);                 
                                  shape.Z(4)=+cave.dz(1);                 
                                  shape.Z(5)=+cave.dz(2);                 
                                  shape.Rmax(0)=+cave.rmax(2);                 
                                  shape.Rmax(1)=+cave.rmax(2);                 
                                  shape.Rmax(2)=+cave.rmax(1);                 
                                  shape.Rmax(3)=+cave.rmax(1);                 
                                  shape.Rmax(4)=+cave.rmax(2);                 
                                  shape.Rmax(5)=+cave.rmax(2);                 
                                  /// Shape Pgon phi1=45 dphi=360 npdiv=4                  
                                  _same_shape &= _stacker->SearchVolume( shape, _attribute );                 
                                  _shape = shape;                 
                                  if (_same_shape) goto END_OF_CAVE;                 
                                  _stacker -> Build(this);                 
                            }              
                      }           
                      else           
                      {              
                            {  AgShape shape = AgShape("Pcon");                 
                                  shape     .Inherit( AgBlock::previous() );                 
                                  create     .SetParameters(shape);                 
                                  shape.par("phi1")=0;                 
                                  shape.par("dphi")=360;                 
                                  shape.Z(0)=-cave.dz(2);                 
                                  shape.Z(1)=-cave.dz(1);                 
                                  shape.Z(2)=-cave.dz(1);                 
                                  shape.Z(3)=+cave.dz(1);                 
                                  shape.Z(4)=+cave.dz(1);                 
                                  shape.Z(5)=+cave.dz(2);                 
                                  shape.Rmax(0)=+cave.rmax(2);                 
                                  shape.Rmax(1)=+cave.rmax(2);                 
                                  shape.Rmax(2)=+cave.rmax(1);                 
                                  shape.Rmax(3)=+cave.rmax(1);                 
                                  shape.Rmax(4)=+cave.rmax(2);                 
                                  shape.Rmax(5)=+cave.rmax(2);                 
                                  /// Shape Pcon phi1=0 dphi=360                  
                                  _same_shape &= _stacker->SearchVolume( shape, _attribute );                 
                                  _shape = shape;                 
                                  if (_same_shape) goto END_OF_CAVE;                 
                                  _stacker -> Build(this);                 
                            }              
                      }           
                      END_OF_CAVE:           
                      mCurrent = _save;           
                ///@}        
          } // End Block CAVE     
    // ----------------------------------------------------------------------- geoctr
       void CaveGeo::ConstructGeometry( const Char_t *dummy )     
       {        
             ///@addtogroup CaveGeo_revision        
             ///@{           
                   /// Author: Peter Jacobs, LBL           
             ///@}        
             ///@addtogroup CaveGeo_revision        
             ///@{           
                   /// Created:   March 10, 1995            
             ///@}        
             AddBlock("CAVE");        
             AddBlock("HALL");        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup cvcf_doc        
             ///@{           
                   ++cvcf._index;           
                   cvcf . version = 1; //   version            
                   /// cvcf . version = 1; //   version            
                   cvcf . config = 1; //   default config            
                   /// cvcf . config = 1; //   default config            
                   //           
                   cvcf.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup cave_doc        
             ///@{           
                   ++cave._index;           
                   cave . version = 1; //  geometry version            
                   /// cave . version = 1; //  geometry version            
                   cave . rmin = 0; //  inner radius            
                   /// cave . rmin = 0; //  inner radius            
                   cave . rmax.at(0) = 400; //  outer radius            
                   ///cave . rmax.at(0) = 400; //  outer radius            
                   cave . rmax.at(1) = 100; //  outer radius            
                   ///cave . rmax.at(1) = 100; //  outer radius            
                   cave . dz.at(0) = 800; //  half length            
                   ///cave . dz.at(0) = 800; //  half length            
                   cave . dz.at(1) = 2000; //  half length            
                   ///cave . dz.at(1) = 2000; //  half length            
                   cave . dconc = 20; //  concrete thickness            
                   /// cave . dconc = 20; //  concrete thickness            
                   //           
                   cave.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup cave_doc        
             ///@{           
                   ++cave._index;           
                   cave . version = 2; //  geometry version            
                   /// cave . version = 2; //  geometry version            
                   cave . rmin = 0; //  inner radius            
                   /// cave . rmin = 0; //  inner radius            
                   cave . rmax.at(0) = 400; //  outer radius            
                   ///cave . rmax.at(0) = 400; //  outer radius            
                   cave . rmax.at(1) = 213; //  outer radius            
                   ///cave . rmax.at(1) = 213; //  outer radius            
                   cave . dz.at(0) = 800; //  half length            
                   ///cave . dz.at(0) = 800; //  half length            
                   cave . dz.at(1) = 5000; //  half length            
                   ///cave . dz.at(1) = 5000; //  half length            
                   cave . dconc = 50; //  concrete thickness            
                   /// cave . dconc = 50; //  concrete thickness            
                   //           
                   cave.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup cave_doc        
             ///@{           
                   ++cave._index;           
                   cave . version = 3; //  geometry version            
                   /// cave . version = 3; //  geometry version            
                   cave . rmin = 0; //  inner radius            
                   /// cave . rmin = 0; //  inner radius            
                   cave . rmax.at(0) = 450; //  outer radius            
                   ///cave . rmax.at(0) = 450; //  outer radius            
                   cave . rmax.at(1) = 100; //  outer radius            
                   ///cave . rmax.at(1) = 100; //  outer radius            
                   cave . dz.at(0) = 800; //  half length            
                   ///cave . dz.at(0) = 800; //  half length            
                   cave . dz.at(1) = 2000; //  half length            
                   ///cave . dz.at(1) = 2000; //  half length            
                   cave . dconc = 50; //  concrete thickness            
                   /// cave . dconc = 50; //  concrete thickness            
                   //           
                   cave.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup cave_doc        
             ///@{           
                   ++cave._index;           
                   cave . version = 4; //  geometry version            
                   /// cave . version = 4; //  geometry version            
                   cave . rmin = 0; //  inner radius            
                   /// cave . rmin = 0; //  inner radius            
                   cave . rmax.at(0) = 450; //  outer radius            
                   ///cave . rmax.at(0) = 450; //  outer radius            
                   cave . rmax.at(1) = 100; //  outer radius            
                   ///cave . rmax.at(1) = 100; //  outer radius            
                   cave . dz.at(0) = 950; //  half length            
                   ///cave . dz.at(0) = 950; //  half length            
                   cave . dz.at(1) = 2000; //  half length            
                   ///cave . dz.at(1) = 2000; //  half length            
                   cave . dconc = 50; //  concrete thickness            
                   /// cave . dconc = 50; //  concrete thickness            
                   //           
                   cave.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup cave_doc        
             ///@{           
                   ++cave._index;           
                   cave . version = 5; //  version            
                   /// cave . version = 5; //  version            
                   cave . rmin = 0; //  Inner radius of cave had better be zero            
                   /// cave . rmin = 0; //  Inner radius of cave had better be zero            
                   cave . rmax.at(0) = 1575.02; //  furthest distance to a concrete wall            
                   ///cave . rmax.at(0) = 1575.02; //  furthest distance to a concrete wall            
                   cave . rmax.at(1) = 329.95 ; //  furthest distance to a concrete wall            
                   ///cave . rmax.at(1) = 329.95 ; //  furthest distance to a concrete wall            
                   cave . dz.at(0) = 807.72; //  Dz(1) is distance to east/west wall from center of STAR, Dz(2) depth into tunnels            
                   ///cave . dz.at(0) = 807.72; //  Dz(1) is distance to east/west wall from center of STAR, Dz(2) depth into tunnels            
                   cave . dz.at(1) = 4000.0 ; //  Dz(1) is distance to east/west wall from center of STAR, Dz(2) depth into tunnels            
                   ///cave . dz.at(1) = 4000.0 ; //  Dz(1) is distance to east/west wall from center of STAR, Dz(2) depth into tunnels            
                   cave . dconc = 50.0; //  concrete thickness            
                   /// cave . dconc = 50.0; //  concrete thickness            
                   //           
                   cave.fill();           
             ///@}        
             //        
             /// USE cvcf _index=1;        
             cvcf.Use();        
             /// USE cave version=cvcf.config ;        
             cave.Use("version",(Float_t)cvcf.config );        
             _create = AgCreate("HALL");        
             {           
                   AgShape myshape; // undefined shape           
                   ///Create HALL           
                   Create("HALL");            
             }        
             if ( cave.version==5 )        
             {           
                   AgDETPnew( "wall" );// CALL AgDETPnew           
                   AgDETPadd( "cdim(1).rmax=",cave.rmax(1),1 );// CALL AgDETPadd           
                   AgDETPadd( "tdim(1).rmax=",cave.rmax(2),1 );// CALL AgDETPadd           
                   AgDETPadd( "cdim(1).dz=",cave.dz(1),1 );// CALL AgDETPadd           
                   AgDETPadd( "tdim(1).dz=",cave.dz(2)-cave.dz(1),1 );// CALL AgDETPadd           
             }        
       }; // CaveGeo     
 }; // namespace CaveGeo  
 