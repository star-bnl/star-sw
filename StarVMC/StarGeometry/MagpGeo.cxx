#include "MagpGeo.h"  
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
 namespace MAGPGEO // $NMSPC  
 {     
       //     
       // ---------------------------------------------------------------------------------------------------     
          //  -----------------------------------------------------     
          /// @defgroup magg_doc     
          /// \class Magg_t     
          /// \brief User-defined structure     
          ///                             
          /// AgML structure members:     
          ///                             
          ///Float_t version;     
          ///Float_t rmax;     
          ///Float_t length;     
          ///Float_t test;     
          ///Int_t _index;     
          //     
          Magg_t magg;     
          //     
          //  -----------------------------------------------------     
          /// @defgroup mbar_doc     
          /// \class Mbar_t     
          /// \brief User-defined structure     
          ///                             
          /// AgML structure members:     
          ///                             
          ///Float_t coilrmn;     
          ///Float_t coilrmx;     
          ///Float_t coillen;     
          ///Float_t retyrmn;     
          ///Float_t retylen;     
          ///Float_t barwidin;     
          ///Float_t barwidou;     
          ///Float_t barheigh;     
          ///Float_t ringrmn;     
          ///Float_t ncoil;     
          ///Array_t<Float_t> zcoil;     
          ///Array_t<Float_t> dzcoil;     
          ///Int_t _index;     
          //     
          Mbar_t mbar;     
          //     
          //  -----------------------------------------------------     
          /// @defgroup mend_doc     
          /// \class Mend_t     
          /// \brief User-defined structure     
          ///                             
          /// AgML structure members:     
          ///                             
          ///Float_t polermn;     
          ///Float_t polez;     
          ///Float_t polermx;     
          ///Float_t tcoilrmn;     
          ///Float_t tcoilrmx;     
          ///Float_t polecavr;     
          ///Float_t polecavd;     
          ///Float_t tcoildz;     
          ///Float_t etacut;     
          ///Int_t _index;     
          //     
          Mend_t mend;     
          //     
          ///@addtogroup MagpGeo_vars     
          ///@{        
                Float_t tantheta,rcorner,zcut,d;        
                //        
                /// Float_t tantheta,rcorner,zcut,d        
          ///@}     
          ///@addtogroup MagpGeo_vars     
          ///@{        
                Int_t i_coil;        
                //        
                /// Int_t i_coil        
          ///@}     
       MagpGeo::MagpGeo()     
         : AgModule("MagpGeo"," is the geometry of the STAR magnet ")     
       {        
       }     
          // ---------------------------------------------------------------------------------------------------     
          void MAGP::Block( AgCreate create )     
          {         
                ///@addtogroup MAGP_doc        
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
                      if ( magg.test==1 )           
                      {              
                            {  AgShape shape = AgShape("Tube");                 
                                  shape     .Inherit( AgBlock::previous() );                 
                                  create     .SetParameters(shape);                 
                                  shape.par("rmin")=mend.polermn;                 
                                  shape.par("rmax")=magg.rmax;                 
                                  shape.par("dz")=magg.length/2;                 
                                  /// Shape Tube rmin=mend.polermn rmax=magg.rmax dz=magg.length/2                  
                                  _same_shape &= _stacker->SearchVolume( shape, _attribute );                 
                                  _shape = shape;                 
                                  if (_same_shape) goto END_OF_MAGP;                 
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
                                  shape.Z(0)=-magg.length/2;                 
                                  shape.Z(1)=-mend.polez;                 
                                  shape.Z(2)=-mend.polez;                 
                                  shape.Z(3)=mend.polez;                 
                                  shape.Z(4)=mend.polez;                 
                                  shape.Z(5)=magg.length/2;                 
                                  shape.Rmin(0)=mend.polermn;                 
                                  shape.Rmin(1)=mend.polermn;                 
                                  shape.Rmin(2)=mbar.coilrmn;                 
                                  shape.Rmin(3)=mbar.coilrmn;                 
                                  shape.Rmin(4)=mend.polermn;                 
                                  shape.Rmin(5)=mend.polermn;                 
                                  shape.Rmax(0)=magg.rmax;                 
                                  shape.Rmax(1)=magg.rmax;                 
                                  shape.Rmax(2)=magg.rmax;                 
                                  shape.Rmax(3)=magg.rmax;                 
                                  shape.Rmax(4)=magg.rmax;                 
                                  shape.Rmax(5)=magg.rmax;                 
                                  /// Shape Pcon phi1=0 dphi=360 nz=6                  
                                  _same_shape &= _stacker->SearchVolume( shape, _attribute );                 
                                  _shape = shape;                 
                                  if (_same_shape) goto END_OF_MAGP;                 
                                  _stacker -> Build(this);                 
                            }              
                      }           
                      _create = AgCreate("COIL");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create COIL              
                            Create("COIL");               
                      }           
                      { AgPlacement place = AgPlacement("COIL","MAGP");              
                            /// Add daughter volume COIL to mother MAGP              
                            _stacker -> Position( AgBlock::Find("COIL"), place );              
                      } // end placement of COIL           
                      _create = AgCreate("MRET");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create MRET              
                            Create("MRET");               
                      }           
                      { AgPlacement place = AgPlacement("MRET","MAGP");              
                            /// Add daughter volume MRET to mother MAGP              
                            _stacker -> Position( AgBlock::Find("MRET"), place );              
                      } // end placement of MRET           
                      _create = AgCreate("MPTV");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create MPTV              
                            Create("MPTV");               
                      }           
                      { AgPlacement place = AgPlacement("MPTV","MAGP");              
                            /// Add daughter volume MPTV to mother MAGP              
                            place.TranslateZ(+mend.polez);              
                            /// Translate z = +mend.polez              
                            _stacker -> Position( AgBlock::Find("MPTV"), place );              
                      } // end placement of MPTV           
                      { AgPlacement place = AgPlacement("MPTV","MAGP");              
                            /// Add daughter volume MPTV to mother MAGP              
                            place.TranslateZ(-mend.polez);              
                            /// Translate z = -mend.polez              
                            /// G3 Reference: thetax = 90              
                            /// G3 Reference: phix = 0              
                            /// G3 Reference: thetay = 90              
                            /// G3 Reference: phiy = 90              
                            /// G3 Reference: thetaz = 180              
                            /// G3 Reference: phiz = 0              
                            Double_t _thetax=90,_phix=0,_thetay=90,_phiy=90,_thetaz=180,_phiz=0;              
                            place.Reference( _thetax, _phix, _thetay, _phiy, _thetaz, _phiz );              
                            _stacker -> Position( AgBlock::Find("MPTV"), place );              
                      } // end placement of MPTV           
                      _create = AgCreate("MRGV");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create MRGV              
                            Create("MRGV");               
                      }           
                      { AgPlacement place = AgPlacement("MRGV","MAGP");              
                            /// Add daughter volume MRGV to mother MAGP              
                            place.TranslateZ(+mbar.coillen/2);              
                            /// Translate z = +mbar.coillen/2              
                            _stacker -> Position( AgBlock::Find("MRGV"), place );              
                      } // end placement of MRGV           
                      { AgPlacement place = AgPlacement("MRGV","MAGP");              
                            /// Add daughter volume MRGV to mother MAGP              
                            place.TranslateZ(-mbar.coillen/2);              
                            /// Translate z = -mbar.coillen/2              
                            /// G3 Reference: thetax = 90              
                            /// G3 Reference: phix = 0              
                            /// G3 Reference: thetay = 90              
                            /// G3 Reference: phiy = 90              
                            /// G3 Reference: thetaz = 180              
                            /// G3 Reference: phiz = 0              
                            Double_t _thetax=90,_phix=0,_thetay=90,_phiy=90,_thetaz=180,_phiz=0;              
                            place.Reference( _thetax, _phix, _thetay, _phiy, _thetaz, _phiz );              
                            _stacker -> Position( AgBlock::Find("MRGV"), place );              
                      } // end placement of MRGV           
                      END_OF_MAGP:           
                      mCurrent = _save;           
                ///@}        
          } // End Block MAGP     
          // ---------------------------------------------------------------------------------------------------     
          void COIL::Block( AgCreate create )     
          {         
                ///@addtogroup COIL_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("COIL");              
                            attr.par("seen")=0;              
                            attr.par("colo")=2;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=mbar.coilrmn;              
                            shape.par("rmax")=mbar.coilrmx;              
                            shape.par("dz")=mbar.coillen/2;              
                            /// Shape Tube rmin=mbar.coilrmn rmax=mbar.coilrmx dz=mbar.coillen/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_COIL;              
                            _stacker -> Build(this);              
                      }           
                      /// Loop on i_coil from 1 to nint(mbar.ncoil/2) step=1           
                      for ( i_coil=1; (1>0)? (i_coil<=nint(mbar.ncoil/2)):(i_coil>=nint(mbar.ncoil/2)); i_coil+=1 )           
                      {              
                            _create = AgCreate("MCSE");              
                            {                 
                                  AgShape myshape; // undefined shape                 
                                  ///Create MCSE                 
                                  Create("MCSE");                  
                            }              
                            { AgPlacement place = AgPlacement("MCSE","COIL");                 
                                  /// Add daughter volume MCSE to mother COIL                 
                                  place.TranslateZ(+mbar.zcoil(i_coil));                 
                                  /// Translate z = +mbar.zcoil(i_coil)                 
                                  _stacker -> Position( AgBlock::Find("MCSE"), place );                 
                            } // end placement of MCSE              
                            { AgPlacement place = AgPlacement("MCSE","COIL");                 
                                  /// Add daughter volume MCSE to mother COIL                 
                                  place.TranslateZ(-mbar.zcoil(i_coil));                 
                                  /// Translate z = -mbar.zcoil(i_coil)                 
                                  _stacker -> Position( AgBlock::Find("MCSE"), place );                 
                            } // end placement of MCSE              
                      }           
                      END_OF_COIL:           
                      mCurrent = _save;           
                ///@}        
          } // End Block COIL     
          // ---------------------------------------------------------------------------------------------------     
          void MCSE::Block( AgCreate create )     
          {         
                ///@addtogroup MCSE_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Aluminium            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Aluminium");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("MCSE");              
                            attr.par("seen")=1;              
                            attr.par("colo")=3;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dz")=mbar.dzcoil(i_coil)/2;              
                            /// Shape Tube dz=mbar.dzcoil(i_coil)/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_MCSE;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_MCSE:           
                      mCurrent = _save;           
                ///@}        
          } // End Block MCSE     
          // ---------------------------------------------------------------------------------------------------     
          void MRET::Block( AgCreate create )     
          {         
                ///@addtogroup MRET_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("MRET");              
                            attr.par("seen")=0;              
                            attr.par("colo")=3;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=mbar.retyrmn;              
                            shape.par("rmax")=magg.rmax;              
                            shape.par("dz")=mbar.retylen/2;              
                            /// Shape Tube rmin=mbar.retyrmn rmax=magg.rmax dz=mbar.retylen/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_MRET;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("MSEC");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create MSEC              
                            Create("MSEC");               
                      }           
                      END_OF_MRET:           
                      mCurrent = _save;           
                ///@}        
          } // End Block MRET     
          // ---------------------------------------------------------------------------------------------------     
          void MSEC::Block( AgCreate create )     
          {         
                ///@addtogroup MSEC_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      {  AgShape shape = AgShape("Division");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("ndiv")=30;              
                            shape.par("iaxis")=2;              
                            /// Shape Division ndiv=30 iaxis=2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_MSEC;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("MBAR");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create MBAR              
                            Create("MBAR");               
                      }           
                      { AgPlacement place = AgPlacement("MBAR","MSEC");              
                            /// Add daughter volume MBAR to mother MSEC              
                            place.TranslateX(mbar.retyrmn+60.0/2);              
                            /// Translate x = mbar.retyrmn+60.0/2              
                            /// G3 Reference: thetax = 90              
                            /// G3 Reference: phix = 0              
                            /// G3 Reference: thetay = 90              
                            /// G3 Reference: phiy = 90              
                            /// G3 Reference: thetaz = 0              
                            /// G3 Reference: phiz = 0              
                            place.Ortho( "YZX" ); // ORT=YZX              
                            /// Axis substitution: XYZ --> YZX              
                            _stacker -> Position( AgBlock::Find("MBAR"), place );              
                      } // end placement of MBAR           
                      END_OF_MSEC:           
                      mCurrent = _save;           
                ///@}        
          } // End Block MSEC     
          // ---------------------------------------------------------------------------------------------------     
          void MBAR::Block( AgCreate create )     
          {         
                ///@addtogroup MBAR_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Iron            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Iron");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("MBAR");              
                            attr.par("seen")=1;              
                            attr.par("colo")=3;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Trd1");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx1")=mbar.barwidin/2;              
                            shape.par("dx2")=mbar.barwidou/2;              
                            shape.par("dy")=mbar.retylen/2;              
                            shape.par("dz")=mbar.barheigh/2;              
                            /// Shape Trd1 dx1=mbar.barwidin/2 dx2=mbar.barwidou/2 dy=mbar.retylen/2 dz=mbar.barheigh/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_MBAR;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_MBAR:           
                      mCurrent = _save;           
                ///@}        
          } // End Block MBAR     
          // ---------------------------------------------------------------------------------------------------     
          void MRGV::Block( AgCreate create )     
          {         
                ///@addtogroup MRGV_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      d = (mbar.retylen-mbar.coillen)/2;           
                      /// Material Iron            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Iron");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("MRGV");              
                            attr.par("seen")=1;              
                            attr.par("colo")=6;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Pcon");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("phi1")=0;              
                            shape.par("dphi")=360;              
                            shape.par("nz")=4;              
                            shape.Z(0)=0;              
                            shape.Z(1)=d;              
                            shape.Z(2)=d;              
                            shape.Z(3)=(magg.length-mbar.coillen)/2;              
                            shape.Rmin(0)=mbar.ringrmn;              
                            shape.Rmin(1)=mbar.ringrmn;              
                            shape.Rmin(2)=mbar.ringrmn;              
                            shape.Rmin(3)=mbar.ringrmn;              
                            shape.Rmax(0)=mbar.retyrmn;              
                            shape.Rmax(1)=mbar.retyrmn;              
                            shape.Rmax(2)=magg.rmax;              
                            shape.Rmax(3)=magg.rmax;              
                            /// Shape Pcon phi1=0 dphi=360 nz=4               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_MRGV;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_MRGV:           
                      mCurrent = _save;           
                ///@}        
          } // End Block MRGV     
          // ---------------------------------------------------------------------------------------------------     
          void MPTV::Block( AgCreate create )     
          {         
                ///@addtogroup MPTV_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      zcut     = mend.tcoilrmn*tantheta-mend.polez;           
                      rcorner  = magg.length/tantheta/2;           
                      /// Material Iron            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Iron");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("MPTV");              
                            attr.par("seen")=1;              
                            attr.par("colo")=6;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Pcon");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("phi1")=0;              
                            shape.par("dphi")=360;              
                            shape.par("nz")=3;              
                            shape.Z(0)=0;              
                            shape.Z(1)=zcut;              
                            shape.Z(2)=magg.length/2-mend.polez;              
                            shape.Rmin(0)=mend.tcoilrmn;              
                            shape.Rmin(1)=mend.tcoilrmn;              
                            shape.Rmin(2)=rcorner;              
                            shape.Rmax(0)=mend.polermx;              
                            shape.Rmax(1)=mend.polermx;              
                            shape.Rmax(2)=mend.polermx;              
                            /// Shape Pcon phi1=0 dphi=360 nz=3               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_MPTV;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("MPCV");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create MPCV              
                            Create("MPCV");               
                      }           
                      { AgPlacement place = AgPlacement("MPCV","MPTV");              
                            /// Add daughter volume MPCV to mother MPTV              
                            place.TranslateZ(+mend.polecavd/2);              
                            /// Translate z = +mend.polecavd/2              
                            _stacker -> Position( AgBlock::Find("MPCV"), place );              
                      } // end placement of MPCV           
                      END_OF_MPTV:           
                      mCurrent = _save;           
                ///@}        
          } // End Block MPTV     
          // ---------------------------------------------------------------------------------------------------     
          void MPCV::Block( AgCreate create )     
          {         
                ///@addtogroup MPCV_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Aluminium            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Aluminium");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("MPCV");              
                            attr.par("seen")=1;              
                            attr.par("colo")=7;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=mend.tcoilrmn;              
                            shape.par("rmax")=mend.polecavr;              
                            shape.par("dz")=mend.polecavd/2;              
                            /// Shape Tube rmin=mend.tcoilrmn rmax=mend.polecavr dz=mend.polecavd/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_MPCV;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("MTCL");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create MTCL              
                            Create("MTCL");               
                      }           
                      { AgPlacement place = AgPlacement("MTCL","MPCV");              
                            /// Add daughter volume MTCL to mother MPCV              
                            place.TranslateZ(+mend.tcoildz/2-mend.polecavd/2);              
                            /// Translate z = +mend.tcoildz/2-mend.polecavd/2              
                            _stacker -> Position( AgBlock::Find("MTCL"), place );              
                      } // end placement of MTCL           
                      END_OF_MPCV:           
                      mCurrent = _save;           
                ///@}        
          } // End Block MPCV     
          // ---------------------------------------------------------------------------------------------------     
          void MTCL::Block( AgCreate create )     
          {         
                ///@addtogroup MTCL_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Aluminium            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Aluminium");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("MTCL");              
                            attr.par("seen")=1;              
                            attr.par("colo")=3;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=mend.tcoilrmn;              
                            shape.par("rmax")=mend.tcoilrmx;              
                            shape.par("dz")=mend.tcoildz/2;              
                            /// Shape Tube rmin=mend.tcoilrmn rmax=mend.tcoilrmx dz=mend.tcoildz/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_MTCL;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_MTCL:           
                      mCurrent = _save;           
                ///@}        
          } // End Block MTCL     
    // ----------------------------------------------------------------------- geoctr
       void MagpGeo::ConstructGeometry( const Char_t *dummy )     
       {        
             ///@addtogroup MagpGeo_revision        
             ///@{           
                   /// Author: Pavel Nevski           
             ///@}        
             ///@addtogroup MagpGeo_revision        
             ///@{           
                   /// Created:    19 March 1996            
             ///@}        
             AddBlock("MAGP");        
             AddBlock("COIL");        
             AddBlock("MTCL");        
             AddBlock("MPTV");        
             AddBlock("MPCV");        
             AddBlock("MRET");        
             AddBlock("MRGV");        
             AddBlock("MSEC");        
             AddBlock("MBAR");        
             AddBlock("MCSE");        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup magg_doc        
             ///@{           
                   ++magg._index;           
                   magg . version = 1; //  version number            
                   /// magg . version = 1; //  version number            
                   magg . rmax = 364.29; //  outer radius of the magnet system            
                   /// magg . rmax = 364.29; //  outer radius of the magnet system            
                   magg . length = 715.00; //  magnet system full length            
                   /// magg . length = 715.00; //  magnet system full length            
                   magg . test = 0; //  geometry type: 0-standard, 1-test            
                   /// magg . test = 0; //  geometry type: 0-standard, 1-test            
                   //           
                   magg.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup mbar_doc        
             ///@{           
                   ++mbar._index;           
                   mbar . coilrmn = 264.90; //  barrel coil inner radius (barrel minimum)            
                   /// mbar . coilrmn = 264.90; //  barrel coil inner radius (barrel minimum)            
                   mbar . coilrmx = 299.30; //  barrel coil outer radius            
                   /// mbar . coilrmx = 299.30; //  barrel coil outer radius            
                   mbar . coillen = 627.40; //  barrel coil full length            
                   /// mbar . coillen = 627.40; //  barrel coil full length            
                   mbar . retyrmn = 303.29; //  Return Yoke minimum radius            
                   /// mbar . retyrmn = 303.29; //  Return Yoke minimum radius            
                   mbar . retylen = 684.40; //  Return Yoke full length            
                   /// mbar . retylen = 684.40; //  Return Yoke full length            
                   mbar . barwidin = 44.34; //  return yoke bare inner width            
                   /// mbar . barwidin = 44.34; //  return yoke bare inner width            
                   mbar . barwidou = 57.15; //  return yoke bare outer width            
                   /// mbar . barwidou = 57.15; //  return yoke bare outer width            
                   mbar . barheigh = 60.00; //  return yoke bare height            
                   /// mbar . barheigh = 60.00; //  return yoke bare height            
                   mbar . ringrmn = 263.68; //  Return Ring minimum radius            
                   /// mbar . ringrmn = 263.68; //  Return Ring minimum radius            
                   mbar . ncoil = 12; //  total number of barrel coils            
                   /// mbar . ncoil = 12; //  total number of barrel coils            
                   mbar . zcoil.at(0) = 30.95; //  coil position            
                   ///mbar . zcoil.at(0) = 30.95; //  coil position            
                   mbar . zcoil.at(1) = 89.05; //  coil position            
                   ///mbar . zcoil.at(1) = 89.05; //  coil position            
                   mbar . zcoil.at(2) = 147.17; //  coil position            
                   ///mbar . zcoil.at(2) = 147.17; //  coil position            
                   mbar . zcoil.at(3) = 205.25; //  coil position            
                   ///mbar . zcoil.at(3) = 205.25; //  coil position            
                   mbar . zcoil.at(4) = 249.0; //  coil position            
                   ///mbar . zcoil.at(4) = 249.0; //  coil position            
                   mbar . zcoil.at(5) = 288.35; //  coil position            
                   ///mbar . zcoil.at(5) = 288.35; //  coil position            
                   mbar . dzcoil.at(0) = 45.24; //  coil width              
                   ///mbar . dzcoil.at(0) = 45.24; //  coil width              
                   mbar . dzcoil.at(1) = 45.24; //  coil width              
                   ///mbar . dzcoil.at(1) = 45.24; //  coil width              
                   mbar . dzcoil.at(2) = 45.24; //  coil width              
                   ///mbar . dzcoil.at(2) = 45.24; //  coil width              
                   mbar . dzcoil.at(3) = 45.24; //  coil width              
                   ///mbar . dzcoil.at(3) = 45.24; //  coil width              
                   mbar . dzcoil.at(4) = 22.71; //  coil width              
                   ///mbar . dzcoil.at(4) = 22.71; //  coil width              
                   mbar . dzcoil.at(5) = 45.24; //  coil width              
                   ///mbar . dzcoil.at(5) = 45.24; //  coil width              
                   //           
                   mbar.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup mend_doc        
             ///@{           
                   ++mend._index;           
                   mend . polez = 310.007; //  pole tip nominal position            
                   /// mend . polez = 310.007; //  pole tip nominal position            
                   mend . polermn = 90.028; //  Pole tip inner radius (encap minimum)            
                   /// mend . polermn = 90.028; //  Pole tip inner radius (encap minimum)            
                   mend . polermx = 252.882; //  Pole tip outer radius            
                   /// mend . polermx = 252.882; //  Pole tip outer radius            
                   mend . polecavr = 152.4; //  Pole tip cavity outer radius            
                   /// mend . polecavr = 152.4; //  Pole tip cavity outer radius            
                   mend . polecavd = 18; //  Pole tip cavity depth            
                   /// mend . polecavd = 18; //  Pole tip cavity depth            
                   mend . tcoilrmn = 91.34; //  Pole tip trim coil inner radius            
                   /// mend . tcoilrmn = 91.34; //  Pole tip trim coil inner radius            
                   mend . tcoilrmx = 141.28; //  Pole tip trim coil outer radius            
                   /// mend . tcoilrmx = 141.28; //  Pole tip trim coil outer radius            
                   mend . tcoildz = 16.5; //  full width of Pole Tip trim Coil            
                   /// mend . tcoildz = 16.5; //  full width of Pole Tip trim Coil            
                   mend . etacut = 2; //  eta limits for the Pole             
                   /// mend . etacut = 2; //  eta limits for the Pole             
                   //           
                   mend.fill();           
             ///@}        
             //        
             /// USE magg version=1  ;        
             magg.Use("version",(Float_t)1  );        
             /// USE mbar _index=1;        
             mbar.Use();        
             /// USE mend _index=1;        
             mend.Use();        
             tantheta = (exp(+mend.etacut)-exp(-mend.etacut))/2;        
             _create = AgCreate("MAGP");        
             {           
                   AgShape myshape; // undefined shape           
                   ///Create MAGP           
                   Create("MAGP");            
             }        
             { AgPlacement place = AgPlacement("MAGP","CAVE");           
                   /// Add daughter volume MAGP to mother CAVE           
                   _stacker -> Position( AgBlock::Find("MAGP"), place );           
             } // end placement of MAGP        
       }; // MagpGeo     
 }; // namespace MagpGeo  
 