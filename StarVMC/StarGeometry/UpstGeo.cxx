#include "UpstGeo.h"  
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
 namespace UPSTGEO // $NMSPC  
 {     
       //     
       // ---------------------------------------------------------------------------------------------------     
          //  -----------------------------------------------------     
          /// @defgroup pipu_doc     
          /// \class Pipu_t     
          /// \brief User-defined structure     
          ///                             
          /// AgML structure members:     
          ///                             
          ///Float_t version;     
          ///Float_t zposit;     
          ///Float_t dz_upst;     
          ///Float_t p1innr;     
          ///Float_t p1outr;     
          ///Float_t p1leng;     
          ///Float_t p2innr;     
          ///Float_t p2outr;     
          ///Float_t p2leng;     
          ///Float_t p3innr;     
          ///Float_t p3outr;     
          ///Float_t p3leng;     
          ///Float_t dxinnr;     
          ///Float_t dxoutr;     
          ///Float_t dxleng;     
          ///Float_t csinnr;     
          ///Float_t csoutr;     
          ///Float_t ceinnr;     
          ///Float_t ceoutr;     
          ///Float_t cleng;     
          ///Float_t pginnr;     
          ///Float_t pgoutr;     
          ///Float_t pgleng;     
          ///Int_t _index;     
          //     
          Pipu_t pipu;     
          //     
          ///@addtogroup UpstGeo_vars     
          ///@{        
                Float_t z1,z2,z3,z4,z5,z6;        
                //        
                /// Float_t z1,z2,z3,z4,z5,z6        
          ///@}     
       UpstGeo::UpstGeo()     
         : AgModule("UpstGeo"," is the geometry  of the UPSTREAM AreA. ")     
       {        
       }     
          // ---------------------------------------------------------------------------------------------------     
          void UPST::Block( AgCreate create )     
          {         
                ///@addtogroup UPST_doc        
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
                      { AgAttribute attr = AgAttribute("Upst");              
                            attr.par("seen")=0;              
                            attr.par("colo")=2;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=0.;              
                            shape.par("rmax")=40.0;              
                            shape.par("dz")=pipu.dz_upst;              
                            /// Shape Tube rmin=0. rmax=40.0 dz=pipu.dz_upst               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_UPST;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("PUPD");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create PUPD              
                            Create("PUPD");               
                      }           
                      { AgPlacement place = AgPlacement("PUPD","UPST");              
                            /// Add daughter volume PUPD to mother UPST              
                            place.TranslateZ(z2);              
                            /// Translate z = z2              
                            _stacker -> Position( AgBlock::Find("PUPD"), place );              
                      } // end placement of PUPD           
                      _create = AgCreate("PUPE");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create PUPE              
                            Create("PUPE");               
                      }           
                      { AgPlacement place = AgPlacement("PUPE","UPST");              
                            /// Add daughter volume PUPE to mother UPST              
                            place.TranslateZ(z3);              
                            /// Translate z = z3              
                            _stacker -> Position( AgBlock::Find("PUPE"), place );              
                      } // end placement of PUPE           
                      _create = AgCreate("PUPF");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create PUPF              
                            Create("PUPF");               
                      }           
                      { AgPlacement place = AgPlacement("PUPF","UPST");              
                            /// Add daughter volume PUPF to mother UPST              
                            place.TranslateZ(z3);              
                            /// Translate z = z3              
                            _stacker -> Position( AgBlock::Find("PUPF"), place );              
                      } // end placement of PUPF           
                      _create = AgCreate("DXMG");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create DXMG              
                            Create("DXMG");               
                      }           
                      { AgPlacement place = AgPlacement("DXMG","UPST");              
                            /// Add daughter volume DXMG to mother UPST              
                            place.TranslateZ(z3);              
                            /// Translate z = z3              
                            _stacker -> Position( AgBlock::Find("DXMG"), place );              
                      } // end placement of DXMG           
                      _create = AgCreate("DCON");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create DCON              
                            Create("DCON");               
                      }           
                      { AgPlacement place = AgPlacement("DCON","UPST");              
                            /// Add daughter volume DCON to mother UPST              
                            place.TranslateZ(z4+pipu.cleng);              
                            /// Translate z = z4+pipu.cleng              
                            _stacker -> Position( AgBlock::Find("DCON"), place );              
                      } // end placement of DCON           
                      _create = AgCreate("PUPG");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create PUPG              
                            Create("PUPG");               
                      }           
                      { AgPlacement place = AgPlacement("PUPG","UPST");              
                            /// Add daughter volume PUPG to mother UPST              
                            place.TranslateZ(z6+pipu.pgleng);              
                            /// Translate z = z6+pipu.pgleng              
                            _stacker -> Position( AgBlock::Find("PUPG"), place );              
                      } // end placement of PUPG           
                      END_OF_UPST:           
                      mCurrent = _save;           
                ///@}        
          } // End Block UPST     
          // ---------------------------------------------------------------------------------------------------     
          void PUPD::Block( AgCreate create )     
          {         
                ///@addtogroup PUPD_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Iron            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Iron");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("PUPd");              
                            attr.par("seen")=1;              
                            attr.par("colo")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=0;              
                            shape.par("rmax")=pipu.p1outr;              
                            shape.par("dz")=pipu.p1leng;              
                            /// Shape Tube rmin=0 rmax=pipu.p1outr dz=pipu.p1leng               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_PUPD;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("PVAD");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create PVAD              
                            Create("PVAD");               
                      }           
                      { AgPlacement place = AgPlacement("PVAD","PUPD");              
                            /// Add daughter volume PVAD to mother PUPD              
                            _stacker -> Position( AgBlock::Find("PVAD"), place );              
                      } // end placement of PVAD           
                      END_OF_PUPD:           
                      mCurrent = _save;           
                ///@}        
          } // End Block PUPD     
          // ---------------------------------------------------------------------------------------------------     
          void PVAD::Block( AgCreate create )     
          {         
                ///@addtogroup PVAD_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Vacuum            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Vacuum");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmax")=pipu.p1innr;              
                            /// Shape Tube rmax=pipu.p1innr               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_PVAD;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_PVAD:           
                      mCurrent = _save;           
                ///@}        
          } // End Block PVAD     
          // ---------------------------------------------------------------------------------------------------     
          void PUPE::Block( AgCreate create )     
          {         
                ///@addtogroup PUPE_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Iron            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Iron");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("PUPe");              
                            attr.par("seen")=1;              
                            attr.par("colo")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=0;              
                            shape.par("rmax")=pipu.p2outr;              
                            shape.par("dz")=pipu.p2leng;              
                            /// Shape Tube rmin=0 rmax=pipu.p2outr dz=pipu.p2leng               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_PUPE;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("PVAE");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create PVAE              
                            Create("PVAE");               
                      }           
                      { AgPlacement place = AgPlacement("PVAE","PUPE");              
                            /// Add daughter volume PVAE to mother PUPE              
                            _stacker -> Position( AgBlock::Find("PVAE"), place );              
                      } // end placement of PVAE           
                      END_OF_PUPE:           
                      mCurrent = _save;           
                ///@}        
          } // End Block PUPE     
          // ---------------------------------------------------------------------------------------------------     
          void PVAE::Block( AgCreate create )     
          {         
                ///@addtogroup PVAE_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Vacuum            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Vacuum");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmax")=pipu.p2innr;              
                            /// Shape Tube rmax=pipu.p2innr               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_PVAE;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_PVAE:           
                      mCurrent = _save;           
                ///@}        
          } // End Block PVAE     
          // ---------------------------------------------------------------------------------------------------     
          void PUPF::Block( AgCreate create )     
          {         
                ///@addtogroup PUPF_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Iron            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Iron");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("PUPf");              
                            attr.par("seen")=1;              
                            attr.par("colo")=2;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=pipu.p3innr;              
                            shape.par("rmax")=pipu.p3outr;              
                            shape.par("dz")=pipu.p3leng;              
                            /// Shape Tube rmin=pipu.p3innr rmax=pipu.p3outr dz=pipu.p3leng               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_PUPF;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_PUPF:           
                      mCurrent = _save;           
                ///@}        
          } // End Block PUPF     
          // ---------------------------------------------------------------------------------------------------     
          void DXMG::Block( AgCreate create )     
          {         
                ///@addtogroup DXMG_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Iron            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Iron");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("Dxmg");              
                            attr.par("seen")=1;              
                            attr.par("colo")=3;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=pipu.dxinnr;              
                            shape.par("rmax")=pipu.dxoutr;              
                            shape.par("dz")=pipu.dxleng;              
                            /// Shape Tube rmin=pipu.dxinnr rmax=pipu.dxoutr dz=pipu.dxleng               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_DXMG;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_DXMG:           
                      mCurrent = _save;           
                ///@}        
          } // End Block DXMG     
          // ---------------------------------------------------------------------------------------------------     
          void DCON::Block( AgCreate create )     
          {         
                ///@addtogroup DCON_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Iron            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Iron");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("Dcon");              
                            attr.par("seen")=1;              
                            attr.par("colo")=4;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Cone");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dz")=pipu.cleng;              
                            shape.par("rmn1")=0;              
                            shape.par("rmx1")=pipu.csoutr;              
                            shape.par("rmn2")=0;              
                            shape.par("rmx2")=pipu.ceoutr;              
                            /// Shape Cone dz=pipu.cleng rmn1=0 rmx1=pipu.csoutr rmn2=0 rmx2=pipu.ceoutr               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_DCON;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("DVAC");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create DVAC              
                            Create("DVAC");               
                      }           
                      { AgPlacement place = AgPlacement("DVAC","DCON");              
                            /// Add daughter volume DVAC to mother DCON              
                            _stacker -> Position( AgBlock::Find("DVAC"), place );              
                      } // end placement of DVAC           
                      END_OF_DCON:           
                      mCurrent = _save;           
                ///@}        
          } // End Block DCON     
          // ---------------------------------------------------------------------------------------------------     
          void DVAC::Block( AgCreate create )     
          {         
                ///@addtogroup DVAC_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Vacuum            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Vacuum");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Cone");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmx1")=pipu.csinnr;              
                            shape.par("rmx2")=pipu.ceinnr;              
                            /// Shape Cone rmx1=pipu.csinnr rmx2=pipu.ceinnr               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_DVAC;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_DVAC:           
                      mCurrent = _save;           
                ///@}        
          } // End Block DVAC     
          // ---------------------------------------------------------------------------------------------------     
          void PUPG::Block( AgCreate create )     
          {         
                ///@addtogroup PUPG_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Iron            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Iron");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("PUPg");              
                            attr.par("seen")=1;              
                            attr.par("colo")=4;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=0;              
                            shape.par("rmax")=pipu.pgoutr;              
                            shape.par("dz")=pipu.pgleng;              
                            /// Shape Tube rmin=0 rmax=pipu.pgoutr dz=pipu.pgleng               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_PUPG;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("PVAG");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create PVAG              
                            Create("PVAG");               
                      }           
                      { AgPlacement place = AgPlacement("PVAG","PUPG");              
                            /// Add daughter volume PVAG to mother PUPG              
                            _stacker -> Position( AgBlock::Find("PVAG"), place );              
                      } // end placement of PVAG           
                      END_OF_PUPG:           
                      mCurrent = _save;           
                ///@}        
          } // End Block PUPG     
          // ---------------------------------------------------------------------------------------------------     
          void PVAG::Block( AgCreate create )     
          {         
                ///@addtogroup PVAG_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Vacuum            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Vacuum");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmax")=pipu.pginnr;              
                            /// Shape Tube rmax=pipu.pginnr               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_PVAG;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_PVAG:           
                      mCurrent = _save;           
                ///@}        
          } // End Block PVAG     
    // ----------------------------------------------------------------------- geoctr
       void UpstGeo::ConstructGeometry( const Char_t *dummy )     
       {        
             ///@addtogroup UpstGeo_revision        
             ///@{           
                   /// Created:   27-Dec-1996             
             ///@}        
             ///@addtogroup UpstGeo_revision        
             ///@{           
                   /// Author: W.B.Christie           
             ///@}        
             AddBlock("UPST");        
             AddBlock("PUPD");        
             AddBlock("PUPE");        
             AddBlock("PUPF");        
             AddBlock("DXMG");        
             AddBlock("PVAD");        
             AddBlock("PVAE");        
             AddBlock("DCON");        
             AddBlock("PUPG");        
             AddBlock("DVAC");        
             AddBlock("PVAG");        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup pipu_doc        
             ///@{           
                   ++pipu._index;           
                   pipu . version = 1; //  geometry version              
                   /// pipu . version = 1; //  geometry version              
                   pipu . zposit = 1131.83; //  position of the upstream part            
                   /// pipu . zposit = 1131.83; //  position of the upstream part            
                   pipu . dz_upst = 385.63; //  Half length of the UPSTREAM mother volume            
                   /// pipu . dz_upst = 385.63; //  Half length of the UPSTREAM mother volume            
                   pipu . p1innr = 6.08; //  Inner radius of Pipe in Hall            
                   /// pipu . p1innr = 6.08; //  Inner radius of Pipe in Hall            
                   pipu . p1outr = 6.35; //  Outer radius of pipe in Hall            
                   /// pipu . p1outr = 6.35; //  Outer radius of pipe in Hall            
                   pipu . p1leng = 105.5; //  Length of Pipe in hall            
                   /// pipu . p1leng = 105.5; //  Length of Pipe in hall            
                   pipu . p2innr = 6.99; //  Large OD steel section inner radius            
                   /// pipu . p2innr = 6.99; //  Large OD steel section inner radius            
                   pipu . p2outr = 7.14; //  Large OD steel section outer radius            
                   /// pipu . p2outr = 7.14; //  Large OD steel section outer radius            
                   pipu . p2leng = 207.92; //  Large OD steel section half length            
                   /// pipu . p2leng = 207.92; //  Large OD steel section half length            
                   pipu . p3innr = 9.53; //  Inner radius of large DX Pipe            
                   /// pipu . p3innr = 9.53; //  Inner radius of large DX Pipe            
                   pipu . p3outr = 10.16; //  Outer radius of Large DX pipe            
                   /// pipu . p3outr = 10.16; //  Outer radius of Large DX pipe            
                   pipu . p3leng = 207.92; //  Length of Large DX Pipe            
                   /// pipu . p3leng = 207.92; //  Length of Large DX Pipe            
                   pipu . dxinnr = 15.34; //  Inner RAdius of DX Iron Yoke.            
                   /// pipu . dxinnr = 15.34; //  Inner RAdius of DX Iron Yoke.            
                   pipu . dxoutr = 37.0; //  Outer RAdius of DX Iron Yoke.             
                   /// pipu . dxoutr = 37.0; //  Outer RAdius of DX Iron Yoke.             
                   pipu . dxleng = 207.92; //  HALF Length of DX MAgnet.            
                   /// pipu . dxleng = 207.92; //  HALF Length of DX MAgnet.            
                   pipu . csinnr = 7.14; //  Inner radius of Start of DX Cone            
                   /// pipu . csinnr = 7.14; //  Inner radius of Start of DX Cone            
                   pipu . csoutr = 7.77; //  Outer radius of Start of DX cone            
                   /// pipu . csoutr = 7.77; //  Outer radius of Start of DX cone            
                   pipu . ceinnr = 14.60; //  Inner radius at End of DX cone            
                   /// pipu . ceinnr = 14.60; //  Inner radius at End of DX cone            
                   pipu . ceoutr = 15.24; //  Outer radius at End of DX cone            
                   /// pipu . ceoutr = 15.24; //  Outer radius at End of DX cone            
                   pipu . cleng = 21.21; //  Half Length of Cone at end of DX.            
                   /// pipu . cleng = 21.21; //  Half Length of Cone at end of DX.            
                   pipu . pginnr = 14.60; //  Inner radius of last Pipe            
                   /// pipu . pginnr = 14.60; //  Inner radius of last Pipe            
                   pipu . pgoutr = 15.24; //  Outer radius of last Pipe            
                   /// pipu . pgoutr = 15.24; //  Outer radius of last Pipe            
                   pipu . pgleng = 51.0; //  Half length of last pipe            
                   /// pipu . pgleng = 51.0; //  Half length of last pipe            
                   //           
                   pipu.fill();           
             ///@}        
             //        
             /// USE pipu version=1 ;        
             pipu.Use("version",(Float_t)1 );        
             z1 = 746.2      ;// end of the std star beam pipe plus 2 mm;        
             z2 = pipu.p1leng - pipu.dz_upst     ;// center of beam pipe before dx;        
             z3 = z2 + pipu.p1leng+ pipu.p2leng  ;// center of dx magnet (33.29);        
             z4 = z3 + pipu.p2leng               ;// end of dx magnet    (241.21);        
             z5 = z4 + pipu.cleng                ;// center of dx cone   (262.42);        
             z6 = z5 + pipu.cleng                ;// end of dx cone      (283.63);        
             _create = AgCreate("UPST");        
             {           
                   AgShape myshape; // undefined shape           
                   ///Create UPST           
                   Create("UPST");            
             }        
             { AgPlacement place = AgPlacement("UPST","CAVE");           
                   /// Add daughter volume UPST to mother CAVE           
                   place.TranslateZ(pipu.zposit);           
                   /// Translate z = pipu.zposit           
                   place.par("only")=AgPlacement::kMany;           
                   /// Overlap: agplacement::kmany           
                   _stacker -> Position( AgBlock::Find("UPST"), place );           
             } // end placement of UPST        
             { AgPlacement place = AgPlacement("UPST","CAVE");           
                   /// Add daughter volume UPST to mother CAVE           
                   place.TranslateZ(-pipu.zposit);           
                   /// Translate z = -pipu.zposit           
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
                   _stacker -> Position( AgBlock::Find("UPST"), place );           
             } // end placement of UPST        
       }; // UpstGeo     
 }; // namespace UpstGeo  
 