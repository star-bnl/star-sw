#include "ShapGeo.h"  
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
 namespace SHAPGEO // $NMSPC  
 {     
       //     
       // ---------------------------------------------------------------------------------------------------     
          ///@addtogroup ShapGeo_vars     
          ///@{        
                float x=-150.0,dx=75.0,y=-30.0;        
                //        
                /// float x=-150.0,dx=75.0,y=-30.0        
          ///@}     
          ///@addtogroup ShapGeo_vars     
          ///@{        
                int icolor=27;        
                //        
                /// int icolor=27        
          ///@}     
       ShapGeo::ShapGeo()     
         : AgModule("ShapGeo","Shapes Geometry : places AgML shapes into the cave")     
       {        
       }     
          // ---------------------------------------------------------------------------------------------------     
          void BBOX::Block( AgCreate create )     
          {         
                ///@addtogroup BBOX_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("BBOX");              
                            attr.par("colo")=icolor;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      icolor+=1;           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=20;              
                            shape.par("dy")=20;              
                            shape.par("dz")=40;              
                            /// Shape Bbox dx=20 dy=20 dz=40               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_BBOX;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_BBOX:           
                      mCurrent = _save;           
                ///@}        
          } // End Block BBOX     
          // ---------------------------------------------------------------------------------------------------     
          void PARA::Block( AgCreate create )     
          {         
                ///@addtogroup PARA_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("PARA");              
                            attr.par("colo")=icolor;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      icolor+=1;           
                      {  AgShape shape = AgShape("Para");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=20;              
                            shape.par("dy")=20;              
                            shape.par("dz")=40;              
                            shape.par("alph")=20;              
                            shape.par("thet")=20;              
                            shape.par("phi")=45;              
                            /// Shape Para dx=20 dy=20 dz=40 alph=20 thet=20 phi=45               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_PARA;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_PARA:           
                      mCurrent = _save;           
                ///@}        
          } // End Block PARA     
          // ---------------------------------------------------------------------------------------------------     
          void TRDA::Block( AgCreate create )     
          {         
                ///@addtogroup TRDA_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("TRDA");              
                            attr.par("colo")=icolor;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      icolor+=1;           
                      {  AgShape shape = AgShape("Trd1");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx1")=10;              
                            shape.par("dx2")=20;              
                            shape.par("dy")=30;              
                            shape.par("dz")=40;              
                            /// Shape Trd1 dx1=10 dx2=20 dy=30 dz=40               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_TRDA;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_TRDA:           
                      mCurrent = _save;           
                ///@}        
          } // End Block TRDA     
          // ---------------------------------------------------------------------------------------------------     
          void TRDB::Block( AgCreate create )     
          {         
                ///@addtogroup TRDB_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("TRDB");              
                            attr.par("colo")=icolor;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      icolor+=1;           
                      {  AgShape shape = AgShape("Trd2");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx1")=10;              
                            shape.par("dx2")=20;              
                            shape.par("dy1")=30;              
                            shape.par("dy2")=10;              
                            shape.par("dz")=40;              
                            /// Shape Trd2 dx1=10 dx2=20 dy1=30 dy2=10 dz=40               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_TRDB;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_TRDB:           
                      mCurrent = _save;           
                ///@}        
          } // End Block TRDB     
          // ---------------------------------------------------------------------------------------------------     
          void TRAP::Block( AgCreate create )     
          {         
                ///@addtogroup TRAP_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("TRAP");              
                            attr.par("colo")=icolor;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      icolor+=1;           
                      {  AgShape shape = AgShape("Trap");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dz")=40;              
                            shape.par("thet")=15;              
                            shape.par("phi")=30;              
                            shape.par("h1")=20;              
                            shape.par("bl1")=10;              
                            shape.par("tl1")=15;              
                            shape.par("alp1")=0.0;              
                            shape.par("h2")=20;              
                            shape.par("bl2")=10;              
                            shape.par("tl2")=15;              
                            shape.par("alp2")=0.0;              
                            /// Shape Trap dz=40 thet=15 phi=30 h1=20 bl1=10 tl1=15 alp1=0.0 h2=20 bl2=10 tl2=15 alp2=0.0               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_TRAP;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_TRAP:           
                      mCurrent = _save;           
                ///@}        
          } // End Block TRAP     
          // ---------------------------------------------------------------------------------------------------     
          void GTRA::Block( AgCreate create )     
          {         
                ///@addtogroup GTRA_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("GTRA");              
                            attr.par("colo")=icolor;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      icolor+=1;           
                      {  AgShape shape = AgShape("Gtra");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dz")=40;              
                            shape.par("thet")=15;              
                            shape.par("phi")=30;              
                            shape.par("twis")=30;              
                            shape.par("h1")=20;              
                            shape.par("bl1")=10;              
                            shape.par("tl1")=15;              
                            shape.par("alp1")=0.0;              
                            shape.par("h2")=20;              
                            shape.par("bl2")=10;              
                            shape.par("tl2")=15;              
                            shape.par("alp2")=0.0;              
                            /// Shape Gtra dz=40 thet=15 phi=30 twis=30 h1=20 bl1=10 tl1=15 alp1=0.0 h2=20 bl2=10 tl2=15 alp2=0.0               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_GTRA;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_GTRA:           
                      mCurrent = _save;           
                ///@}        
          } // End Block GTRA     
          // ---------------------------------------------------------------------------------------------------     
          void SPHE::Block( AgCreate create )     
          {         
                ///@addtogroup SPHE_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("SPHE");              
                            attr.par("colo")=icolor;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      icolor+=1;           
                      {  AgShape shape = AgShape("Sphe");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=18.0;              
                            shape.par("rmax")=20.0;              
                            /// Shape Sphe rmin=18.0 rmax=20.0               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_SPHE;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_SPHE:           
                      mCurrent = _save;           
                ///@}        
          } // End Block SPHE     
          // ---------------------------------------------------------------------------------------------------     
          void TUBE::Block( AgCreate create )     
          {         
                ///@addtogroup TUBE_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("TUBE");              
                            attr.par("colo")=icolor;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      icolor+=1;           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=5.0;              
                            shape.par("rmax")=10.0;              
                            shape.par("dz")=15;              
                            /// Shape Tube rmin=5.0 rmax=10.0 dz=15               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_TUBE;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_TUBE:           
                      mCurrent = _save;           
                ///@}        
          } // End Block TUBE     
          // ---------------------------------------------------------------------------------------------------     
          void TUBS::Block( AgCreate create )     
          {         
                ///@addtogroup TUBS_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("TUBS");              
                            attr.par("colo")=icolor;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      icolor+=1;           
                      {  AgShape shape = AgShape("Tubs");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=5.0;              
                            shape.par("rmax")=10.0;              
                            shape.par("phi1")=-120.0;              
                            shape.par("phi2")=+120.0;              
                            shape.par("dz")=15;              
                            /// Shape Tubs rmin=5.0 rmax=10.0 phi1=-120.0 phi2=+120.0 dz=15               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_TUBS;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_TUBS:           
                      mCurrent = _save;           
                ///@}        
          } // End Block TUBS     
          // ---------------------------------------------------------------------------------------------------     
          void CTUB::Block( AgCreate create )     
          {         
                ///@addtogroup CTUB_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("CTUB");              
                            attr.par("colo")=icolor;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      icolor+=1;           
                      {  AgShape shape = AgShape("Ctub");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=5.0;              
                            shape.par("rmax")=10.0;              
                            shape.par("phi1")=-120.0;              
                            shape.par("phi2")=+120.0;              
                            shape.par("dz")=15;              
                            shape.par("hx")=1.0;              
                            shape.par("hy")=1.0;              
                            shape.par("hz")=1.0;              
                            shape.par("lx")=0.0;              
                            shape.par("ly")=0.0;              
                            shape.par("lz")=1.0;              
                            /// Shape Ctub rmin=5.0 rmax=10.0 phi1=-120.0 phi2=+120.0 dz=15 hx=1.0 hy=1.0 hz=1.0 lx=0.0 ly=0.0 lz=1.0               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_CTUB;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_CTUB:           
                      mCurrent = _save;           
                ///@}        
          } // End Block CTUB     
          // ---------------------------------------------------------------------------------------------------     
          void ELTU::Block( AgCreate create )     
          {         
                ///@addtogroup ELTU_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("ELTU");              
                            attr.par("colo")=icolor;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      icolor+=1;           
                      {  AgShape shape = AgShape("Eltu");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("p1")=15.0;              
                            shape.par("p2")=10.0;              
                            shape.par("dz")=30.0;              
                            /// Shape Eltu p1=15.0 p2=10.0 dz=30.0               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_ELTU;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_ELTU:           
                      mCurrent = _save;           
                ///@}        
          } // End Block ELTU     
          // ---------------------------------------------------------------------------------------------------     
          void CONE::Block( AgCreate create )     
          {         
                ///@addtogroup CONE_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("CONE");              
                            attr.par("colo")=icolor;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      icolor+=1;           
                      {  AgShape shape = AgShape("Cone");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dz")=40;              
                            shape.par("rmn1")=5.0;              
                            shape.par("rmx1")=20.0;              
                            shape.par("rmn2")=5.0;              
                            shape.par("rmx2")=7.5;              
                            /// Shape Cone dz=40 rmn1=5.0 rmx1=20.0 rmn2=5.0 rmx2=7.5               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_CONE;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_CONE:           
                      mCurrent = _save;           
                ///@}        
          } // End Block CONE     
          // ---------------------------------------------------------------------------------------------------     
          void CONS::Block( AgCreate create )     
          {         
                ///@addtogroup CONS_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("CONS");              
                            attr.par("colo")=icolor;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      icolor+=1;           
                      {  AgShape shape = AgShape("Cons");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dz")=40.0;              
                            shape.par("rmn1")=5.0;              
                            shape.par("rmx1")=20.0;              
                            shape.par("rmn2")=5.0;              
                            shape.par("rmx2")=7.5;              
                            shape.par("phi1")=-120;              
                            shape.par("phi2")=+120;              
                            /// Shape Cons dz=40.0 rmn1=5.0 rmx1=20.0 rmn2=5.0 rmx2=7.5 phi1=-120 phi2=+120               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_CONS;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_CONS:           
                      mCurrent = _save;           
                ///@}        
          } // End Block CONS     
          // ---------------------------------------------------------------------------------------------------     
          void PCON::Block( AgCreate create )     
          {         
                ///@addtogroup PCON_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("PCON");              
                            attr.par("colo")=icolor;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      icolor+=1;           
                      {  AgShape shape = AgShape("Pcon");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("phi1")=0;              
                            shape.par("dphi")=360;              
                            shape.par("nz")=5;              
                            shape.Z(0)=-20.;              
                            shape.Z(1)=-5.0;              
                            shape.Z(2)=10.0;              
                            shape.Z(3)=15.0;              
                            shape.Z(4)=20.0;              
                            shape.Rmin(0)=0;              
                            shape.Rmin(1)=0;              
                            shape.Rmin(2)=0;              
                            shape.Rmin(3)=0;              
                            shape.Rmin(4)=0;              
                            shape.Rmax(0)=15.0;              
                            shape.Rmax(1)=15.0;              
                            shape.Rmax(2)=8.0;              
                            shape.Rmax(3)=12.0;              
                            shape.Rmax(4)=15.0;              
                            /// Shape Pcon phi1=0 dphi=360 nz=5               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_PCON;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_PCON:           
                      mCurrent = _save;           
                ///@}        
          } // End Block PCON     
          // ---------------------------------------------------------------------------------------------------     
          void PGON::Block( AgCreate create )     
          {         
                ///@addtogroup PGON_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("PGON");              
                            attr.par("colo")=icolor;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      icolor+=1;           
                      {  AgShape shape = AgShape("Pgon");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("phi1")=0;              
                            shape.par("dphi")=360;              
                            shape.par("npdiv")=6;              
                            shape.par("nz")=5;              
                            shape.Z(0)=-20.;              
                            shape.Z(1)=-5.0;              
                            shape.Z(2)=10.0;              
                            shape.Z(3)=15.0;              
                            shape.Z(4)=20.0;              
                            shape.Rmin(0)=12.0;              
                            shape.Rmin(1)=12.0;              
                            shape.Rmin(2)=5.0;              
                            shape.Rmin(3)= 9.0;              
                            shape.Rmin(4)=12.0;              
                            shape.Rmax(0)=15.0;              
                            shape.Rmax(1)=15.0;              
                            shape.Rmax(2)=8.0;              
                            shape.Rmax(3)=12.0;              
                            shape.Rmax(4)=15.0;              
                            /// Shape Pgon phi1=0 dphi=360 npdiv=6 nz=5               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_PGON;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_PGON:           
                      mCurrent = _save;           
                ///@}        
          } // End Block PGON     
    // ----------------------------------------------------------------------- geoctr
       void ShapGeo::ConstructGeometry( const Char_t *dummy )     
       {        
             ///@addtogroup ShapGeo_revision        
             ///@{           
                   /// Author: Jason C. Webb           
             ///@}        
             ///@addtogroup ShapGeo_revision        
             ///@{           
                   /// Created: 11/01/2011           
             ///@}        
             AddBlock("BBOX");        
             AddBlock("PARA");        
             AddBlock("TRDA");        
             AddBlock("TRDB");        
             AddBlock("TRAP");        
             AddBlock("SPHE");        
             AddBlock("TUBE");        
             AddBlock("TUBS");        
             AddBlock("CONE");        
             AddBlock("CONS");        
             AddBlock("ELTU");        
             AddBlock("PCON");        
             AddBlock("PGON");        
             AddBlock("CTUB");        
             AddBlock("GTRA");        
             x=-150.0;        
             y=-30;;        
             _create = AgCreate("BBOX");        
             {           
                   AgShape myshape; // undefined shape           
                   ///Create BBOX           
                   Create("BBOX");            
             }        
             { AgPlacement place = AgPlacement("BBOX","CAVE");           
                   /// Add daughter volume BBOX to mother CAVE           
                   place.TranslateX(x);           
                   /// Translate x = x           
                   place.TranslateY(y);           
                   /// Translate y = y           
                   _stacker -> Position( AgBlock::Find("BBOX"), place );           
             } // end placement of BBOX        
             x+=dx;;        
             _create = AgCreate("PARA");        
             {           
                   AgShape myshape; // undefined shape           
                   ///Create PARA           
                   Create("PARA");            
             }        
             { AgPlacement place = AgPlacement("PARA","CAVE");           
                   /// Add daughter volume PARA to mother CAVE           
                   place.TranslateX(x);           
                   /// Translate x = x           
                   place.TranslateY(y);           
                   /// Translate y = y           
                   _stacker -> Position( AgBlock::Find("PARA"), place );           
             } // end placement of PARA        
             x+=dx;;        
             _create = AgCreate("TRDA");        
             {           
                   AgShape myshape; // undefined shape           
                   ///Create TRDA           
                   Create("TRDA");            
             }        
             { AgPlacement place = AgPlacement("TRDA","CAVE");           
                   /// Add daughter volume TRDA to mother CAVE           
                   place.TranslateX(x);           
                   /// Translate x = x           
                   place.TranslateY(y);           
                   /// Translate y = y           
                   _stacker -> Position( AgBlock::Find("TRDA"), place );           
             } // end placement of TRDA        
             x+=dx;;        
             _create = AgCreate("TRDB");        
             {           
                   AgShape myshape; // undefined shape           
                   ///Create TRDB           
                   Create("TRDB");            
             }        
             { AgPlacement place = AgPlacement("TRDB","CAVE");           
                   /// Add daughter volume TRDB to mother CAVE           
                   place.TranslateX(x);           
                   /// Translate x = x           
                   place.TranslateY(y);           
                   /// Translate y = y           
                   _stacker -> Position( AgBlock::Find("TRDB"), place );           
             } // end placement of TRDB        
             x+=dx;;        
             _create = AgCreate("TRAP");        
             {           
                   AgShape myshape; // undefined shape           
                   ///Create TRAP           
                   Create("TRAP");            
             }        
             { AgPlacement place = AgPlacement("TRAP","CAVE");           
                   /// Add daughter volume TRAP to mother CAVE           
                   place.TranslateX(x);           
                   /// Translate x = x           
                   place.TranslateY(y);           
                   /// Translate y = y           
                   _stacker -> Position( AgBlock::Find("TRAP"), place );           
             } // end placement of TRAP        
             x+=dx;;        
             _create = AgCreate("SPHE");        
             {           
                   AgShape myshape; // undefined shape           
                   ///Create SPHE           
                   Create("SPHE");            
             }        
             { AgPlacement place = AgPlacement("SPHE","CAVE");           
                   /// Add daughter volume SPHE to mother CAVE           
                   place.TranslateX(x);           
                   /// Translate x = x           
                   place.TranslateY(y);           
                   /// Translate y = y           
                   _stacker -> Position( AgBlock::Find("SPHE"), place );           
             } // end placement of SPHE        
             x+=dx;;        
             _create = AgCreate("TUBE");        
             {           
                   AgShape myshape; // undefined shape           
                   ///Create TUBE           
                   Create("TUBE");            
             }        
             { AgPlacement place = AgPlacement("TUBE","CAVE");           
                   /// Add daughter volume TUBE to mother CAVE           
                   place.TranslateX(x);           
                   /// Translate x = x           
                   place.TranslateY(y);           
                   /// Translate y = y           
                   _stacker -> Position( AgBlock::Find("TUBE"), place );           
             } // end placement of TUBE        
             x+=dx;;        
             _create = AgCreate("TUBS");        
             {           
                   AgShape myshape; // undefined shape           
                   ///Create TUBS           
                   Create("TUBS");            
             }        
             { AgPlacement place = AgPlacement("TUBS","CAVE");           
                   /// Add daughter volume TUBS to mother CAVE           
                   place.TranslateX(x);           
                   /// Translate x = x           
                   place.TranslateY(y);           
                   /// Translate y = y           
                   _stacker -> Position( AgBlock::Find("TUBS"), place );           
             } // end placement of TUBS        
             x+=dx;;        
             x=-150.0;        
             y=+30.0;        
             _create = AgCreate("CTUB");        
             {           
                   AgShape myshape; // undefined shape           
                   ///Create CTUB           
                   Create("CTUB");            
             }        
             { AgPlacement place = AgPlacement("CTUB","CAVE");           
                   /// Add daughter volume CTUB to mother CAVE           
                   place.TranslateX(x);           
                   /// Translate x = x           
                   place.TranslateY(y);           
                   /// Translate y = y           
                   _stacker -> Position( AgBlock::Find("CTUB"), place );           
             } // end placement of CTUB        
             x+=dx;;        
             _create = AgCreate("ELTU");        
             {           
                   AgShape myshape; // undefined shape           
                   ///Create ELTU           
                   Create("ELTU");            
             }        
             { AgPlacement place = AgPlacement("ELTU","CAVE");           
                   /// Add daughter volume ELTU to mother CAVE           
                   place.TranslateX(x);           
                   /// Translate x = x           
                   place.TranslateY(y);           
                   /// Translate y = y           
                   _stacker -> Position( AgBlock::Find("ELTU"), place );           
             } // end placement of ELTU        
             x+=dx;;        
             _create = AgCreate("CONE");        
             {           
                   AgShape myshape; // undefined shape           
                   ///Create CONE           
                   Create("CONE");            
             }        
             { AgPlacement place = AgPlacement("CONE","CAVE");           
                   /// Add daughter volume CONE to mother CAVE           
                   place.TranslateX(x);           
                   /// Translate x = x           
                   place.TranslateY(y);           
                   /// Translate y = y           
                   _stacker -> Position( AgBlock::Find("CONE"), place );           
             } // end placement of CONE        
             x+=dx;;        
             _create = AgCreate("CONS");        
             {           
                   AgShape myshape; // undefined shape           
                   ///Create CONS           
                   Create("CONS");            
             }        
             { AgPlacement place = AgPlacement("CONS","CAVE");           
                   /// Add daughter volume CONS to mother CAVE           
                   place.TranslateX(x);           
                   /// Translate x = x           
                   place.TranslateY(y);           
                   /// Translate y = y           
                   _stacker -> Position( AgBlock::Find("CONS"), place );           
             } // end placement of CONS        
             x+=dx;;        
             _create = AgCreate("PCON");        
             {           
                   AgShape myshape; // undefined shape           
                   ///Create PCON           
                   Create("PCON");            
             }        
             { AgPlacement place = AgPlacement("PCON","CAVE");           
                   /// Add daughter volume PCON to mother CAVE           
                   place.TranslateX(x);           
                   /// Translate x = x           
                   place.TranslateY(y);           
                   /// Translate y = y           
                   _stacker -> Position( AgBlock::Find("PCON"), place );           
             } // end placement of PCON        
             x+=dx;;        
             _create = AgCreate("PGON");        
             {           
                   AgShape myshape; // undefined shape           
                   ///Create PGON           
                   Create("PGON");            
             }        
             { AgPlacement place = AgPlacement("PGON","CAVE");           
                   /// Add daughter volume PGON to mother CAVE           
                   place.TranslateX(x);           
                   /// Translate x = x           
                   place.TranslateY(y);           
                   /// Translate y = y           
                   _stacker -> Position( AgBlock::Find("PGON"), place );           
             } // end placement of PGON        
             x+=dx;;        
             _create = AgCreate("GTRA");        
             {           
                   AgShape myshape; // undefined shape           
                   ///Create GTRA           
                   Create("GTRA");            
             }        
             { AgPlacement place = AgPlacement("GTRA","CAVE");           
                   /// Add daughter volume GTRA to mother CAVE           
                   place.TranslateX(x);           
                   /// Translate x = x           
                   place.TranslateY(y);           
                   /// Translate y = y           
                   _stacker -> Position( AgBlock::Find("GTRA"), place );           
             } // end placement of GTRA        
             x+=dx;;        
       }; // ShapGeo     
 }; // namespace ShapGeo  
 