//*-- Author :    Valeri Fine  19/02/2000 begin_html mailto://fine@bnl.gov  end_html
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// The collection of typdef declaration to allow a transition from thee //
// STAR St_base library to ROOT-based libSTAR from the regular ROOT     //
// distribution                                                         //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#ifndef STAR_baseTypeDef
#define STAR_baseTypeDef

// $Id: StTypeDefs.h,v 1.1 2000/02/25 00:48:06 fine Exp $

class StCL;
class TDataSet;
class TDataSetIter;
class TFileSet;
class TVolume;
class TVolumePosition;
class TVolumeView;
class TVolumeViewIter;
class TObjectSet;
class TPointPosition;
class TPoints3D;
class TPointsArray3D;
class TPolyLineShape;
class TTable;
class TTable3Points;
class TTableElementDescriptor;
class TTableIter;
class TTablePoints;
class TTableSorter;
class TPoint3_Table;
class TTableDescriptor;
 
typedef StCL             TCL; 
typedef TDataSet         St_DataSet ;
typedef TDataSetIter     St_DataSetIter;
typedef TFileSet         St_FileSet;
typedef TVolume          St_Node;
typedef TVolumePosition  St_NodePosition;
typedef TVolumeView      St_NodeView;
typedef TVolumeViewIter  St_NodeViewIter;
typedef TObjectSet       St_ObjectSet;
typedef TPointPosition   St_PointPosition;
typedef TPoints3D        St_Points3D;
typedef TPointsArray3D   St_PointsArray3D;
typedef TPolyLineShape   St_PolyLineShape;
typedef TTable           St_Table;
typedef TTable3Points    St_Table3Points;
typedef TTableElementDescriptor St_TableElementDescriptor;
typedef TTableIter       St_TableIter;
typedef TTablePoints     St_TablePoints;
typedef TTableSorter     St_TableSorter;
typedef TPoint3_Table    St_point3_Table;
typedef TTableDescriptor St_tableDescriptor;

// $Log: StTypeDefs.h,v $
// Revision 1.1  2000/02/25 00:48:06  fine
// temporary interface for ROOT 2.23/12 with STAR classes inside
//
#endif
