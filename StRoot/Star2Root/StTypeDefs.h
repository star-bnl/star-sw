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

// $Id: StTypeDefs.h,v 1.4 2000/04/27 17:41:48 fine Exp $

class TCL;
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
class TTableIter;
class TTablePoints;
class TTableSorter;
class TTableDescriptor;
 
typedef TCL              StCL; 
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
typedef TTableIter       St_TableIter;
typedef TTablePoints     St_TablePoints;
typedef TTableSorter     St_TableSorter;
typedef TTableDescriptor St_tableDescriptor;

// $Log: StTypeDefs.h,v $
// Revision 1.4  2000/04/27 17:41:48  fine
//  wrong typedef for TCL class fixed
//
// Revision 1.3  2000/04/27 17:16:16  fine
//  wrong typedef for TCL class fixed
//
// Revision 1.2  2000/03/24 20:38:45  fine
// *** empty log message ***
//
// Revision 1.1  2000/02/25 00:48:06  fine
// temporary interface for ROOT 2.23/12 with STAR classes inside
//
#endif
