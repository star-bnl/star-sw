#ifndef STAR_tableredefine
#define STAR_tableredefine
// $Id: tableredef.h,v 1.1 2000/04/07 16:03:33 fine Exp $
// $Log: tableredef.h,v $
// Revision 1.1  2000/04/07 16:03:33  fine
// redefinition of the tabledescriptor datamebers
//
#if ROOT_VERSION_CODE < ROOT_VERSION(2,24,0)
#  define fColumnName m_ColumnName
#  define fIndexArray m_IndexArray
#  define fOffset     m_Offset
#  define fSize       m_Size
#  define fTypeSize   m_TypeSize
#  define fDimensions m_Dimensions
#  define fType       m_Type
#endif

#endif

