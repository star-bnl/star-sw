/* tableDescriptor.h */
#ifndef TABLEDESCRIPTOR_H
#define TABLEDESCRIPTOR_H
// $Id: tableDescriptor.h,v 1.7 2000/01/25 22:25:46 fine Exp $"
#ifdef NORESTRICTIONS
# define TABLEDESCRIPTOR_SPEC   \
 "struct tableDescriptor {      \
    char         *m_ColumnName; \
    unsigned int *m_IndexArray; \
    unsigned int m_Offset;      \
    unsigned int m_Size;        \
    unsigned int m_TypeSize;    \
    unsigned int m_Dimensions;  \
    EColumnType  m_Type;        \
};"
#else
# define TABLEDESCRIPTOR_SPEC      \
 "struct tableDescriptor {         \
    char         m_ColumnName[20]; \
    unsigned int m_IndexArray[3];  \
    unsigned int m_Offset;         \
    unsigned int m_Size;           \
    unsigned int m_TypeSize;       \
    unsigned int m_Dimensions;     \
    EColumnType  m_Type;           \
};"
#endif
 
/*   this is a name clas with ROOT 
 * enum EColumnType {kNAN, kFloat, kInt, kLong, kShort, kDouble, kUInt
 *                     ,kULong, kUShort, kUChar, kChar };
 */

/*  This is to introduce an artificial restriction demanded by STAR database group
 *
 *    1. the name may be 19 symbols at most
 *    2. the number of the dimensions is 2 at most
 *
 *  To lift this restriction one has to provide -DNORESTRICTIONS CPP symbol and
 *  recompile code.
 */
typedef struct tableDescriptor_st {
#ifdef NORESTRICTIONS
    char         *m_ColumnName; /* The name of this data-member                                          */
    unsigned int *m_IndexArray; /* The array of the sizes for each dimensions m_IndexArray[m_Dimensions] */
#else
    char         m_ColumnName[32];  /* The name of this data-member: see dstype.h for dsl compatible mode */
    unsigned int m_IndexArray[3];   /* The array of the sizes for each dimensions m_IndexArray[m_Dimensions] */
#endif
    unsigned int m_Offset;      /* The first byte in the row of this column                              */
    unsigned int m_Size;        /* The full size of the selected column in bytes                         */
    unsigned int m_TypeSize;    /* The type size of the selected column in bytes                         */
    unsigned int m_Dimensions;  /* The number of the dimensions for array                                */
    Int_t        m_Type;        /* The data type of the selected column                                  */
} TABLEDESCRIPTOR_ST;
// $Log: tableDescriptor.h,v $
// Revision 1.7  2000/01/25 22:25:46  fine
// the number of dimensions for table arrays became 3 instead of 2
//
// Revision 1.6  2000/01/12 18:07:25  fine
//  cvs symbols have been added and copyright class introduced
//"
#endif /* TABLEDESCRIPTOR_H */
