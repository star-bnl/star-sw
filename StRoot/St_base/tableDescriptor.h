/* tableDescriptor.h */
#ifndef TABLEDESCRIPTOR_H
#define TABLEDESCRIPTOR_H
#define TABLEDESCRIPTOR_SPEC   \
"struct tableDescriptor {      \
    char         *m_ColumnName;\
    unsigned int m_Offset;     \
    unsigned int m_Size;       \
    unsigned int m_TypeSize;   \
    unsigned int m_Dimensions; \
    unsigned int* m_IndexArray;\
    EColumnType  m_Type;       \
};"
 
/*   this is a name clas with ROOT 
 * enum EColumnType {kNAN, kFloat, kInt, kLong, kShort, kDouble, kUInt
 *                     ,kULong, kUShort, kUChar, kChar };
 */
typedef struct tableDescriptor_st {
    char        *m_ColumnName;  /* The name of this data-member                                          */
    unsigned int m_Offset;      /* The first byte in the row of this column                              */
    unsigned int m_Size;        /* The full size of the selected column in bytes                         */
    unsigned int m_TypeSize;    /* The type size of the selected column in bytes                         */
    unsigned int m_Dimensions;  /* The number of the dimensions for array                                */
    unsigned int *m_IndexArray; /* The array of the sizes for each dimensions m_IndexArray[m_Dimensions] */
    Int_t        m_Type;        /* The data type of the selected column                                  */
} TABLEDESCRIPTOR_ST;
#endif /* TABLEDESCRIPTOR_H */
