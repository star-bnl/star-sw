/***************************************************************************
 *
 * $Id: EztEmcRawData.h,v 1.1 2004/10/28 00:10:19 mvl Exp $
 *
 * Author: Alex Suaide, Mar 2004, JB
 ***************************************************************************
 *
 * Description: ~1:1 copy of StEztEmcRawData but without StElephant dependence
 *
 ***************************************************************************/

#ifndef EztEmcRawData_hh
#define EztEmcRawData_hh

#include "TObject.h"
#include <TArrayS.h>

class EztEmcRawData : public TObject {
public:
  enum {MAXEMCDATABANK=60}; // 48 would be enough for EEMC

    EztEmcRawData();
    EztEmcRawData(const EztEmcRawData&);
    ~EztEmcRawData();
        
    const UShort_t* header(int) const;
    const UShort_t* data(int) const; 

    const int     sizeHeader(int) const;
    const int     sizeData(int) const;
    
    const int     getNBlocks() const { return MAXEMCDATABANK;}
    void          createBank(int, int, int);
        
    void          setHeader(int, unsigned short*);
    void          setData(int, unsigned short*);

    const UChar_t getCorruption(int ib) const { return  mCorrupt[ib];}
    void          setCorruption(int ib, UChar_t x) {mCorrupt[ib]=x;}

protected:
    TArrayS       mHeader[MAXEMCDATABANK];
    TArrayS       mData[MAXEMCDATABANK];
    UChar_t       mCorrupt[MAXEMCDATABANK];// encodes all corruptions, filled in fly

 private:
    void          deleteBank(int);
    
    ClassDef(EztEmcRawData,1)
};
#endif

/**************************************************************************
 *
 * $Log: EztEmcRawData.h,v $
 * Revision 1.1  2004/10/28 00:10:19  mvl
 * Initial revision of ezTree classes (for EEmc raw data)
 *
 *
 **************************************************************************/

