#include "tables/St_Survey_Table.h"

TDataSet *CreateTable() {
    if (!TClass::GetClass("St_Survey")) return 0;
Survey_st row;
St_Survey *tableSet = new St_Survey("hssOnFst",2);

    memset(&row,0,tableSet->GetRowSize());
        row.Id   = 1;
        row.r00  = 1.0;
        row.r01  = 0.0;
        row.r02  = 0.0;
        row.r10  = 0.0;
        row.r11  = 1.0;
        row.r12  = 0.0;
        row.r20  = 0.0;
        row.r21  = 0.0;
        row.r22  = 1.0;
        row.t0   = 0.07126;
        row.t1   = -0.30102;
        row.t2   = 0.0;
    memcpy(&row.comment,"FST Left Half\x00",1);
    tableSet->AddAt(&row);

    memset(&row,0,tableSet->GetRowSize());
        row.Id   = 2;
        row.r00  = 1.0;
        row.r01  = 0.0;
        row.r02  = 0.0;
        row.r10  = 0.0;
        row.r11  = 1.0;
        row.r12  = 0.0;
        row.r20  = 0.0;
        row.r21  = 0.0;
        row.r22  = 1.0;
        row.t0   = 0.22626;
        row.t1   = -0.33302;
        row.t2   = 0.0;
    memcpy(&row.comment,"FST Right Half\x00",1);
    tableSet->AddAt(&row);

return (TDataSet *)tableSet;
}
