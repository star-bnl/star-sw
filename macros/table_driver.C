/* 
   root.exe Db.C 'table_drive.C+(table)'
*/
#include "Riostream.h"
#include "TDataSet.h"
#include "TTable.h"
#include "TBufferFile.h"
//________________________________________________________________________________
void a_write(TBuffer &buf, TTable *table) {
  buf.Reset();
  buf.SetWriteMode();

  cout << "Will write the table " << table->GetName() << endl;
  buf << table;
}
//________________________________________________________________________________

TTable *a_read(TBuffer &buf) {

  buf.Reset();
  buf.SetReadMode();
  TTable *tableR = 0;
  buf >> tableR;
  if (tableR) {
    cout << "read the table = " << tableR->GetName() << endl;
  }
  return tableR;
}
//________________________________________________________________________________
TTable *table_driver(TTable *table) {
  if (! table) return 0;
  table->Print(0,2);
  TBuffer* buf = new TBufferFile(TBuffer::kWrite);
  TDataSet *parent = table->GetParent();
  table->SetParent();
  a_write(*buf, table);
  table->SetParent(parent);
  //  cout << buf->BufferSize() << "\t" <<  buf->Buffer() << endl;
  TBuffer* bufr = new TBufferFile(TBuffer::kRead, buf->BufferSize(), buf->Buffer());
  buf->DetachBuffer();
  TTable *tableR = a_read(*bufr);
  tableR->Print(0,2);
  delete buf;
  delete bufr;
  
  return tableR;
}
