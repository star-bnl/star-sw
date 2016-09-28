/// Checking that TArrayI do not work .....
/// Philippe Canal, 09/27/16
#include "Riostream.h"
#include "TArrayI.h"
#include "TBufferFile.h"

void a_write(TBuffer &buf) {

  TArrayI * array = new TArrayI(20);
  for(int i=0; i<10; i++) {

    (*array)[i] = i;

  }
  buf.Reset();
  buf.SetWriteMode();

  fprintf(stderr,"Will write the array\n");
  buf << array;

};

void a_read(TBuffer &buf) {

  buf.Reset();
  buf.SetReadMode();

  TArrayI * array = 0; // = new TArrayI();
  
  fprintf(stderr,"Will read the array\n");
  buf >> array;
  
  for(int i=0; i<10; i++) {

    // fprintf(stderr,"%d : %d \n", i, (*array)[i]);
    if ( (*array)[i] != i ) {
      fprintf(stderr,"Error: TArrayI  not read properly! ");
      fprintf(stderr,"Expected %d and got %d\n", 
              i,
              (*array)[i]);
    };

  }
  

};

void array_driver() {
  TBuffer* buf = new TBufferFile(TBuffer::kWrite);
  a_write(*buf);
  cout << buf->BufferSize() << "\t" <<  buf->Buffer() << endl;
  TBuffer* bufr = new TBufferFile(TBuffer::kRead, buf->BufferSize(), buf->Buffer());
  buf->DetachBuffer();
  a_read(*bufr);
  delete buf;
  delete bufr;
}
