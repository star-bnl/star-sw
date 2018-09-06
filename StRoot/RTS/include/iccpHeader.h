#ifndef ICCP_HEADER_HH
#define ICCP_HEADER_HH


// MWS 3-29  Header changed. Elements of the strutct are resized and 
//           reordered 
// Tonko, 4/24/2002 Changed "transaction" to "dest_id"

#pragma pack(1)

#ifndef RTS_LITTLE_ENDIAN   
// Big endian header definition
struct ic_msg_head 
{
  unsigned char    daq_cmd  ;
  unsigned char    dst_task ;
  unsigned status :4;           
  unsigned token :12 ;
//-------------------------------------------
  unsigned valid_words :12 ;    
  unsigned domain :4 ;
  unsigned short source_id  ;
//-------------------------------------------
  unsigned short dest_id ;
  unsigned char  reserved ;
  unsigned char  src_task ;
};
#else
// Little endian header definition (For alpha and pentium)
struct ic_msg_head 
{
  unsigned token :12;
  unsigned status :4;
  unsigned char  dst_task;
  unsigned char daq_cmd;
//-------------------------------------------
  unsigned short source_id;
  unsigned domain :4 ;
  unsigned valid_words :12 ;
//-------------------------------------------
  unsigned char  src_task ;
  unsigned char  reserved ;
  unsigned short dest_id ;
};
#endif // RTS_LITTLE_ENDIAN

#pragma pack()

#endif
