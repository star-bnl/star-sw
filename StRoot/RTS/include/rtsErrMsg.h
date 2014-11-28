#ifndef _RTS_ERR_MSG_H
#define _RTS_ERR_MSG_H
#include "iccp.h"
#include <stdio.h>

#define MSG_ERR(MSGP,STRING,A1,A2,A3,A4,A5) \
        MSG_ERR_LOCAL(MSGP, __FILE__, STRING, __LINE__, A1, A2, A3, A4, A5)

	  // payload
	  //        line#|file|string
	  //        
	  //        

#if defined(__linux__) || defined(__APPLE__)
inline void MSG_ERR_LOCAL(ic_msg *msg, 
		     char *file,
		     char *str,
		     long line,
		     long a1,
		     long a2,
		     long a3,
		     long a4,
		     long a5)
{
  char *c = (char *)&msg->ld;
  char buff[255];

  sprintf(buff, "%ld|%s|",line,file);
  int i = strlen(buff);
  sprintf(&buff[i], str, a1, a2, a3, a4, a5);

  for(int i=0;i<107;i+=4)
  {
#ifdef RTS_LITTLE_ENDIAN
    c[i] = buff[i+3];
    c[i+1] = buff[i+2];
    c[i+2] = buff[i+1];
    c[i+3] = buff[i];
#else
    c[i] = buff[i];
    c[i+1] = buff[i+1];
    c[i+2] = buff[i+2];
    c[i+3] = buff[i+3];
#endif
  }
  c[108] = 0;

  msg->head.valid_words = (strlen(c) + 7) / 4;
}
#else
inline void MSG_ERR_LOCAL(ic_msg *msg, 
		     char *file,
		     char *str,
		     int line,
		     int a1,
		     int a2,
		     int a3,
		     int a4,
		     int a5)
{
  char *c = (char *)&msg->ld;
  char buff[255];

  sprintf(buff, "%d|%s|",line,file);
  int i = strlen(buff);
  sprintf(&buff[i], str, a1, a2, a3, a4, a5);

  strncpy(c,buff,106);
  c[107] = 0;
  msg->head.valid_words = (strlen(c) + 7) / 4;
}
#endif

#endif
