#include <termios.h>
#include <unistd.h>
#include <stdlib.h>
#include <signal.h>
#include <stdio.h>
#include <string.h>


static struct termios save_termios;
static int ttysavefd = -1;
static enum { CBREAK, RESET } ttystate = RESET;

#define HISTORY_SIZE 100
char history[HISTORY_SIZE][255];

int tty_cbreak(int fd)
{
  struct termios buf;
  if(tcgetattr(fd, & save_termios) < 0)
    return -1;

  buf = save_termios;

  buf.c_lflag &= ~(ECHO | ICANON);
  buf.c_cc[VMIN] = 1;
  buf.c_cc[VTIME] = 0;

  if(tcsetattr(fd, TCSAFLUSH, &buf) < 0)
    return -1;
  
  ttysavefd = fd;
  ttystate = CBREAK;
  return 0;
}

int tty_reset(int fd)
{
  if(ttystate != CBREAK) return 0;
  if(tcsetattr(fd, TCSAFLUSH, &save_termios) < 0)
    return -1;
  ttystate = RESET;
  return 0;
}

// static void sig_catch(int);

// static void sig_catch(int signo)
// {
//   printf("Signal caught\n");
//   tty_reset(STDIN_FILENO);
//   exit(0);
// }

static int c_line = 0;

void get_line(char *buff)
{
//   memset(history,0,sizeof(history));

//   if(signal(SIGINT, sig_catch) == SIG_ERR)
//   {
//     printf("Error\n");
//     exit(0);
//   }
//   if(signal(SIGQUIT, sig_catch) == SIG_ERR)
//   {
//     printf("Error 1\n");
//     exit(0);
//   }
//   if(signal(SIGTERM, sig_catch) == SIG_ERR)
//   {
//     printf("Error 2\n");
//     exit(0);
//   }

  if(tty_cbreak(STDIN_FILENO) < 0)
  {
    //printf("Error 3\n");
    exit(0);
  }

  int h_line;
  int c_char = 0;
  int l_char = 0;
  int t;
  int i;
  char c;

  h_line = c_line;
  c_char = 0;
  l_char = 0;
  memset(history[c_line],0,sizeof(history[c_line]));

  while( (i = read(STDIN_FILENO, &c, 1)) == 1) 
  {
    c &= 0xff;
    switch(c)
    {
    case 0xa:  // Return
      putchar(0xa);
      fflush(stdout);
      break;
    case 0x8:  // BACK 
    case 0x7f: // DEL
      // Change the history
      if(c_char == 0) break;
      for(t=c_char;t<=l_char;t++)
      {
	history[c_line][t-1] = history[c_line][t];
      }
      history[c_line][l_char] = '\0';
      l_char--;
      c_char--;

      // Print
      putchar(0x8);
      for(t=c_char;t<l_char;t++)
      {
	putchar(history[c_line][t]);
      }
      putchar(' ');
      for(t=c_char;t<l_char+1;t++)
	putchar(0x8);
	
      fflush(stdout);
      break;

    case 0x1b:
      {
	i = read(STDIN_FILENO, &c, 1);
	i = read(STDIN_FILENO, &c, 1);
	if(c == 0x43) // right arrow
	{
	  if(c_char < l_char) 
	  {
	    c_char++;
	    putchar(0x1b);
	    putchar(0x5b);
	    putchar(0x43);
	  }
	}
	else if(c==0x44) // left arrow
	{
	  if(c_char > 0) 
	  {
	    c_char--;
	    putchar(0x1b);
	    putchar(0x5b);
	    putchar(0x44);
	  }
	}
	else if(c==0x41) // up arrow
	{
	  h_line--;
	  if(h_line < 0) h_line = HISTORY_SIZE-1;
	  
	  for(t=c_char;t>0;t--)
	  {
	    putchar(0x8);
	  }
	  for(t=0;t<l_char;t++)
	  {
	    putchar(' ');
	  }
	  for(t=l_char;t>0;t--)
	  {
	    putchar(0x8);
	  }
	  l_char = 0;
	  c_char = 0;
	  memset(history[c_line],0,sizeof(history[c_line]));
	  while(history[h_line][c_char])
	  {
	    putchar(history[h_line][c_char]);
	    history[c_line][c_char] = history[h_line][c_char];
	    c_char++;
	  }
	  l_char = c_char;
	}
	else if(c==0x42) // down arrow
	{
	  h_line++;
	  if(h_line >= HISTORY_SIZE) h_line = 0;
	  for(t=c_char;t>0;t--)
	  {
	    putchar(0x8);
	  }
	  for(t=0;t<l_char;t++)
	  {
	    putchar(' ');
	  }
	  for(t=l_char;t>0;t--)
	  {
	    putchar(0x8);
	  }
	  l_char = 0;
	  c_char = 0;
	  memset(history[c_line],0,sizeof(history[c_line]));
	  while(history[h_line][c_char])
	  {
	    putchar(history[h_line][c_char]);
	    history[c_line][c_char] = history[h_line][c_char];
	    c_char++;
	  }
	  l_char = c_char;
	}
	fflush(stdout);
	break;
      }

    default:
      // Change history
      for(t=l_char;t>c_char;t--)
      {
	history[c_line][t] = history[c_line][t-1];
      }
      history[c_line][c_char] = c;
      l_char++;
      c_char++;

      // Print
      for(t=c_char-1;t<l_char;t++)
      {
	putchar(history[c_line][t]);
      }
      for(t=c_char;t<l_char;t++)
      {
	putchar(0x8);
      }
      fflush(stdout);
      break;
    }
    
    if(c == 0xa) break;
  }

  //  printf("line[%d]: %s\n",c_line,history[c_line]);
  strcpy(buff,history[c_line]);

  c_line++;
  if(c_line >= HISTORY_SIZE) c_line = 0;

  tty_reset(STDIN_FILENO);
}
       
char one_line_buff[255];

void get_one_line(char *buff)
{
  fflush(stdout);
  if(tty_cbreak(STDIN_FILENO) < 0)
  {
    //printf("Error 3\n");
    exit(0);
  }

  int c_char = 0;
  int l_char = 0;
  int t;
  int i;
  char c;

  c_char = 0;
  l_char = 0;
  memset(one_line_buff,0,sizeof(one_line_buff));

  while( (i = read(STDIN_FILENO, &c, 1)) == 1) 
  {
    c &= 0xff;
    switch(c)
    {
    case 0xa:  // Return
      putchar(0xa);
      fflush(stdout);
      break;
    case 0x8:  // BACK 
    case 0x7f: // DEL
      // Change the history
      if(c_char == 0) break;
      for(t=c_char;t<=l_char;t++)
      {
	one_line_buff[t-1] = one_line_buff[t];
      }
      one_line_buff[l_char] = '\0';
      l_char--;
      c_char--;

      // Print
      putchar(0x8);
      for(t=c_char;t<l_char;t++)
      {
	putchar(one_line_buff[t]);
      }
      putchar(' ');
      for(t=c_char;t<l_char+1;t++)
	putchar(0x8);
	
      fflush(stdout);
      break;

    case 0x1b:
      {
	i = read(STDIN_FILENO, &c, 1);
	i = read(STDIN_FILENO, &c, 1);
	if(c == 0x43) // right arrow
	{
	  if(c_char < l_char) 
	  {
	    c_char++;
	    putchar(0x1b);
	    putchar(0x5b);
	    putchar(0x43);
	  }
	}
	else if(c==0x44) // left arrow
	{
	  if(c_char > 0) 
	  {
	    c_char--;
	    putchar(0x1b);
	    putchar(0x5b);
	    putchar(0x44);
	  }
	}
	fflush(stdout);
	break;
      }

    default:
      // Change history
      for(t=l_char;t>c_char;t--)
      {
	one_line_buff[t] = one_line_buff[t-1];
      }
      one_line_buff[c_char] = c;
      l_char++;
      c_char++;

      // Print
      for(t=c_char-1;t<l_char;t++)
      {
	putchar(one_line_buff[t]);
      }
      for(t=c_char;t<l_char;t++)
      {
	putchar(0x8);
      }
      fflush(stdout);
      break;
    }
    
    if(c == 0xa) break;
  }

  //  printf("line[%d]: %s\n",c_line,history[c_line]);
  strcpy(buff,one_line_buff);

  tty_reset(STDIN_FILENO);
}
