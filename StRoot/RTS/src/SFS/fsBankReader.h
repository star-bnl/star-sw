#ifndef _FSBANKREADER_H_
#define _FSBANKREADER_H_

class fsBankReader {
 public:

  fsBankReader() {};
  ~fsBankReader() { close(); };

  int open(int fd, fs_index *idx, char *name);
  void close();

  void dump();

  static void headerdump(char *buff);
  static void decdump(char *buff, int sz);

  static void hexdump(char *buff, int sz);

 private:
  int fd;
  fs_index *idx;
  char name[256];
};

#endif
