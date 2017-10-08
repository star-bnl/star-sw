#include <cstdio>
#include "wcpplib/stream/definp.h"
#include "wcpplib/stream/findmark.h"

namespace Heed {

int definp_int(const std::string& str) {
  mfunnamep("int definp_int(const std::string& str)");
  int i = 0;
  mcout << "definp_int: starting, read int " << str << "\n";
  if (str != std::string()) {
    // search for mark
    int i_ret = findmark(std::cin, str.c_str());
    check_econd11(i_ret, != 1, mcerr);
  }
  std::cin >> i;
  Iprintn(mcout, i);
  check_econd11(std::cin.good(), != 1, mcerr);
  mcout << "int is read\n";
  return i;
}

long set_position(const std::string& word, std::istream& istrm, int s_rewind,
                  int s_req_sep) {
  mfunnamep(
      "int set_position(const std::string& word, std::istream& istrm, int "
      "s_rewind, int s_req_sep)");
  check_econd11a(istrm.good(), != 1,
                 "before seekg, call for variable named " << word << '\n',
                 mcerr);
  long nbeg, nnext;
  char prev;
  if (s_rewind == 1) istrm.seekg(0);
  if (s_req_sep == 0) {
    // int iret = findmark(istrm, word.c_str());
    int iret = findmark_b(istrm, word, word.length(), nbeg, nnext, prev);

    check_econd11a(iret, != 1,
                   "The keyword \"" << word.c_str() << "\" is not found\n",
                   mcerr);
    check_econd11a(istrm.good(), != 1,
                   "after the call of findmark for variable named " << word
                                                                    << '\n',
                   mcerr);
  } else {
    do {
      int iret = findmark_b(istrm, word, word.length(), nbeg, nnext, prev);
      check_econd11a(iret, != 1,
                     "The keyword \"" << word.c_str() << "\" is not found\n",
                     mcerr);
      check_econd11a(istrm.good(), != 1,
                     "after the call of findmark for variable named " << word
                                                                      << '\n',
                     mcerr);
      if (nbeg == 0) return nbeg;  // no need to search for separator
      if (prev == '\n' || prev == ' ') return nbeg;  // good separator
    } while (1);                                     // infinite loop
  }
  check_econd11a(istrm.good(), != 1,
                 "after findmark_b, call for variable named " << word << '\n',
                 mcerr);
  return nbeg;
}

}
