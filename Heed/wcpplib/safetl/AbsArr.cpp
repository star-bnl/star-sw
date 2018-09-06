/*
Copyright (c) 1999-2004 I. B. Smirnov

The file can be used, copied, modified, and distributed
according to the terms of GNU Lesser General Public License version 2.1
as published by the Free Software Foundation,
and provided that the above copyright notice, this permission notice,
and notices about any modifications of the original text
appear in all copies and in supporting documentation.
The file is provided "as is" without express or implied warranty.
*/
#include <iomanip>
#include "wcpplib/safetl/AbsArr.h"

namespace Heed {

long max_qel_DynLinArr = 100000000;

void print_DynLinArr_int(std::ostream& file, const DynLinArr<int>& f) {
  Ifile << "DynLinArr<int>:";
  long q = f.get_qel();
  file << " q=" << q << '\n';
  f.check();
  if (q <= 0) return;
  indn.n += 2;
  if (q <= pq_arrelem_in_line) file << indn;
  for (long i = 0; i < q; i++) {
    if (q <= pq_arrelem_in_line)
      file << f[i] << ' ';  // all in one line
    else
      Ifile << std::setw(4) << i << ' ' << f[i] << '\n';  // column
  }
  if (q <= pq_arrelem_in_line) file << '\n';
  indn.n -= 2;
  file << std::flush;
}
void print_DynLinArr_long(std::ostream& file, const DynLinArr<long>& f) {
  Ifile << "DynLinArr<long>:";
  long q = f.get_qel();
  file << " q=" << q << '\n';
  f.check();
  if (q <= 0) return;
  indn.n += 2;
  if (q <= pq_arrelem_in_line) file << indn;
  long i;
  for (i = 0; i < q; i++) {
    if (q <= pq_arrelem_in_line)
      file << f[i] << ' ';  // all in one line
    else
      Ifile << std::setw(4) << i << ' ' << f[i] << '\n';  // column
  }
  if (q <= pq_arrelem_in_line) file << '\n';
  indn.n -= 2;
  file << std::flush;
}

void print_DynLinArr_float(std::ostream& file, const DynLinArr<float>& f) {
  Ifile << "DynLinArr<float>:";
  long q = f.get_qel();
  file << " q=" << q << '\n';
  f.check();
  if (q <= 0) return;
  indn.n += 2;
  if (q <= pq_arrelem_in_line) file << indn;
  long i;
  for (i = 0; i < q; i++) {
    if (q <= pq_arrelem_in_line)
      file << f[i] << ' ';  // all in one line
    else
      Ifile << std::setw(4) << i << ' ' << f[i] << '\n';  // column
  }
  if (q <= pq_arrelem_in_line) file << '\n';
  indn.n -= 2;
  file << std::flush;
}

void print_DynLinArr_double(std::ostream& file, const DynLinArr<double>& f) {
  Ifile << "DynLinArr<double>:";
  long q = f.get_qel();
  file << " q=" << q << '\n';
  f.check();
  if (q <= 0) return;
  indn.n += 2;
  if (q <= pq_arrelem_in_line) file << indn;
  long i;
  for (i = 0; i < q; i++) {
    if (q <= pq_arrelem_in_line)
      file << f[i] << ' ';  // all in one line
    else
      Ifile << std::setw(4) << i << ' ' << f[i] << '\n';  // column
  }
  if (q <= pq_arrelem_in_line) file << '\n';
  indn.n -= 2;
  file << std::flush;
}

void print_DynLinArr_double2(std::ostream& file, const DynLinArr<double>& f1,
                             const DynLinArr<double>& f2) {
  Ifile << "Two arrays DynLinArr<double>:";
  long q1 = f1.get_qel();
  long q2 = f2.get_qel();
  long q_max = q1;
  if (q_max > q2) q_max = q2;
  file << " q1=" << q1 << " q2=" << q2 << '\n';
  f1.check();
  f2.check();
  if (q_max <= 0) return;
  indn.n += 2;
  if (q_max <= pq_arrelem_in_line) file << indn;
  if (q_max >= pq_arrelem_in_line) {
    Ifile << "index            array1             array2\n";
    long i;
    for (i = 0; i < q_max; i++) {
      Ifile << std::setw(4) << i << ' ';
      if (i < q1)
        file << std::setw(18) << f1[i] << ' ';
      else
        file << "                   ";  // 19 blanks
      if (i < q2)
        file << std::setw(18) << f2[i] << '\n';
      else
        file << "                  \n";  // 18 blanks
    }
  } else {
    long i;
    Ifile << "array1=";
    for (i = 0; i < q1; i++) {
      file << std::setw(18) << f1[i] << ' ';  // all in one line
    }
    file << '\n';
    Ifile << "array2=";
    for (i = 0; i < q2; i++) {
      file << std::setw(18) << f2[i] << ' ';  // all in one line
    }
    file << '\n';
  }
  indn.n -= 2;
  file << std::flush;
}

void print_DynLinArr_int_double(std::ostream& file, const DynLinArr<int>& iar,
                                const DynLinArr<double>& dar) {
  Ifile << "One DynLinArr<int> array and one DynLinArr<double>:\n";
  long qiar = iar.get_qel();
  long qdar = dar.get_qel();
  long q_max = qiar;
  if (q_max < qdar) q_max = qdar;
  indn.n += 2;

  Ifile << " qiar=" << qiar << " qdar=" << qdar << '\n';
  iar.check();
  dar.check();
  if (q_max <= 0) {
    indn.n -= 2;
    return;
  }
  Ifile << "index int array    double array\n";
  long i;
  for (i = 0; i < q_max; i++) {
    Ifile << std::setw(4) << i << ' ';
    if (i < qiar)
      file << std::setw(8) << iar[i] << ' ';
    else
      file << "         ";  // 19 blanks
    if (i < qdar)
      file << std::setw(18) << dar[i] << ' ';
    else
      file << "                   ";  // 18 blanks
  }
  indn.n -= 2;
  file << std::flush;
}

void print_DynLinArr_int_double3(std::ostream& file, const DynLinArr<int>& iar,
                                 const DynLinArr<double>& dar1,
                                 const DynLinArr<double>& dar2,
                                 const DynLinArr<double>& dar3) {
  Ifile << "One DynLinArr<int> array and three arrays DynLinArr<double>:\n";
  long qiar = iar.get_qel();
  long qdar1 = dar1.get_qel();
  long qdar2 = dar2.get_qel();
  long qdar3 = dar3.get_qel();
  long q_max = qiar;
  if (q_max < qdar1) q_max = qdar1;
  if (q_max < qdar2) q_max = qdar2;
  if (q_max < qdar3) q_max = qdar3;
  indn.n += 2;

  Ifile << "qiar=" << qiar << " qdar1=" << qdar1 << " qdar2=" << qdar2
        << " qdar3=" << qdar3 << '\n';
  iar.check();
  dar1.check();
  dar2.check();
  dar3.check();
  if (q_max <= 0) {
    indn.n -= 2;
    return;
  }
  Ifile << "index int array    double array1      double array2      double "
           "array3\n";
  long i;
  for (i = 0; i < q_max; i++) {
    Ifile << std::setw(4) << i << ' ';
    if (i < qiar)
      file << std::setw(8) << iar[i] << ' ';
    else
      file << "         ";  // 19 blanks
    if (i < qdar1)
      file << std::setw(18) << dar1[i] << ' ';
    else
      file << "                   ";  // 18 blanks
    if (i < qdar2)
      file << std::setw(18) << dar2[i] << ' ';
    else
      file << "                   ";  // 18 blanks
    if (i < qdar3)
      file << std::setw(18) << dar3[i] << '\n';
    else
      file << "                  \n";  // 18 blanks
  }
  indn.n -= 2;
  file << std::flush;
}

void print_DynArr_int_w(std::ostream& file, const DynArr<int>& f, int w) {
  mfunname(
      "void print_DynArr_int_w(ostream& file, const DynArr<int>& f, int w)");
  f.check();
  Ifile << "DynArr<int>:";
  long qdim = f.get_qdim();
  file << " qdim=" << qdim << " sizes:";
  const DynLinArr<long>& qe = f.get_qel();
  qe.check();
  long n;
  for (n = 0; n < qdim; n++) {
    file << ' ' << qe[n];
  }
  file << '\n';
  indn.n += 2;
  if (qdim == 1) {
    long q = qe[0];
    if (q <= 0) return;
    indn.n += 2;
    if (indn.n + 3 + q * (w + 1) <= 80) {  // printing in one line
      Ifile << "ar=";
      long i;
      for (i = 0; i < q; i++) {
        file << ' ' << std::setw(w) << f.ac(i);
      }
      file << '\n';
    } else {
      Ifile << "array:\n";
      long i;
      for (i = 0; i < q; i++) {
        Ifile << std::setw(4) << i << ' ' << f.ac(i) << '\n';  // column
      }
    }
  } else if (qdim == 2) {
    long qr = qe[0];
    long qc = qe[1];
    if (qr <= 0 && qc <= 0) return;
    indn.n += 2;
    if (indn.n + 3 + qc * (w + 1) <= 80) {  // lines in lines
      Ifile << "first index - columns, second index - lines\n";
      Ifile << "first index\n";
      long ir, ic;
      for (ir = 0; ir < qr; ir++) {
        Ifile << std::setw(3) << ir;
        for (ic = 0; ic < qc; ic++) {
          Ifile << ' ' << std::setw(w) << f.ac(ir, ic);
        }
        file << '\n';
      }
    } else if (indn.n + 3 + qr * (w + 1) <= 80) {
      Imcout << "first index - lines, second index - columns\n";
      Imcout << "second index\n";
      long ir, ic;
      for (ic = 0; ic < qc; ic++) {
        Ifile << std::setw(3) << ic;
        for (ir = 0; ir < qr; ir++) {
          Ifile << ' ' << std::setw(w) << f.ac(ir, ic);
        }
        file << '\n';
      }
    } else {
      Ifile << " row  col  value of element\n";
      long ir, ic;
      for (ir = 0; ir < qr; ir++) {
        for (ic = 0; ic < qc; ic++) {
          Ifile << std::setw(4) << ir << ' ' << std::setw(4) << ic << ' ' << f.ac(ir, ic)
                << '\n';  // column
        }
      }
    }
  } else {
    IterDynArr<int> iter_f(&((DynArr<int>&)f));
    int* at;
    while ((at = iter_f.more()) != NULL) {
      Ifile << "ncur=" << noindent << iter_f.get_ncur() << yesindent;
      Ifile << "element=" << noindent << (*at) << yesindent << '\n';
    }
    file << yesindent;
  }
  // if(qc<=pq_elem_in_line) file<<'\n';
  indn.n -= 2;
  /*
  }
  else
  {
    file<<f;
  }
  */
}

void print_DynArr_double(std::ostream& file, const DynArr<double>& f) {
  mfunname("void print_DynArr_double(ostream& file, const DynArr<double>& f)");
  f.check();
  Ifile << "DynArr<double>:";
  long qdim = f.get_qdim();
  file << " qdim=" << qdim << " sizes:";
  const DynLinArr<long>& qe = f.get_qel();
  qe.check();
  long n;
  for (n = 0; n < qdim; n++) {
    file << ' ' << qe[n];
  }
  file << '\n';
  if (qdim == 1) {
    long q = qe[0];
    if (q <= 0) return;
    indn.n += 2;
    if (q <= pq_arrelem_in_line) file << indn;
    long i;
    for (i = 0; i < q; i++) {
      if (q <= pq_arrelem_in_line)
        file << f.ac(i) << ' ';  // all in one line
      else
        Ifile << std::setw(4) << i << ' ' << f.ac(i) << '\n';  // column
    }
    if (q <= pq_arrelem_in_line) file << '\n';
    indn.n -= 2;
  } else if (qdim == 2) {
    long qr = qe[0];
    long qc = qe[1];
    if (qr <= 0 && qc <= 0) return;
    indn.n += 2;
    if (qc > pq_arrelem_in_line) Ifile << " row  col  value of element\n";
    long ir, ic;
    for (ir = 0; ir < qr; ir++) {
      if (qc <= pq_arrelem_in_line) file << indn;
      for (ic = 0; ic < qc; ic++) {
        if (qc <= pq_arrelem_in_line)
          file << f.ac(ir, ic) << ' ';  // all in one line
        else
          Ifile << std::setw(4) << ir << ' ' << std::setw(4) << ic << ' ' << f.ac(ir, ic)
                << '\n';  // column
      }
      if (qc <= pq_arrelem_in_line) file << '\n';
    }
    // if(qc<=pq_elem_in_line) file<<'\n';
    indn.n -= 2;
  } else {
    file << f;
  }
}

void print_DynArr_float(std::ostream& file, const DynArr<float>& f) {
  mfunname("void print_DynArr_float(ostream& file, const DynArr<float>& f)");
  f.check();
  Ifile << "DynArr<float>:";
  long qdim = f.get_qdim();
  file << " qdim=" << qdim << " sizes:";
  const DynLinArr<long>& qe = f.get_qel();
  qe.check();
  for (long n = 0; n < qdim; n++) {
    file << ' ' << qe[n];
  }
  file << '\n';
  if (qdim == 1) {
    long q = qe[0];
    if (q <= 0) return;
    indn.n += 2;
    if (q <= pq_arrelem_in_line) file << indn;
    long i;
    for (i = 0; i < q; i++) {
      if (q <= pq_arrelem_in_line)
        file << f.ac(i) << ' ';  // all in one line
      else
        Ifile << std::setw(4) << i << ' ' << f.ac(i) << '\n';  // column
    }
    if (q <= pq_arrelem_in_line) file << '\n';
    indn.n -= 2;
  } else if (qdim == 2) {
    long qr = qe[0];
    long qc = qe[1];
    if (qr <= 0 && qc <= 0) return;
    indn.n += 2;
    if (qc > pq_arrelem_in_line) Ifile << " row  col  value of element\n";
    long ir, ic;
    for (ir = 0; ir < qr; ir++) {
      if (qc <= pq_arrelem_in_line) file << indn;
      for (ic = 0; ic < qc; ic++) {
        if (qc <= pq_arrelem_in_line)
          file << f.ac(ir, ic) << ' ';  // all in one line
        else
          Ifile << std::setw(4) << ir << ' ' << std::setw(4) << ic << ' ' << f.ac(ir, ic)
                << '\n';  // column
      }
      if (qc <= pq_arrelem_in_line) file << '\n';
    }
    // if(qc<=pq_elem_in_line) file<<'\n';
    indn.n -= 2;
  } else {
    file << f;
  }
}

int gconfirm_ind(const DynLinArr<long>& qel, const DynLinArr<long>& ind) {
  if (qel.get_qel() != ind.get_qel()) {
    mcerr << "gconfirm_ind(...): "
          << "qel.get_qel()!= ind.get_qel()\n"
          << "qel.get_qel()=" << qel.get_qel()
          << "ind.get_qel()=" << ind.get_qel() << '\n';
    spexit(mcerr);
  }
  long qd = qel.get_qel();
  // if( ind.get_qel() < qd) qd=ind.get_qel();
  long n;
  for (n = 0; n < qd; n++)
    if (ind[n] < 0 || ind[n] >= qel[n]) return 0;
  return 1;
}
int gconfirm_ind_ext(const DynLinArr<long>& qel, const DynLinArr<long>& ind) {
  if (qel.get_qel() > ind.get_qel()) {
    mcerr << "gconfirm_ind_ext(...): "
          << "qel.get_qel()> ind.get_qel()\n"
          << "qel.get_qel()=" << qel.get_qel()
          << " ind.get_qel()=" << ind.get_qel() << '\n';
    spexit(mcerr);
  }
  long qd = qel.get_qel();
  long n;
  for (n = 0; n < qd; n++)
    if (ind[n] < 0 || ind[n] >= qel[n]) return 0;
  for (n = qd; n < ind.get_qel(); n++)
    if (ind[n] != 0) return 0;
  return 1;
}

int find_next_comb(const DynLinArr<long>& qel, DynLinArr<long>& f) {
  long n;
  long qdim = qel.get_qel();
  if (qdim <= 0) return 0;
  if (qdim != f.get_qel()) return 0;  //@@
#ifdef DEBUG_DYNARR
  for (n = qdim - 1; n >= 0; n--) {
    if (f[n] < qel[n] - 1) {
      f[n]++;
      return 1;
    } else {
      f[n] = 0;
    }  // the first element

  }                                                  // it was last combination
  for (n = 0; n < qdim - 1; n++) f[n] = qel[n] - 1;  // the last element
  f[qdim - 1] = qel[qdim - 1];                       // next after last
#else
  for (n = qdim - 1; n >= 0; n--) {
    if (f.acu(n) < qel.acu(n) - 1) {
      f.acu(n)++;
      return 1;
    } else {
      f.acu(n) = 0;
    }  // the first element

  }  // it was last combination
  for (n = 0; n < qdim - 1; n++) f.acu(n) = qel.acu(n) - 1;  // the last element
  f.acu(qdim - 1) = qel.acu(qdim - 1);                       // next after last
#endif
  return 0;
}

int find_next_comb_not_less(const DynLinArr<long>& qel, DynLinArr<long>& f) {
  long n;
  long qdim = qel.get_qel();
  if (qdim <= 0) return 0;
  if (qdim != f.get_qel()) return 0;  //@@
  for (n = qdim - 1; n >= 0; n--) {
    if (f[n] < qel[n] - 1) {
      f[n]++;
      int n1;
      for (n1 = n + 1; n1 < qdim; n1++) f[n1] = f[n];
      return 1;
    }
  }                                                  // it was last combination
  for (n = 0; n < qdim - 1; n++) f[n] = qel[n] - 1;  // the last element
  f[qdim - 1] = qel[qdim - 1];                       // next after last
  return 0;
}

int find_prev_comb(const DynLinArr<long>& qel, DynLinArr<long>& f) {
  long n;
  long qdim = qel.get_qel();
  if (qdim <= 0) return 0;
  if (qdim != f.get_qel()) return 0;  //@@
  for (n = qdim - 1; n >= 0; n--) {
    if (f[n] >= 1) {
      f[n]--;
      return 1;
    } else {
      f[n] = qel[n] - 1;
    }  // the last element
  }
  for (n = 0; n < qdim - 1; n++) f[n] = 0;  // the first element
  f[qdim - 1] = -1;
  return 0;  // previous before first
}

DynLinArr<long> qel_communicat;
}
