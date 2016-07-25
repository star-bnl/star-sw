// multimap::equal_range
#include <iostream>
#include <map>

int main ()
{
  std::multimap<char,int> mymm;

  mymm.insert(std::pair<char,int>('a',10));
  mymm.insert(std::pair<char,int>('b',20));
  mymm.insert(std::pair<char,int>('b',30));
  mymm.insert(std::pair<char,int>('b',40));
  mymm.insert(std::pair<char,int>('c',50));
  mymm.insert(std::pair<char,int>('c',60));
  mymm.insert(std::pair<char,int>('d',60));

  std::cout << "mymm contains:\n";
  for (char ch='a'; ch<='e'; ch++)
  {
    std::pair <std::multimap<char,int>::iterator, std::multimap<char,int>::iterator> ret;
    ret = mymm.equal_range(ch);
    std::cout << ch << " =>";
    for (std::multimap<char,int>::iterator it=ret.first; it!=ret.second; ++it)
      std::cout << ' ' << it->second;
    std::cout << '\n';
  }

  return 0;
}
