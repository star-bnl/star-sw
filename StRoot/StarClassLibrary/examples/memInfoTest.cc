#include "StMemoryInfo.hh"

int main()
{
    char *buf[4];
    StMemoryInfo *info = StMemoryInfo::instance();
    
    info->snapshot();
    info->print();

    buf[0] = new char[10000];
    cout << "\n===> 10000 bytes allocated\n" << endl;
    buf[0][9999] = 'a';

    info->snapshot();
    info->print();

    buf[1] = new char[10000];
    cout << "\n===>  10000 bytes allocated\n" << endl;
    buf[1][9999] = 'a';
    
    info->snapshot();
    info->print();

    buf[2] = new char[100000];
    cout << "\n===>  100000 bytes allocated\n" << endl;
    buf[2][99999] = 'a';
    
    info->snapshot();
    info->print();

    buf[3] = new char[50000];
    cout << "\n===>  50000 bytes allocated\n" << endl;
    buf[3][49999] = 'a';
    
    info->snapshot();
    info->print();

    delete [] buf[2];
    cout << "\n===>  100000 bytes freed\n" << endl;
    
    info->snapshot();
    info->print();

    buf[2] = new char[100000];
    cout << "\n===>  100000 bytes allocated\n" << endl;
    buf[2][99999] = 'a';

    info->snapshot();
    info->print();
    
    for (int i=0; i<4; i++) delete buf[i];
    cout << "\n===> all allocated memory freed\n" << endl;
    
    info->snapshot();
    info->print();

    cout << "\ndone" << endl;
    return 0;
}
