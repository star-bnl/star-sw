/*
 * physmem.h
 * last update: 02-Aug-2006
 */

#define PHYSMEM_MAJOR 122
#define PHYSMEM_NAME  "physmem"

#define PHYSMEM_4GB  0x100000000ULL  
#define PHYSMEM_2GB  0x080000000L  
#define PHYSMEM_96MB 0x006000000L
#define PHYSMEM_16MB 0x001000000L      

#define PHYSMEM_MAXDEVICE 2  // physmem0, physmem1

#define PHYSMEM_GETADDR  _IOR('x', 1, u_long)  // get physical address
#define PHYSMEM_GETSIZE  _IOR('x', 2, u_long)  // get size in bytes


/* physmem driver for a 2.4 kernel    version 3.0
   ===============================  

INSTALLATION
============

- all commands must be done as root

- partition the physical memory between Linux and physmem:
  for this purpose modify the configuration file for the 
  the boot loader lilo or grub (depending on your system)
  for grub: edit file /boot/grub/grub.conf file
  for lilo: edit file /etc/lilo.conf, execute /sbin/lilo
  in this file add the parameter "mem=512M" for example to 
  limit Linux to 512 MB and physmem gets the remaining memory

- reboot the machine with 
  /sbin/shutdown -r now

- get sources it for example in /usr/local/physmem

- create the device file /dev/physmem0 and /dev/physmem1 with
  gmake dev

- compile the physmem driver and the test program with
  gmake driver
  gmake 

- load the driver for testing purpose with
  /sbin/insmod physmem.o

- optionally the parameter physmemsize in MB can be passed
  it given the total amount of physmem
  for example the following command assigns 100 MB
  /sbin/insmod physmem.o physmemsize=100 

- optionally the parameter physmemsize0 in MB can be passed
  it given the amount of physmem0, default is 128 MB
  for example the following command assigns 16 MB
  /sbin/insmod physmem.o physmemsize0=16

- a test with physmemTest is recommended

- check how much memory Linux and physmem sees
  for Linux: cat /proc/meminfo
  for physmem: grep physmem /var/log/messages

- add the following line in the /etc/rc.d/rc.local file
  to load the driver at boot time
  /sbin/insmod /opt/physmem/physmem.o


USAGE
=====

- a simple test can be done with the program physmemTest
  it does a write-read-back test for all physmem devices

- the API is the following using standard Linux file operations:

  #include "physmem.h"

  int err;
  int physmem_fd;
  void *addr_user_physmem;  
  unsigned long phys_addr_physmem; 
  unsigned long phys_size_physmem; 

  1) open the device:
     physmem_fd = open("/dev/physmem0",O_RDWR)

  2) get physical start address:
     err = ioctl (physmem_fd, PHYSMEM_GETADDR, &physmem_addr)

  3) get available size in bytes:
     err = ioctl (physmem_fd, PHYSMEM_GETSIZE, &physmem_size)

  4) map to user space:
     addr_user_physmem = mmap(0,physmem_size,PROT_READ|PROT_WRITE, 
                              MAP_SHARED,physmem_fd,0)

  5) unmap from user space:
     err = munmap(physmem_addr,physmem_size)

  6) close the device:
     err = close (physmem_fd)

- a simple programming example can be found in physmemTest.c 
  

*/
