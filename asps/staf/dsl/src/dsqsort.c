/* Copyright 1991, Lawrence Berkeley Laboratory */

/* sortlib.c - multi-thread quick sort and binary search */

/*
modification history
--------------------
10sep91,whg  written.
01jul92,whg  add error handling
08mar95,whg  total rewrite, convert to ansi C
*/
/*
DESCRIPTION
Quick sort and binary search with extra argument to implement
general sorts and searches in a multi-thread environment.
The	compare function, cmp(a, b, key),  must return:
   1 if a > b 
   0 if a = b
  -1 if a < b.
Any other value indicates an error.    
*/
#define FALSE 0
#define NULL (void *)0
#define TRUE 1
 
struct long_test {char c; long l;};
#define LONG_ALIGNED(p) (0 == (((char *)(p) - (char *)0)&\
	(sizeof(struct long_test) - sizeof(long) - sizeof(char))))
	 
static int dsQuickSortR(char *pLeft, char *pRight, unsigned size,
	int (*cmp)(char *base1, char *base2, char *keyDef), char *key);
/******************************************************************************
*
* bKeySearch - like ansi bsearch with cmp(key, ptr)
*  returns first element if duplicate keys
*
* RETURN: TRUE if element found else FALSE
*
*/
int dsBinSearch(char **pFound, char *value, char *base, unsigned count,
	unsigned size, int (*cmp)(char *base1, char *base2, char *key), char *key)
{
  char *ptr = 0; /* Initialize to quiet compiler. */
  int left, mid, right, test;
    
	left = 0;
	right = (int)count - 1;
	test = 2;
	while (left <= right) {
		mid = (left + right)/2;
		ptr = base + mid * size;
		test = cmp(value, ptr, key);
		
		if (left == right) {
			break;
		}
		if (test == 1) {
			left = mid + 1;
		}
		else if (test == -1) {
			right = mid - 1;
		}
		else if (test == 0) {
			right = mid;
		}
		else {
			break;
		}
	}
	*pFound = (-1 <= test && test <= 1) ? ptr : NULL;
	return test == 0 ? TRUE : FALSE;
}
/******************************************************************************
*
* memSwap - swap n bytes between locations pointed to by p1 and p2
*
*/
void memSwap(void *p1, void *p2, unsigned n)
{
	char c;
	unsigned i;
	long l;
	
	/* check size and alignment for four byte swap */
	if (n%sizeof(long) == 0 && LONG_ALIGNED(p1) && LONG_ALIGNED(p2)) {
		n /= sizeof(long);
		for (i = 0; i < n; i++) {
			l = ((long *)p1)[i];
			((long *)p1)[i] = ((long *)p2)[i];
			((long *)p2)[i] = l;
		}
	}
	else {
		for (i = 0; i < n; i++) {
			c = ((char *)p1)[i];
			((char *)p1)[i] = ((char *)p2)[i];
			((char *)p2)[i] = c;
		}
	}
}
/******************************************************************************
*
* dsQuickSort - like ansi qsort with an extra argument passed to cmp
*
*/
int dsQuickSort(char *base, unsigned count, int size,
	int (*cmp)(char * base1, char *base2, char *key), char *key)
{
	return dsQuickSortR(base, base + size*(count - 1), size, cmp, key);
}
/******************************************************************************
*
* dsQuickSortR - based on quick sort from Algorithms in C by Robert Sedgewick
*
* RETURN: FALSE if value from cmp is invalid or bug detected else TRUE
*/
static int dsQuickSortR(char *pLeft, char *pRight, unsigned size,
	int (*cmp)(char *base1, char *base2, char *key), char *key)
{
	char *pI, *pJ, *ptr;
	int c, n;
	
	/* done if less than two elements */
	if (pRight <= pLeft) {
		return TRUE;
	}
	/* start partition, will do sort for three or fewer elements */
	if ((c = cmp(pLeft, pRight, key)) == 1) {
		memSwap(pLeft, pRight, size);
	}
	else if (c != -1 && c != 0) {
		return FALSE;
	}
	/* done if two elements */
	if ((n = (pRight - pLeft)/size) < 2) {
		return TRUE;
	}
	ptr = pLeft + (n/2) * size;
	if ((c = cmp(pLeft, ptr, key)) == 1) {
		memSwap(pLeft, ptr, size);
	}
	else if (c != -1 && c != 0) {
		return FALSE;
	}
	if ((c = cmp(ptr, pRight, key)) == 1) {
		memSwap(ptr, pRight, size);
	}
	else if (c != -1 && c != 0) {
		return FALSE;
	}
	/* done if three elements */
	if (n < 3) {
		return TRUE;
	}
	/*
	 * do "median-of-three" partitioning
	 * swap to prevent N**2 preformance for sorted data
	 */
	pI = pLeft;	 
	pJ = pRight - size;
	memSwap(ptr, pJ, size);
	ptr = pJ;

	for (;;) {
		do {
			pI += size;
			if (pI >= pRight) {
				return FALSE;
			}
		} while ((c = cmp(ptr, pI, key)) == 1);
		if (c != -1 && c != 0) {
			return FALSE;
		}
		do {
			pJ -= size;
			if (pJ < pLeft) {
				return FALSE;
			}
		} while ((c = cmp(pJ, ptr, key)) == 1);
		if (c != -1 && c != 0) {
			return FALSE;
		}
		if (pI >= pJ) {
			break;
		}
		memSwap(pI, pJ, size);
	}
	memSwap(pI, ptr, size);
	/* sort subfiles */
	if (!dsQuickSortR(pLeft, pI - size, size, cmp, key) ||
 		!dsQuickSortR(pI + size, pRight, size, cmp, key)) {
 		return FALSE;
 	}
 	return TRUE;
}
