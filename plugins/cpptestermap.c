#include "cpptestermap.h"
#include "cpptestercode.h"
#include "newfunc.h"

#include <stdio.h>
#include <stdlib.h>

int runtester(int times, int max) 
{
		int i, j;
		int ret;

		if ((ret = init())) return ret;
		
		for (j = 0; j < times; j++) {
				for (i = 0; i < max; i++) 
						if ((ret = addElem(i))) return ret;
				for (i = 0; i < max; i++)
						if ((ret = removeElem())) return ret;
				for (i = 0; i < max; i++)
						if ((ret = addElem(i+max))) return ret;
				for (i = 0; i < max; i++)
						if ((ret = removeElem())) return ret;
				if ((ret = resetElems())) return ret;
		}

		if ((ret = fini())) return ret;
		if (leaked()) return 3;

		return 0;
}


