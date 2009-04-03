#ifndef DM_SWAPBYTES_H
#define DM_SWAPBYTES_H

#include "dm.h"

DM_INLINE_STATIC void swapbytes(B* arr, B n1, B n2) {
  B temp = arr[n2];
  arr[n2] = arr[n1];
  arr[n1] = temp;
}

DM_INLINE_STATIC void swap2bytes(B* arr) {swapbytes(arr, 0, 1);}

DM_INLINE_STATIC void swap4bytes(B* arr) {
  swapbytes(arr, 0, 3);
  swapbytes(arr, 1 ,2);
}

DM_INLINE_STATIC void swap8bytes(B* arr) {
  swapbytes(arr, 0, 7);
  swapbytes(arr, 1, 6);
  swapbytes(arr, 2, 5);
  swapbytes(arr, 3, 4);
}

#define swaplongbytes(arr, isnonnative) do {      \
    P r = swaplongbytes_int((B*) (arr), isnonnative);	\
    if (r != OK) return r;                        \
  } while (0)

#define checkovf(hi, lo) do {                   \
    if (hi)                                     \
      if ((hi) != -1 || (lo) >= 0)              \
        return LONG_OVF;                        \
  } while (0)

#define signextend(hi, lo) do {                 \
    (hi) = (lo) < 0 ? -1 : 0;                   \
  } while (0)

DM_INLINE_STATIC P swaplongbytes_int(B* arr, B isnonnative) {
  L32* warr = (L32*) arr;
#if DM_WORDS_BIGENDIAN && DM_HOST_IS_32_BIT
  if (HASNATIVEBITS(isnonnative)) {// && ! HASNATIVEENDIAN
    swap4bytes(arr);
  } 
  else if (HASNATIVEENDIAN(isnonnative)) { // && ! HASNATIVEBITS
    checkovf(warr[0], warr[1]);
    warr[0] = warr[1];
  }
  else { // !HASNATIVEBITS && !HASNATIVEENDIAN
    swap4bytes(arr);
    checkovf(warr[1], warr[0]);
  }
#elif ! DM_WORDS_BIGENDIAN && DM_HOST_IS_32_BIT
  if (HASNATIVEBITS(isnonnative)) {// && ! HASNATIVEENDIAN
    swap4bytes(arr);
  }
  else if (HASNATIVEENDIAN(isnonnative)) { // && ! HASNATIVEBITS
    checkovf(warr[1], warr[0]);
  } 
  else { // !HASNATIVEBITS && !HASNATIVEENDIAN
    swap4bytes(arr+4);
    checkovf(warr[0], warr[1]);
    warr[0] = warr[1];
  }
#elif DM_WORDS_BIGENDIAN // && ! DM_HOST_IS_32_BIT
  if (HASNATIVEBITS(isnonnative)) {// && ! HASNATIVEENDIAN
    swap8bytes(arr);
  }
  else { // ! HASNATIVEBITS
    if (! HASNATIVEENDIAN(isnonnative)) // && ! HASNATIVEBITS
      swap4bytes(arr);
    warr[1] = warr[0];
    signextend(warr[0], warr[1]);
  }
#else //!DM_WORDS_BIGENDIAN && ! DM_HOST_IS_32_BIT
  if (HASNATIVEBITS(isnonnative)) {// && ! HASNATIVEENDIAN
    swap8bytes(arr);
  }
  else { // ! HASNATIVEBITS
    if (! HASNATIVEENDIAN(isnonnative)) // && ! HASNATIVEBITS
      swap4bytes(arr);
    signextend(warr[1], warr[0]);
  }
#endif //DM_WORDS_BIGENDIAN && DM_HOST_IS_32_BIT
  
  return OK;
}

#endif //DM_SWAPBYTES_H
