#include "dm.h"
#include "dm-swapbytes.h"

/* ======================== move frame(s) =============================
  NOTA BENE: this implies FRAMEBYTES = n*8 for sake of speed!
*/

void moveframe(B *source, B *dest)
{
  unsigned int i;
  int64_t *s,*d;

  s = (int64_t *)source; d = (int64_t *)dest;
  for (i=0;i<FRAMEBYTES/sizeof(int64_t);++i)
    *(d++) = *(s++);
}

void moveframes(B *source, B *dest, P n)
{
  unsigned int i;
  int64_t *s,*d;

  s = (int64_t*)source; d = (int64_t*)dest;
  for (; n>0; n--) 
    for (i=0;i<FRAMEBYTES/sizeof(int64_t);++i) 
      *(d++) = *(s++);
}

/* ========================== move block ==============================

These move blocks of different data sizes among aligned locations     */

void moveB(B *source, B *dest, P n)
{
  for (; n>0; n--) *(dest++) = *(source++);
}

void moveW(W *source, W *dest, P n)
{ 
  for (; n>0; n--) *(dest++) = *(source++);
}

void moveL32(L32 *source, L32 *dest, P n)
{ 
  for (; n>0; n--) *(dest++) = *(source++);
}

void moveL64(L64 *source, L64 *dest, P n)
{
  for (; n>0; n--) *(dest++) = *(source++);
}

void moveLBIG(LBIG* source, LBIG* dest, P n)
{
  moveL64(source, dest, n);
}

void moveS(S *source, S *dest, P n)
{ 
  for (; n>0; n--) *(dest++) = *(source++);
}

void moveD(D *source, D *dest, P n)
{ 
  for (; n>0; n--) *(dest++) = *(source++);
}

P deendian_frame(B *frame, B isnonnative) {
  if (! isnonnative) return OK;

  if (HASNATIVEENDIAN(isnonnative)) switch (CLASS(frame)) {
    case NUM: return OK;
    case NAME: return OK;
  };

  switch (CLASS(frame)) {
    case NULLOBJ: case BOOL: case MARK:
      return OK;

    case NUM:
      switch (TYPE(frame)) {
        case BYTETYPE:
          return OK;
        case WORDTYPE:
          swap2bytes(NUM_VAL(frame));
          return OK;
        case LONG32TYPE: 
          swap4bytes(NUM_VAL(frame));
        return OK;
        case LONG64TYPE:
          swap8bytes(NUM_VAL(frame));
        return OK;
        case SINGLETYPE:
          swap4bytes(NUM_VAL(frame));
        return OK;
        case DOUBLETYPE:
          swap8bytes(NUM_VAL(frame));
          return OK;
      };
      return OPD_TYP;

    case OP:
      return OPD_CLA;

    case NAME: {
      int i;
      for (i = 2; i < FRAMEBYTES; i+= 2)
        swap2bytes(frame+i);
      return OK;
    }

    case ARRAY: case LIST:
      swaplongbytes(&VALUE_BASE(frame), isnonnative);
      swaplongbytes(&ARRAY_SIZE(frame), isnonnative);
      return OK;

    case DICT:
      swaplongbytes(&VALUE_BASE(frame), isnonnative);
      swaplongbytes(&DICT_NB(frame), isnonnative);
      //CURR=NB
      return OK;

    case BOX:
      swaplongbytes(&VALUE_BASE(frame), isnonnative);
      swaplongbytes(&BOX_NB(frame), isnonnative);
      return OK;

    default:
      return OPD_CLA;
  };
}
