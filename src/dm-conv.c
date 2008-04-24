#include "dm.h"
#include "dm-swapbytes.h"

/* ======================== move frame(s) =============================
  NOTA BENE: this implies FRAMEBYTES = n*8 for sake of speed!
*/

void moveframe(B *socket, B *dest)
{
  unsigned int i;
  int64_t *s,*d;

  s = (int64_t *)socket; d = (int64_t *)dest;
  for (i=0;i<FRAMEBYTES/sizeof(int64_t);++i)
    *(d++) = *(s++);
}

void moveframes(B *socket, B *dest, P n)
{
  unsigned int i;
  int64_t *s,*d;

  s = (int64_t*)socket; d = (int64_t*)dest;
  for (; n>0; n--) 
    for (i=0;i<FRAMEBYTES/sizeof(int64_t);++i) 
      *(d++) = *(s++);
}

/* ========================== move block ==============================

These move blocks of different data sizes among aligned locations     */

void moveB(B *socket, B *dest, P n)
{
  for (; n>0; n--) *(dest++) = *(socket++);
}

void moveW(W *socket, W *dest, P n)
{ 
  for (; n>0; n--) *(dest++) = *(socket++);
}

void moveL32(L32 *socket, L32 *dest, P n)
{ 
  for (; n>0; n--) *(dest++) = *(socket++);
}

void moveL64(L64 *socket, L64 *dest, P n)
{
  for (; n>0; n--) *(dest++) = *(socket++);
}

void moveLBIG(LBIG* socket, LBIG* dest, P n)
{
  moveL64(socket, dest, n);
}

void moveS(S *socket, S *dest, P n)
{ 
  for (; n>0; n--) *(dest++) = *(socket++);
}

void moveD(D *socket, D *dest, P n)
{ 
  for (; n>0; n--) *(dest++) = *(socket++);
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
