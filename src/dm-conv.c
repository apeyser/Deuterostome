/*

Copyright 2011 Alexander Peyser & Wolfgang Nonner

This file is part of Deuterostome.

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

*/
#include "dm.h"
#include "dm-swapbytes.h"

/* ======================== move frame(s) =============================
  NOTA BENE: this implies FRAMEBYTES = n*8 for sake of speed!
*/

void moveframe(const B *restrict source, B *restrict dest)
{
  unsigned int i;
  const LBIG *restrict s = (const LBIG*) source;
  LBIG *restrict d = (LBIG*) dest;

  for (i=0;i<FRAMEBYTES/sizeof(LBIG);++i)
    *(d++) = *(s++);
}

void moveframes(const B *restrict source, B *restrict dest, P n)
{
  unsigned int i;
  const LBIG *restrict s = (const LBIG*) source;
  LBIG *restrict d = (LBIG*) dest;

  for (; n>0; n--) 
    for (i=0;i<FRAMEBYTES/sizeof(LBIG);++i) 
      *(d++) = *(s++);
}

/* ========================== move block ==============================

These move blocks of different data sizes among aligned locations     */

void moveB(const B *restrict source, B *restrict dest, P n)
{
  for (; n>0; n--) *(dest++) = *(source++);
}

void moveW(const W *restrict source, W *restrict dest, P n)
{ 
  for (; n>0; n--) *(dest++) = *(source++);
}

void moveL32(const L32 *restrict source, L32 *restrict dest, P n)
{ 
  for (; n>0; n--) *(dest++) = *(source++);
}

void moveL64(const L64 *restrict source, L64 *restrict dest, P n)
{
  for (; n>0; n--) *(dest++) = *(source++);
}

void moveLBIG(const LBIG*restrict  source, LBIG *restrict  dest, P n)
{
  moveL64(source, dest, n);
}

void moveS(const S *restrict source, S *restrict dest, P n)
{ 
  for (; n>0; n--) *(dest++) = *(source++);
}

void moveD(const D *restrict source, D *restrict dest, P n)
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
