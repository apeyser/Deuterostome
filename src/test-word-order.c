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
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#if ! NO_ENDIAN_HDR
#include ENDIAN_HDR
#endif //NO_ENDIAN_HDR

#if ! NO_ENDIAN_HDR && __FLOAT_WORD_ORDER
#if __FLOAT_WORD_ORDER != __BYTE_ORDER
#error "Can't handle float word order __FLOAT_WORD_ORDER"
#endif //__FLOAT_WORD_ORDER
#else //! NO_ENDIAN_HDR
#warning "Confirm that float word order = word order"
#endif //! NO_ENDIAN_HDR

int main(void) {return 0;}
